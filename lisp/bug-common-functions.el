;;; bug-common-functions.el --- simple functions shared by several modules
;;
;; Copyright (c) 2010-2015 bug-mode developers
;;
;; See the AUTHORS.md file for a full list:
;; https://raw.githubusercontent.com/bwachter/bug-mode/master/AUTHORS.md
;;
;; Keywords: tools
;;
;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
;; for more details. http://www.gnu.org/copyleft/gpl.html
;;
;;; History:
;;
;; This file is maintained at https://github.com/bwachter/bug-mode/
;; Check the git history for details.
;;
;;; Code:

(require 'bug-persistent-data)
(require 'cl-lib)

(defun filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun bug--backend-function (format-string &optional args instance)
  "Call a backend specific function, selected based on the backend specified by
the configuration for `instance'

The `format-string' needs to contain exactly one placeholder for a string (%s),
which will get replaced by a backend identifier. The backend functions need to
be defined with two arguments, `args' and `instance'. If more than one argument
is required they should be passed as list in `args'.

Example usage:

(defun some-bz-function (&optional args instance)
  (message \"Bugzilla\"))

(defun some-rally-function (&optional args instance)
  (message \"Rally\"))

(bug--backend-function \"some-%s-function\" nil instance)
"
  (let ((func))
    (fset 'func
          (intern (format format-string
                          (prin1-to-string (bug--backend-type instance) t))))
    (func args instance)))

(defun bug--backend-type (instance)
  "Return the backend type for the given bug tracker instance"
  (let ((type (bug--instance-property :type instance)))
    (if (equal nil type)
        'bz
      type)))

(defun bug--position-in-array (data field field-value)
  "Search for a bug with a value 'field-value' in field 'field' in a query
response. For example, to check if a rally user story 815 exists in the results
the call would look like this:

 (bug--position-in-array results 'FormattedID \"US815\")"
  (let ((pos))
    (let ((count (- (length data) 1 )))
      (while (>= count 0)
        (if (string= field-value (cdr (assoc field (aref data count))))
            (progn
              (setq pos count)
              (setq count 0)))
        (setq count (- count 1))))
    pos))

(defun bug--query-instance ()
  "Query for a bug tracker instance, providing completion with the instances
configured in bug-instance-plist. Returns the entered bug tracker instance.

Instance name only needs to be entered enough to get a match."
  (let ((completions
         (cl-remove-if nil
                    (cl-loop for record in bug-instance-plist collect
                             (unless (listp record)
                               (replace-regexp-in-string "^:" "" (prin1-to-string record)))))))
    (completing-read "Instance: " completions nil t)))

(defun bug--query-remembered-lists ()
  "Query for the name of a locally remembered bug list. Completion is seeded
with names of lists across all bug tracker instances"
  (let ((instance-keys) (category-keys))
    ;; first read the instance keys from highlevel hash
    (maphash #'(lambda (key value)
                 (push key instance-keys)) bug-remember-list)
    (dolist (instance instance-keys)
      (let ((lists-for-instance (gethash instance bug-remember-list)))
        ;; now read all keys from the lists for each instance
        (maphash #'(lambda (key value)
                     (push key category-keys)) lists-for-instance)))
    (delete-dups category-keys)
    (completing-read "List name: " category-keys nil nil)))

;;;;;;
;; caching functions

(defvar bug--cache nil)

(defun bug--cache-put (key value instance)
  "Cache a key/value pair for a specific instance"
  (let* ((instance (bug--instance-to-symbolp instance))
         (tmp-alist (plist-get bug--cache instance)))
    (if (assoc key tmp-alist)
        (setf (cdr (assoc key tmp-alist)) value)
      (setq bug--cache
            (plist-put bug--cache instance
                       (push (cons key value) tmp-alist))))))

(defun bug--cache-get (key instance)
  "Return a cached value for a specific instance"
  (let ((instance (bug--instance-to-symbolp instance)))
    (cdr (assoc key (plist-get bug--cache instance)))))

(defun bug-cache-clear (&optional instance)
  "Clear the cache, either globally, or for a specific instance"
  (interactive
   (if current-prefix-arg
       (list (bug--instance-to-symbolp (bug--query-instance)))))
  (if instance
      (cl-remf bug--cache instance)
    (setq bug--cache nil)))

(defun bug--get-fields (instance &optional object)
  "Download fields used by this bug tracker instance or returns them from cache"
  (let* ((cache-key (if object
                        (intern (concat "fields-" (prin1-to-string object t)))
                      'fields))
         (instance (bug--instance-to-symbolp instance))
         (fields (if (bug--cache-get cache-key instance) nil
                   (bug--backend-function "bug--rpc-%s-get-fields" object instance)))
         (field-hash (make-hash-table :test 'equal)))
    (if fields
        (progn
          (mapc (lambda (field)
                    (let* ((key (cdr (assoc 'name field)))
                           (bz-mapped-field (bug--rpc-bz-map-field key)))
                      ;; workaround for missing or oddly named fields in
                      ;; Bugzillas field list
                      (if (and bz-mapped-field
                               (not (gethash bz-mapped-field field-hash)))
                          (puthash bz-mapped-field field field-hash))
                      (puthash key field field-hash)))
                  (cdr (car (cdr (car fields)))))
          (bug--cache-put cache-key field-hash instance)
          ))
    (bug--cache-get cache-key instance)))

(defun bug--field-name (field-name instance)
  "Return the instance-specific internal field name for `field-name'.

Field names currently handled this way are:
- :bug-uuid -- the unique ID of the bug
- :bug-friendly-id -- the ID to be presented to the user
- :bug-summary -- the short bug summary/description

For very special instances the backend specific types may be overridden by
setting those values in the instance configuration.
"
  (if (bug--instance-property field-name instance)
      (bug--instance-property field-name instance)
    (bug--backend-function "bug--%s-field-name" field-name instance)))

;; TODO: - pass in object, and first check for property in object, and if not
;;       found, check generic one
(defun bug--get-field-property (field-name property instance &optional object)
  "Return a property for a bug field from the field definition.

For example, to find the display name for the field 'foo' you could do
the following:
 (bug--get-field-property 'foo 'display_name instance)"
  (cdr
   (assoc property
          (gethash (symbol-name field-name) (bug--get-fields instance)))))

(defun bug--instance-property (property instance)
  "Return the value for a PROPERTY of the instance INSTANCE, or the default
instance if INSTANCE is empty"
  (let* ((instance(bug--instance-to-symbolp instance))
         (property-list (plist-get bug-instance-plist instance)))
    (if (and (equal property :url)
             (equal 'rally (bug--backend-type instance)))
        (let ((rally-url (plist-get property-list property)))
          (or rally-url bug-rally-url))
    (plist-get property-list property))))

(defun bug--instance-to-symbolp (instance)
  "Make sure that the instance handle is symbolp; returns default instance
if instance is nil"
  (let* ( ; check if instance already is correct type, if not, check if it starts with :
          ; if it does, just convert, otherwise prepend : and assume all is fine now
          ; bug-default-instance is always assumed to be correct
         (instance (if instance
                       (cond ((symbolp instance) instance)
                             ((string-match "^:" instance) (intern instance))
                             (t (intern (concat ":" instance))))
                     bug-default-instance)))
    instance))

(defun bug--list-columns (instance &optional object)
  "Read the list headers for a bugtracker instance.

If the given instance does not have a :list-columns property defaults
are used.
"
  (if (bug--instance-property :list-columns instance)
      (bug--instance-property :list-columns instance)
    (bug--backend-function "bug--%s-list-columns" object instance)))

;;;;;;
;; functions suitable as defaults for use from modes keymaps

;;;###autoload
(defun bug--mode-default-quit-window ()
  "Close the search result window"
  (interactive)
  (quit-window t))

(provide 'bug-common-functions)
;;; bug-common-functions.el ends here
