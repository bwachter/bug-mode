;;; bug-common-functions.el --- simple functions shared by several modules -*- lexical-binding: t; -*-
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
(require 'project)
(require 'cl-lib)
(require 'vtable)

(defun filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun bug--backend-feature (instance &optional feature)
  "Without optional argument returns all features of a backend. Otherwise
checks with `memq' if `feature' is present"
  (let* ((features (bug--backend-function "bug--backend-%s-features"
                                          nil instance)))
    (if feature
        (if (and (keywordp feature)
                 (string-prefix-p "experimental-" (substring (symbol-name feature) 1)))
            ;; Strip ":experimental-" prefix and check base feature if experimental enabled
            (and bug-experimental
                 (memq (intern (substring (symbol-name feature) (length "experimental-"))) features))
          ;; Normal (non-experimental) feature check
          (memq feature features))
      ;; Return all features, possibly filtering experimental if bug-experimental is false
      (if bug-experimental
          features
        ;; filter out experimental features when disabled
        (cl-remove-if (lambda (f)
                        (and (keywordp f)
                             (string-prefix-p "experimental-"
                                              (substring (symbol-name f) 1))))
                      features)))))

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
  (let ((function-name (intern (format format-string
                               (prin1-to-string (bug--backend-type instance) t)))))
    (if (fboundp function-name)
        (funcall function-name args instance)
      (error (format "Backend function '%s' not defined"
                     function-name)))))

(defun bug--backend-type (instance)
  "Return the backend type for the given bug tracker instance"
  (let ((type (bug--instance-property :type instance)))
    (if (equal nil type)
        (error (format "Instance '%s' is missing the backend type"
                       (prin1-to-string instance t)))
      type)))

(defun bug--position-in-array (data field field-value)
  "Search for a bug with a value `field-value' in field `field' in a query
response. For example, to check if a rally user story 815 exists in the results
the call would look like this:

 (bug--position-in-array results \='FormattedID \"US815\")"
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
    (if (= (length completions) 1)
        (car completions)
      (completing-read "Instance: " completions nil t))))

;;;;;;
;; Instance management functions (bug-instances-list support)

(defun bug--get-project-instance ()
  "Get the bug tracker instance for the current project.

Checks `bug-project-instances-alist' against the current project root.
Supports both exact string matches and regexp patterns.
Returns the instance symbol if found, nil otherwise."
  (when-let ((project (or (and (fboundp 'project-current) (project-current))
                          default-directory)))
    (let ((root (if (consp project)
                    (project-root project)
                  (if (stringp project)
                      project
                    default-directory))))
      (or
       ;; Exact match
       (cdr (assoc root bug-project-instances-alist))
       ;; Regexp match
       (cl-loop for (pattern . instance) in bug-project-instances-alist
                when (string-match-p pattern root)
                return instance)))))

(defun bug--get-active-instance ()
  "Get the currently active instance following resolution hierarchy.

Resolution order:
1. Buffer-local bug---instance (if set and buffer exists)
2. Project-specific instance (via `bug-project-instances-alist')
3. bug-active-instance (if set)
4. bug-default-instance (backward compatibility)

Returns instance symbol or nil if no instance is explicitly configured.
When nil is returned, callers should prompt the user to select an instance."
  (or
   ;; 1. Buffer-local instance (only in bug-related buffers)
   (and (boundp 'bug---instance) bug---instance)
   ;; 2. Project-specific instance
   (bug--get-project-instance)
   ;; 3. Active instance (when set)
   bug-active-instance
   ;; 4. Default instance (backward compatibility)
   bug-default-instance))

(defun bug--check-instance-access (instance)
  "Check if INSTANCE can be accessed based on active instance restrictions.

Returns t if access is allowed, nil otherwise.
Shows a warning if the buffer-local instance differs from active instance."
  (cond
   ;; No active instance restriction - allow all
   ((null bug-active-instance) t)
   ;; Same as active instance - allow
   ((eq instance bug-active-instance) t)
   ;; Buffer-local instance different from active - allow with warning
   ((and (boundp 'bug---instance)
         bug---instance
         (not (eq bug---instance bug-active-instance)))
    (message "Warning: This buffer uses instance '%s', but active instance is '%s'"
             bug---instance bug-active-instance)
    t)
   ;; Trying to use non-active instance for new operation - block
   (t
    (error "Instance '%s' is not active. Current active instance: '%s'. Use `bug-switch-instance' to change."
           instance bug-active-instance))))

;;;###autoload
(defun bug-switch-instance (instance-name)
  "Switch to INSTANCE-NAME as the active bug tracker instance.

Only this instance will be accessible for new operations until switched again.
Existing buffers with different instances continue to work but show warnings.

INSTANCE-NAME can come from either `bug-instances-list' or `bug-instance-plist'."
  (interactive (list (bug--select-instance-from-all)))
  (setq bug-active-instance (bug--instance-to-symbolp instance-name))
  (message "Switched to instance: %s" bug-active-instance))

;;;###autoload
(defun bug-activate-instance (instance-name)
  "Activate INSTANCE-NAME without interactive prompt.
See `bug-switch-instance' for details."
  (setq bug-active-instance (bug--instance-to-symbolp instance-name))
  (message "Activated instance: %s" bug-active-instance))

;;;###autoload
(defun bug-deactivate-instance ()
  "Deactivate the current active instance, allowing all instances to be used."
  (interactive)
  (let ((was-active bug-active-instance))
    (setq bug-active-instance nil)
    (if was-active
        (message "Deactivated instance: %s (all instances now accessible)" was-active)
      (message "No active instance was set"))))

;;;###autoload
(defun bug-list-instances ()
  "Display all configured bug tracker instances with their status."
  (interactive)
  (let ((buffer (get-buffer-create "*Bug Instances*"))
        (all-instances (bug--get-all-instances)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Bug Tracker Instances\n" 'face 'bold)
                (make-string 60 ?=) "\n\n")
        (if bug-active-instance
            (insert (propertize (format "Active Instance: %s\n"
                                       bug-active-instance)
                               'face 'warning))
          (insert "Active Instance: (none - all instances accessible)\n"))
        (if (null all-instances)
            (progn
              (insert "\nNo instances configured.\n\n"
                      "Configure instances via:\n"
                      "  M-x customize-variable RET bug-instances-list\n"
                      "  M-x customize-variable RET bug-instance-plist\n"))
          (insert "\n")
          (insert "Use 's' on a row to switch to that instance.\n")
          (insert "Use 'd' to deactivate instance restrictions.\n\n")
          (make-vtable
           :columns '((:name "Name" :width 20)
                      (:name "Type" :width 15)
                      (:name "Status" :width 12)
                      (:name "URL/Details" :width 40))
           :objects all-instances
           :getter (lambda (inst-info column vtable)
                     (let* ((name (car inst-info))
                            (plist (cdr inst-info))
                            (type (plist-get plist :type))
                            (url (or (plist-get plist :url) "(Rally API)"))
                            (status (cond
                                    ((eq name bug-active-instance) "[ACTIVE]")
                                    ((eq name bug-default-instance) "[DEFAULT]")
                                    (t ""))))
                       (pcase (vtable-column vtable column)
                         ("Name" (symbol-name name))
                         ("Type" (symbol-name type))
                         ("Status" status)
                         ("URL/Details" url))))
           :actions `("s" ,(lambda (inst-info)
                             (bug-activate-instance (car inst-info))
                             (bug-list-instances))
                      "d" ,(lambda (_inst-info)
                             (bug-deactivate-instance)
                             (bug-list-instances))))))
      (special-mode)
      (goto-char (point-min)))
    (pop-to-buffer buffer)))

(defun bug--select-instance-from-all ()
  "Select an instance from both bug-instances-list and bug-instance-plist.

Returns the instance symbol in its original format (keyword for plist,
regular symbol for instances-list), or nil if canceled."
  (let* ((all-instances (bug--get-all-instances))
         (names (mapcar (lambda (x) (symbol-name (car x))) all-instances))
         (selected (completing-read "Instance: " names nil t)))
    ;; Intern the selected string, preserving keyword/symbol format
    (when (and selected (not (string-empty-p selected)))
      (intern selected))))

(defun bug--get-all-instances ()
  "Get all configured instances from both bug-instances-list and
bug-instance-plist.

Returns an alist of (NAME . PLIST) pairs."
  (append
   ;; From bug-instances-list (already in correct format)
   bug-instances-list
   ;; From bug-instance-plist (extract cons cells)
   (cl-loop for record in bug-instance-plist
            when (and (not (listp record)) (keywordp record))
            collect (cons record (plist-get bug-instance-plist record)))))

(defun bug--query-remembered-lists ()
  "Query for the name of a locally remembered bug list. Completion is seeded
with names of lists across all bug tracker instances"
  (let ((instance-keys) (category-keys))
    ;; first read the instance keys from highlevel hash
    (maphash #'(lambda (key _value)
                 (push key instance-keys)) bug-remember-list)
    (dolist (instance instance-keys)
      (let ((lists-for-instance (gethash instance bug-remember-list)))
        ;; now read all keys from the lists for each instance
        (maphash #'(lambda (key _value)
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
                           (bz-mapped-field (bug--rpc-bz-rpc-map-field key)))
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
(defun bug--get-field-property (field-name property instance &optional _object)
  "Return a property for a bug field from the field definition.

For example, to find the display name for the field `foo' you could do
the following:
 (bug--get-field-property \='foo \='display_name instance)"
  (cdr
   (assoc property
          (gethash (symbol-name field-name) (bug--get-fields instance)))))

(defvar bug--api-key-cache (make-hash-table :test 'equal)
  "Cache for API keys loaded from files.
Keys are (instance . file-path) cons cells, values are the trimmed file contents.")

(defun bug--read-api-key-file (file-path instance)
  "Read API key from FILE-PATH, trimming whitespace.
Supports transparent GPG decryption for .gpg files.
Results are cached per instance."
  (let ((cache-key (cons instance file-path)))
    (or (gethash cache-key bug--api-key-cache)
        (let* ((expanded-path (expand-file-name file-path))
               (key (when (file-exists-p expanded-path)
                      (with-temp-buffer
                        (insert-file-contents expanded-path)
                        (string-trim (buffer-string))))))
          (when key
            (puthash cache-key key bug--api-key-cache))
          key))))

(defun bug--instance-property (property instance)
  "Return the value for a PROPERTY of the instance INSTANCE, or the default
instance if INSTANCE is empty.

Checks both `bug-instances-list' and `bug-instance-plist'.

Special handling:
- :api-key - If not present, checks for :api-key-file and reads the key from that file
- :url - For Rally instances, falls back to bug-rally-url if not specified"
  (let* ((instance (bug--instance-to-symbolp instance))
         ;; Try bug-instances-list first, then bug-instance-plist
         (property-list (or (cdr (assoc instance bug-instances-list))
                           (plist-get bug-instance-plist instance))))
    (cond
     ;; Special handling for API key - check for file-based key
     ((equal property :api-key)
      (or (plist-get property-list :api-key)
          (when-let ((key-file (plist-get property-list :api-key-file)))
            (bug--read-api-key-file key-file instance))))
     ;; Special handling for Rally URL fallback
     ((and (equal property :url)
           (equal 'rally (bug--backend-type instance)))
      (let ((rally-url (plist-get property-list property)))
        (or rally-url bug-rally-url)))
     ;; Default: just get the property
     (t (plist-get property-list property)))))

(defun bug--instance-to-symbolp (instance)
  "Make sure that the instance handle is symbolp; returns default instance
if instance is nil.

When instance is nil, uses the instance resolution hierarchy via
`bug--get-active-instance' which checks:
1. Buffer-local bug---instance
2. Project-specific instance
3. bug-active-instance
4. bug-default-instance

If no instance is explicitly configured, prompts the user to select one
from the available instances in bug-instances-list or bug-instance-plist."
  (let* ( ; check if instance already is correct type, if not, convert string to symbol
         (instance (if instance
                       (cond ((symbolp instance) instance)
                             ((stringp instance) (intern instance))
                             (t instance))
                     ;; Use the full resolution hierarchy when instance is nil
                     (or (bug--get-active-instance)
                         ;; If no explicit instance, prompt user to select one
                         (when (or bug-instances-list bug-instance-plist)
                           (bug--select-instance-from-all))))))
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
