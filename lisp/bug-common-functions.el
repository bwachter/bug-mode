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
(require 'bug-instance)
(require 'cl-lib)

(defun filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun bug--add-or-replace (data key value)
  "Quick and dirty test helper, don't use otherwise."
  (if (assoc key data)
      (progn
        (setf (cdr (assoc key data)) value)
        data)
    (push (cons key value) (nthcdr 0 data))))

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

(defun bug--query-instance (&optional required-feature)
  "Prompt for an instance, optionally filtered by `required-feature'.

Reads instances from both `bug-instances-list' and `bug-instance-plist'.
When `required-feature' is a keyword, only instances whose backend declares
that feature are offered.

Returns the instance symbol directly (without prompting) when exactly one
qualifying instance exists."
  (let* ((all (bug--instance-get-all))
         (matching
          (if required-feature
              (cl-remove-if-not
               (lambda (inst)
                 (condition-case nil
                     (bug--instance-backend-feature (car inst) required-feature)
                   (error nil)))
               all)
            all))
         (names (mapcar (lambda (x) (symbol-name (car x))) matching)))
    (cond
     ((null names)
      (if required-feature
          (error "No instances found supporting %s" required-feature)
        (error "No bug tracker instances configured")))
     ((= (length names) 1)
      (intern (car names)))
     (t
      (intern (completing-read "Instance: " names nil t))))))

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

(defun bug--cache-put-timed (key value ttl instance)
  "Cache `key' with `value' for `instance', expiring after `ttl' seconds.

Use `bug--cache-get-valid' to retrieve values stored this way."
  (bug--cache-put key (cons value (+ (float-time) ttl)) instance))

(defun bug--cache-get-valid (key instance)
  "Return cached value for `key' in `instance' if not yet expired, else nil.

Expects values stored by `bug--cache-put-timed'."
  (let ((cached (bug--cache-get key instance)))
    (when (and cached (consp cached) (> (cdr cached) (float-time)))
      (car cached))))

(defun bug-cache-clear (&optional instance)
  "Clear the cache, either globally, or for a specific instance"
  (interactive
   (if current-prefix-arg
       (list (bug--instance-to-symbolp (bug--query-instance)))))
  (if instance
      (cl-remf bug--cache instance)
    (setq bug--cache nil)))

(defun bug-cache-clear-matching (key-prefix &optional instance)
  "Clear cache entries whose key name starts with `key-prefix'

With `instance', only clear within that instance's cache; otherwise
clear matching entries across all known instances.

When called interactively, prompts for the prefix string.
With prefix argument, also prompts for the instance."
  (interactive
   (list (read-string "Cache key prefix: ")
         (when current-prefix-arg
           (bug--instance-to-symbolp (bug--query-instance)))))
  (let ((remove-matching (lambda (entries)
                           (cl-remove-if
                            (lambda (e)
                              (string-prefix-p key-prefix (symbol-name (car e))))
                            entries))))
    (if instance
        (setq bug--cache
              (plist-put bug--cache instance
                         (funcall remove-matching (plist-get bug--cache instance))))
      (let ((plist bug--cache)
            (new-cache nil))
        (while plist
          (let* ((inst (pop plist))
                 (entries (pop plist)))
            (setq new-cache
                  (plist-put new-cache inst (funcall remove-matching entries)))))
        (setq bug--cache new-cache))))
  (message "Cleared cache entries matching '%s'%s" key-prefix
           (if instance (format " for instance %s" instance) "")))

(defun bug--get-fields (instance &optional object)
  "Download fields used by this bug tracker instance or returns them from cache"
  (let* ((cache-key (if object
                        (intern (concat "fields-" (prin1-to-string object t)))
                      'fields))
         (instance (bug--instance-to-symbolp instance))
         (fields (if (bug--cache-get cache-key instance) nil
                   (bug--instance-backend-function "bug--rpc-%s-get-fields" object instance)))
         (field-hash (make-hash-table :test 'equal)))
    (if fields
        (progn
          (mapc (lambda (field)
                  (let* ((key (cdr (assoc 'name field)))
                         (mapped-field (bug--instance-backend-function-optional "bug--rpc-%s-map-field" key instance)))
                    ;; workaround for missing or oddly named fields in
                    ;; Bugzillas field list
                    (if (and mapped-field
                             (not (gethash mapped-field field-hash)))
                        (puthash mapped-field field field-hash))
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
    (bug--instance-backend-function "bug--%s-field-name" field-name instance)))

(defun bug--object-friendly-id (object-data instance)
  "Return the friendly ID string from an object reference alist, or nil.

`object-data' is an alist like ((_ref . url) (_refObjectName . name) ...).
Uses the backend-specific field name for :bug-friendly-id (e.g.
FormattedID for Rally, _display_id for GitHub, id for Bugzilla)."
  (let ((fid-field (bug--field-name :bug-friendly-id instance)))
    (when fid-field
      (cdr (assoc fid-field object-data)))))

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

(defun bug--list-columns (instance &optional object)
  "Read the list headers for a bugtracker instance.

If the given instance does not have a :list-columns property defaults
are used.
"
  (if (bug--instance-property :list-columns instance)
      (bug--instance-property :list-columns instance)
    (bug--instance-backend-function "bug--%s-list-columns" object instance)))

;;;;;;
;; functions suitable as defaults for use from modes keymaps

;;;###autoload
(defun bug--mode-default-quit-window ()
  "Close the search result window"
  (interactive)
  (quit-window t))

(defun bug--get-update-id (instance)
  "Get the appropriate ID for update operations."
  (bug--instance-backend-function "bug--backend-%s-get-update-id" nil instance))

(defun bug-update (id fields instance)
  "Update fields in a bug using the backend-specific update function.

`id' is the backend-specific bug identifier, as returned by `bug--get-update-id'
`fields' is an alist of field names and values to update
`instance' is the bug tracker instance

Returns the updated bug data from the backend."
  (message "Updating bug %s with fields: %s" id fields)
  (bug--instance-backend-function "bug--update-%s-bug" (list id fields) instance))

;;;;;;
;; Section header functions

(defun bug--insert-section-header (section-name &optional display-label)
  "Insert a section header for SECTION-NAME into the current buffer.

Ensures the header is always preceded by a blank line regardless of what
the preceding content ends with.  The label is propertized with
`bug-section-header' face and a `bug-section' text property set to
SECTION-NAME (a symbol).  DISPLAY-LABEL overrides the default label,
which is the capitalized symbol name.

`rear-nonsticky' on the header text prevents the properties from bleeding
into content inserted immediately after the header.

Use `bug--find-section-content-start' to locate the insertion point
when filling a section asynchronously."
  (let ((label (or display-label (capitalize (symbol-name section-name)))))
    ;; Start on a fresh line if the preceding content did not end with one
    (unless (or (bobp) (eq (char-before) ?\n))
      (insert "\n"))
    ;; Blank line before the header for visual separation
    (insert "\n")
    (insert (propertize label
                        'face 'bug-section-header
                        'bug-section section-name
                        'rear-nonsticky t))
    (insert "\n")))

(defun bug--find-section-content-start (section-name)
  "Return the buffer position just after the SECTION-NAME header line.

Searches for the section by its `bug-section' text property rather than
by text content, so it is not fooled by content that contains the section
label.  Returns nil when the section is not present in the current buffer."
  (let* ((hstart (text-property-any (point-min) (point-max)
                                    'bug-section section-name))
         (hend   (when hstart
                   (or (text-property-not-all hstart (point-max)
                                              'bug-section section-name)
                       (point-max)))))
    (when hend
      ;; Skip the newline inserted after the label text
      (if (and (< hend (point-max)) (eq (char-after hend) ?\n))
          (1+ hend)
        hend))))

(provide 'bug-common-functions)
;;; bug-common-functions.el ends here
