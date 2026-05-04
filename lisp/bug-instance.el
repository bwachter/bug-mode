;;; bug-list-mode.el --- deal with bug backend instances -*- lexical-binding: t; -*-
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
;; This file handles the lower level instance specific code, including defining
;; the methods for invoking backend functions.
;;
;;; Code:

(require 'vtable)
(require 'transient)
(require 'project)
(require 'bug-custom)

(defun bug--instance-backend-function (format-string &optional args instance)
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

 (bug--instance-backend-function \"some-%s-function\" nil instance)
"
  (let ((function-name (intern (format format-string
                                       (prin1-to-string (bug--instance-backend-type instance) t)))))
    (if (fboundp function-name)
        (funcall function-name args instance)
      (error (format "Backend function '%s' not defined"
                     function-name)))))

(defun bug--instance-backend-function-optional (format-string &optional args instance)
  "Call a backend specific function, selected based on the backend specified by
the configuration for `instance'. Returns nil if the function is not defined.

Like `bug--instance-backend-function' but returns nil instead of throwing an
error when the function doesn't exist."
  (let ((function-name (intern (format format-string
                                       (prin1-to-string (bug--instance-backend-type instance) t)))))
    (when (fboundp function-name)
      (funcall function-name args instance))))

(defun bug--instance-backend-type (instance)
  "Return the backend type for the given bug tracker instance"
  (let ((type (bug--instance-property :type instance)))
    (if (equal nil type)
        (error (format "Instance '%s' is missing the backend type"
                       (prin1-to-string instance t)))
      type)))

(defun bug--instance-backend-feature (instance &optional feature)
  "Without optional argument returns all features of a backend. Otherwise
checks with `memq' if `feature' is present"
  (let* ((features (bug--instance-backend-function "bug--backend-%s-features"
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

(defclass bug-instance-prefix (transient-prefix)
  ((current-instance-name :initarg :current-instance-name :initform nil)
   (current-instance-type :initarg :current-instance-type :initform nil))
  "Custom transient class tracking three specific bug attributes.")

;;; bytecompiler stubs for functions in the transient (and only those)
;;  make sure they can be autoloaded!

(declare-function bug-list-projects "bug-project")
(declare-function bug-rally-subscription "bug-rally-subscription")
(declare-function bug-create "bug-mode")

(transient-define-prefix bug-instance-mode-menu ()
  "Transient for bug-instance-mode"
  :class 'bug-instance-prefix
  :init-value (lambda (obj)
                (let ((entry (vtable-current-object)))
                  (oset obj current-instance-name (car entry))
                  (oset obj current-instance-type (plist-get (cdr entry) :type))))
  [[:description
    (lambda ()
      (let ((obj (transient-prefix-object)))
        (format "Instance%s"
                (if-let ((name (oref obj current-instance-name)))
                    (format " %s" name)
                  ""))))
    ("a" "Activate instance" (lambda ()
                               (interactive)
                               (let ((obj (transient-prefix-object)))
                                 (bug-instance-activate (oref obj current-instance-name))
                                 (bug-list-instances)))
     :if (lambda ()
           (let ((obj (transient-prefix-object)))
             (oref obj current-instance-name))))
    ("d" "Deactivate instance" (lambda ()
                                 (interactive)
                                 (bug-instance-deactivate)
                                 (bug-list-instances)))]
   ["Interact"
    ("i"  "Info"  bug--bug-mode-info)
    ;; offer creation, unless point is on an instance not supporting creation
    ("c"  "Create bug" (lambda ()
                         (interactive)
                         (let* ((obj (transient-prefix-object))
                                (instance-name (oref obj current-instance-name)))
                           (bug-create instance-name)))
     :if (lambda ()
                (let* ((obj (transient-prefix-object))
                       (instance-name (oref obj current-instance-name)))
                  (or (not instance-name)
                      (bug--instance-backend-feature instance-name :create)))))]
   ["Rally"
    :if (lambda ()
          (let* ((obj (transient-prefix-object))
                 (instance-name (oref obj current-instance-name)))
            (and instance-name
                 (equal 'rally (bug--instance-backend-type instance-name)))))
    ("s" "Subscription" (lambda ()
                          (interactive)
                          (let ((obj (transient-prefix-object)))
                            (bug-rally-subscription (oref obj current-instance-name)))))]])

(defvar bug-instance-mode-map
  (let ((keymap (copy-keymap special-mode-map)))
    (define-key keymap bug-menu-key #'bug-instance-mode-menu)
    keymap)
  "Keymap for bug instance mode")

(define-derived-mode bug-instance-mode special-mode "Bug instances"
  "Mode for a list of bug instances")

;;;###autoload
(defun bug-list-instances ()
  "Display all configured bug tracker instances with their status."
  (interactive)
  (let ((buffer (get-buffer-create "*Bug Instances*"))
        (all-instances (bug--instance-get-all)))
    (with-current-buffer buffer
      (bug-instance-mode)
      (when (bound-and-true-p flyspell-mode)
        (flyspell-mode -1))
      (display-line-numbers-mode -1)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if bug-active-instance
            (insert (propertize (format "\nActive Instance: %s\n\n"
                                        bug-active-instance)
                                'face 'warning))
          (insert "\nActive Instance: (none - all instances accessible)\n\n"))
        (if (null all-instances)
            (progn
              (insert "\nNo instances configured.\n\n"
                      "Configure instances via:\n"
                      "  M-x customize-variable RET bug-instances-list\n"
                      "  M-x customize-variable RET bug-instance-plist\n"))
          (make-vtable
           :face 'bug-header-field
           :columns '((:name "Name" :width 20)
                      (:name "Type" :width 15)
                      (:name "Status"  :width 12)
                      (:name "URL/Details" :width 40))
           :objects all-instances
           :formatter (lambda (value _column _vtable)
                        (propertize (format "%s" value) 'face 'bug-list-item))
           :getter (lambda (inst-info column vtable)
                     (let* ((name (car inst-info))
                            (plist (cdr inst-info))
                            (type (plist-get plist :type))
                            (url (or (bug--instance-property :url name) ""))
                            (status (cond
                                     ;; TODO, make that look nicer. also, properly
                                     ;; handle default
                                     ((eq name bug-active-instance) "[ACTIVE]")
                                     ((eq name bug-default-instance) "[DEFAULT]")
                                     (t ""))))
                       (pcase (vtable-column vtable column)
                         ("Name" (symbol-name name))
                         ("Type" (symbol-name type))
                         ("Status" status)
                         ("URL/Details" url))))
           ;; TODO, we probably should also check if the instance has project support
           :actions `("RET" ,(lambda (inst-info)
                                     (bug-list-projects (car inst-info)))))
          (goto-char (point-max))))
      (goto-char (point-min)))
    (pop-to-buffer buffer)))

;;;###autoload
(defun bug-instance-activate (instance-name)
  "Activate INSTANCE-NAME without interactive prompt.
See `bug-instance-switch' for details."
  (setq bug-active-instance (bug--instance-to-symbolp instance-name))
  (message "Activated instance: %s" bug-active-instance))

;;;###autoload
(defun bug-instance-deactivate ()
  "Deactivate the current active instance, allowing all instances to be used."
  (interactive)
  (let ((was-active bug-active-instance))
    (setq bug-active-instance nil)
    (if was-active
        (message "Deactivated instance: %s (all instances now accessible)" was-active)
      (message "No active instance was set"))))

;;;###autoload
(defun bug-instance-switch (instance-name)
  "Switch to INSTANCE-NAME as the active bug tracker instance.

Only this instance will be accessible for new operations until switched again.
Existing buffers with different instances continue to work but show warnings.

INSTANCE-NAME can come from either `bug-instances-list' or `bug-instance-plist'."
  (interactive (list (bug--instance-select-from-all)))
  (setq bug-active-instance (bug--instance-to-symbolp instance-name))
  (message "Switched to instance: %s" bug-active-instance))

(defun bug--instance-get-all ()
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

(defun bug--instance-select-from-all ()
  "Select an instance from both bug-instances-list and bug-instance-plist.

Returns the instance symbol in its original format (keyword for plist,
regular symbol for instances-list), or nil if canceled."
  (let* ((all-instances (bug--instance-get-all))
         (names (mapcar (lambda (x) (symbol-name (car x))) all-instances))
         (selected (completing-read "Instance: " names nil t)))
    ;; Intern the selected string, preserving keyword/symbol format
    (when (and selected (not (string-empty-p selected)))
      (intern selected))))

(defun bug-instance-get-active ()
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


(defun bug--instance-check-access (instance)
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
    (error "Instance '%s' is not active. Current active instance: '%s'. Use `bug-instance-switch' to change."
           instance bug-active-instance))))

(defun bug--instance-property (property instance)
  "Return the value for a PROPERTY of the instance INSTANCE, or the default
instance if INSTANCE is empty.

Checks both `bug-instances-list' and `bug-instance-plist'.

Special handling:
- :api-key - If not present, checks for :api-key-file and reads the key from
             that file
- :url - For Rally instances, falls back to bug-rally-url if not specified"
  (let* ((instance (bug--instance-to-symbolp instance))
         ;; bug-instances-list uses plain symbols; bug-instance-plist uses keywords
         (plain-sym (if (keywordp instance)
                        (intern (substring (symbol-name instance) 1))
                      instance))
         (keyword-sym (if (keywordp instance)
                          instance
                        (intern (concat ":" (symbol-name instance)))))
         (property-list (or (cdr (assoc plain-sym bug-instances-list))
                            (plist-get bug-instance-plist keyword-sym))))
    (cond
     ;; Special handling for API key - check for file-based key
     ((equal property :api-key)
      (or (plist-get property-list :api-key)
          (when-let ((key-file (plist-get property-list :api-key-file)))
            (bug--read-api-key-file key-file instance))))
     ;; For :url, fall back to a backend-provided default if not explicitly configured
     ((equal property :url)
      (or (plist-get property-list property)
          (bug--instance-backend-function-optional "bug--backend-%s-default-url" nil instance)))
     ;; Default: just get the property
     (t (plist-get property-list property)))))

(defun bug--instance-to-symbolp (instance)
  "Make sure that the instance handle is symbolp; returns default instance
if instance is nil.

When instance is nil, uses the instance resolution hierarchy via
`bug-instance-get-active' which checks:
1. Buffer-local bug---instance
2. Project-specific instance
3. bug-active-instance
4. bug-default-instance

If no instance is explicitly configured, prompts the user to select one
from the available instances in bug-instances-list or bug-instance-plist."
  (let* ( ; check if instance already is correct type, if not, convert string to symbol
         (instance (if instance
                       (cond ((symbolp instance) instance)
                             ((stringp instance)
                              (intern (if (string-prefix-p ":" instance)
                                          instance
                                        (concat ":" instance))))
                             (t instance))
                     ;; Use the full resolution hierarchy when instance is nil
                     (or (bug-instance-get-active)
                         ;; If no explicit instance, prompt user to select one
                         (when (or bug-instances-list bug-instance-plist)
                           (bug--instance-select-from-all))))))
    instance))

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

(defvar bug--api-key-cache (make-hash-table :test 'equal)
  "Cache for API keys loaded from files.

Keys are (instance . file-path) cons cells, while values are the trimmed
file contents.")

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

(provide 'bug-instance)

;;; bug-instance.el ends here
