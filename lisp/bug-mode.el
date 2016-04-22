;;; bug-mode.el --- display a single bug
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

(require 'bug-rpc)
(require 'bug-common-functions)

(defvar bug-mode-map
  (let ((keymap (copy-keymap special-mode-map)))
    (define-key keymap (kbd "RET") 'bug--bug-mode-open-attachment)
    (define-key keymap "b"         'bug--bug-mode-browse-bug)
    ;; TODO: change this to a 'change bug' popup
    (define-key keymap "c"         'bug--bug-mode-create-comment)
    (define-key keymap "d"         'bug--bug-mode-download-attachment)
    (define-key keymap "e"         'bug--bug-mode-edit-field)
    (define-key keymap "i"         'bug--bug-mode-info)
    (define-key keymap "r"         'bug--bug-mode-remember-bug)
    ;; TODO: this should change to 'status change' instead of 'resolve'
    (define-key keymap "s"         'bug--bug-mode-resolve-bug)
    (define-key keymap "u"         'bug--bug-mode-update-bug)
    (define-key keymap "q"         'bug--bug-mode-quit-window)
    keymap)
  "Keymap for BZ bug mode")

(define-derived-mode bug-mode special-mode "Bug"
  "Show a single bug"
  )

;;;###autoload
(defun bug-open (id &optional instance)
  "Retrieve and show a single bug"
  (interactive
   (if current-prefix-arg
       (list
        (read-string "Bug ID: " nil nil t)
        (bug-query-instance))
     (list (read-string "Bug ID: " nil nil t))))
  (let* ((type (bug-instance-property :type instance))
         (bug-content (cond ((string= type "rally")
                             (bug--fetch-rally-bug id instance))
                            (t (bug--fetch-bz-bug id instance)))))
    (if bug-content (bug-show bug-content instance))))

(defun bug-show (bug &optional instance)
  "Display an existing bug buffer in bug-mode"
  (bug-debug-log-time "bug-show")
  (let ((type (bug-instance-property :type instance))
        (tmp-bug-id))
    (cond ((string= type "rally")
           (setq tmp-bug-id (cdr (assoc 'FormattedID bug)))
           (switch-to-buffer (format "*rally bug: %s*"
                                     tmp-bug-id)))
          (t
           (setq tmp-bug-id (cdr (assoc 'id bug)))
           (switch-to-buffer (format "*bugzilla bug: %s*" tmp-bug-id))))

    (bug-mode)
    ;; the tmp-bug-id bit is needed as setting the mode clears buffer-local variables
    (make-local-variable 'bug---id)
    (setq bug---id tmp-bug-id)
    (setq bug (sort bug (lambda (a b)(string< (car a)(car b)))))
    (make-local-variable 'bug---data)
    (setq bug---data bug)
    (make-local-variable 'bug---instance)
    (setq bug---instance instance)
    (make-local-variable 'bug---changed-data)
    (setq bug---changed-data nil)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert
     (mapconcat
      (lambda (prop)
        (concat
         (bug--bug-format-field-name (car prop) instance)
         (bug--bug-format-field-value prop instance t)))
      (filter (lambda (prop)
                (and (not (equal :json-false (bug--bug-get-field-property (car prop) 'is_visible)))
                     ;; referenced objects are included as a list. If there's
                     ;; a `Count' property with value `0' it's safe to assume
                     ;; we don't need to retrieve it (might be rally only)
                     (not (and
                           (listp (cdr prop))
                           (equal 0 (cdr (assoc 'Count (cdr prop))))))
                     (not (equal (cdr prop) nil))
                     (not (string-match "^[[:space:]]*$" (prin1-to-string (cdr prop) t)))
                     (not (string= (car prop) "internals")))) bug) "\n"))

    (unless (string= type "rally")
      ;; TODO: Rally has multiple objects which need to be loaded separately,
      ;;       the bugzilla style loading of attachements and comments won't
      ;;       scale for that.
      ;;       Additionally it'd be better to select the insertion points by
      ;;       using text properties.
      (bug-insert-hr)
      (insert "\nATTACHMENTS:\n")
      (bug-insert-hr)
      (insert "\nCOMMENTS:\n")
      (if (and bug---id bug-autoload-attachments)
          (bug-get-attachments bug---id instance))
      (if (and bug---id bug-autoload-comments)
          (bug-get-comments bug---id instance)))
    (goto-char 0)
    (setq buffer-read-only t)
    (bug-debug-log-time "stop")))

(defun bug--bug-format-html (html &optional base-url)
  "Parse an HTML string and return it formatted suitable for inserting
into the buffer. If HTML parsing is not possible the unparsed HTML is
returned as string."
  (if (fboundp 'libxml-parse-html-region)
      (with-temp-buffer
        (insert html)
        (let ((parsed-html
               (libxml-parse-html-region (point-min) (point-max) base-url)))
          (with-temp-buffer
            (shr-insert-document parsed-html)
            (buffer-string))))
    html))

(defun bug--bug-format-field-name (field-name &optional instance)
  "Format a bug field name for display, taking into account instance
specific field descriptions."
  (propertize
   (concat
    (prin1-to-string (or
                      (bug--bug-get-field-property
                       field-name 'display_name instance)
                      `(,field-name)) t)
    ": ")
    'face 'bug-field-description
    'bug-field-name field-name))

;;;###autoload
(defun bug--bug-format-field-value (field &optional instance long)
  "Format a bug field value for display, taking into account instance
specific field descriptions. Unlike bug--bug-format-field-name this function
requires both field name and content, therefore taking the complete cons
cell as argument

If the optional parameter `long' is non-nil display functions output
is formatted to take more space"
  (let ((content-type (bug--bug-get-field-property
                       (car field) 'type instance)))
    (propertize
     (cond
      ((equal :json-false (cdr field))
       "No")
      ((equal :json-true (cdr field))
       "Yes")
      ;; some rally objects in a bug contain _refObjectName, which is
      ;; enough information to display -> just display that to save
      ;; an RPC call
      ;; TODO: save object attributes to allow querying the object
      ((and (listp (cdr field))
            (assoc '_refObjectName (cdr field)))
       (propertize
        (concat "-> "
                (prin1-to-string (cdr (assoc '_refObjectName (cdr field))) t))
        'face 'bug-field-type-98))
      ((equal content-type 5)
       (propertize (bug--format-time-date (cdr field) long)
                   'face 'bug-field-type-5))
      ((equal content-type 6)
       (propertize (prin1-to-string (cdr field) t)
                   'face 'bug-field-type-6))
      ((equal content-type 98)
       (propertize (prin1-to-string (cdr field) t)))
      ((equal content-type 99)
       (propertize (replace-regexp-in-string "[[:space:]]*$" ""
                                             (bug--bug-format-html (cdr field)))
                   'face 'bug-field-type-99))
      (t
       (prin1-to-string (cdr field) t)))
     'bug-field-type content-type
     'bug-field-name (car field))))

;;;###autoload
(defun bug--bug-get-field-property (field-name property &optional instance)
  "Return a property for a bug field from the field definition.

For example, to find the display name for the field 'foo' you could do
the following:
 (bug--bug-get-field-property 'foo 'display_name instance)"
  (cdr
   (assoc property
          (gethash (symbol-name field-name) (bug-get-fields instance)))))

(defun bug-update (id fields &optional instance)
  "Update fields in the bug on Bugzilla"
  (message (format "fields: %s" (append fields `((ids . ,id)))))
  (bug-rpc "Bug.update" (append fields `((ids . ,id))) instance))

(defun bug-get-comments (id &optional instance)
  "Request comments for a bug and add it to an existing(!) bug buffer
via bug-handle-comments-response"
  (bug-handle-comments-response id (bug-rpc "Bug.comments" `(("ids" . ,id)) instance)))

(defun bug-handle-comments-response (id response)
  "Add received comments into an existing bug buffer"
  (if (and
       (assoc 'result response)
       (assoc 'bugs (assoc 'result response)))
      (let* ((bugs (cdr (assoc 'bugs (assoc 'result response))))
             (comments (cdr (cadr (car bugs)))))
        (save-excursion
          (switch-to-buffer (format "*bugzilla bug: %s*" id))
          (setq buffer-read-only nil)
          (goto-char 0)
          (if (re-search-forward "^COMMENTS:$" nil t)
              (progn
                (delete-region (point) (point-max))
                (insert "\n")
                (insert (mapconcat (lambda (comment)
                                     (format "[Comment #%s] %s %s:\n%s"
                                             (cdr (assoc 'count comment))
                                             (cdr (assoc 'time comment))
                                             (cdr (assoc 'creator comment))
                                             (cdr (assoc 'text comment))))
                                   comments "\n\n"))
                (setq buffer-read-only t))
            (error "Could not find area for comments in buffer"))))))

;; functions usually called through keybindings in bug-mode
;;;###autoload
(defun bug--bug-mode-browse-bug ()
  "Open the current bug in browser"
  (interactive)
  (let ((url (concat (bug-instance-property :url bug---instance) "/show_bug.cgi?id=" bug---id)))
    (browse-url url)))

;;;###autoload
(defun bug--bug-mode-create-comment ()
  "Create a comment on the current bug"
  (interactive)
  (bug-comment bug---id bug---instance))

;;;###autoload
(defun bug--bug-mode-download-attachment ()
  "Download the current attachment to the home directory"
  (interactive)
  (w3m-download
   (bug-find-attachment-url bug---instance)
   (expand-file-name (concat "~/" (match-string 3)))))

;;;###autoload
(defun bug--bug-mode-open-attachment ()
  "Open the current attachment in the web browser"
  (interactive)
  (browse-url (bug-find-attachment-url bug---instance)))

;;;###autoload
(defun bug--bug-mode-edit-field ()
  "Edit the bug field at or near point"
  (interactive)
  ())

;;;###autoload
(defun bug--bug-mode-info ()
  "Display some information about thing at or near point

This is mostly useful for debugging text properties"
  (interactive)
  (let ((text-property (get-text-property (point) 'bug-field-name))
        (content-type (get-text-property (point) 'bug-field-type)))
    (message
     (concat
      "type = "
      (prin1-to-string content-type)
      "; "
      (prin1-to-string text-property)
      " = "
      (prin1-to-string (cdr (assoc text-property bug---data)))
      ))))

;;;###autoload
(defun bug--bug-mode-remember-bug (list-name &optional id instance)
  "Remember the current bug in a local search"
  (interactive
   (if (and (boundp 'bug---id) (boundp 'bug---data))
       (list
        (bug-query-remembered-lists))
     (list
      (bug-query-remembered-lists)
      (read-string "Bug: " nil nil t)
      (if current-prefix-arg (bug-query-instance)))))
  (let* ((instance (bug-instance-to-symbolp instance))
         (lists-for-instance (gethash instance bug-remember-list))
         (list-entries (if lists-for-instance
                           (gethash list-name lists-for-instance)))
         (bug-id (if (boundp 'bug---id) bug---id id)))
    (add-to-list 'list-entries bug-id)
    (delete-dups list-entries)
    (if lists-for-instance
        (puthash list-name list-entries lists-for-instance)
      (let ((lists-for-instance (make-hash-table :test 'equal)))
        (puthash list-name list-entries lists-for-instance)
        (puthash instance lists-for-instance bug-remember-list)
        ))
    (bug-write-data-file)))

;;;###autoload
(defun bug--bug-mode-resolve-bug ()
  "Resolve the current bug"
  (interactive)
  (let ((resolution
         (completing-read
          "resolution: "
          (filter
           (lambda (x)
             (> (length x) 0))
           (mapcar
            (lambda (x)
              (cdr (assoc 'name x)))
            (cdr (assoc 'values (gethash "resolution" (bug-get-fields bug---instance)))))))))
    (bug-update bug---id `((status . "RESOLVED") (resolution . ,resolution)) bug---instance))
  (bug-open bug---id bug---instance))

;;;###autoload
(defun bug--bug-mode-update-bug ()
  "Update the bug by reloading it from the bug tracker"
  (interactive)
  (bug-open bug---id bug---instance))

;;;###autoload
(defun bug--bug-mode-quit-window ()
  "Close the search result window"
  (interactive)
  ;; TODO: check if bug---changed-data is non-nil, and prompt about losing changes
  (quit-window t))

;; attachment handling functions
(defun bug-get-attachments (id &optional instance)
    "Request attachment details for a bug and add it to an existing(!) bug buffer
via bug-handle-attachments-response"
  (bug-handle-attachments-response id (bug-rpc "Bug.attachments" `(("ids" . ,id)) instance)))

(defun bug-handle-attachments-response (id response)
  "Add received attachment info into an existing bug buffer"
  (if (and
       (assoc 'result response)
       (assoc 'bugs (assoc 'result response)))
      (let* ((bugs (cdr (assoc 'bugs (assoc 'result response))))
             (attachments (cdr (car bugs))))
        (save-excursion
          (switch-to-buffer (format "*bugzilla bug: %s*" id))
          (setq buffer-read-only nil)
          (goto-char 0)
          (if (re-search-forward "^ATTACHMENTS:$" nil t)
              (progn
                (insert "\n")
                (insert (mapconcat (lambda (attachment)
                                     (format "attachment %s: %s; %s; %s"
                                             (cdr (assoc 'id attachment))
                                             (cdr (assoc 'description attachment))
                                             (cdr (assoc 'file_name attachment))
                                             (cdr (assoc 'content_type attachment))))
                                   attachments "\n"))
                (setq buffer-read-only t))
            (error "Could not find area for attachments in buffer"))))))

(defun bug-find-attachment-url (&optional instance)
  "Construct the URL required to download an attachment"
  (save-excursion
    (let ((end (re-search-forward "$" nil t)))
      (move-beginning-of-line nil)
      ;; FIXME: breaks if ; in filenames/descriptions.. heh
      (if (re-search-forward "^attachment \\([0-9]+\\): \\([^;]+\\); \\([^;]+\\);" end t)
          (format "%s/attachment.cgi?id=%s" (bug-instance-property :url instance) (match-string 1))
        (error "No attachment near point")))))

;; layout. should get dropped eventually
(defun bug-insert-hr ()
  (insert "\n")
  (insert-char ?- (floor (/ (window-width) 1.5)))
  (insert "\n"))

(provide 'bug-mode)
;;; bug-mode.el ends here
