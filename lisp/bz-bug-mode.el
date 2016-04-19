;;; bz-bug-mode.el --- display a single Bugzilla bug
;;
;; Copyright (c) 2010-2015 bz-mode developers
;;
;; See the AUTHORS.md file for a full list:
;; https://raw.githubusercontent.com/bwachter/bz-mode/master/AUTHORS.md
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
;; This file is maintained at https://github.com/bwachter/bz-mode/
;; Check the git history for details.
;;
;;; Code:

(require 'bz-rpc)
(require 'bz-common-functions)

(defvar bz-bug-mode-map (let ((keymap (copy-keymap special-mode-map)))
                          (define-key keymap (kbd "RET") 'bz--bug-mode-open-attachment)
                          (define-key keymap "b"         'bz--bug-mode-browse-bug)
                          ;; TODO: change this to a 'change bug' popup
                          (define-key keymap "c"         'bz--bug-mode-create-comment)
                          (define-key keymap "d"         'bz--bug-mode-download-attachment)
                          (define-key keymap "r"         'bz--bug-mode-remember-bug)
                          ;; TODO: this should change to 'status change' instead of 'resolve'
                          (define-key keymap "s"         'bz--bug-mode-resolve-bug)
                          (define-key keymap "u"         'bz--bug-mode-update-bug)
                          keymap)
  "Keymap for BZ bug mode")

(define-derived-mode bz-bug-mode special-mode "Bugzilla bug"
  "Show a single Bugzilla bug"
  )

;;;###autoload
(defun bz-bug (id &optional instance)
  "Retrieve and show a single bug"
  (interactive
   (if current-prefix-arg
       (list
        (read-string "Bug ID: " nil nil t)
        (bz-query-instance))
     (list (read-string "Bug ID: " nil nil t))))
  (let* ((type (bz-instance-property :type instance))
         (bug-content (cond ((string= type "rally")
                             (bz--fetch-rally-bug id instance))
                            (t (bz--fetch-bz-bug id instance)))))
    (if bug-content (bz-bug-show id bug-content instance))))

(defun bz-bug-show (id bug &optional instance)
  "Display an existing bug buffer in bz-bug-mode"
  (bz-debug-log-time "bz-bug-show")
  (let ((type (bz-instance-property :type instance)))

    (cond ((string= type "rally")
           (switch-to-buffer (format "*rally bug: %s*"
                                     (cdr (assoc 'FormattedID bug)))))
          (t
           (switch-to-buffer (format "*bugzilla bug: %s*" (cdr (assoc 'id bug))))))

    (bz-bug-mode)
    (setq bug (sort bug (lambda (a b)(string< (car a)(car b)))))
    (make-local-variable 'bz-id)
    (setq bz-id id)
    (make-local-variable 'bz-bug)
    (setq bz-bug bug)
    (make-local-variable 'bz-instance)
    (setq bz-instance instance)
    (make-local-variable 'bz-changed-data)
    (setq bz-changed-data nil)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert
     (mapconcat
      (lambda (prop)
        (concat
         (bz--bug-format-field-name (car prop) instance)
         (bz--bug-format-field-value prop instance)))
      (filter (lambda (prop)
                (and (not (equal :json-false (bz--bug-get-field-property (car prop) 'is_visible)))
                     ;; TODO: implement retrieval of additional rally objects
                     ;;       until it's implemented just hide them to avoid
                     ;;       polluting the bug buffer
                     (not (equal 98 (bz--bug-get-field-property (car prop) 'type)))
                     (not (equal (cdr prop) nil))
                     (not (string-match "^[[:space:]]*$" (prin1-to-string (cdr prop))))
                     (not (string= (car prop) "internals")))) bug) "\n"))

    (unless (string= type "rally")
      ;; TODO: Rally has multiple objects which need to be loaded separately,
      ;;       the bugzilla style loading of attachements and comments won't
      ;;       scale for that.
      ;;       Additionally it'd be better to select the insertion points by
      ;;       using text properties.
      (bz-insert-hr)
      (insert "\nATTACHMENTS:\n")
      (bz-insert-hr)
      (insert "\nCOMMENTS:\n")
      (if bz-autoload-attachments
          (bz-get-attachments id instance))
      (if bz-autoload-comments
          (bz-get-comments id instance)))
    (goto-char 0)
    (setq buffer-read-only t)
    (bz-debug-log-time "stop")))

(defun bz--bug-format-html (html &optional base-url)
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

(defun bz--bug-format-field-name (field-name &optional instance)
  "Format a bug field name for display, taking into account instance
specific field descriptions."
  (propertize
   (concat
    (prin1-to-string (or
                      (bz--bug-get-field-property
                       field-name 'display_name instance)
                      `(,field-name)) t)
    ": ")
    'face 'bz-bug-field-description
    'bz-bug-field-name field-name))

(defun bz--bug-format-field-value (field &optional instance)
  "Format a bug field value for display, taking into account instance
specific field descriptions. Unlike bz--bug-format-field-name this function
requires both field name and content, therefore taking the complete cons
cell as argument"
  (let ((content-type (bz--bug-get-field-property
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
                (prin1-to-string (cdr (assoc '_refObjectName (cdr field)))))
        'face 'bz-bug-field-type-98))
      ((equal content-type 5)
       (propertize (bz--format-time-date (cdr field) t)
                   'face 'bz-bug-field-type-5))
      ((equal content-type 6)
       (propertize (prin1-to-string (cdr field) t)
                   'face 'bz-bug-field-type-6))
      ((equal content-type 98)
       ())
      ((equal content-type 99)
       (propertize (bz--bug-format-html (cdr field))
                   'face 'bz-bug-field-type-99))
      (t
       (prin1-to-string (cdr field) t)))
     'bz-bug-field-type content-type
     'bz-bug-field-name (car field))))

(defun bz--bug-get-field-property (field-name property &optional instance)
  "Return a property for a bug field from the field definition.

For example, to find the display name for the field 'foo' you could do
the following:
 (bz--bug-get-field-property 'foo 'display_name instance)"
  (cdr
   (assoc property
          (gethash (symbol-name field-name) (bz-get-fields instance)))))

(defun bz-update (id fields &optional instance)
  "Update fields in the bug on Bugzilla"
  (message (format "fields: %s" (append fields `((ids . ,id)))))
  (bz-rpc "Bug.update" (append fields `((ids . ,id))) instance))

(defun bz-get-comments (id &optional instance)
  "Request comments for a bug and add it to an existing(!) bug buffer
via bz-handle-comments-response"
  (bz-handle-comments-response id (bz-rpc "Bug.comments" `(("ids" . ,id)) instance)))

(defun bz-handle-comments-response (id response)
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

;; functions usually called through keybindings in bz-bug-mode
;;;###autoload
(defun bz--bug-mode-browse-bug ()
  "Open the current bug in browser"
  (interactive)
  (let ((url (concat (bz-instance-property :url bz-instance) "/show_bug.cgi?id=" bz-id)))
    (browse-url url)))

;;;###autoload
(defun bz--bug-mode-create-comment ()
  "Create a comment on the current bug"
  (interactive)
  (bz-comment bz-id bz-instance))

;;;###autoload
(defun bz--bug-mode-download-attachment ()
  "Download the current attachment to the home directory"
  (interactive)
  (w3m-download
   (bz-find-attachment-url bz-instance)
   (expand-file-name (concat "~/" (match-string 3)))))

;;;###autoload
(defun bz--bug-mode-open-attachment ()
  "Open the current attachment in the web browser"
  (interactive)
  (browse-url (bz-find-attachment-url bz-instance)))

;;;###autoload
;;;###autoload
(defun bz--bug-mode-remember-bug (list-name &optional id instance)
  "Remember the current bug in a local search"
  (interactive
   (if (and (boundp 'bz-id) (boundp 'bz-bug))
       (list
        (bz-query-remembered-lists))
     (list
      (bz-query-remembered-lists)
      (read-string "Bug: " nil nil t)
      (if current-prefix-arg (bz-query-instance)))))
  (let* ((instance (bz-instance-to-symbolp instance))
         (lists-for-instance (gethash instance bz-bug-remember-list))
         (list-entries (if lists-for-instance
                           (gethash list-name lists-for-instance)))
         (bz-id (if (boundp 'bz-id) bz-id id))
         (bz-instance (if (boundp 'bz-instance) bz-instance instance)))
    (add-to-list 'list-entries bz-id)
    (delete-dups list-entries)
    (if lists-for-instance
        (puthash list-name list-entries lists-for-instance)
      (let ((lists-for-instance (make-hash-table :test 'equal)))
        (puthash list-name list-entries lists-for-instance)
        (puthash instance lists-for-instance bz-bug-remember-list)
        ))
    (bz-write-data-file)))

;;;###autoload
(defun bz--bug-mode-resolve-bug ()
  "Resolve the current bug"
  (interactive)
  (let ((resolution (completing-read "resolution: "
                                     (filter (lambda (x)
                                               (> (length x) 0))
                                             (mapcar (lambda (x)
                                                       (cdr (assoc 'name x)))
                                                     (cdr (assoc 'values (gethash "resolution" (bz-get-fields bz-instance)))))))))
    (bz-update bz-id `((status . "RESOLVED") (resolution . ,resolution)) bz-instance))
  (bz-bug bz-id bz-instance))

;;;###autoload
(defun bz--bug-mode-update-bug ()
  "Update the bug by reloading it from Bugzilla"
  (interactive)
  (bz-bug bz-id bz-instance))

;; attachment handling functions
(defun bz-get-attachments (id &optional instance)
    "Request attachment details for a bug and add it to an existing(!) bug buffer
via bz-handle-attachments-response"
  (bz-handle-attachments-response id (bz-rpc "Bug.attachments" `(("ids" . ,id)) instance)))

(defun bz-handle-attachments-response (id response)
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

(defun bz-find-attachment-url (&optional instance)
  "Construct the URL required to download an attachment"
  (save-excursion
    (let ((end (re-search-forward "$" nil t)))
      (move-beginning-of-line nil)
      ;; FIXME: breaks if ; in filenames/descriptions.. heh
      (if (re-search-forward "^attachment \\([0-9]+\\): \\([^;]+\\); \\([^;]+\\);" end t)
          (format "%s/attachment.cgi?id=%s" (bz-instance-property :url instance) (match-string 1))
        (error "No attachment near point")))))

;; layout. should get dropped eventually
(defun bz-insert-hr ()
  (insert "\n")
  (insert-char ?- (floor (/ (window-width) 1.5)))
  (insert "\n"))

(provide 'bz-bug-mode)
;;; bz-bug-mode.el ends here
