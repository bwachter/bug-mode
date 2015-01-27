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
                          (define-key keymap (kbd "RET") 'bz-bug-mode-open-attachment)
                          (define-key keymap "b"         'bz-bug-mode-browse-bug)
                          (define-key keymap "c"         'bz-bug-mode-create-comment)
                          (define-key keymap "d"         'bz-bug-mode-download-attachment)
                          (define-key keymap "r"         'bz-bug-mode-remember-bug)
                          ;; TODO: this should change to 'status change' instead of 'resolve'
                          (define-key keymap "s"         'bz-bug-mode-resolve-bug)
                          (define-key keymap "u"         'bz-bug-mode-update-bug)
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
  (let ((search-response (bz-rpc "Bug.get" `(("ids" . ,id)) instance)))
    (if (and (assoc 'result search-response)
             (assoc 'bugs (assoc 'result search-response)))
        (let ((bugs (cdr (assoc 'bugs (assoc 'result search-response)))))
          (cond
           ((= (length bugs) 0)
            (message (concat "Bug " id " not found.")))
           ((= (length bugs) 1)
            (bz-bug-show id (aref bugs 0) instance))
           (t (message "You should never see this message")))))))

;; TODO: only called via direct search, and does not load comments. Merge with bz-bug?
(defun bz-bug-show (id bug &optional instance)
  "Display an existing bugzilla bug buffer in bz-bug-mode"
  (switch-to-buffer (format "*bugzilla bug: %s*" (cdr (assoc 'id bug))))
  (bz-bug-mode)
  (make-local-variable 'bz-id)
  (setq bz-id id)
  (make-local-variable 'bz-bug)
  (setq bz-bug bug)
  (make-local-variable 'bz-instance)
  (setq bz-instance instance)
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert (mapconcat (lambda (prop)
                       (format "%s: %s"
                               (or (cdr (assoc 'display_name (gethash (symbol-name (car prop)) (bz-get-fields instance))))
                                   (car prop))
                               (cdr prop)))
                     (filter (lambda (prop)
                               (not (string= (car prop) "internals"))) bug) "\n"))
  (bz-insert-hr)
  (insert "\nATTACHMENTS:\n")
  (bz-insert-hr)
  (insert "\nCOMMENTS:\n")
  (bz-get-attachments id instance)
  (bz-get-comments id instance)
  (goto-char 0)
  (setq buffer-read-only t))

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
(defun bz-bug-mode-browse-bug ()
  "Open the current bug in browser"
  (interactive)
  (let ((url (concat (bz-instance-property :url bz-instance) "/show_bug.cgi?id=" bz-id)))
    (browse-url url)))

;;;###autoload
(defun bz-bug-mode-create-comment ()
  "Create a comment on the current bug"
  (interactive)
  (bz-comment bz-id bz-instance))

;;;###autoload
(defun bz-bug-mode-download-attachment ()
  "Download the current attachment to the home directory"
  (interactive)
  (w3m-download
   (bz-find-attachment-url bz-instance)
   (expand-file-name (concat "~/" (match-string 3)))))

;;;###autoload
(defun bz-bug-mode-open-attachment ()
  "Open the current attachment in the web browser"
  (interactive)
  (browse-url (bz-find-attachment-url bz-instance)))

;;;###autoload
(defun bz-bug-mode-remember-bug (list-name &optional id instance)
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
(defun bz-bug-mode-resolve-bug ()
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
(defun bz-bug-mode-update-bug ()
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
