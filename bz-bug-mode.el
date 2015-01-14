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

(defvar bz-bug-mode-map (let ((keymap (copy-keymap special-mode-map)))
                          (define-key keymap (kbd "RET") 'bz-bug-mode-open-attachment)
                          (define-key keymap "b"         'bz-bug-mode-browse-bug)
                          (define-key keymap "c"         'bz-bug-mode-create-comment)
                          (define-key keymap "d"         'bz-bug-mode-download-attachment)
                          (define-key keymap "r"         'bz-bug-mode-resolve-bug)
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
  (bz-handle-search-response id (bz-rpc "Bug.get" `(("ids" . ,id)) instance) instance)
  (bz-get-attachments id instance)
  (bz-get-comments id instance))

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
  (goto-char 0)
  (setq buffer-read-only t))

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

(provide 'bz-bug-mode)
;;; bz-bug-mode.el ends here
