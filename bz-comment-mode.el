;;; bz-comment-mode.el --- handle commenting on a Bugzilla bug
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

(defvar bz-comment-mode-map (let ((keymap (copy-keymap text-mode-map)))
                          (define-key keymap "\C-c\C-c"         'bz-comment-commit)
                          keymap)
  "Keymap for BZ comment mode")

(define-derived-mode bz-comment-mode text-mode "Bugzilla bug"
  "Handle commenting on a Bugzilla bug"
  )

;; TODO: make sure that a comment gets comitted to the bug on the right instance
(let ((fields '((status . "RESOLVED") (resolution . "FIXED"))))
  (append fields '((id . "123"))))
;; as that only should be run in the context of a bug the local
;; variable bz-instance should be valid there, and instance
;; query is not needed
(defun bz-comment-commit ()
  (interactive)
  (if (not (string= major-mode "bz-comment-mode"))
      (error "not visisting a bugzilla comment buffer"))
  (let ((params (make-hash-table :test 'equal)))
    (puthash "id" bz-id params)
    (save-excursion
      (goto-char 0)
      (while (re-search-forward "^\\([^:]*\\): ?\\(.*\\)$" nil t)
        (puthash (match-string 1) (match-string 2) params))
      (re-search-forward "^[^\n]" nil t)
      (move-beginning-of-line nil)
      (puthash "comment" (buffer-substring (point) (point-max)) params)
      (let ((result (bz-rpc "Bug.add_comment" params bz-instance)))
        (message (format "comment id: %s" (cdr (cadr (car result)))))
        (kill-buffer (current-buffer))))
    (bz-bug bz-id bz-instance)))

;; TODO: send to the right instance
(defun bz-comment (id &optional instance)
  (interactive "nid:")
  (switch-to-buffer (format "*bugzilla add comment: %s*" id))
  (bz-comment-mode)
  (make-local-variable 'bz-id)
  (setq bz-id id)
  (make-local-variable 'bz-instance)
  (setq bz-instance instance)
  (erase-buffer)
  ;;(insert "is_private: false\n")
  (insert "hours_worked: 0.0\n\n")
  (goto-char (point-max)))

(provide 'bz-comment-mode)
;;; bz-comment-mode.el ends here
