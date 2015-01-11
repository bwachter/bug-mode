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
                                                     (cdr (assoc 'values (gethash "resolution" bz-fields))))))))
    (bz-update bz-id `((status . "RESOLVED") (resolution . ,resolution)) bz-instance))
  (bz-get bz-id bz-instance))

;;;###autoload
(defun bz-bug-mode-update-bug ()
  "Update the bug by reloading it from Bugzilla"
  (interactive)
  (bz-get bz-id bz-instance))

(provide 'bz-bug-mode)
;;; bz-bug-mode.el ends here
