;;; bz-list-mode.el --- handle a list of bugzilla bugs
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

(defvar bz-list-mode-map (let ((keymap (copy-keymap special-mode-map)))
                           (define-key keymap (kbd "RET") 'bz-list-mode-select-bug)
                           (define-key keymap "u"         'bz-list-mode-update-list)
                           keymap)
  "Keymap for BZ list mode")

(define-derived-mode bz-list-mode special-mode "Bugzilla list"
  "Operate on a list of bugzilla items"
  )

(defun bz-list-mode-show (query parsed &optional instance)
  "Display the result of a Bugzilla search returning a list of bugs"
  (switch-to-buffer (format "*bugzilla results: %s*" (pretty-kvs query)))
  (bz-list-mode)
  (make-local-variable 'bz-query)
  (setq bz-query query)
  (make-local-variable 'bz-instance)
  (setq bz-instance instance)
  (setq buffer-read-only nil)
  (erase-buffer)
  (let* ((bugs (mapcar 'bz-bug-filtered-and-sorted-properties parsed)))
    (let* ((headers bugzilla-columns)
           (header-widths (bz-header-widths bugs))
           (header-item-length (/ (window-width) (length headers))))
      (setq header-line-format
            (let ((column 0)
                  (header '()))
              (mapconcat (lambda (heading)
                           (let ((result (concat
                                          (propertize " " 'display (list 'space :align-to column)
                                                      'face 'fixed-pitch)
                                          heading)))
                             (setq column (+ column (cdr (assoc heading header-widths)) 1))
                             result))
                         headers "")))
      (insert (mapconcat 'bz-bug-format bugs "\n")))
    (goto-char 0)
    (setq buffer-read-only t)))

;;;###autoload
(defun bz-list-mode-select-bug ()
  "Open the bug at point in the list"
  (interactive)
  (save-excursion
    (move-beginning-of-line nil)
    (if (re-search-forward "^\\([0-9]+\\)" nil t)
        (bz-get (match-string 1) bz-instance)
      (error "WTF? No id in beginning?"))))

;;;###autoload
(defun bz-list-mode-update-list ()
  "Update the list by running the original search query again"
  (interactive)
  (bz-do-search bz-query bz-instance))

(provide 'bz-list-mode)
;;; bz-list-mode.el ends here