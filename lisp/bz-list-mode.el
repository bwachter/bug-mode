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

(require 'bz-rpc)
(require 'bz-search-common)
(require 'bz-common-functions)

(defvar bz-list-mode-map (let ((keymap (copy-keymap special-mode-map)))
                           (define-key keymap (kbd "RET") 'bz-list-mode-select-bug)
                           (define-key keymap "u"         'bz-list-mode-update-list)
                           keymap)
  "Keymap for BZ list mode")

(define-derived-mode bz-list-mode special-mode "Bugzilla list"
  "Operate on a list of bugzilla items"
  )

(defun bz-list-show (query parsed &optional instance)
  "Display the result of a Bugzilla search returning a list of bugs"
  (bz-debug-log-time "bz-list-show")
  (let ((type (bz-instance-property :type instance)))
    (cond
     ((string= type "rally")
      (switch-to-buffer (format "*rally results: %s*" (pretty-kvs query))))
     (t (switch-to-buffer (format "*bugzilla results: %s*" (pretty-kvs query))))))
  (bz-list-mode)
  (make-local-variable 'bz-query)
  (setq bz-query query)
  (make-local-variable 'bz-instance)
  (setq bz-instance instance)
  (setq buffer-read-only nil)
  (erase-buffer)
  (let* ((list-columns (bz-list-columns instance))
         (bugs (mapcar (lambda (bug)
                         (bz-bug-filtered-and-sorted-properties bug list-columns))
                         parsed)))
    (let* ((header-widths (bz-header-widths bugs list-columns))
           (header-item-length (/ (window-width) (length list-columns))))
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
                         list-columns "")))
      (insert (mapconcat 'bz-bug-format bugs "\n")))
    (goto-char 0)
    (setq buffer-read-only t)
    (bz-debug-log-time "stop")))

;; layout/output control
;; TODO: properly document those
(defun bz-header-widths (bugs list-columns)
  (mapcar* (lambda (x y)
             `(,x . ,y))
           list-columns
           (reduce (lambda (l1 l2)
                     (mapcar* 'max l1 l2))
                   (mapcar (lambda (bug)
                             (mapcar (lambda (prop) (+ (length (format "%s" (cdr prop))) 5)) bug))
                           bugs))))

(defun bz-bug-sort-properties (bug list-columns)
  (sort bug
        (lambda (a b)
          (< (position (symbol-name (car a)) list-columns :test 'string=)
             (position (symbol-name (car b)) list-columns :test 'string=)))))

(defun bz-bug-format (bug)
  (mapconcat (lambda (property)
               (let ((hw (cdr (assoc (symbol-name (car property)) header-widths))))
                 (format (format "%%-%d.%ds"
                                 hw
                                 hw) (cdr property))))
             bug " "))

(defun bz-bug-filtered-and-sorted-properties (bug list-columns)
  (bz-bug-sort-properties (filter (lambda (property)
                                    (member (symbol-name (car property)) list-columns))
                                  bug) list-columns))

(defun pretty-kvs (kvs)
  (if (hash-table-p kvs)
      (setq kvs (ht-to-alist kvs)))
  (mapconcat (lambda (kv)
               (format "%s: %s" (car kv) (cdr kv)))
             kvs ", "))

(defun ht-to-alist (ht)
  (let (result)
    (maphash (lambda (key val) (setq result (cons `(,key . ,val) result))) ht)
    result))

;; functions usually called through keybindings in bz-list-mode
;;;###autoload
(defun bz-list-mode-select-bug ()
  "Open the bug at point in the list"
  (interactive)
  (save-excursion
    (move-beginning-of-line nil)
    (let ((type (bz-instance-property :type bz-instance)))
      (cond
       ((string= type "rally")
        (if (re-search-forward "^\\(\\(F\\|DE\\|TA\\|US\\)[0-9]+\\)" nil t)
            (bz-search (match-string 1) bz-instance)))
       (t (if (re-search-forward "^\\([0-9]+\\)" nil t)
              (bz-bug (match-string 1) bz-instance)))))))

;;;###autoload
(defun bz-list-mode-update-list ()
  "Update the list by running the original search query again"
  (interactive)
  (bz-do-search bz-query bz-instance))

(provide 'bz-list-mode)
;;; bz-list-mode.el ends here
