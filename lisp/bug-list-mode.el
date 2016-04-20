;;; bug-list-mode.el --- handle a list of bugs
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
;; TODO:
;;  - drop columns if none of the selected bugs has that field
;;  - calculate the column width based on buffer width and truncate
;;    column display contents, if necessary
;;  - remember cursor position when refreshing the buffer contents
;;  - add support for results over multiple pages:
;;    - add indicater that there are additional pages
;;    - allow jumping forward/backward
;;
;;; Code:

(require 'bug-rpc)
(require 'bug-search-common)
(require 'bug-common-functions)

(defvar bug-list-mode-map
  (let ((keymap (copy-keymap special-mode-map)))
    (define-key keymap (kbd "RET") 'bug--list-mode-select-bug)
    (define-key keymap "i"         'bug--list-mode-info)
    (define-key keymap "u"         'bug--list-mode-update-list)
    (define-key keymap "q"         'bug--list-mode-quit-window)
    keymap)
  "Keymap for BZ list mode")

(define-derived-mode bug-list-mode tabulated-list-mode "Bug list"
  "Operate on a list of bugs"
  )

(defun bug-list-show (query parsed &optional instance)
  "Display the result of a bug search returning a list of bugs"
  (bug-debug-log-time "bug-list-show")
  (let ((type (bug-instance-property :type instance)))
    (cond
     ((string= type "rally")
      (switch-to-buffer (format "*rally results: %s*" (pretty-kvs query))))
     (t (switch-to-buffer (format "*bugzilla results: %s*" (pretty-kvs query))))))
  (bug-list-mode)
  (make-local-variable 'bug---query)
  (setq bug---query query)
  (make-local-variable 'bug---instance)
  (setq bug---instance instance)
  (setq buffer-read-only nil)

  (let* ((list-columns (bug-list-columns instance))
         (bugs (mapcar (lambda (bug)
                         (bug-to-filtered-vector bug list-columns))
                       parsed)))
    ;; populate header
    ;; as all entries are strings list can be sorted by any column
    (setq tabulated-list-format
          (make-vector (length (bug-list-columns instance)) nil))
    (let ((count 0)
          (header-widths (bug-header-widths bugs list-columns)))
      (dolist (element list-columns)
        (aset tabulated-list-format count
              `(,(bug--list-format-header-field element instance)
                ,(cdr (assoc element header-widths)) t))
        (setq count (+ 1 count))))
    (tabulated-list-init-header)

    ;; populate list entries
    (dolist (element bugs)
      (add-to-list 'tabulated-list-entries `(nil ,element)))
    (tabulated-list-print t)
    (bug-debug-log-time "stop")))

(defun bug--list-format-header-field (header-field &optional instance)
  "Format a header field name for display, taking into account instance
specific field descriptions."
  (propertize
   (prin1-to-string (or
                     (bug--bug-get-field-property
                      (intern header-field) 'display_name instance)
                     header-field) t)
    'face 'bug-header-field))

(defun bug-to-filtered-vector (bug list-columns)
  "Extract fields listed in the header from bug and return a vector suitable
for inclusion in tabulated-list-entries"
  (let ((data (make-vector (length list-columns) ""))
        (count 0))
    (dolist (header-item list-columns)
      (let* ((field (assoc (intern header-item) (cdr bug)))
             (value (or (cdr field) ""))
             (bug-id (cdr (assoc (bug--uuid-field-name bug---instance) (cdr bug))))
             (formatted-string))
        (setq formatted-string
              (propertize
               (bug--bug-format-field-value field bug---instance)
               'bug-id bug-id))
        (aset data count formatted-string))
      (setq count (+ 1 count)))
    data))

(defun bug-header-widths (bugs list-columns)
  "Check the longest field for each header entries in a list of bug and return
an alist with (type . length) cells containing the longest length"
  (mapcar* (lambda (x y)
             `(,x . ,y))
           list-columns
           (reduce (lambda (l1 l2)
                     (mapcar* 'max l1 l2))
                   (mapcar (lambda (bug)
                             (mapcar (lambda (prop) (+ (length (format "%s" prop)) 5)) bug))
                           bugs))))

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

;; functions usually called through keybindings in bug-list-mode
;;;###autoload
(defun bug--list-mode-info ()
  "Display some information about thing at or near point

This is mostly useful for debugging text properties"
  (interactive)
  (let ((bug-id (get-text-property (point) 'bug-id)))
    (message
     (concat
      "ID = "
      (prin1-to-string bug-id)
      "; "
      ))))

;;;###autoload
(defun bug--list-mode-select-bug ()
  "Open the current bug from the list. The bug identifier is read from text
properties at point, or -- if that fails -- from the beginning of the current
line"
  (interactive)
  (let ((bug-id (or (get-text-property (point) 'bug-id)
                    (save-excursion
                      (forward-line 0)
                      (get-text-property (point) 'bug-id)))))
    (if bug-id
        (bug-open bug-id bug---instance)
      (message "No bug ID found. Misconfigured bug UUID property?"))))

;;;###autoload
(defun bug--list-mode-update-list ()
  "Update the list by running the original search query again"
  (interactive)
  (bug-do-search bug---query bug---instance))

;;;###autoload
(defun bug--list-mode-quit-window ()
  "Close the search result window"
  (interactive)
  (quit-window t))

(provide 'bug-list-mode)
;;; bug-list-mode.el ends here
