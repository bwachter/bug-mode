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
                           (define-key keymap (kbd "RET") 'bz--list-mode-select-bug)
                           (define-key keymap "i"         'bz--list-mode-info)
                           (define-key keymap "u"         'bz--list-mode-update-list)
                           (define-key keymap "q"         'bz--list-mode-quit-window)
                           keymap)
  "Keymap for BZ list mode")

(define-derived-mode bz-list-mode tabulated-list-mode "Bugzilla list"
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

  (let* ((list-columns (bz-list-columns instance))
         (bugs (mapcar (lambda (bug)
                         (bz-bug-to-filtered-vector bug list-columns))
                       parsed)))
    ;; populate header
    ;; as all entries are strings list can be sorted by any column
    (setq tabulated-list-format
          (make-vector (length (bz-list-columns instance)) nil))
    (let ((count 0)
          (header-widths (bz-header-widths bugs list-columns)))
      (dolist (element list-columns)
        (aset tabulated-list-format count
              `(,(bz--list-format-header-field element instance)
                ,(cdr (assoc element header-widths)) t))
        (setq count (+ 1 count))))
    (tabulated-list-init-header)

    ;; populate list entries
    (dolist (element bugs)
      (add-to-list 'tabulated-list-entries `(nil ,element)))
    (tabulated-list-print t)
    (bz-debug-log-time "stop")))

(defun bz--list-format-header-field (header-field &optional instance)
  "Format a header field name for display, taking into account instance
specific field descriptions."
  (propertize
   (prin1-to-string (or
                     (bz--bug-get-field-property
                      (intern header-field) 'display_name instance)
                     header-field) t)
    'face 'bz-bug-header-field))

(defun bz-bug-to-filtered-vector (bug list-columns)
  "Extract fields listed in the header from bug and return a vector suitable
for inclusion in tabulated-list-entries"
  (let ((data (make-vector (length list-columns) ""))
        (count 0))
    (dolist (header-item list-columns)
      (let ((value (or (cdr (assoc (intern header-item) (cdr bug))) ""))
            (bug-id (cdr (assoc (bz--uuid-field-name bz-instance) (cdr bug))))
            (formatted-string))
        ;; TODO: parse value according to datatype, and set matching face
        (setq formatted-string
              (propertize (prin1-to-string value t) 'bz-bug-id bug-id))
        (aset data count formatted-string))
      (setq count (+ 1 count)))
    data))

(defun bz-header-widths (bugs list-columns)
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

;; functions usually called through keybindings in bz-list-mode
;;;###autoload
(defun bz--list-mode-info ()
  "Display some information about thing at or near point

This is mostly useful for debugging text properties"
  (interactive)
  (let ((bug-id (get-text-property (point) 'bz-bug-id)))
    (message
     (concat
      "ID = "
      (prin1-to-string bug-id)
      "; "
      ))))

;;;###autoload
(defun bz--list-mode-select-bug ()
  "Open the bug at point in the list"
  (interactive)
  (let ((bug-id (get-text-property (point) 'bz-bug-id)))
    (if bug-id
        (bz-bug bug-id bz-instance)
      (message "No bug ID found. Misconfigured bug UUID property?"))))

;;;###autoload
(defun bz--list-mode-update-list ()
  "Update the list by running the original search query again"
  (interactive)
  (bz-do-search bz-query bz-instance))

;;;###autoload
(defun bz--list-mode-quit-window ()
  "Close the search result window"
  (interactive)
  (quit-window t))

(provide 'bz-list-mode)
;;; bz-list-mode.el ends here
