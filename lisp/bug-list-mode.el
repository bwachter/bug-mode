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
(require 'bug-format)

(defvar bug-list-mode-map
  (let ((keymap (copy-keymap special-mode-map)))
    (define-key keymap (kbd "RET") 'bug--list-mode-select-bug)
    (define-key keymap "b"         'bug--list-mode-browse-bug)
    (define-key keymap "i"         'bug--list-mode-info)
    (define-key keymap "u"         'bug--list-mode-update-list)
    (define-key keymap "q"         'bug--mode-default-quit-window)
    keymap)
  "Keymap for BZ list mode")

(define-derived-mode bug-list-mode tabulated-list-mode "Bug list"
  "Operate on a list of bugs"
  )

(defun bug-list-show (query parsed &optional instance)
  "Display the result of a bug search returning a list of bugs"
  (bug--debug-log-time "bug-list-show")
  (switch-to-buffer (format "*%s results: %s*"
                            (prin1-to-string (bug--backend-type instance) t)
                            (pretty-kvs query)))
  (bug-list-mode)
  (make-local-variable 'bug---query)
  (setq bug---query query)
  (make-local-variable 'bug---instance)
  (setq bug---instance instance)
  (setq buffer-read-only nil)

  (let* ((list-columns (bug--list-columns instance))
         (bugs (mapcar (lambda (bug)
                         (bug-to-filtered-vector bug list-columns))
                       parsed)))
    ;; populate header
    ;; as all entries are strings list can be sorted by any column
    (setq tabulated-list-format
          (make-vector (length (bug--list-columns instance)) nil))
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
    (bug--debug-log-time "stop")))

(defun bug--list-format-header-field (header-field &optional instance)
  "Format a header field name for display, taking into account instance
specific field descriptions."
  (let ((header-field (if (listp header-field)
                          (car header-field)
                        header-field)))
    (propertize
     (prin1-to-string (or
                       (bug--get-field-property
                        (intern header-field) 'display_name instance)
                       header-field) t)
     'face 'bug-header-field)))

(defun bug--get-field-by-key (bug key)
  "Return the bug field matching the key. In case key is a list, return
the first field matching any of the keys in the list."
  (if (listp key)
      (let ()
        (catch 'return
          (dolist (field key)
            (if (assoc (intern field) (cdr bug))
                (throw 'return (assoc (intern field) (cdr bug)))))))
    (assoc (intern key) (cdr bug))))

(defun bug-to-filtered-vector (bug list-columns)
  "Extract fields listed in the header from bug and return a vector suitable
for inclusion in tabulated-list-entries"
  (let ((data (make-vector (length list-columns) ""))
        (count 0))
    (dolist (header-item list-columns)
      (let* ((field (bug--get-field-by-key bug header-item))
             (value (or (cdr field) ""))
             (map (make-sparse-keymap))
             (bug-uuid (cdr (assoc (bug--field-name :bug-uuid bug---instance) (cdr bug))))
             (bug-id (cdr (assoc (bug--field-name :bug-friendly-id bug---instance) (cdr bug))))
             (formatted-string))
        (define-key map [mouse-1] 'bug--list-mode-select-bug-with-mouse)
        (define-key map [mouse-2] 'bug--list-mode-select-bug-with-mouse)
        (setq formatted-string
              (propertize
               (bug--format-field-value field bug---instance)
               'bug-uuid bug-uuid
               'bug-id bug-id
               'keymap map))
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

(defun bug-list-mode-bug-near-point ()
  "Return details of the bug at or near point, in the form of an alist with
the following keys:
- bug-id -- the user friendly bug ID
- bug-uuid -- the unique ID of the bug
- instance -- the bugtracker instance

This function may be called by external modules to get information about a bug
at or near point. If no valid bug was found bug-id and bug-uuid are `nil'"
  (let ((bug-uuid (or (get-text-property (point) 'bug-uuid)
                      (save-excursion
                        (forward-line 0)
                        (get-text-property (point) 'bug-uuid))))
        (bug-id (or (get-text-property (point) 'bug-id)
                      (save-excursion
                        (forward-line 0)
                        (get-text-property (point) 'bug-id))))
        (bug-instance (or (and (boundp 'bug---instance) bug---instance) bug-default-instance)))
    `((bug-id . ,bug-id)(bug-uuid . ,bug-uuid)(instance . ,bug-instance))))

;; functions usually called through keybindings in bug-list-mode
;;;###autoload
(defun bug--list-mode-info ()
  "Display some information about thing at or near point

This is mostly useful for debugging text properties"
  (interactive)
  (let ((bug-uuid (get-text-property (point) 'bug-uuid))
        (bug-id (get-text-property (point) 'bug-id)))
    (message
     (concat
      "ID = "
      (prin1-to-string bug-id)
      "; "
      "UUID = "
      (prin1-to-string bug-uuid)
      "; "
      ))))

;;;###autoload
(defun bug--list-mode-browse-bug ()
  "Open the current bug from the list in a web browser. The assumption is that
the backend can handle user friendly IDs for this."
  (interactive)
  (let* ((bug-info (bug-list-mode-bug-near-point))
         (bug-id (cdr (assoc 'bug-id bug-info))))
    (if 'bug-id
        (bug--backend-function "bug--browse-%s-bug" bug-id bug---instance))))

;;;###autoload
(defun bug--list-mode-select-bug-with-mouse (event)
  "Move the cursor to the position of the mouse pointer, and then
open the bug."
  (interactive "e")
  (goto-char (posn-point (event-start event)))
  (bug--list-mode-select-bug))

;;;###autoload
(defun bug--list-mode-select-bug ()
  "Open the current bug from the list. The bug identifier is read from text
properties at point, or -- if that fails -- from the beginning of the current
line"
  (interactive)
  (let* ((bug-info (bug-list-mode-bug-near-point))
         (bug-uuid (cdr (assoc 'bug-uuid bug-info))))
    (if 'bug-uuid
        (bug-open bug-uuid bug---instance)
      (message "No bug ID found. Misconfigured bug UUID property?"))))

;;;###autoload
(defun bug--list-mode-update-list ()
  "Update the list by running the original search query again"
  (interactive)
  (bug--do-search bug---query bug---instance))

(provide 'bug-list-mode)
;;; bug-list-mode.el ends here
