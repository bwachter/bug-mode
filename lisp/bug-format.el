;; bug-format.el --- functions to nicely format bug details
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
;;; Code:

(defun bug--format-bool (value)
  "Format a bool converted from JSON to `yes' or `no'"
  (cond ((equal value t)
         "yes")
        ((equal value :json-false)
         "no")
        (t
         (prin1-to-string value))))

(defun bug--format-field-name (field-name &optional instance)
  "Format a bug field name for display, taking into account instance
specific field descriptions."
  (propertize
   (concat
    (prin1-to-string (or
                      (bug--get-field-property
                       field-name 'display_name instance)
                      `(,field-name)) t)
    ": ")
    'face 'bug-field-description
    'bug-field-name field-name))

;;;###autoload
(defun bug--format-field-value (field &optional instance long)
  "Format a bug field value for display, taking into account instance
specific field descriptions. Unlike bug--format-field-name this function
requires both field name and content, therefore taking the complete cons
cell as argument

If the optional parameter `long' is non-nil display functions output
is formatted to take more space"
  (let ((content-type (bug--get-field-property
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
                (prin1-to-string (cdr (assoc '_refObjectName (cdr field))) t))
        'face 'bug-field-type-98))
      ((equal content-type 5)
       (propertize (bug--format-time-date (cdr field) long)
                   'face 'bug-field-type-5))
      ((equal content-type 6)
       (propertize (prin1-to-string (cdr field) t)
                   'face 'bug-field-type-6))
      ((equal content-type 98)
       (propertize (prin1-to-string (cdr field) t)))
      ((equal content-type 99)
       (propertize (replace-regexp-in-string "[[:space:]]*$" ""
                                             (bug--format-html (cdr field)))
                   'face 'bug-field-type-99))
      (t
       (prin1-to-string (cdr field) t)))
     'bug-field-type content-type
     'bug-field-name (car field))))

(defun bug--format-html (html &optional base-url)
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

(defun bug--format-kv (key value)
  "Format a key value pair for prettyprinting. The output is

key: value

with the key formatted as 'bug-field-description and the value as
'bug-field-type-0"
  (concat
   (propertize (concat key ": ")
               'face 'bug-field-description)
   (propertize value 'face 'bug-field-type-0)
   "\n"))

(defun bug--format-time-date (date-string &optional long)
  "Return a formatted time/date string, using customizable format strings. If
'long' is nil a short string will be returned, otherwise a long one.

No adjustments for the local timezone are made. If nil is passed as date
a single dash (-) is returned."
  (let ((format-string (if long bug-time-date-format-long
                         bug-time-date-format-short)))
    (if date-string
        (format-time-string format-string
                            (date-to-time date-string) t)
      "-")))

(provide 'bug-format)
;;; bug-format.el ends here
