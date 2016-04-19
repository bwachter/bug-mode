;;; bz-common-functions.el --- simple functions shared by several modules
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

(defun filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun bz-bug-position-in-array (data field field-value)
  "Search for a bug with a value 'field-value' in field 'field' in a query
response. For example, to check if a rally user story 815 exists in the results
the call would look like this:

 (bz-bug-position-in-array results 'FormattedID \"US815\")"
  (let ((pos))
    (let ((count (- (length results) 1 )))
      (while (>= count 0)
        (if (string= field-value (cdr (assoc field (aref results count))))
            (progn
              (setq pos count)
              (setq count 0)))
        (setq count (- count 1))))
    pos))

(defun bz-query-instance ()
  "Query for a Bugzilla instance, providing completion with the instances configured in
bz-instance-plist. Returns the entered Bugzilla instance. Instance name only needs to be
entered enough to get a match."
  (let ((completions
         (remove-if nil
                    (cl-loop for record in bz-instance-plist collect
                             (unless (listp record)
                               (replace-regexp-in-string "^:" "" (prin1-to-string record)))))))
    (completing-read "Instance: " completions nil t)))

(defun bz-query-remembered-lists ()
  "Query for the name of a locally remembered bug list. Completion is seeded with names of lists across all Bugzilla instances"
  (let ((instance-keys) (category-keys))
    ;; first read the instance keys from highlevel hash
    (maphash #'(lambda (key value)
                 (push key instance-keys)) bz-bug-remember-list)
    (dolist (instance instance-keys)
      (let ((lists-for-instance (gethash instance bz-bug-remember-list)))
        ;; now read all keys from the lists for each instance
        (maphash #'(lambda (key value)
                     (push key category-keys)) lists-for-instance)))
    (delete-dups category-keys)
    (completing-read "List name: " category-keys nil nil)))

(defun bz--format-time-date (date-string &optional long)
  "Return a formatted time/date string, using customizable format strings. If
'long' is nil a short string will be returned, otherwise a long one.

No adjustments for the local timezone are made."
  (let ((format-string (if long bz-time-date-format-long
                         bz-time-date-format-short)))
    (format-time-string format-string
                        (date-to-time date-string) t)))

(provide 'bz-common-functions)
;;; bz-common-functions.el ends here
