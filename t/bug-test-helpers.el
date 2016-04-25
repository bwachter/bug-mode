;; bug-test-helpers.el --- helper functions to generate test data
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

(setq bug--test-helper-cwd
      (file-name-directory (or load-file-name (buffer-file-name))))

(defconst bug-random-data-base10
  "0123456789"
  "Input for generating fixed length random numbers")

(defconst bug-random-data-base16
  "0123456789abcdef"
  "Input for generating fixed length random hex numbers")

(defun bug-random-string (length &optional dataset)
  "Generate a fixed length string of random data from dataset. If no dataset
is provided a base10 number is generated."
  (let* ((random-string "")
        (dataset (or dataset bug-random-data-base10))
        (dataset-length (length dataset)))
    (dotimes (number length random-string)
      (setq random-string
            (concat random-string
                    (string (aref dataset (random dataset-length))))))
    random-string))

(defun bug-fake-rally-object-id ()
  "Generate a fake object ID for rally"
  (bug-random-string 11))

(defun bug-fake-rally-object-uuid ()
  "Generate a fake object UUID for rally"
  (format "%s-%s-%s-%s-%s"
   (bug-random-string 8 bug-random-data-base16)
   (bug-random-string 4 bug-random-data-base16)
   (bug-random-string 4 bug-random-data-base16)
   (bug-random-string 4 bug-random-data-base16)
   (bug-random-string 12 bug-random-data-base16)))

(defun bug--test-reload-all-files ()
  "Reload all(!) lisp files of bug-mode. Unless you're working on bug-mode
that's not what you want."
  (let ((all-files
         (directory-files
          (concat bug--test-helper-cwd "../lisp/") t ".el$")))
    (push
     (concat bug--test-helper-cwd "../bug.el") all-files)
    (dolist (f all-files)
      (load-file f))))

(provide 'bug-tests)
;;; bug-tests.el ends here
