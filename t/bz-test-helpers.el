;; bz-test-helpers.el --- helper functions to generate test data
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

(defconst bz-random-data-base10
  "0123456789"
  "Input for generating fixed length random numbers")

(defconst bz-random-data-base16
  "0123456789abcdef"
  "Input for generating fixed length random hex numbers")

(defun bz-random-string (length &optional dataset)
  "Generate a fixed length string of random data from dataset. If no dataset
is provided a base10 number is generated."
  (let* ((random-string "")
        (dataset (or dataset bz-random-data-base10))
        (dataset-length (length dataset)))
    (dotimes (number length random-string)
      (setq random-string
            (concat random-string
                    (string (aref dataset (random dataset-length))))))
    random-string))

(defun bz-fake-rally-object-id ()
  "Generate a fake object ID for rally"
  (bz-random-string 11))

(defun bz-fake-rally-object-uuid ()
  "Generate a fake object UUID for rally"
  (format "%s-%s-%s-%s-%s"
   (bz-random-string 8 bz-random-data-base16)
   (bz-random-string 4 bz-random-data-base16)
   (bz-random-string 4 bz-random-data-base16)
   (bz-random-string 4 bz-random-data-base16)
   (bz-random-string 12 bz-random-data-base16)))


(provide 'bz-tests)
;;; bz-tests.el ends here
