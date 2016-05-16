;; bug-persistent-data.el --- handling persistent data for bug-mode
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

;;;;;;
;; helper functions and variables for reading static data and user settings

(require 'bug-custom)

(defvar bug-remember-list)

(unless (boundp 'bug-remember-list)
  (setq bug-remember-list (make-hash-table :test 'equal)))

(defun bug--write-data-file ()
  "Write user data to disk"
  (with-temp-buffer
    (insert ";; Foo\n")
    (insert "(setq bug-remember-list ")
    (pp bug-remember-list (current-buffer))
    (insert ")\n")
    (write-file bug-data-file)))

(defun bug--read-data-file ()
  "Restore user data from disk"
  (if (file-exists-p bug-data-file)
      (load (expand-file-name bug-data-file))))

(provide 'bug-persistent-data)
;;; bug-persistent-data.el ends here
