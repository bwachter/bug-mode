;;; bz.el --- work with Bugzilla from within emacs
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

(require 'bz-autoloads)

(defvar bz-debug nil
  "Configure debugging to *bz-debug* buffer")

(defvar bz-default-instance
  "The default bugzilla to use")

(defvar bz-instance-plist nil
  "A list of bugzilla instances to use.

Example:
'(:work (:url \"https://work.example.com\")
  :fun  (:url \"https://fun.example.com\" :login \"username\" :password \"password\"))
")

(defvar bugzilla-columns '("id" "status" "summary" "last_change_time")
  "Default columns in search output")

(defmacro bz-debug (body)
  `(if (and (boundp 'bz-debug) bz-debug)
       (let ((str ,body))
         (with-current-buffer (get-buffer-create "*bz-debug*")
           (goto-char (point-max))
           (insert str)
           (insert "\n")))))

(provide 'bz)
;;; bz.el ends here)
