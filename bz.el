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

(add-to-list 'load-path
             (directory-file-name
              (concat
               (file-name-directory (or load-file-name (buffer-file-name)))
               "lisp")))

(require 'bz-autoloads)

(defgroup bz nil "Bugzilla related settings")

(defcustom bz-debug nil
  "Configure debugging to *bz-debug* buffer"
  :type 'string
  :group 'bz)

(defcustom bz-default-instance ""
  "The default bugzilla to use"
  :type 'string
  :group 'bz)

(defcustom bz-instance-plist nil
  "A list of bugzilla instances to use.

Example:
'(:work   (:url \"https://work.example.com\")
  :secure (:url \"https://secure.example.com\" :authinfo \"~/.netrc\")
  :fun    (:url \"https://fun.example.com\" :login \"username\" :password \"password\"))

The :work instance is either without auth, with auth-data in ~/.authinfo, or behind basic auth with the url-package prompting for credentials

The :secure instance uses regular bz auth, with credentials stored in ~/.netrc. It requires a call to (bz-login \"secure\") before you can modify bugs

The :fun instance uses regular bz auth, with credentials stored inside the configuration, which you should try to avoid for security reasons. It also requires a call to (bz-login \"fun\") before you can modify bugs
"
  :type 'sexp
  :group 'bz)

(defcustom bz-autoload-attachments nil
  "Controls autoloading of attachments when opening a bug"
  :type 'sexp
  :group 'bz)

(defcustom bz-autoload-comments t
  "Controls autoloading of comments when opening a bug"
  :type 'sexp
  :group 'bz)

(defcustom bz-data-directory (locate-user-emacs-file "bz/")
  "The directory containing data files for the bz package"
  :type 'string
  :group 'bz)

(defvar bz-data-file (concat bz-data-directory "data")
  "The file containing saved searches and similar user data. Change bz-data-directory if you don't like the storage location")

(defmacro bz-debug (body)
  `(if (and (boundp 'bz-debug) bz-debug)
       (let ((str ,body))
         (with-current-buffer (get-buffer-create "*bz-debug*")
           (goto-char (point-max))
           (insert str)
           (insert "\n")))))

(setq bz-bug-remember-list (make-hash-table :test 'equal))

(defconst bz-json-data-dir
  (concat
   (file-name-directory (or load-file-name (buffer-file-name)))
   "/json")
  "Location of additional JSON data files used by bz.")

(defun bz-write-data-file ()
  "Write user data to disk"
  (with-temp-buffer
    (insert ";; Foo\n")
    (insert "(setq bz-bug-remember-list ")
    (pp bz-bug-remember-list (current-buffer))
    (insert ")\n")
    (write-file bz-data-file)))

(defun bz-read-data-file ()
  "Restore user data from disk"
  (if (file-exists-p bz-data-file)
      (load (expand-file-name bz-data-file))))

(defun bz-list-columns (&optional instance)
  "Read the list headers for a bugtracker instance.

If the given instance does not have a :list-columns property defaults
are used.
"
  (let ((type (bz-instance-property :type instance)))
    (if (bz-instance-property :list-columns instance)
        (bz-instance-property :list-columns instance)
      (cond
       ((string= type "rally")
        '("FormattedID" "Name" "LastUpdateDate"))
       (t
        '("id" "status" "summary" "last_change_time"))))))

(bz-read-data-file)

(provide 'bz)
;;; bz.el ends here)
