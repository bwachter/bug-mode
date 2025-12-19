;;; bug-auth.el --- functions related to authentication and credentials handling -*- lexical-binding: t; -*-
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

(require 'netrc)
(require 'url-parse)
(require 'bug-rpc)
(require 'bug-common-functions)

(defun bug-credentials (instance)
  "Return credentials for the given bug tracker instances, if set. The
configuration data for the instance and authinfo files will be searched, with
the configuration data taking precedence. Search order for authinfo is :authinfo
property, ~/.authinfo

The return value is a two element list (login password)
"
  (let* ((url (url-generic-parse-url (bug--instance-property :url instance)))
         (host (url-host url))
         (port (prin1-to-string (url-port url)))
         (authinfo-file (expand-file-name
                     (if (bug--instance-property :authinfo instance)
                         (bug--instance-property :authinfo instance) "~/.authinfo")))
         (authinfo (netrc-parse authinfo-file))
         (authrecord (netrc-machine authinfo host port))
         (login (if (bug--instance-property :login instance)
                    (bug--instance-property :login instance)
                  (netrc-get authrecord "login")))
         (password (if (bug--instance-property :password instance)
                       (bug--instance-property :password instance)
                     (netrc-get authrecord "password"))))
    (unless (file-exists-p authinfo-file)
      (error (format "Authinfo file in '%s' does not exist." authinfo-file)))
    (list login password)))

;;;###autoload
(defun bug-logout (&optional instance)
  (interactive
   (if current-prefix-arg
       (list (bug--query-instance))))
  (bug-rpc '((resource . "User")
             (operation . "login")) instance))

;;;###autoload
(defun bug-login (&optional instance)
  (interactive
   (if current-prefix-arg
       (list (bug--query-instance))))
  (bug-rpc `((resource . "User")
             (operation . "login")
             (data .
                   ((login . ,(car (bug-credentials instance)))
                    (password . ,(cadr (bug-credentials instance)))
                    (remember . t)))) instance)
  (bug--get-fields instance)
  (message "Login successful"))

(provide 'bug-auth)
;;; bug-auth.el ends here
