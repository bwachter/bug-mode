;;; bz-auth.el --- functions related to bz auth and credentials handling
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

(require 'netrc)
(require 'url-parse)
(require 'bz-rpc)

(defun bz-credentials (&optional instance)
  "Return credentials for the given Bugzilla instances, if set. The configuration data
for the instance and authinfo files will be searched, with the configuration data
taking precedence. Search order for authinfo is :authinfo property, ~/.authinfo

The return value is a two element list (login password)
"
  (let* ((url (url-generic-parse-url (bz-instance-property :url instance)))
         (host (url-host url))
         (port (prin1-to-string (url-port url)))
         (authinfo (netrc-parse
                    (expand-file-name
                     (if (bz-instance-property :authinfo instance)
                         (bz-instance-property :authinfo instance) "~/.authinfo"))))
         (authrecord (netrc-machine authinfo host port))
         (login (if (bz-instance-property :login instance)
                    (bz-instance-property :login instance)
                  (netrc-get authrecord "login")))
         (password (if (bz-instance-property :password instance)
                       (bz-instance-property :password instance)
                     (netrc-get authrecord "password")))
         )
    (list login password)))

;;;###autoload
(defun bz-logout (&optional instance)
  (interactive
   (if current-prefix-arg
       (list (bz-query-instance))))
  (bz-rpc "User.logout" '() instance))

;;;###autoload
(defun bz-login (&optional instance)
  (interactive
   (if current-prefix-arg
       (list (bz-query-instance))))
  (bz-rpc "User.login" `((login . ,(car (bz-credentials instance)))
                         (password . ,(cadr (bz-credentials instance)))
                         (remember . t)) instance)
  (bz-get-fields)
  (message "Login successful"))

(provide 'bz-auth)
;;; bz-auth.el ends here
