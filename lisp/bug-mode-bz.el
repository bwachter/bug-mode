;; bug-mode-bz.el --- bug mode backend for Bugzilla
;; (c) 2016 Bernd Wachter <bwachter@lart.info>
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

(require 'bug-rpc)

;;;###autoload
(defun bug--fetch-bz-bug (id &optional instance)
  "Retrieve a single bug from Bugzilla"
  (let ((search-response
         (bug-rpc `((resource . "Bug")
                    (operation . "get")
                    (post-data . (("ids" . ,id)))) instance)))
    (if (and (assoc 'result search-response)
             (assoc 'bugs (assoc 'result search-response)))
        (let ((bugs (cdr (assoc 'bugs (assoc 'result search-response)))))
          (cond
           ((= (length bugs) 0)
            (message (concat "Bug " id " not found.")))
           ((= (length bugs) 1)
            (aref bugs 0))
           (t (message "You should never see this message")))))))

;;;###autoload
(defun bug--browse-bz-bug (id instance)
  "Open the current bugzilla bug in browser"
  (let ((url (concat (bug--instance-property :url instance) "/show_bug.cgi?id=" id)))
    (browse-url url)))

(provide 'bug-mode-bz)
;;; bug-mode-bz.el ends here
