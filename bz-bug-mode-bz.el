;; bz-bug-mode-bz.el --- bug mode backend for Bugzilla
;; (c) 2016 Bernd Wachter <bwachter@lart.info>
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

;;;###autoload
(defun bz--fetch-bz-bug (id &optional instance)
  "Retrieve a single bug from Bugzilla"
  (let ((search-response (bz-rpc "Bug.get" `(("ids" . ,id)) instance)))
    (if (and (assoc 'result search-response)
             (assoc 'bugs (assoc 'result search-response)))
        (let ((bugs (cdr (assoc 'bugs (assoc 'result search-response)))))
          (cond
           ((= (length bugs) 0)
            (message (concat "Bug " id " not found.")))
           ((= (length bugs) 1)
            (aref bugs 0))
           (t (message "You should never see this message")))))))

(provide 'bz-bug-mode-bz)
;;; bz-bug-mode-bz.el ends here
