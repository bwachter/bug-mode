;; bz-bug-mode-rally.el --- bug mode backend for Rally
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
(defun bz--fetch-rally-bug (id &optional instance)
  "Retrieve a single bug from Rally"
  (let* ((search-response (bz-rpc "artifact.read" `((object-id . ,id)) instance))
         (return-document-type (caar search-response))
         (return-document (cdr (car search-response))))
    ;; error messages are handled in RPC backend already, and -- unlike in
    ;; bugzilla -- the query is executed with a known to exist UUID. So,
    ;; in theory, from this point on nothing should fail
    return-document))

(provide 'bz-bug-mode-rally)
;;; bz-bug-mode-rally.el ends here
