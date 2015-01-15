;;; bz-search-common.el -- search functions shared between different modules
;; TODO: this might need reworking
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

(defun bz-do-search (params &optional instance)
  "Execute a Bugzilla search query"
  (bz-handle-search-response params (bz-rpc "Bug.search" params instance) instance))

(defun bz-handle-search-response (query response &optional instance)
  "Parse the result of a Bugzilla search and either show a single bug or a bug list"
  (if (and
       (assoc 'result response)
       (assoc 'bugs (assoc 'result response)))
      (let ((bugs (cdr (assoc 'bugs (assoc 'result response)))))
        (if (= (length bugs) 0)
            (message "No results")
          (if (= (length bugs) 1)
              (bz-bug-show (cdr (assoc 'id (aref bugs 0))) (aref bugs 0) instance)
            (bz-list-show query bugs instance))))
    response))

(provide 'bz-search-common)
;;; bz-search-common.el ends here
