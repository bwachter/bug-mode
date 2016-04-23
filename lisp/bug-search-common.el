;;; bug-search-common.el -- search functions shared between different modules
;; TODO: this might need reworking
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

(defun bug-do-search (params &optional instance)
  "Execute a bug search query"
  (cond
   ((equal 'rally (bug--backend-type instance))
    (bug--do-rally-search params instance))
   (t
    (bug-handle-search-response params
                                (bug-rpc "Bug.search" params instance)
                                instance))))

(defun bug-handle-search-response (query response &optional instance)
  "Parse the result of a bug search and either show a single bug or a bug list"
  (if (and
       (assoc 'result response)
       (assoc 'bugs (assoc 'result response)))
      (let ((bugs (cdr (assoc 'bugs (assoc 'result response)))))
        (if (= (length bugs) 0)
            (message "No results")
          (if (= (length bugs) 1)
              (bug-show (aref bugs 0) instance)
            (bug-list-show query bugs instance))))
    response))

(provide 'bug-search-common)
;;; bug-search-common.el ends here
