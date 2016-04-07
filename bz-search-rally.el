;; bz-search-rally.el -- rally specific search functions
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
(defun bz--do-rally-search (params &optional instance)
  "Execute a search query in Rally

This function takes either a query string in Rallys query string syntax,
or an alist as documented for bz--rpc-rally.

When providing just the query string additional options (like fetch, order,
pagesize, ...) can't be supplied:

(bz--do-rally-search \"( FormattedID = \"US1234\" )\")
"
  (let* ((query (cond ((stringp params)
                       `((query-data . ((query ,params)))))
                      ((listp params)
                       (if (assoc 'query-data params)
                           params)
                       (error "Parameter list needs 'query-data' member"))
                      (t (error "Invalid type for search parameters")))))
    (bz--handle-rally-search-response query (bz-rpc "artifact.query" query instance))))


(defun bz--handle-rally-search-response (query response &optional instance)
  "Parse the result of a Rally search"
  (if (and
       (assoc 'QueryResult response)
       (assoc 'TotalResultCount (assoc 'QueryResult response)))
      (let* ((query-result (assoc 'QueryResult response))
             (bugs (cdr (assoc 'TotalResultCount query-result))))
        (if (= bugs 0)
            (message "No results")
          (if (= bugs 1)
              ;; this should display the bug...
              (let* ((bug (aref (cdr (assoc 'Results query-result)) 0))
                     (bug-id (assoc '_refObjectUUID bug)))
                (bz-bug-show bug-id bug instance))
            ;; ... and this should display a list
            (let ((results
                   (cdr (assoc 'Results query-result))))
              (bz-list-show query results instance)))))
    response))

(provide 'bz-search-rally)
;;; bz-search-rally.el ends here
