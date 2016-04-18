;; bz-search-rally.el --- rally specific search functions
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

(require 'bz-common-functions)

;;;###autoload
(defun bz--do-rally-search (params &optional instance method)
  "Execute a search query in Rally

This function takes either a query string in Rallys query string syntax,
or an alist as documented for bz--rpc-rally.

When providing just the query string additional options (like fetch, order,
pagesize, ...) can't be supplied:

 (bz--do-rally-search \"( FormattedID = \"US1234\" )\")
"
  (let* ((query (cond ((stringp params)
                       `((query ,params)))
                      ((listp params)
                       (if (assoc 'query params)
                           params
                         (error "Parameter list needs 'query' member")))
                      (t (error "Invalid type for search parameters")))))
    (unless (assoc 'pagesize query)
      (add-to-list 'query '(pagesize 100)))
    (unless (assoc 'fetch query)
      (add-to-list 'query '(fetch "FormattedID,LastUpdateDate,TaskStatus,Name")))
    (bz--handle-rally-search-response
     query
     (bz-rpc (or method "artifact.query")
             `((query-data . ,query)) instance))))

;; TODO: Rally strips the letters, and just queries the number, leading to
;;       duplicate results. Check the query if we were searching for a single
;;       bug, and break it down, if necessary
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
                     (bug-id (cdr (assoc '_refObjectUUID bug))))
                (bz-bug bug-id instance))
            ;; ... and this should display a list
            (let* ((results
                    (cdr (assoc 'Results query-result)))
                   (stripped-query
                    (replace-regexp-in-string
                     "[ ()\"]" ""
                     (cadr (assoc 'query query)))))
              ;; check if the query was for a single bug
              (if (string-match
                   "^FormattedID=\\(\\(F\\|DE\\|TA\\|US\\)\\{1\\}[0-9]+\\)$"
                   stripped-query)
                  ;; if so, check if the bug is present in the results
                  (let* ((formatted-id (match-string 1 stripped-query))
                         (bug-position
                          (bz-bug-position-in-array
                           results 'FormattedID formatted-id)))
                    ;; if the bug was found, return it as single bug, otherwise,
                    ;; just show the list (which shouldn't happen)
                    (if bug-position
                        (let ((bug (aref results bug-position)))
                          (bz-bug (cdr (assoc '_refObjectUUID bug)) instance))
                      (bz-list-show query results instance)))
                ;; search was not for a single bug, so show the list
                (bz-list-show query results instance))))))
    ;; response didn't contain QueryResult and TotalResultCount, so just
    ;; return the response for debugging, as that should not happen.
    response))

;;;###autoload
(defun bz--parse-rally-search-query (query)
  "Parse search query from minibuffer for rally"
  (cond ;; for userfriendly rally IDs, open bug directly
   ((string-match "^\\(F\\|DE\\|TA\\|US\\)[0-9]+" query)
    `(( query ,(format "( FormattedID = \"%s\" )" query))))
   ;; string contains parentheses -> assume it's a complex rally expression
   ((string-match "\\((\\|)\\)" query)
    `((query ,query)))
   ;; search Name, Notes, Description
   ;; TODO: searching discussion seems to be problematic
   (t
    `(( query
        ,(format "(((Name contains \"%s\") OR (Notes contains \"%s\")) OR (Description contains \"%s\"))"
                 query query query))))))


(provide 'bz-search-rally)
;;; bz-search-rally.el ends here
