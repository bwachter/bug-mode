;;; bug-search-filter.el --- generic filter-based search for bug-mode -*- lexical-binding: t; -*-
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

(require 'bug-common-functions)
(require 'bug-search-common)

(defun bug--parse-search-filter (input)
  "Parse filter expression `input' into a property alist.

Each filter term has the form key:value where `value' may be quoted for
values containing spaces: key:\"multi word value\".

Supported properties:
- title       -- match title or name (contains)
- status      -- filter by status or schedule state
- owner       -- filter by owner name (contains)
- type        -- work item type (story, defect, task, ...)
- iteration   -- filter by iteration or sprint name (contains)
- priority    -- filter by priority
- description -- search in description and notes (contains)
- tag         -- filter by tag or keyword (contains)

Returns an alist of (symbol . string) pairs.  Unrecognized keys are
passed through so backends can handle backend-specific properties."
  (let ((result '())
        (pos 0))
    (while (string-match
            "\\([a-z_]+\\):\\(\"[^\"]*\"\\|[^[:space:]]+\\)"
            input pos)
      (let* ((key (intern (match-string 1 input)))
             (raw (match-string 2 input))
             (value (if (string-prefix-p "\"" raw)
                        (substring raw 1 -1)
                      raw)))
        (push (cons key value) result)
        (setq pos (match-end 0))))
    (nreverse result)))

(defun bug--search-filter-to-query (properties instance)
  "Translate property alist `properties' to a backend-specific search query.

Dispatches to the backend-specific `bug--search-filter-<backend>-query'
function.  Returns a params alist suitable for `bug--do-search'."
  (bug--backend-function "bug--search-filter-%s-query" properties instance))

;;;###autoload
(defun bug-search-filter (input &optional instance)
  "Search using generic filter properties.

`input' is a space-separated list of key:value pairs.  Multi-word values
may be quoted: key:\"multi word value\".

Supported properties:
- title       -- match title or name (contains)
- status      -- filter by status or schedule state
- owner       -- filter by owner name (contains)
- type        -- work item type (story, defect, task, ...)
- iteration   -- filter by iteration or sprint name (contains)
- priority    -- filter by priority
- description -- search in description and notes (contains)
- tag         -- filter by tag or keyword (contains)

With a prefix argument, also prompts for the instance to search."
  (interactive
   (if current-prefix-arg
       (nreverse (list (bug--query-instance) (read-string "Filter: ")))
     (list (read-string "Filter: "))))
  (let ((properties (bug--parse-search-filter input)))
    (unless properties
      (user-error "No filter properties in %S; use KEY:VALUE syntax, e.g. status:open owner:bob" input))
    (bug--do-search
     (bug--search-filter-to-query properties instance)
     instance)))

(provide 'bug-search-filter)
;;; bug-search-filter.el ends here
