;;; bug-search.el --- handle bug searches -*- lexical-binding: t; -*-
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

(require 'bug-search-common)
(require 'bug-debug)
(require 'bug-persistent-data)
(require 'bug-vars)
(require 'bug-instance)
(require 'bug-project)
(require 'bug-custom)
(require 'bug-repo)
(require 'bug-jql)

;;;###autoload
(defun bug-stored-bugs (list-name &optional instance)
  "Display a stored list of bugs"
  (interactive
   (if current-prefix-arg
       (nreverse (list(bug--instance-query) (bug--query-remembered-lists)))
     (list
      (bug--query-remembered-lists))))
  (let* ((instance (bug--instance-to-symbolp instance))
         (lists-for-instance (gethash instance bug-remember-list))
         (list-entries (if lists-for-instance
                           (gethash list-name lists-for-instance))))
    (if list-entries
        (let ((query (make-hash-table :test 'equal)))
          (puthash "id" list-entries query)
          (bug--do-search query instance))
      (message (concat "List " list-name " not found")))
    ))

;;;###autoload
(defun bug-search (query &optional instance)
  "Take a search query from the minibuffer and execute it.

The query is interpreted in the backend's native search language.  The
backend must support the :search feature."
  (interactive
   (if current-prefix-arg
       (nreverse (list
                  (bug--instance-query :search)
                  (read-string "Search query: " nil nil t)))
     (list (read-string "Search query: " nil nil t))))
  (unless instance
    (setq instance (bug--instance-to-symbolp nil :search)))
  (bug--debug-log-time "start")
  (bug--debug (format "search native: %S instance=%S" query instance) '(search . 1))
  (let ((params (bug--instance-backend-function "bug--parse-%s-search-query" query instance)))
    (bug--debug (format "search native params: %S" params) '(search . 2))
    (bug--do-search
     (cons `(native-query . ,query) params)
     instance)))

;;;###autoload
(defun bug-search-jql (query &optional instance)
  "Search using JQL (Jira Query Language) syntax.

The backend must support the :search-jql feature, which provides a
translation layer from JQL to the backend's native query language."
  (interactive
   (if current-prefix-arg
       (nreverse (list
                  (bug--instance-query :search-jql)
                  (bug--jql-read-with-hints "JQL query: " nil)))
     (list (bug--jql-read-with-hints "JQL query: " nil "text ~ \"sample search\""))))
  (unless instance
    (setq instance (bug--instance-to-symbolp nil :search-jql)))
  (bug--debug-log-time "start")
  (bug--debug (format "search jql: %S instance=%S" query instance) '(search . 1))
  (let ((params (bug--instance-backend-function "bug--parse-%s-jql-query" query instance)))
    (bug--debug (format "search jql params: %S" params) '(search . 2))
    (bug--do-search
     (append `((native-query-jql . ,query)
               (native-query .
                             ,(or (cdr (assoc 'query params))
                                  (cdr (assoc 'query (cdr (assoc 'data params))))
                                  query)))
             params)
     instance)))

(defun bug--search-candidates (search-str instance)
  "Search for artifacts matching `search-str' and return a candidate alist.

Each element is (display-string . identifier) for use in `completing-read'.
Returns nil when no results are found or the backend lacks support."
  (let* ((params (bug--instance-backend-function
                  "bug--parse-%s-search-query" search-str instance))
         (results (bug--instance-backend-function-optional
                   "bug--execute-%s-search" params instance)))
    (when (and results (> (cdr results) 0))
      (bug--instance-backend-function-optional
       "bug--format-%s-search-candidates" (car results) instance))))

(defun bug--search-project-scope (instance)
  "Return the project scope for INSTANCE.

Delegates to `bug-project-get-current' which follows the resolution
hierarchy: transient-local, buffer-local, instance property, then
auto-detected from git remotes.  Returns nil if no project is configured."
  (let ((result (bug-project-get-current instance)))
    (bug--debug (format "bug--search-project-scope: instance=%S bug---project=%S result=%S"
                        instance (and (boundp 'bug---project) bug---project) result))
    result))

;;;###autoload
(defun bug-search-project (query &optional instance)
  "Execute a project-scoped native search.

The query is interpreted in the backend's native search language, restricted
to the current project (`bug---project') or the instance's `:project-id'.
The backend must support the :search feature."
  (interactive
   (if current-prefix-arg
       (nreverse (list
                  (bug--instance-query :search)
                  (read-string "Project-scoped search query: " nil nil t)))
     (list (read-string "Project-scoped search query: " nil nil t))))
  (unless instance
    (setq instance (bug--instance-to-symbolp nil :search)))
  (let ((project-scope (bug--search-project-scope instance)))
    (unless project-scope
      (error "No project configured for instance %s" instance))
    (bug--debug-log-time "start")
    (bug--debug (format "bug-search-project: instance=%S query=%S project-scope=%S bug---project=%S"
                        instance query project-scope (and (boundp 'bug---project) bug---project)))
    (let* ((bug---project project-scope)
           (params (bug--instance-backend-function "bug--parse-%s-search-query" query instance)))
      (bug--debug (format "bug-search-project: after let* bug---project=%S params=%S"
                          bug---project params))
      (bug--do-search
       (append `((native-project . ,project-scope)
                 (native-query . ,query))
               params)
       instance))))

;;;###autoload
(defun bug-search-jql-project (query &optional instance)
  "Execute a project-scoped JQL search.

The query is translated from JQL to the backend's native language and
restricted to the current project (`bug---project') or the instance's
`:project-id'.  The backend must support the :search-jql feature."
  (interactive
   (if current-prefix-arg
       (nreverse (list
                  (bug--instance-query :search-jql)
                  (bug--jql-read-with-hints "Project-scoped JQL query: " nil "text ~ \"sample search\"")))
     (list (bug--jql-read-with-hints "Project-scoped JQL query: " nil))))
  (unless instance
    (setq instance (bug--instance-to-symbolp nil :search-jql)))
  (let ((project-scope (bug--search-project-scope instance)))
    (unless project-scope
      (error "No project configured for instance %s" instance))
    (bug--debug-log-time "start")
    (bug--debug (format "search jql-project: %S instance=%S project=%S"
                        query instance project-scope) '(search . 1))
    (let* ((bug---project project-scope)
           (params (bug--instance-backend-function "bug--parse-%s-jql-query" query instance)))
      (bug--debug (format "search jql-project params: %S" params) '(search . 2))
      (bug--do-search
       (append `((native-project . ,project-scope)
                 (native-query-jql . ,query)
                 (native-query .
                               ,(or (cdr (assoc 'query params))
                                    (cdr (assoc 'query (cdr (assoc 'data params))))
                                    query)))
               params)
       instance))))

;;;###autoload
(defun bug-edit-search ()
  "Edit and re-run the native search query associated with the current buffer.
Uses `bug---query-string' as the default.  Project scoping is preserved."
  (interactive)
  (let* ((current-query (if (boundp 'bug---query-string)
                            bug---query-string ""))
         (instance (bug-instance-get-current))
         (project (bug-project-get-current instance))
         (new-query (read-string "Search query: " current-query)))
    (unless (string-empty-p new-query)
      (if project
          (bug-search-project new-query instance)
        (bug-search new-query instance)))))


;;;###autoload
(defun bug-edit-jql-search ()
  "Edit and re-run the JQL search query associated with the current buffer.
Uses `bug---query-jql' as the default.  Project scoping is preserved."
  (interactive)
  (let* ((current-query (if (boundp 'bug---query-jql)
                            bug---query-jql ""))
         (instance (bug-instance-get-current))
         (project (bug-project-get-current instance))
         (new-query (bug--jql-read-with-hints "JQL query: " current-query)))
    (unless (string-empty-p new-query)
      (if project
          (bug-search-jql-project new-query instance)
        (bug-search-jql new-query instance)))))

(provide 'bug-search)
;;; bug-search.el ends here
