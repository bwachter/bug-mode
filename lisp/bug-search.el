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

(defvar bug--pending-query-string nil
  "Temporarily holds the user-typed query string until bug-list-show stores it.")

(defvar bug--pending-query-jql nil
  "Temporarily holds the JQL query string until bug-list-show stores it.")

(defvar bug--pending-project nil
  "Temporarily holds the project scope until bug-list-show stores it.")

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
  (setq bug--pending-query-string query)
  (bug--do-search
   (bug--instance-backend-function "bug--parse-%s-search-query" query instance)
   instance))

;;;###autoload
(defun bug-search-jql (query &optional instance)
  "Search using JQL (Jira Query Language) syntax.

The backend must support the :search-jql feature, which provides a
translation layer from JQL to the backend's native query language."
  (interactive
   (if current-prefix-arg
       (nreverse (list
                  (bug--instance-query :search-jql)
                  (read-string "JQL query: " nil nil t)))
     (list (read-string "JQL query: " nil nil "text ~ \"sample search\""))))
  (unless instance
    (setq instance (bug--instance-to-symbolp nil :search-jql)))
  (bug--debug-log-time "start")
  (setq bug--pending-query-string query)
  (setq bug--pending-query-jql query)
  (bug--do-search
   (bug--instance-backend-function "bug--parse-%s-jql-query" query instance)
   instance))

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
Uses `bug---project' if bound and non-nil, otherwise the instance's
`:project-id' property.  Returns nil if no project is configured."
  (or (and (boundp 'bug---project) bug---project)
      (bug--instance-property :project-id instance)))

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
    (setq bug--pending-query-string query)
    (setq bug--pending-project project-scope)
    (let ((bug---project project-scope))
      (bug--do-search
       (bug--instance-backend-function "bug--parse-%s-search-query" query instance)
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
                  (read-string "Project-scoped JQL query: " nil nil "text ~ \"sample search\"")))
     (list (read-string "Project-scoped JQL query: " nil nil t))))
  (unless instance
    (setq instance (bug--instance-to-symbolp nil :search-jql)))
  (let ((project-scope (bug--search-project-scope instance)))
    (unless project-scope
      (error "No project configured for instance %s" instance))
    (bug--debug-log-time "start")
    (setq bug--pending-query-string query)
    (setq bug--pending-query-jql query)
    (setq bug--pending-project project-scope)
    (let ((bug---project project-scope))
      (bug--do-search
       (bug--instance-backend-function "bug--parse-%s-jql-query" query instance)
       instance))))

;;;###autoload
(defun bug-edit-search ()
  "Edit and re-run the search query associated with the current buffer.
If the current buffer was created by a project-scoped search, re-run the
scoped variant; otherwise re-run the unscoped variant."
  (interactive)
  (let* ((current-query (if (boundp 'bug---query-string) bug---query-string ""))
         (instance (if (boundp 'bug---instance) bug---instance nil))
         (scoped (and (boundp 'bug---project) bug---project))
         (new-query (read-string "Search query: " current-query)))
    (unless (string-empty-p new-query)
      (if scoped
          (bug-search-project new-query instance)
        (bug-search new-query instance)))))

(provide 'bug-search)
;;; bug-search.el ends here
