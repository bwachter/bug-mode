;; bug-backend-github.el --- backend implementation for GitHub Issues -*- lexical-binding: t; -*-
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

;; GitHub REST API documentation: https://docs.github.com/en/rest
;;
;; Instance configuration:
;;   :type       github
;;   :api-key    GitHub personal access token (PAT)
;;   :url        optional, defaults to https://api.github.com
;;   :project-id owner/repo for repo-scoped operations, or org-login for
;;               org-scoped searches

(require 'bug-vars)
(require 'bug-mode)
(require 'bug-list-mode)
(require 'bug-rpc)
(require 'bug-common-functions)
(require 'bug-debug)
(require 'json)
(require 'url)
(require 'url-util)

;;;###autoload
(defun bug--backend-github-features (_arg _instance)
  "Features supported by the GitHub backend."
  '(:read :projects :project-bugs))

;;;###autoload
(defun bug--backend-github-default-url (_args _instance)
  "Return the default GitHub REST API base URL."
  "https://api.github.com")

;;;;;;
;; RPC

(defun bug--rpc-github-method (operation)
  "Map `operation' to an HTTP method string."
  (let ((op (downcase (or operation "get"))))
    (cond ((string= op "post")   "POST")
          ((string= op "patch")  "PATCH")
          ((string= op "delete") "DELETE")
          (t                     "GET"))))

;;;###autoload
(defun bug--rpc-github (args instance)
  "Send a request to the GitHub REST API and return the parsed response.

`args' is an alist with the following keys:
- resource:  API path relative to the base URL
- operation: HTTP method: \"get\" (default), \"post\", \"patch\", \"delete\"
- data:      for GET requests, a list of (key value) query-parameter pairs;
             for POST/PATCH, an alist to encode as the JSON request body"
  (let* ((resource (cdr (assoc 'resource args)))
         (operation (downcase (or (cdr (assoc 'operation args)) "get")))
         (data (cdr (assoc 'data args)))
         (base-url (bug--instance-property :url instance))
         (api-key (bug--instance-property :api-key instance))
         (url-request-method (bug--rpc-github-method operation))
         (query-string (when (and data (string= url-request-method "GET"))
                         (url-build-query-string data)))
         (url (concat base-url "/" resource
                      (when query-string (concat "?" query-string))))
         (url-request-extra-headers
          (append
           `(("Accept" . "application/vnd.github+json")
             ("X-GitHub-Api-Version" . "2022-11-28"))
           (when api-key
             `(("Authorization" . ,(concat "Bearer " api-key))))))
         (url-request-data
          (when (member url-request-method '("POST" "PATCH"))
            (json-encode data))))
    (bug--debug (concat "request " url "\n"))
    (with-current-buffer (url-retrieve-synchronously url)
      (bug--debug (concat "response: \n" (decode-coding-string (buffer-string) 'utf-8)))
      (bug--parse-rpc-response instance))))

;;;###autoload
(defun bug--rpc-github-handle-error (response _instance)
  "Check a GitHub API response for errors.

GitHub error responses are alists with a `message' key."
  (when (and (listp response) (assoc 'message response))
    (error "%s" (cdr (assoc 'message response))))
  response)

;;;;;;
;; Field definitions

(defconst bug--github-issue-fields
  '(;; Visible fields
    ((name . "number")           (display_name . "Issue #")          (type . 6)  (is_readonly . t))
    ((name . "title")            (display_name . "Title")            (type . 1))
    ((name . "state")            (display_name . "State")            (type . 1)  (is_readonly . t))
    ((name . "state_reason")     (display_name . "State reason")     (type . 1)  (is_readonly . t))
    ((name . "user")             (display_name . "Reporter")         (type . 1)  (is_readonly . t))
    ((name . "assignee")         (display_name . "Assignee")         (type . 1))
    ((name . "assignees")        (display_name . "Assignees")        (type . 1))
    ((name . "labels")           (display_name . "Labels")           (type . 1))
    ((name . "milestone")        (display_name . "Milestone")        (type . 1))
    ((name . "comments")         (display_name . "Comments")         (type . 6)  (is_readonly . t))
    ((name . "author_association") (display_name . "Role")           (type . 1)  (is_readonly . t))
    ((name . "html_url")         (display_name . "URL")              (type . 1)  (is_readonly . t))
    ((name . "created_at")       (display_name . "Created")          (type . 5)  (is_readonly . t))
    ((name . "updated_at")       (display_name . "Updated")          (type . 5)  (is_readonly . t))
    ((name . "closed_at")        (display_name . "Closed")           (type . 5)  (is_readonly . t))
    ((name . "locked")           (display_name . "Locked")           (type . 1)  (is_readonly . t))
    ((name . "draft")            (display_name . "Draft")            (type . 1)  (is_readonly . t))
    ;; Description: body content shown separately at the bottom of the buffer
    ((name . "Description")      (display_name . "Description")      (type . 1))
    ;; Internal / URL / noise fields — hidden from display
    ((name . "_display_id")      (display_name . "")  (type . 6)  (is_visible . :json-false))
    ((name . "_resource_path")   (display_name . "")  (type . 1)  (is_visible . :json-false))
    ((name . "body")             (display_name . "")  (type . 1)  (is_visible . :json-false))
    ((name . "id")               (display_name . "")  (type . 6)  (is_visible . :json-false))
    ((name . "node_id")          (display_name . "")  (type . 1)  (is_visible . :json-false))
    ((name . "url")              (display_name . "")  (type . 1)  (is_visible . :json-false))
    ((name . "repository_url")   (display_name . "")  (type . 1)  (is_visible . :json-false))
    ((name . "labels_url")       (display_name . "")  (type . 1)  (is_visible . :json-false))
    ((name . "comments_url")     (display_name . "")  (type . 1)  (is_visible . :json-false))
    ((name . "events_url")       (display_name . "")  (type . 1)  (is_visible . :json-false))
    ((name . "timeline_url")     (display_name . "")  (type . 1)  (is_visible . :json-false))
    ((name . "active_lock_reason") (display_name . "") (type . 1) (is_visible . :json-false))
    ((name . "reactions")        (display_name . "")  (type . 1)  (is_visible . :json-false))
    ((name . "pull_request")     (display_name . "")  (type . 1)  (is_visible . :json-false))
    ((name . "performed_via_github_app") (display_name . "") (type . 1) (is_visible . :json-false)))
  "Static field definitions for GitHub issues.")

;;;###autoload
(defun bug--rpc-github-get-fields (_object _instance)
  "Return static field definitions for GitHub issues."
  `((result . ((fields . ,bug--github-issue-fields)))))

;;;###autoload
(defun bug--github-field-name (field-name _instance)
  "Resolve abstract field names for GitHub issues."
  (cond ((equal :bug-uuid field-name)        '_resource_path)
        ((equal :bug-friendly-id field-name) '_display_id)
        ((equal :bug-summary field-name)     'title)))

;;;###autoload
(defun bug--github-list-columns (_object _instance)
  "Return list columns for GitHub issue lists."
  '("_display_id" "state" "title" "updated_at"))

;;;;;;
;; Issue normalization

(defun bug--github-normalize-issue (issue &optional instance)
  "Normalize a GitHub issue alist for display in bug-mode.

- converts the numeric `number' field to a string
- flattens nested objects:
  - `user' and `assignee' to their login strings
  - `assignees' vector to a comma-separated login string
  - `labels' vector to a comma-separated name string
  - `milestone' to its title string
- boolean fields `locked' and `draft' are set to nil when false so they are
  only shown when true
- adds a `_resource_path' field derived from the API `url' field
  (e.g. \"repos/owner/repo/issues/42\") used as the bug UUID so issues can be
  opened without :project-id configured.
- also adds a `_display_id' field of the form \"repo#N\"
  (e.g. \"combustion-tools#3\") used as the friendly buffer name so issues
  from different repositories with the same number produce distinct buffers."
  (let ((num       (assoc 'number    issue))
        (user      (assoc 'user      issue))
        (assignee  (assoc 'assignee  issue))
        (assignees (assoc 'assignees issue))
        (labels    (assoc 'labels    issue))
        (milestone (assoc 'milestone issue))
        (locked    (assoc 'locked    issue))
        (draft     (assoc 'draft     issue))
        (url-f     (assoc 'url       issue)))
    (when (and num (numberp (cdr num)))
      (setcdr num (number-to-string (cdr num))))
    (when (and user (consp (cdr user)))
      (setcdr user (or (cdr (assoc 'login (cdr user))) "unknown")))
    (when (and assignee (consp (cdr assignee)))
      (let ((login (cdr (assoc 'login (cdr assignee)))))
        (setcdr assignee (when (and login (not (string-empty-p login))) login))))
    (when (and assignees (vectorp (cdr assignees)))
      (let ((logins (mapconcat (lambda (a) (or (cdr (assoc 'login a)) ""))
                               (append (cdr assignees) nil) ", ")))
        (setcdr assignees (unless (string-empty-p logins) logins))))
    (when (and labels (vectorp (cdr labels)))
      (let ((names (mapconcat (lambda (l) (or (cdr (assoc 'name l)) ""))
                              (append (cdr labels) nil) ", ")))
        (setcdr labels (unless (string-empty-p names) names))))
    (when (and milestone (consp (cdr milestone)))
      (setcdr milestone (cdr (assoc 'title (cdr milestone)))))
    ;; hide boolean fields when false — only show when the issue is actually locked/draft
    (when (and locked (equal (cdr locked) :json-false))
      (setcdr locked nil))
    (when (and draft (equal (cdr draft) :json-false))
      (setcdr draft nil))
    (let* ((url-str (when url-f (cdr url-f)))
           (base    (concat (or (when instance (bug--instance-property :url instance))
                                "https://api.github.com")
                            "/"))
           (path    (when (and url-str (stringp url-str) (string-prefix-p base url-str))
                      (substring url-str (length base)))))
      (when path
        (setq issue (cons (cons '_resource_path path) issue)))
      (when (and path (string-match "^repos/[^/]+/\\([^/]+\\)/issues/\\([0-9]+\\)$" path))
        (setq issue (cons (cons '_display_id
                                (format "%s#%s"
                                        (match-string 1 path)
                                        (match-string 2 path)))
                          issue))))
    issue))

;;;;;;
;; Display ID shortening

(defconst bug--github-display-id-max-repo-len 14
  "Maximum length for the repo portion of a _display_id in list columns.

Repos sharing a prefix with others have that prefix abbreviated to initials.
All repos exceeding this length are shortened with token-initial abbreviation
and a trailing \"..\".")

(defun bug--github-find-shared-prefix (tokens all-token-lists)
  "Return the longest token prefix of `tokens' shared by another list.

This is useful for shortening common prefixes for display purposes.

`all-token-lists' is the full set of token lists to search.
Returns nil when no shared prefix exists."
  (catch 'done
    (cl-loop for len from (length tokens) downto 1
             do (when (cl-some (lambda (other)
                                 (and (not (equal other tokens))
                                      (>= (length other) len)
                                      (equal (cl-subseq other 0 len)
                                             (cl-subseq tokens 0 len))))
                               all-token-lists)
                  (throw 'done (cl-subseq tokens 0 len))))
    nil))

(defun bug--github-fit-suffix-tokens (tokens max-chars)
  "Represent `tokens' in `max-chars': whole tokens first, then single initials.

Returns (result-string . abbreviated-p); abbreviated-p is non-nil when any
token was abbreviated or omitted."
  (let ((result '())
        (space max-chars)
        (abbreviated nil)
        (break-phase1 nil))
    ;; Phase 1: include whole tokens
    (dolist (tok tokens)
      (unless break-phase1
        (let* ((sep (if result 1 0))
               (cost (+ sep (length tok))))
          (if (<= cost space)
              (progn (push tok result) (setq space (- space cost)))
            (setq break-phase1 t)))))
    ;; Phase 2: single-letter abbreviations for remaining tokens
    (dolist (tok (nthcdr (length result) tokens))
      (let* ((sep (if result 1 0))
             (cost (+ sep 1)))
        (if (<= cost space)
            (progn (push (substring tok 0 1) result)
                   (setq space (- space cost))
                   (setq abbreviated t))
          (setq abbreviated t))))
    (cons (mapconcat #'identity (nreverse result) "-") abbreviated)))

(defun bug--github-build-display-id-map (repos)
  "Build an alist mapping each repo name to a shortened display name.

Repos sharing a common dash-delimited token prefix have that prefix
replaced by its initials.  Remaining suffixes too long to fit are
represented with token initials and a trailing \"..\"."
  (let* ((max-len bug--github-display-id-max-repo-len)
         (token-map (mapcar (lambda (r) (split-string r "-")) repos)))
    (cl-mapcar
     (lambda (repo tokens)
       (let* ((prefix (bug--github-find-shared-prefix tokens token-map))
              (short
               (cond
                ;; Shared prefix: abbreviate it, fit suffix in remaining budget
                (prefix
                 (let* ((abbrev     (mapconcat (lambda (tok) (substring tok 0 1)) prefix ""))
                        (sfx-tokens (nthcdr (length prefix) tokens))
                        (avail      (- max-len (length abbrev) 3))) ; 1 sep + 2 ".."
                   (cond
                    ((null sfx-tokens) abbrev)
                    ((>= avail 0)
                     (let* ((fit    (bug--github-fit-suffix-tokens sfx-tokens avail))
                            (fit-s  (car fit))
                            (fit-a  (cdr fit)))
                       (if fit-a
                           (concat abbrev "-" fit-s "..")
                         (concat abbrev "-" fit-s))))
                    (t (concat abbrev "..")))))
                ;; No shared prefix and within limit: leave unchanged
                ((<= (length repo) max-len) repo)
                ;; No shared prefix but too long: truncate using same fitting logic
                (t
                 (let* ((fit   (bug--github-fit-suffix-tokens tokens (- max-len 2)))
                        (fit-s (car fit))
                        (fit-a (cdr fit)))
                   (if fit-a (concat fit-s "..") fit-s))))))
         (cons repo short)))
     repos token-map)))

(defun bug--github-abbreviate-display-ids (issues)
  "Shorten _display_id fields across ISSUES where repos share a common prefix.

Returns the modified issue list."
  (let* ((repos (delete-dups
                 (delq nil
                       (mapcar (lambda (issue)
                                 (let ((d (cdr (assoc '_display_id issue))))
                                   (when (and (stringp d)
                                              (string-match "^\\([^#]+\\)#" d))
                                     (match-string 1 d))))
                               issues))))
         (abbrev-map (bug--github-build-display-id-map repos)))
    (mapcar (lambda (issue)
              (let ((d (assoc '_display_id issue)))
                (when (and d (stringp (cdr d))
                           (string-match "^\\([^#]+\\)\\(#[0-9]+\\)$" (cdr d)))
                  (let* ((repo  (match-string 1 (cdr d)))
                         (num   (match-string 2 (cdr d)))
                         (short (cdr (assoc repo abbrev-map))))
                    (when short
                      (setcdr d (concat short num))))))
              issue)
            issues)))

;;;;;;
;; Search functions

(defun bug--github-project-scope (instance)
  "Return the search scope qualifier for the configured project.

If :project-id contains a slash it is treated as `owner/repo' and
\"repo:owner/repo\" is returned.  If it begins with \"user:\" it is a
personal-account scope and is returned as-is.  Otherwise it is treated
as an organisation login and \"org:login\" is returned.  Returns nil
when :project-id is not configured."
  (let ((project-id (bug--instance-property :project-id instance)))
    (when project-id
      (cond ((string-match-p "^user:" project-id) project-id)
            ((string-match-p "/" project-id) (format "repo:%s" project-id))
            (t (format "org:%s" project-id))))))

;;;###autoload
(defun bug--parse-github-search-query (query instance)
  "Parse a GitHub search query string into RPC params.

A bare number is treated as a direct issue lookup when :project-id
is set to an owner/repo.  Everything else becomes a full-text search
constrained by the :project-id scope."
  (let ((project-id (bug--instance-property :project-id instance)))
    (cond
     ;; bare number with a repo-scoped project → direct issue fetch
     ((and (stringp query)
           (string-match "^[0-9]+$" query)
           project-id
           (string-match-p "/" project-id))
      `((resource . ,(format "repos/%s/issues/%s" project-id query))
        (operation . "get")))
     ;; everything else → search/issues
     (t
      (let* ((scope (bug--github-project-scope instance))
             (q (if scope (concat scope " " query) query)))
        `((resource . "search/issues")
          (operation . "get")
          (data . ((q ,q) (per_page "100")))))))))

;;;###autoload
(defun bug--do-github-search (params instance)
  "Execute a GitHub search or direct issue fetch and display the result.

Handles three response shapes:
- Single issue object (direct repos/.../issues/N fetch)
- Array of issue objects (repos/.../issues list endpoint)
- Search result object with total_count and items (search/issues)"
  (let* ((resource (cdr (assoc 'resource params)))
         (direct-p (and resource
                        (string-match-p "^repos/.+/issues/[0-9]+$" resource)))
         (response (unless direct-p (bug-rpc params instance))))
    (cond
     (direct-p
      (let ((issue (bug-rpc params instance)))
        (when issue
          (bug-show (bug--github-normalize-issue issue instance) instance))))
     ((vectorp response)
      ;; array endpoint (e.g. repos/{owner}/{repo}/issues)
      (let ((issues (bug--github-abbreviate-display-ids
                     (mapcar (lambda (i) (bug--github-normalize-issue i instance))
                             (append response nil)))))
        (if (null issues)
            (message "No results")
          (bug-list-show params issues instance))))
     (t
      ;; search/issues: {total_count, items}
      (let* ((total (or (cdr (assoc 'total_count response)) 0))
             (items (cdr (assoc 'items response))))
        (if (= total 0)
            (message "No results")
          (if (= total 1)
              (bug-show (bug--github-normalize-issue (aref items 0) instance) instance)
            (bug-list-show params
                           (bug--github-abbreviate-display-ids
                            (mapcar (lambda (i) (bug--github-normalize-issue i instance))
                                    (append items nil)))
                           instance))))))))

(defun bug--execute-github-search (params instance)
  "Execute a GitHub search and return (results-array . total-count).

Used by the search-candidate infrastructure for type-98 field editing."
  (let* ((resource (cdr (assoc 'resource params)))
         (direct-p (and resource
                        (string-match-p "^repos/.+/issues/[0-9]+$" resource))))
    (if direct-p
        (let ((issue (bug-rpc params instance)))
          (if issue
              (cons (vector (bug--github-normalize-issue issue instance)) 1)
            (cons [] 0)))
      (let* ((response (bug-rpc params instance))
             (total (or (cdr (assoc 'total_count response)) 0))
             (items (cdr (assoc 'items response))))
        (cons (or items []) total)))))

(defun bug--format-github-search-candidates (results)
  "Format GitHub issue results as ((\"#N: title\" . \"N\") ...) alist."
  (mapcar (lambda (issue)
            (let* ((num   (cdr (assoc 'number issue)))
                   (title (cdr (assoc 'title  issue)))
                   (num-str (cond ((stringp num)  num)
                                  ((numberp num)  (number-to-string num))
                                  (t              "?"))))
              (cons (format "#%s: %s" num-str (or title ""))
                    num-str)))
          (append results nil)))

;;;;;;
;; Bug display

;;;###autoload
(defun bug--fetch-github-bug (id instance)
  "Retrieve a single GitHub issue by number.

`id' is the issue number as a string or integer.  :project-id must be
configured as owner/repo in the instance settings."
  (let* ((id-str (cond ((numberp id) (number-to-string id))
                       ((stringp id) id)
                       (t (error "Invalid issue ID: %S" id))))
         (resource (if (string-match-p "^repos/" id-str)
                       id-str
                     (let ((project-id (bug--instance-property :project-id instance)))
                       (if project-id
                           (format "repos/%s/issues/%s" project-id id-str)
                         (error "No :project-id configured for GitHub instance"))))))
    (bug--github-normalize-issue
     (bug-rpc `((resource . ,resource) (operation . "get")) instance) instance)))

;;;###autoload
(defun bug--browse-github-bug (id instance)
  "Open a GitHub issue in the browser."
  (let* ((url (cond
               ;; _resource_path format: repos/owner/repo/issues/N
               ((string-match "^repos/\\([^/]+/[^/]+\\)/issues/\\([0-9]+\\)$" id)
                (format "https://github.com/%s/issues/%s"
                        (match-string 1 id) (match-string 2 id)))
               ;; _display_id format: repo#N — owner from :project-id
               ((string-match "^\\([^#]+\\)#\\([0-9]+\\)$" id)
                (let* ((repo  (match-string 1 id))
                       (num   (match-string 2 id))
                       (pid   (bug--instance-property :project-id instance))
                       (owner (cond
                               ((and pid (string-match "^\\([^/]+\\)/" pid))
                                (match-string 1 pid))
                               ((and pid (not (string-match-p "/" pid))
                                     (not (string-match-p "^user:" pid)))
                                pid))))
                  (if owner
                      (format "https://github.com/%s/%s/issues/%s" owner repo num)
                    (error "Cannot determine owner for issue %s" id))))
               (t
                (let ((project-id (bug--instance-property :project-id instance)))
                  (if (and project-id (string-match-p "/" project-id))
                      (format "https://github.com/%s/issues/%s" project-id id)
                    (error "Cannot determine GitHub URL for issue %s" id)))))))
    (browse-url url)))

;;;;;;
;; Projects

;;;###autoload
(defun bug--list-github-project-bugs (project-id instance)
  "Return search params for listing all open issues in `project-id'.

`project-id' may be:
- owner/repo    — list open issues for that single repository via the
                  issues list endpoint (first 100 results)
- org-login     — search across all repositories in the organisation
                  via the search/issues endpoint
- user:login    — search across all personal repositories of the user
                  via the search/issues endpoint

Falls back to :project-id in instance config when `project-id' is nil."
  (let ((scope (or project-id (bug--instance-property :project-id instance))))
    (unless scope
      (error "No project-id provided and no :project-id configured"))
    (cond
     ((string-match-p "/" scope)
      ;; single-repo scope: use the issues list endpoint
      `((resource . ,(format "repos/%s/issues" scope))
        (operation . "get")
        (data . ((state "open") (per_page "100")))))
     ((string-match-p "^user:" scope)
      ;; personal account scope: search/issues with user: qualifier
      `((resource . "search/issues")
        (operation . "get")
        (data . ((q ,(format "%s is:open is:issue" scope))
                 (per_page "100")))))
     (t
      ;; org scope: use the search endpoint
      `((resource . "search/issues")
        (operation . "get")
        (data . ((q ,(format "org:%s is:open is:issue" scope))
                 (per_page "100"))))))))

;;;###autoload
(defun bug--list-github-projects (_args instance)
  "Return projects accessible to the configured token.

Includes the authenticated user's personal account (stored as \"user:login\"
for use as :project-id) followed by all organisations the token can access
(stored as org-login).  Returns an alist of (display-name . id) pairs
suitable for `bug-list-projects' and `bug-select-project'."
  (let* ((user-response (bug-rpc `((resource . "user") (operation . "get")) instance))
         (user-login (when (and user-response (listp user-response))
                       (let ((l (cdr (assoc 'login user-response))))
                         (when l (if (symbolp l) (symbol-name l) l)))))
         (personal (when user-login
                     (list (cons (format "Personal (%s)" user-login)
                                 (format "user:%s" user-login)))))
         (orgs-response (bug-rpc `((resource . "user/orgs")
                                   (operation . "get")
                                   (data . ((per_page "100"))))
                                 instance))
         (orgs (when (vectorp orgs-response)
                 (mapcar (lambda (org)
                           (let* ((login (let ((l (cdr (assoc 'login org))))
                                           (if (symbolp l) (symbol-name l) l)))
                                  (name  (let ((n (cdr (assoc 'name org))))
                                           (if (and n (not (eq n :json-false))
                                                    (not (string-empty-p
                                                          (if (symbolp n) (symbol-name n) n))))
                                               (if (symbolp n) (symbol-name n) n)
                                             login))))
                             (cons name login)))
                         (append orgs-response nil)))))
    (append personal orgs)))

;;;;;;
;; Comments

(defun bug--github-get-comments (resource-path id instance &optional issue)
  "Fetch and display comments for the GitHub issue at `resource-path'.

`id' is the friendly issue number used to locate the bug buffer.
When `issue' is provided, its body is shown as the first entry."
  (let* ((response (bug-rpc `((resource . ,(concat resource-path "/comments"))
                              (operation . "get"))
                            instance))
         (issue-user    (when issue (cdr (assoc 'user issue))))
         (issue-created (when issue (cdr (assoc 'created_at issue))))
         (issue-body    (when issue (cdr (assoc 'body issue)))))
    (when (or issue-body (vectorp response))
      (with-current-buffer (bug--buffer-string id instance)
        (setq buffer-read-only nil)
        (save-excursion
          (let ((cstart (bug--find-section-content-start 'comments)))
            (if cstart
                (let ((count 0))
                  (delete-region cstart (point-max))
                  (insert "\n")
                  (when (and issue-body (not (string-empty-p issue-body)))
                    (insert (bug--format-comment-entry
                             "Issue" nil issue-user issue-created issue-body))
                    (when (and (vectorp response) (> (length response) 0))
                      (insert "\n\n")))
                  (when (vectorp response)
                    (insert (mapconcat
                             (lambda (comment)
                               (setq count (1+ count))
                               (let* ((user-obj (cdr (assoc 'user comment)))
                                      (login    (or (when (consp user-obj)
                                                      (let ((l (cdr (assoc 'login user-obj))))
                                                        (if (symbolp l) (symbol-name l) l)))
                                                    "unknown"))
                                      (created  (or (cdr (assoc 'created_at comment)) ""))
                                      (body     (or (cdr (assoc 'body comment)) "")))
                                 (bug--format-comment-entry
                                  "Comment" count login created body)))
                             (append response nil)
                             "\n\n"))))
              (error "Could not find comments section in buffer"))))
        (setq buffer-read-only t)))))

;;;###autoload
(defun bug--backend-github-show-additional-data (bug instance)
  "Insert and load GitHub issue comments in the current bug buffer."
  (bug--insert-section-header 'comments)
  (when (and bug---uuid bug-autoload-comments)
    (bug--github-get-comments bug---uuid bug---id instance bug)))

(provide 'bug-backend-github)
;;; bug-backend-github.el ends here
