;;; bug-repo.el --- repository integration for bug-mode -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2010-2026 bug-mode developers
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
;; Repository integration complements the existing bug-tracker "project"
;; support in bug-project.el.  Whereas "project" there refers to a Rally
;; workspace project, a Bugzilla product, or a GitHub org/repo scope, the
;; functions here deal with the *git repository* the user is working in.
;;
;; This makes it possible to open issues for the current repository without
;; explicitly configuring :project-id in the instance settings.
;;
;; Future work / mixed backend support:
;; A single git repository may have remotes pointing to multiple hosts
;; (e.g. GitHub origin + GitLab upstream).  We currently detect scope
;; for a single instance at a time.  A future enhancement could be
;; `bug-repo-issues-all' which queries all matching backends and merges
;; results into a single view.  This needs careful design for:
;; - consistent ID display across backends
;; - deduplication of issues mirrored across hosts
;; - unified filtering/sorting
;; - instance-specific auth handling
;;
;;; Code:

(require 'bug-instance)
(require 'bug-custom)
(require 'project)
(require 'vc-git)

(defvar bug-repo-override nil
  "Dir-local override for the bug tracker scope of the current repository.

When non-nil, this value is used as the scope instead of
auto-detecting from git remotes or reading :project-id from the
instance configuration.  Set via .dir-locals.el:

  ((nil . ((bug-repo-override . \"owner/repo\"))))")

(defun bug--repo-root ()
  "Return the root of the current project, or nil.

Uses `project-current' from project.el.  Only returns a root when
the current buffer is inside a recognized project."
  (when-let ((project (project-current)))
    (project-root project)))

(defun bug--repo-remotes (&optional dir)
  "Return an alist of (name . url) for git remotes in `dir'.

`dir' defaults to the current project root.  Uses `vc-git-command'
rather than raw git subprocess calls."
  (let ((default-directory (or dir (bug--repo-root) default-directory)))
    (with-temp-buffer
      (when (ignore-errors
              (= 0 (vc-git-command t 0 nil
                                   "config" "--local" "--get-regexp"
                                   "^remote\\..*\\.url$")))
        (delq nil
              (mapcar
               (lambda (line)
                 (when (string-match
                        "^remote\\.\\(.+\\)\\.url \\(.+\\)$"
                        line)
                   (cons (match-string 1 line)
                         (match-string 2 line))))
               (split-string (buffer-string) "\n" t)))))))

(defun bug--repo-parse-url (url)
  "Parse a git remote `url' into a plist.

Returns `(:host HOST :owner OWNER :repo REPO)' for recognized URLs.
Supports HTTPS and SSH formats.

Examples:
  https://github.com/owner/repo.git
  git@github.com:owner/repo.git
  ssh://git@github.com/owner/repo.git"
  (cond
   ;; HTTPS: https://host/owner/repo.git
   ((string-match
     "^https?://\\([^/:]+\\(?::[0-9]+\\)?\\)/\\([^/]+\\)/\\([^/]+?\\)\\(?:\\.git\\)?/?$"
     url)
    (list :host (match-string 1 url)
          :owner (match-string 2 url)
          :repo (match-string 3 url)))
   ;; SSH: git@host:owner/repo.git
   ((string-match
     "^git@\\([^:]+\\):\\([^/]+\\)/\\([^/]+?\\)\\(?:\\.git\\)?$"
     url)
    (list :host (match-string 1 url)
          :owner (match-string 2 url)
          :repo (match-string 3 url)))
   ;; SSH alternative: ssh://git@host/owner/repo.git
   ((string-match
     "^ssh://git@\\([^/]+\\)/\\([^/]+\\)/\\([^/]+?\\)\\(?:\\.git\\)?$"
     url)
    (list :host (match-string 1 url)
          :owner (match-string 2 url)
          :repo (match-string 3 url)))
   (t nil)))

(defun bug--repo-detect-scope (instance &optional remote-name)
  "Derive a bug tracker scope for `instance' from the current git repository.

Uses `remote-name' (default `bug-repo-default-remote').  Returns nil
if auto-detection is disabled, the backend does not support it,
or the remote cannot be parsed."
  (when bug-repo-detect-from-remotes
    (let* ((remote-name (or remote-name bug-repo-default-remote))
           (remotes (bug--repo-remotes)))
      (when-let ((url (cdr (assoc remote-name remotes))))
        (let ((parsed (bug--repo-parse-url url)))
          (when parsed
            (bug--instance-backend-function-optional
             "bug--backend-%s-repo-scope" parsed instance)))))))

(defun bug--repo-scope (instance)
  "Return the effective scope for `instance'.

Resolution order:
- `bug-repo-override' (dir-local or project-specific)
- auto-detected from git remotes (when `bug-repo-detect-from-remotes' is t)"
  (or (and (boundp 'bug-repo-override) bug-repo-override)
      (bug--repo-detect-scope instance)))

(defun bug--repo-matching-instances ()
  "Return non-nil when a configured backend matches current project remotes.

Only checks remotes when inside a project detected by `project.el'.
This is a building block for future mixed-backend support.  Each
backend that supports repository detection is asked whether it can
handle any of the current remotes.  The result is a list of
(instance . scope) pairs, or nil when no match is found."
  (when (project-current)
    (let ((remotes (bug--repo-remotes))
          (results nil))
      (dolist (inst-info (bug--instance-get-all))
        (let ((instance (car inst-info)))
          (dolist (remote remotes)
            (let ((parsed (bug--repo-parse-url (cdr remote))))
              (when parsed
                (when-let ((scope (bug--instance-backend-function-optional
                                   "bug--backend-%s-repo-scope"
                                   parsed instance)))
                  (push (cons instance scope) results)))))))
      (delete-dups results))))

(provide 'bug-repo)
;;; bug-repo.el ends here
