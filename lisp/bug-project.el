;;; bug-project.el --- generic project support for bug-mode -*- lexical-binding: t; -*-
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
(require 'bug-vars)
(require 'bug-instance)
(require 'bug-transient-builder)
(require 'bug-repo)
(require 'vtable)

(bug-transient-define-prefix bug--project-mode-menu
  :blocks '(project interact rally))

(defvar bug-project-mode-map
  (let ((keymap (copy-keymap special-mode-map)))
    (define-key keymap bug-menu-key #'bug--project-mode-menu)
    keymap)
  "Keymap for bug project mode")

(define-derived-mode bug-project-mode special-mode "Bug instances"
  "Mode for a list of bug instances")

;;;###autoload
(defun bug-list-projects (&optional instance)
  "Display all projects for `instance' in a buffer.

Requires the backend to support the :projects feature.  Each row shows
the project name and its ID (the value to use for :project-id in instance
config).  Press `c' on a row to copy the ID to the kill ring."
  (interactive (list (bug--instance-query :projects)))
  (let* ((projects (bug--instance-backend-function "bug--list-%s-projects" nil instance))
         (buffer (get-buffer-create
                  (format "*%s projects*"
                          (prin1-to-string (bug--instance-backend-type instance) t)))))
    (if (null projects)
        (message "No projects found")
      (with-current-buffer buffer
        (bug-project-mode)
        (when (bound-and-true-p flyspell-mode)
          (flyspell-mode -1))
        (setq bug---instance instance)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "Total: %d projects\n\n" (length projects)))
          (insert "To set a default project, add :project-id \"<ProjectID>\" to your instance config.\n\n")

          (make-vtable
           :face 'bug-header-field
           :columns '((:name "Project Name" :width 40)
                      (:name "ID"           :width 20))
           :objects projects
           :formatter (lambda (value _column _vtable)
                        (propertize (format "%s" value) 'face 'bug-list-item))
           :getter (lambda (project column vtable)
                     (pcase (vtable-column vtable column)
                       ("Project Name" (car project))
                       ("ID"           (cdr project))))
           :actions `("RET" ,(lambda (project)
                               (bug-list-project-bugs (cdr project) bug---instance))))
          (goto-char (point-min))))
      (pop-to-buffer buffer))))

;;;###autoload
(defun bug-select-project (&optional instance)
  "Interactively select a project for `instance' and return its ID string.

The returned value is the project identifier suitable for use as
:project-id in instance config.  Returns nil if no projects are found
or the selection is cancelled."
  (interactive (list (bug--instance-query :projects)))
  (let ((projects (bug--instance-backend-function "bug--list-%s-projects" nil instance)))
    (if (null projects)
        (progn (message "No projects found") nil)
      (let* ((name (completing-read "Select project: " (mapcar #'car projects) nil t))
             (id   (cdr (assoc name projects))))
        id))))

;;;###autoload
(defun bug-list-project-bugs (&optional project-id instance)
  "List all open bugs for the configured project of `instance'.

Requires the backend to support the :project-bugs feature.  Uses
:project-id from instance config; if not set, falls back to an
interactive project selection when the backend also supports :projects."
  (interactive (list (bug--instance-query :project-bugs)))
  (unless (bug--instance-feature instance :project-bugs)
    (error "Backend does not support project bug listing"))
  (let* ((project-id
          (or project-id
              (bug--instance-property :project-id instance)
              (when (bug--instance-feature instance :projects)
                (bug-select-project instance))))
         (params (bug--instance-backend-function
                  "bug--list-%s-project-bugs" project-id instance)))
    (unless project-id
      (user-error "No :project-id configured and no project selection available"))
    (if params
        (bug--do-search params instance)
      (message "No open bugs found for project"))))

;;;###autoload
(defun bug-project-create (name &optional instance)
  "Create a new project in the bug tracker.

`name' is the project name. Requires the backend to support the
:project-create feature and appropriate permissions (e.g. admin). We
don't check for permissions before trying to create.

With a prefix argument, prompts for which instance to use."
  (interactive
   (list (read-string "Project Name: ")
         (if current-prefix-arg
             (bug--instance-to-symbolp (bug--instance-query :project-create))
           (bug--instance-to-symbolp nil))))
  (unless (bug--instance-feature instance :project-create)
    (error "Backend does not support project creation"))
  (bug--instance-backend-function "bug--create-%s-project" name instance))

;;;###autoload
(defun bug-project-get-current (&optional instance)
  "Get the currently active project scope following resolution hierarchy.

Resolution order:
1. Transient-local project (when inside a dynamic transient menu)
2. Buffer-local `bug---project' (if set and buffer exists)
3. Instance's `:project-id' property
4. Auto-detected from git remotes via `bug--repo-scope'

Optional `instance' is used for steps 3 and 4; if nil, resolves via
`bug-instance-get-current'.

Returns project scope string or nil if no project is configured."
  (or
   ;; 1. Transient-local project
   (and (boundp 'transient--prefix)
        transient--prefix
        (plist-get (oref transient--prefix scope) :project))
   ;; 2. Buffer-local project
   (and (boundp 'bug---project) bug---project)
   ;; 3. Instance-specific project
   (let ((inst (or instance (bug-instance-get-current))))
     (and inst (bug--instance-property :project-id inst)))
   ;; 4. Auto-detected from repository
   (bug--repo-scope (or instance (bug-instance-get-current)))))

(provide 'bug-project)
;;; bug-project.el ends here
