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
(require 'transient)
(require 'vtable)

;; TODO, we probably should add a transient here too, for stuff like copy id
(defclass bug-project-prefix (transient-prefix)
  ((current-project-name :initarg :current-project-name :initform nil)
   (current-project-id :initarg :current-project-id :initform nil))
  "Custom transient class tracking project specific bug attributes.")

;;; bytecompiler stubs for functions in the transient (and only those)
;;  make sure they can be autoloaded!

(declare-function bug-rally-subscription "bug-rally-subscription")
(declare-function bug-create "bug-mode")

(transient-define-prefix bug-project-mode-menu ()
  "Transient for bug-project-mode"
  :class 'bug-project-prefix
  :init-value (lambda (obj)
                (let ((entry (vtable-current-object)))
                  (oset obj current-project-name (car entry))
                  (oset obj current-project-id (cdr entry))))
  [[:description
    (lambda ()
      (let ((obj (transient-prefix-object)))
        (format "Project%s"
                (if-let ((name (oref obj current-project-name)))
                    (format " %s" name)
                  ""))))
    ("p" "Copy project ID" (lambda ()
                             (interactive)
                             (let* ((obj (transient-prefix-object))
                                    (project-id (oref obj current-project-id)))
                               (kill-new project-id)
                               (message "Copied ID: %s" project-id)))
     :if (lambda ()
           (let ((obj (transient-prefix-object)))
             (oref obj current-project-id))))
    ("l" "List bugs for project" (lambda ()
                                   (interactive)
                                   (let* ((obj (transient-prefix-object))
                                          (project-id (oref obj current-project-id)))
                                     (bug-list-project-bugs project-id bug---instance))))]
   ["Interact"
    ("i"  "Info"  bug--bug-mode-info)
    ("c"  "Create bug" (lambda ()
                         (interactive)
                         (bug-create bug---instance))
     :if (lambda () (and
                     (bound-and-true-p bug---instance)
                     (bug--instance-backend-feature bug---instance :create))))]
   ["Rally"
    :if (lambda () (and (bound-and-true-p bug---instance)
                        (equal 'rally (bug--instance-backend-type bug---instance))))
    ("s" "Subscription" (lambda ()
                          (interactive)
                          (bug-rally-subscription bug---instance)))]])

(defvar bug-project-mode-map
  (let ((keymap (copy-keymap special-mode-map)))
    (define-key keymap bug-menu-key #'bug-project-mode-menu)
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
  (interactive (list (bug--query-instance :projects)))
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
  (interactive (list (bug--query-instance :projects)))
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
  (interactive (list (bug--query-instance :project-bugs)))
  (unless (bug--instance-backend-feature instance :project-bugs)
    (error "Backend does not support project bug listing"))
  (let* ((project-id
          (or project-id
              (bug--instance-property :project-id instance)
              (when (bug--instance-backend-feature instance :projects)
                (bug-select-project instance))))
         (params (bug--instance-backend-function
                  "bug--list-%s-project-bugs" project-id instance)))
    (unless project-id
      (user-error "No :project-id configured and no project selection available"))
    (if params
        (bug--do-search params instance)
      (message "No open bugs found for project"))))

(provide 'bug-project)
;;; bug-project.el ends here
