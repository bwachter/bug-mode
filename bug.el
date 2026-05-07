;;; bug.el --- work with bug trackers from within emacs  -*- lexical-binding: t; -*-
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

(require 'transient)

(unless (or (> emacs-major-version 24)
            (and (= emacs-major-version 24)
                 (>= emacs-minor-version 3)))
  (error "Your emacs is too old, you need at least 24.3"))

(add-to-list 'load-path
             (directory-file-name
              (concat
               (file-name-directory (or load-file-name (buffer-file-name)))
               "lisp")))

(defvar bug--available-backends
  (let (backends)
    (dolist (backend-file
             (directory-files
              (concat (file-name-directory (or load-file-name
                                               (buffer-file-name)))
                      "/lisp/")
              nil "bug-backend-.*.el$"))
      (let (backend)
        (setq backend (replace-regexp-in-string "^bug-backend-" "" backend-file))
        (setq backend (replace-regexp-in-string ".el$" "" backend))
        (push backend backends)))
    backends))

(require 'bug-autoloads)
(require 'bug-custom)
(require 'bug-persistent-data)
(require 'bug-repo)

;;;;;;
;; startup code to read persistent data
(bug--read-data-file)

;; Unhandled, as they are still bz specific, and need work:
;; bug-login/bug-logout
(transient-define-prefix bug-menu ()
  "Top level bug mode menu"
  [:pad-keys t
             ["Instances"
              ("l" "List instances" bug-list-instances)
              ("a" "Switch active instance" bug-instance-switch)
              ("d" "Deactivate current active instance" bug-instance-deactivate)]
             ["Bug"
              ("c" "Create by prompting for instance" bug-create)
              ("o" "Open by prompting ID and instance" bug-open)]
             ["Search"
              ("s" "Search string" bug-search)
              ("j" "JQL search" bug-search-jql)
              ("S" "Project-scoped search" bug-search-project)
              ("J" "Project-scoped JQL search" bug-search-jql-project)]
             ["Repository"
              :if (lambda () (bug--repo-matching-instances))
              ("r" "Repo issues" (lambda ()
                                   (interactive)
                                   (let* ((matches (bug--repo-matching-instances))
                                          (single (and (= 1 (length matches))
                                                       (car matches)))
                                          (instance (or (car single)
                                                        (bug--instance-to-symbolp nil :project-bugs)))
                                          (scope (or (cdr single)
                                                     (bug--repo-scope instance))))
                                     (bug-list-project-bugs scope instance))))]
             ["Misc"
              ("R" "Clear metadata cache" bug-cache-clear)]])

(provide 'bug)
;;; bug.el ends here
