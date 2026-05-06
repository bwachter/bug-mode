;; org-bug.el --- org-mode integration for bug-mode -*- lexical-binding: t; -*-
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

(require 'org)
(require 'bug-list-mode)
(require 'bug-search)

(org-link-set-parameters "bug" :follow 'org-bug-open)
(org-link-set-parameters "bug-search" :follow 'org-bug-open-search)
(add-hook 'org-store-link-functions 'org-bug-store-link)

(defun org-bug-open (bug)
  "Open the specified bug"
  (let* ((bug (split-string bug "/" t))
         (bug-id (cadr bug))
         (bug-instance (car bug)))
    (bug-open bug-id bug-instance)))

(defun org-bug-open-search (bug)
  "Search for the specified bug.

Link format: instance[/jql][/project-id]/query

The path is parsed as follows:
  instance/query              → native, unscoped
  instance//query             → native, default project scope
  instance/pid/query          → native, project-scoped to pid
  instance/jql/query          → JQL, unscoped
  instance/jql//query         → JQL, default project scope
  instance/jql/pid/query      → JQL, project-scoped to pid

The optional /jql/ segment selects JQL translation.
A double slash (//) after instance (or after /jql/) means
project-scoped to the instance's default project.
A project-id between slashes means project-scoped to that project."
  (let* ((parts (split-string bug "/" nil))
         (bug-instance (car parts))
         (parts (cdr parts))
         (jql-p (and parts (string= (car parts) "jql")))
         (parts (if jql-p (cdr parts) parts))
         (project (cond ((null parts) nil)
                        ((string= (car parts) "") t)
                        ((> (length parts) 1) (car parts))
                        (t nil)))
         (query (mapconcat #'identity
                           (if (or (null parts)
                                   (string= (car parts) "")
                                   (> (length parts) 1))
                               (cdr parts)
                             parts)
                           "/")))
    (bug--debug (format "org-bug-open-search: raw=%S parts=%S instance=%S jql=%S project=%S query=%S"
                        bug parts bug-instance jql-p project query))
    (if jql-p
        (if (eq project t)
            (bug-search-jql-project query bug-instance)
          (if project
              (let ((bug---project project))
                (bug-search-jql-project query bug-instance))
            (bug-search-jql query bug-instance)))
      (if (eq project t)
          (bug-search-project query bug-instance)
        (if project
            (let ((bug---project project))
              (bug-search-project query bug-instance))
          (bug-search query bug-instance))))))

(defun org-bug-store-link ()
  "Store a link to a bug."
  (when (memq major-mode '(bug-mode bug-list-mode))
    (let* ((bug-details (org-bug-get-details))
           (bug-instance (cdr (assoc 'instance bug-details)))
           (bug-id (cdr (assoc 'bug-id bug-details)))
           (bug-uuid (cdr (assoc 'bug-uuid bug-details)))
           (link-type (if bug-uuid "bug" "bug-search"))
           (link (concat link-type
                         ":" (prin1-to-string bug-instance t) "/"
                         (prin1-to-string (or bug-uuid bug-id) t)))
           (description (format "Bug %s" (prin1-to-string bug-id t))))
      (org-link-store-props
       :type link-type
       :link link
       :description description))))

(defun org-bug-get-details ()
  "Find bug details from current bug-list-mode or bug-mode buffer"
  (cond ((eq major-mode 'bug-list-mode)
         (bug-list-mode-bug-near-point))
        ((eq major-mode 'bug-mode)
         (let* ((bug-instance (if (boundp 'bug---instance)
                                  bug---instance bug-default-instance))
                (bug-uuid (if (boundp 'bug---uuid)
                              bug---uuid nil))
                (bug-id (if (boundp 'bug---id)
                            bug---id nil)))
           `((bug-id . ,bug-id)(bug-uuid . ,bug-uuid)(instance . ,bug-instance))))
        (t
         (error "How did you get here?"))))

(provide 'org-bug)
;;; org-bug.el ends here
