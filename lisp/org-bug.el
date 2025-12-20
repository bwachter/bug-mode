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

(org-link-set-parameters "bug" :activate-func 'org-bug-open)
(org-link-set-parameters "bug-search" :activate-func 'org-bug-open-search)
(add-hook 'org-store-link-functions 'org-bug-store-link)

(defun org-bug-open (bug)
  "Open the specified bug"
  (let* ((bug (split-string bug "/" t))
         (bug-id (cadr bug))
         (bug-instance (car bug)))
    (bug-open bug-id bug-instance)))

(defun org-bug-open-search (bug)
  "Search for the the specified bug"
  (let* ((bug (split-string bug "/" t))
         (bug-id (cadr bug))
         (bug-instance (car bug)))
  (bug-search bug-id bug-instance)))

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
