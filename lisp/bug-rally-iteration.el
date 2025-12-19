;; bug-rally-iteration.el --- queries for Rally iterations -*- lexical-binding: t; -*-
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

(require 'bug-rpc)
(require 'bug-common-functions)
(require 'bug-search-common)
(require 'org)

;; TODO: Add filtering by project
;;       Add property to configure tasks in iteration
;;       Fetch list of users for completion
(defun bug-rally-iteration-for (user &optional instance)
  "Display the current iteration for a specific user"
  (interactive
   (let ((default-user)
         (user-prompt "User: ")
         (instance nil))
     (when current-prefix-arg
       (setq instance (bug--query-instance)))
     (setq default-user (bug--instance-property :user instance))
     (if default-user (setq user-prompt (format "User (%s): " default-user)))
     (list (read-string user-prompt nil nil default-user) instance)))
  (bug-rally-iteration instance user nil))

(defun bug-rally-iteration-at (date &optional instance)
  "Display the iteration at a selected date"
  (interactive
   (if current-prefix-arg
       (nreverse (list (bug--query-instance)
                       (org-read-date)))
     (list (org-read-date))))
  (bug-rally-iteration instance nil date))

(defun bug-rally-iteration (instance &optional user date)
  "When called interactively display the current iteration. If the current "
  (let* ((date (or date "today"))
         (user (or user (bug--instance-property :user instance)))
         (query-string
          (format "(( Iteration.StartDate <= %s ) AND ( Iteration.EndDate >= %s ))"
                  date date)))
    (when user
      (setq query-string
            (format "(( Owner.Name = %s ) AND %s )" user query-string)))

    (bug--do-search
     `((resource . "HierarchicalRequirement")
       (list-columns . ("FormattedID" "Name" "ScheduleState" "Owner" "PlanEstimate" "ToDo" "LastUpdateDate"))
       (data .
             ((fetch "Iteration,PlanEstimate,ScheduleState,Owner,ToDo,Name,FormattedID,LastUpdateDate")
              (query ,query-string))))
     instance)))

(provide 'bug-rally-iteration)
;;; bug-rally-iteration.el ends here
