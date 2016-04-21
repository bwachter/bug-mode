;; bug-rally-subscription-mode.el --- display details for a Rally subscription
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

(defvar bug-rally-subscription-mode-map
  (let ((keymap (copy-keymap special-mode-map)))
    (define-key keymap "q"         'bug--mode-default-quit-window)
    keymap)
  "Keymap for Rally subscription mode")

(define-derived-mode bug-rally-subscription-mode special-mode "Rally subscription"
  "Show information about a rally subscription"
  )

;;;###autoload
(defun bug-rally-subscription (&optional instance)
  "Show details about a Rally subscription"
  (interactive
   (if current-prefix-arg
       (list (bug-query-instance))))
  (unless (string= "rally" (bug-instance-property :type instance))
    (error "Not a Rally instance"))
  (let ((subscription (car (bug-rpc "Subscription.query"
                                    '((query-data
                                       ((query ""))))))))
    (switch-to-buffer
     (format "rally subscription: %s"
             (cdr (assoc 'Name subscription))))
    (bug-rally-subscription-mode)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert
     (propertize "Subscription information" 'face 'bug-section-header)
     "\n\n"

     (bug--format-kv "Name"
                     (cdr (assoc 'Name subscription)))
     (bug--format-kv "ID"
                     (prin1-to-string
                      (cdr (assoc 'SubscriptionID subscription))))
     (bug--format-kv "Type"
                     (cdr (assoc 'SubscriptionType subscription)))
     (bug--format-kv "Created"
                     (bug--format-time-date
                      (cdr (assoc 'CreationDate subscription)) t))
     (bug--format-kv "Expires"
                     (bug--format-time-date
                      (cdr (assoc 'ExpirationDate subscription)) t))

     "\n"
     (propertize "Workspaces" 'face 'bug-section-header)
     "\n\n"
     (let ((workspaces
            (cdr (assoc 'Results
                        (assoc 'QueryResult
                               (bug-rpc
                                "Subscription.read"
                                `((object-id .
                                   ,(prin1-to-string
                                     (cdr (assoc 'ObjectID subscription))))
                                  (object-type . "Workspaces")))))))
           (workspace-string ""))
       (let ((count (- (length workspaces) 1)))
         (while (>= count 0)
           (setq workspace-string
                 (concat workspace-string
                         "- "
                         (cdr (assoc 'Name (aref workspaces count)))
                         "\n"))
           (setq count (- count 1))))
       workspace-string)

     "\n"
     (propertize "Enabled features" 'face 'bug-section-header)
     "\n\n"

     (bug--format-kv "API Keys"
                     (bug--format-bool(cdr (assoc 'ApiKeysEnabled subscription))))
     (bug--format-kv "Email"
                     (bug--format-bool(cdr (assoc 'EmailEnabled subscription))))
     (bug--format-kv "JSONP"
                     (bug--format-bool(cdr (assoc 'JSONPEnabled subscription))))
     (bug--format-kv "Project hierarchy"
                     (bug--format-bool
                      (cdr
                       (assoc 'ProjectHierarchyEnabled subscription))))

     (let ((hierarchy-enabled (cdr
                               (assoc 'StoryHierarchyEnabled subscription)))
           (value-string))
       (if hierarchy-enabled
           (setq value-string (concat
                               (bug--format-bool hierarchy-enabled)
                               " ("
                               (cdr (assoc 'StoryHierarchyType subscription))
                               ")"))
         (setq value-string (bug--format-bool hierarchy-enabled)))
       (bug--format-kv "Story hierarchy" value-string))

      "\n"
     (propertize "Subscription limits" 'face 'bug-section-header)
     "\n\n"

     (bug--format-kv "Password expiration time (days)"
                     (prin1-to-string
                      (cdr (assoc 'PasswordExpirationDays subscription))))
     (bug--format-kv "Password history length"
                     (prin1-to-string
                      (cdr (assoc 'PreviousPasswordCount subscription))))
     (bug--format-kv "Maximum number of projects"
                     (prin1-to-string
                      (cdr (assoc 'MaximumProjects subscription))))
     (bug--format-kv "Maximum number of custom fields"
                     (prin1-to-string
                      (cdr (assoc 'MaximumCustomUserFields subscription))))
     (bug--format-kv "Session timeout (seconds)"
                     (prin1-to-string
                      (cdr (assoc 'SessionTimeoutSeconds subscription))))

     "\n"
     (propertize "Enabled modules" 'face 'bug-section-header)
     "\n\n"

     (let ((modules (split-string (cdr (assoc 'Modules subscription)) ","))
           (module-string ""))
       (while modules
         (setq module-string
               (concat module-string
                       (propertize (format "- %s\n" (car modules))
                     'face 'bug-field-type-0)))
         (setq modules (cdr modules)))
       module-string)))
  (goto-char 0)
  (setq buffer-read-only t))

(provide 'bug-rally-subscription-mode)
;;; bug-rally-subscription-mode.el ends here
