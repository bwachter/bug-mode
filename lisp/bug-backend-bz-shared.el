;;; bug-backend-bz-shared.el --- Implementation shared between REST and RPC -*- lexical-binding: t; -*-
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
;;; Code:

(require 'bug-rpc)
(require 'bug-mode)
(require 'bug-list-mode)
(require 'bug-common-functions)
(require 'bug-format)
(require 'bug-custom)

;;;;;;
;; Field metadata

(defun bug--bz-shared-get-fields (_object instance)
  "Download the field list for Bugzilla, map types, and add synthetic
metadata for fields returned by Bug.get that are missing from Bug.fields."
  (let* ((response (bug-rpc '((resource . "Bug")
                              (operation . "fields")) instance))
         (result (cdr (assoc 'result response)))
         (bz-fields (cdr (assoc 'fields result)))
         (synthetic-fields
          '(((name . "actual_time") (display_name . "Actual Time") (type . 0))
            ((name . "remaining_time") (display_name . "Remaining Time") (type . 0))
            ((name . "estimated_time") (display_name . "Estimated Time") (type . 0))
            ((name . "assigned_to_detail") (display_name . "Assignee Details") (type . 0))
            ((name . "cc_detail") (display_name . "CC Details") (type . 0))
            ((name . "creator_detail") (display_name . "Creator Details") (type . 0))
            ((name . "dupe_of") (display_name . "Duplicate Of") (type . 6))
            ((name . "is_open") (display_name . "Open") (type . 0))
            ((name . "update_token") (display_name . "Update Token") (type . 0))
            ((name . "see_also") (display_name . "See Also") (type . 7)))))
    `((result . ((fields . ,(append (append bz-fields nil) synthetic-fields)))))))

(defun bug--bz-shared-map-field (field-name _instance)
  "Map between Bugzilla internal field names and Bug.get JSON field names.

Bug.fields uses names like bug_id/short_desc/creation_ts while Bug.get
returns id/summary/creation_time.  Returns the mapped name, or nil if
no mapping is needed.

`_instance' is ignored but required by the backend function dispatcher."
  (cond
   ;; Bug.fields → Bug.get
   ((string= field-name "bug_id")           "id")
   ((string= field-name "short_desc")       "summary")
   ((string= field-name "creation_ts")      "creation_time")
   ((string= field-name "delta_ts")         "last_change_time")
   ((string= field-name "blocked")          "blocks")
   ((string= field-name "dependson")        "depends_on")
   ((string= field-name "bug_status")       "status")
   ((string= field-name "bug_severity")     "severity")
   ((string= field-name "rep_platform")     "platform")
   ((string= field-name "bug_file_loc")    "url")
   ((string= field-name "reporter")        "creator")
   ((string= field-name "reporter_accessible")  "is_creator_accessible")
   ((string= field-name "cclist_accessible")    "is_cc_accessible")
   ((string= field-name "everconfirmed")    "is_confirmed")
   ((string= field-name "status_whiteboard")   "whiteboard")
   ((string= field-name "bug_group")        "groups")
   ;; Bug.get → Bug.fields (reverse, for updates etc.)
   ((string= field-name "id")                "bug_id")
   ((string= field-name "summary")           "short_desc")
   ((string= field-name "creation_time")     "creation_ts")
   ((string= field-name "last_change_time")    "delta_ts")
   ((string= field-name "blocks")            "blocked")
   ((string= field-name "depends_on")        "dependson")
   ((string= field-name "status")            "bug_status")
   ((string= field-name "severity")          "bug_severity")
   ((string= field-name "platform")          "rep_platform")
   ((string= field-name "url")               "bug_file_loc")
   ((string= field-name "creator")           "reporter")
   ((string= field-name "is_creator_accessible")  "reporter_accessible")
   ((string= field-name "is_cc_accessible")       "cclist_accessible")
   ((string= field-name "is_confirmed")     "everconfirmed")
   ((string= field-name "whiteboard")        "status_whiteboard")
   ((string= field-name "groups")            "bug_group")
   (t nil)))

;;;;;;
;; Column and field name resolution

(defun bug--bz-shared-list-columns (_object _instance)
  "Return list columns for Bugzilla"
  '("id" "status" "summary" "last_change_time"))

(defun bug--bz-shared-field-name (field-name _instance)
  "Resolve field names for Bugzilla"
  (cond ((equal :bug-uuid field-name)
         'id)
        ((equal :bug-friendly-id field-name)
         'id)
        ((equal :bug-summary field-name)
         'summary)))

;;;;;;
;; Bug normalization

(defun bug--bz-shared-normalize-bug (bug)
  "Normalize Bugzilla bug data for display in bug-mode.
- _detail objects become human-readable names (real_name, name, email)
- Vectors of simple values become comma-separated strings
- null values become empty strings"
  (mapcar (lambda (entry)
            (let* ((key (car entry))
                   (value (cdr entry))
                   (new-value
                    (cond
                     ;; _detail objects: prefer real_name, then name/email
                     ((and (consp value)
                           (or (assoc 'real_name value)
                               (assoc 'name value)
                               (assoc 'email value)))
                      (or (cdr (assoc 'real_name value))
                          (cdr (assoc 'name value))
                          (cdr (assoc 'email value))
                          ""))
                     ;; Arrays of objects with names (cc_detail): join
                     ((and (vectorp value)
                           (> (length value) 0)
                           (consp (aref value 0))
                           (or (assoc 'real_name (aref value 0))
                               (assoc 'name (aref value 0))))
                      (let ((names (mapcar (lambda (obj)
                                             (or (cdr (assoc 'real_name obj))
                                                 (cdr (assoc 'name obj))
                                                 ""))
                                           (append value nil))))
                        (if names (mapconcat #'identity names ", ") "")))
                     ;; Arrays of simple values: join with commas
                     ((vectorp value)
                      (let ((items (append value nil)))
                        (if (null items) ""
                          (mapconcat (lambda (x) (format "%s" x)) items ", "))))
                     ;; null
                     ((null value) "")
                     (t value))))
              (cons key new-value)))
          bug))

;;;;;;
;; Search functions

(defun bug--bz-shared-handle-search-response (query response instance)
  "Parse the result of a bug search and either show a single bug or a bug list"
  (if (and
       (assoc 'result response)
       (assoc 'bugs (assoc 'result response)))
      (let ((bugs (cdr (assoc 'bugs (assoc 'result response)))))
        (if (= (length bugs) 0)
            (message "No results")
          (let ((normalized (mapcar #'bug--bz-shared-normalize-bug (append bugs nil))))
            (if (= (length normalized) 1)
                (bug-show (car normalized) instance)
              (bug-list-show query normalized instance)))))
    response))

(defun bug--bz-shared-format-search-candidates (results)
  "Format a Bugzilla bug array as ((\"ID: Summary\" . id-string) ...) alist."
  (mapcar (lambda (bug)
            (cons (format "%s: %s"
                          (or (cdr (assoc 'id bug)) "?")
                          (or (cdr (assoc 'summary bug)) ""))
                  (number-to-string (or (cdr (assoc 'id bug)) 0))))
          (append results nil)))

(defun bug--bz-shared-parse-search-query (query _instance)
  "Parse search query from minibuffer for Bugzilla"
  (if (string-match "^\\([^ ]+\\):\\(.+\\)$" query)
      `((,(match-string 1 query) . ,(match-string 2 query)))
    (if (string-match "[[:space:]]*[0-9]+[:space:]*" query)
        `((id . ,(string-to-number query)))
      `((summary . ,query)))))

(defun bug--bz-shared-search-filter-to-query (properties _instance)
  "Translate generic search `properties'  to Bugzilla search parameters.

Dispatched from `bug--search-filter-to-query' by the frontend.

Property-to-field mapping:
- `title'    -- summary (contains)
- `status'   -- status (exact)
- `owner'    -- assigned_to (contains)
- `type'     -- component
- `priority' -- priority (exact)
- `tag'      -- keywords (contains)

`iteration' and `description' have no Bugzilla equivalent and are ignored."
  (let ((result '()))
    (dolist (prop properties)
      (pcase (car prop)
        ('title    (push (cons 'summary     (cdr prop)) result))
        ('status   (push (cons 'status      (cdr prop)) result))
        ('owner    (push (cons 'assigned_to (cdr prop)) result))
        ('type     (push (cons 'component   (cdr prop)) result))
        ('priority (push (cons 'priority    (cdr prop)) result))
        ('tag      (push (cons 'keywords    (cdr prop)) result))))
    (nreverse result)))

;;;;;;
;; Comments and attachments

(defun bug--bz-shared-get-comments (id instance)
  "Request comments for a bug and add them to an existing bug buffer."
  (bug--bz-shared-handle-comments-response
   id (bug-rpc `((resource . "Bug")
                 (operation . "comments")
                 (data . (("ids" . ,id)))) instance)))

(defun bug--bz-shared-handle-comments-response (id response)
  "Add received comments into an existing bug buffer."
  (if (and
       (assoc 'result response)
       (assoc 'bugs (assoc 'result response)))
      (let* ((bugs (cdr (assoc 'bugs (assoc 'result response))))
             (comments (cdr (cadr (car bugs)))))
        (with-current-buffer (bug--buffer-string id bug---instance)
          (setq buffer-read-only nil)
          (save-excursion
            (let ((cstart (bug--find-section-content-start 'comments)))
              (if cstart
                  (progn
                    (delete-region cstart (point-max))
                    (insert "\n")
                    (insert (mapconcat (lambda (comment)
                                         (bug--format-comment-entry
                                          "Comment"
                                          (cdr (assoc 'count comment))
                                          (cdr (assoc 'creator comment))
                                          (cdr (assoc 'time comment))
                                          (cdr (assoc 'text comment))))
                                       comments "\n\n")))
                (error "Could not find comments section in buffer"))))
          (setq buffer-read-only t)))))

(defun bug--bz-shared-get-attachments (id instance)
  "Request attachment details for a bug and add them to an existing bug buffer."
  (bug--bz-shared-handle-attachments-response
   id (bug-rpc `((resource . "Bug")
                 (operation . "attachments")
                 (data . (("ids" . ,id)))) instance)))

(defun bug--bz-shared-handle-attachments-response (id response)
  "Add received attachment info into an existing bug buffer."
  (if (and
       (assoc 'result response)
       (assoc 'bugs (assoc 'result response)))
      (let* ((bugs (cdr (assoc 'bugs (assoc 'result response))))
             (attachments (cdr (car bugs))))
        (with-current-buffer (bug--buffer-string id bug---instance)
          (setq buffer-read-only nil)
          (save-excursion
            (let ((cstart (bug--find-section-content-start 'attachments)))
              (if cstart
                  (progn
                    (goto-char cstart)
                    (insert "\n")
                    (insert (mapconcat (lambda (attachment)
                                         (format "attachment %s: %s; %s; %s"
                                                 (cdr (assoc 'id attachment))
                                                 (cdr (assoc 'description attachment))
                                                 (cdr (assoc 'file_name attachment))
                                                 (cdr (assoc 'content_type attachment))))
                                       attachments "\n")))
                (error "Could not find attachments section in buffer"))))
          (setq buffer-read-only t)))))

;;;;;;
;; Additional data (comments + attachments sections)

(defun bug--bz-shared-show-additional-data (_bug instance)
  "Insert comment and attachment sections and load their content."
  (bug--insert-section-header 'attachments)
  (bug--insert-section-header 'comments)
  (when (and bug---id bug-autoload-attachments)
    (bug--bz-shared-get-attachments bug---id instance))
  (when (and bug---id bug-autoload-comments)
    (bug--bz-shared-get-comments bug---id instance)))

;;;;;;
;; Attachment helpers

(defun bug--bz-shared-find-attachment-url (_args instance)
  "Construct the URL for the Bugzilla attachment near point."
  (save-excursion
    (let ((end (re-search-forward "$" nil t)))
      (move-beginning-of-line nil)
      ;; FIXME: breaks if ; appears in filenames/descriptions
      (if (re-search-forward "^attachment \\([0-9]+\\): \\([^;]+\\); \\([^;]+\\);" end t)
          (format "%s/attachment.cgi?id=%s" (bug--instance-property :url instance) (match-string 1))
        (error "No attachment near point")))))

(defun bug--bz-shared-download-attachment (_args instance)
  "Download the Bugzilla attachment near point to the home directory."
  (let ((url (bug--bz-shared-find-attachment-url nil instance))
        (dest (expand-file-name (concat "~/" (match-string 3)))))
    (url-copy-file url dest t)))

(defun bug--bz-shared-open-thing (_args instance)
  "Open the Bugzilla attachment near point in the browser."
  (browse-url (bug--bz-shared-find-attachment-url nil instance)))

(provide 'bug-backend-bz-shared)
;;; bug-backend-bz-shared.el ends here
