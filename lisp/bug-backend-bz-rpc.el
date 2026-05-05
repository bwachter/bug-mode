;; bug-backend-bz-rpc.el --- backend implementation for Bugzilla JSON-RPC -*- lexical-binding: t; -*-
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

(require 'bug-auth)
(require 'bug-vars)
(require 'bug-search-common)
(require 'bug-mode)
(require 'bug-list-mode)

(require 'bug-backend-bz-shared)
(require 'bug-common-functions)
(require 'bug-rpc)
(require 'bug-comment-mode)
(require 'bug-debug)
(require 'url-vars)
(require 'json)

;;;###autoload
(defun bug--backend-bz-rpc-features (_arg _instance)
  "Features supported by Bugzilla JSON-RPC backend"
  '(:read :write))

;;;###autoload
(defun bug--rpc-bz-rpc--send (args instance)
  "Internal helper to send a single Bugzilla JSON-RPC request.
Parses the response, stores cookies, and returns the alist."
  (let* ((method (concat (cdr (assoc 'resource args)) "."
                         (cdr (assoc 'operation args))))
         (data (cdr (assoc 'data args)))
         (api-key (bug--instance-property :api-key instance))
         (data-with-key (if api-key
                            (append data `((Bugzilla_api_key . ,api-key)))
                          data))
         (json-str (json-encode `((method . ,method) (params . [,data-with-key]) (id . 11))))
         (url (concat (bug--instance-property :url instance) "/jsonrpc.cgi"))
         (url-request-method "POST")
         (cookie-header (bug--rpc-cookie-header instance))
         (url-request-extra-headers
          (append '(("Content-Type" . "application/json"))
                  (when cookie-header (list cookie-header))))
         (url-request-data json-str))
    (bug--debug (concat "request " url "\n" json-str "\n"))
    (with-current-buffer (url-retrieve-synchronously url)
      (bug--rpc-response-store-cookies instance)
      (bug--debug (concat "response: \n" (decode-coding-string (buffer-string) 'utf-8)))
      (bug--parse-rpc-response instance))))

(defun bug--rpc-bz-rpc-login (instance)
  "Log in to the Bugzilla INSTANCE using stored credentials.
This is called automatically when a request requires authentication."
  (let ((creds (condition-case nil
                   (bug--auth-credentials instance)
                 (error nil))))
    (unless creds
      (error "Bugzilla requires login, but no credentials are configured for instance '%s'"
             (prin1-to-string instance t)))
    (message "Logging in to Bugzilla...")
    (bug--rpc-bz-rpc--send
     `((resource . "User")
       (operation . "login")
       (data . ((login . ,(car creds))
                (password . ,(cadr creds))
                (remember . t))))
     instance)
    (bug--get-fields instance)
    (message "Bugzilla login successful")))

;;;###autoload
(defun bug--rpc-bz-rpc (args instance)
  "Send an RPC request to the given Bugzilla instance and return the parsed
response.

If an API key is configured it is passed automatically in the request params.
If the server responds with a login-required error (code 410) and credentials
are available, log in transparently and retry the request once."
  (condition-case err
      (bug--rpc-bz-rpc--send args instance)
    (error
     (if (and (stringp (error-message-string err))
              (string-match "You must log in" (error-message-string err)))
         (progn
           (bug--rpc-bz-rpc-login instance)
           (bug--rpc-bz-rpc--send args instance))
       (signal (car err) (cdr err))))))

;;;###autoload
(defun bug--rpc-bz-rpc-handle-error (response _instance)
  "Check data returned from Bugzilla for errors"
  (if (and (assoc 'error response) (assoc 'message (assoc 'error response)))
      (error (cdr (assoc 'message (assoc 'error response)))))
  response)

;;;###autoload
(defun bug--rpc-bz-rpc-get-fields (_object instance)
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

;;;###autoload
(defun bug--rpc-bz-rpc-map-field (field-name _instance)
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

;;;###autoload
(defun bug--bz-rpc-list-columns (_object _instance)
  "Return list columns for Bugzilla"
  '("id" "status" "summary" "last_change_time"))

;;;###autoload
(defun bug--bz-rpc-field-name (field-name _instance)
  "Resolve field names for Bugzilla"
  (cond ((equal :bug-uuid field-name)
         'id)
        ((equal :bug-friendly-id field-name)
         'id)
        ((equal :bug-summary field-name)
         'summary)))


;;;;;;
;; search functions

;;;###autoload
(defun bug--do-bz-rpc-search (params instance)
  "Execute a search query in Bugzilla.

This function takes a pre-parsed Bugzilla search query as argument."
  (bug--handle-bz-rpc-search-response params
                                      (bug-rpc `((resource . "Bug")
                                                 (operation . "search")
                                                 (data . ,params))
                                               instance)
                                      instance))

(defun bug--handle-bz-rpc-search-response (query response instance)
  "Parse the result of a bug search and either show a single bug or a bug list"
  (if (and
       (assoc 'result response)
       (assoc 'bugs (assoc 'result response)))
      (let ((bugs (cdr (assoc 'bugs (assoc 'result response)))))
        (if (= (length bugs) 0)
            (message "No results")
          (let ((normalized (mapcar #'bug--normalize-bz-rpc-bug (append bugs nil))))
            (if (= (length normalized) 1)
                (bug-show (car normalized) instance)
              (bug-list-show query normalized instance)))))
    response))

(defun bug--execute-bz-rpc-search (params instance)
  "Execute a Bugzilla search RPC and return (bugs-array . count)."
  (let ((response (bug-rpc `((resource . "Bug")
                             (operation . "search")
                             (data . ,params))
                           instance)))
    (if (and (assoc 'result response)
             (assoc 'bugs (assoc 'result response)))
        (let* ((bugs (cdr (assoc 'bugs (assoc 'result response))))
               (normalized (mapcar #'bug--normalize-bz-rpc-bug (append bugs nil))))
          (cons (vconcat normalized) (length normalized)))
      (cons [] 0))))

(defun bug--format-bz-rpc-search-candidates (results)
  "Format a Bugzilla bug array as ((\"ID: Summary\" . id-string) ...) alist."
  (mapcar (lambda (bug)
            (cons (format "%s: %s"
                          (or (cdr (assoc 'id bug)) "?")
                          (or (cdr (assoc 'summary bug)) ""))
                  (number-to-string (or (cdr (assoc 'id bug)) 0))))
          (append results nil)))

;;;###autoload
(defun bug--parse-bz-rpc-search-query (query _instance)
  "Parse search query from minibuffer for Bugzilla"
  (if (string-match "^\\([^ ]+\\):\\(.+\\)$" query)
      `((,(match-string 1 query) . ,(match-string 2 query)))
    (if (string-match "[[:space:]]*[0-9]+[:space:]*" query)
        `((id . ,(string-to-number query)))
      `((summary . ,query)))))

(defun bug--search-filter-bz-rpc-query (properties _instance)
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
;; bug-mode functions

(defun bug--normalize-bz-rpc-bug (bug)
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

;;;###autoload
(defun bug--fetch-bz-rpc-bug (id instance)
  "Retrieve a single bug from Bugzilla"
  (let ((search-response
         (bug-rpc `((resource . "Bug")
                    (operation . "get")
                    (data . (("ids" . ,id)))) instance)))
    (if (and (assoc 'result search-response)
             (assoc 'bugs (assoc 'result search-response)))
        (let ((bugs (cdr (assoc 'bugs (assoc 'result search-response)))))
          (cond
           ((= (length bugs) 0)
            (message (concat "Bug " id " not found.")))
           ((= (length bugs) 1)
            (bug--normalize-bz-rpc-bug (aref bugs 0)))
           (t (message "You should never see this message")))))))

;;;###autoload
(defun bug--update-bz-rpc-bug (args instance)
  "Update fields in a Bugzilla bug.

ARGS is a list containing (ID FIELDS) where:
  ID is the numeric bug ID
  FIELDS is an alist of field names and values to update
INSTANCE is the Bugzilla instance.

Returns the response from Bugzilla's Bug.update call."
  (let* ((id (car args))
         (fields (cadr args))
         (fields-with-id (append fields `((ids . ,id)))))
    (bug-rpc `((resource . "Bug")
               (operation . "update")
               (data . ,fields-with-id))
             instance)))

;;;###autoload
(defun bug--browse-bz-rpc-bug (id instance)
  "Open the current bugzilla bug in browser"
  (let ((url (concat (bug--instance-property :url instance) "/show_bug.cgi?id=" id)))
    (browse-url url)))

(defun bug--backend-bz-rpc-get-update-id (_args _instance)
  "Return the Bugzilla bug ID for use as the update identifier."
  bug---id)

;;;###autoload
(defun bug--backend-bz-rpc-create-comment (args _instance)
  "Open a Bugzilla comment composition buffer for the current bug."
  (bug-comment args))

;;;;;;
;; Comments and attachments

(defun bug-get-comments (id instance)
  "Request comments for a bug and add them to an existing bug buffer."
  (bug-handle-comments-response
   id (bug-rpc `((resource . "Bug")
                 (operation . "comments")
                 (data . (("ids" . ,id)))) instance)))

(defun bug-handle-comments-response (id response)
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

(defun bug-get-attachments (id instance)
  "Request attachment details for a bug and add them to an existing bug buffer."
  (bug-handle-attachments-response
   id (bug-rpc `((resource . "Bug")
                 (operation . "attachments")
                 (data . (("ids" . ,id)))) instance)))

(defun bug-handle-attachments-response (id response)
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

;;;###autoload
(defun bug--backend-bz-rpc-show-additional-data (_bug instance)
  "Insert comment and attachment sections and load their content."
  (bug--insert-section-header 'attachments)
  (bug--insert-section-header 'comments)
  (when (and bug---id bug-autoload-attachments)
    (bug-get-attachments bug---id instance))
  (when (and bug---id bug-autoload-comments)
    (bug-get-comments bug---id instance)))

(defun bug--backend-bz-rpc-find-attachment-url (_args instance)
  "Construct the URL for the Bugzilla attachment near point."
  (save-excursion
    (let ((end (re-search-forward "$" nil t)))
      (move-beginning-of-line nil)
      ;; FIXME: breaks if ; appears in filenames/descriptions
      (if (re-search-forward "^attachment \\([0-9]+\\): \\([^;]+\\); \\([^;]+\\);" end t)
          (format "%s/attachment.cgi?id=%s" (bug--instance-property :url instance) (match-string 1))
        (error "No attachment near point")))))

;;;###autoload
(defun bug--backend-bz-rpc-download-attachment (_args instance)
  "Download the Bugzilla attachment near point to the home directory."
  (let ((url (bug--backend-bz-rpc-find-attachment-url nil instance))
        (dest (expand-file-name (concat "~/" (match-string 3)))))
    (url-copy-file url dest t)))

;;;###autoload
(defun bug--backend-bz-rpc-open-thing (_args instance)
  "Open the Bugzilla attachment near point in the browser."
  (browse-url (bug--backend-bz-rpc-find-attachment-url nil instance)))

(provide 'bug-backend-bz-rpc)
;;; bug-backend-bz-rpc.el ends here
