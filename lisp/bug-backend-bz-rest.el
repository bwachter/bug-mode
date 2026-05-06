;;; bug-backend-bz-rest.el --- backend implementation for Bugzilla REST API -*- lexical-binding: t; -*-
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

(require 'bug-auth)
(require 'bug-vars)
(require 'bug-search-common)
(require 'bug-mode)
(require 'bug-list-mode)

(require 'bug-backend-bz-shared)
(require 'bug-common-functions)
(require 'bug-rpc)
(require 'bug-rich-edit)
(require 'bug-debug)
(require 'url-vars)
(require 'json)

;;;###autoload
(defun bug--backend-bz-rest-features (_arg _instance)
  "Features supported by Bugzilla REST backend"
  '(:read :search :search-jql))

;;;;;;
;; REST transport

(defun bug--rpc-bz-rest--url (resource operation data instance)
  "Build the REST URL for a Bugzilla request.

Returns a cons cell (METHOD . URL) where METHOD is the HTTP verb
and URL is the full request URL."
  (let* ((base-url (bug--instance-property :url instance))
         ;; Extract ID from data if present
         (id (or (cdr (assoc 'id data))
                 (cdr (assoc 'ids data))))
         (path
          (pcase operation
            ((or "get" "update")
             (format "/rest/bug/%s" id))
            ((or "comments" "attachments")
             (format "/rest/bug/%s/%s" id (if (string= operation "comments") "comment" operation)))
            ("add_comment"
             (format "/rest/bug/%s/comment" id))
            ("fields"
             "/rest/field/bug")
            ("search"
             "/rest/bug")
            ("login"
             "/rest/login")
            (_ (format "/rest/%s" resource)))))
    (cons
     (pcase operation
       ((or "get" "comments" "attachments" "fields" "search" "login") "GET")
       ((or "update" "add_comment") "PUT")
       (_ "GET"))
     (concat base-url path))))

(defun bug--rpc-bz-rest--send (args instance)
  "Internal helper to send a single Bugzilla REST request.
Parses the response, stores cookies, and returns the alist."
  (let* ((resource (cdr (assoc 'resource args)))
         (operation (cdr (assoc 'operation args)))
         (data (cdr (assoc 'data args)))
         (api-key (bug--instance-property :api-key instance))
         (method-and-url (bug--rpc-bz-rest--url resource operation data instance))
         (method (car method-and-url))
         (url (cdr method-and-url))
         (cookie-header (bug--rpc-cookie-header instance))
         (url-request-extra-headers
          (append '(("Content-Type" . "application/json")
                    ("Accept" . "application/json"))
                  (when cookie-header (list cookie-header))))
         (url-request-method method))
    ;; Add API key as query parameter if available
    (when api-key
      (setq url (concat url (if (string-match-p "\\?" url) "&" "?")
                        "api_key=" (url-encode-url api-key))))
    ;; For PUT/POST, encode data as JSON body
    (when (member method '("PUT" "POST"))
      (setq url-request-data (json-encode data)))
    (bug--debug (concat "request " method " " url "\n"
                        (or url-request-data "") "\n"))
    (with-current-buffer (url-retrieve-synchronously url)
      (bug--rpc-response-store-cookies instance)
      (bug--debug (concat "response: \n" (decode-coding-string (buffer-string) 'utf-8)))
      (let ((parsed (bug--parse-rpc-response instance)))
        ;; REST responses wrap data differently than RPC.
        ;; Normalise to the RPC shape so shared code works unchanged.
        (if (and (assoc 'bugs parsed)
                 (not (assoc 'result parsed)))
            `((result . ,parsed))
          parsed)))))

(defun bug--rpc-bz-rest-login (instance)
  "Log in to the Bugzilla INSTANCE using stored credentials."
  (let ((creds (condition-case nil
                   (bug--auth-credentials instance)
                 (error nil))))
    (unless creds
      (error "Bugzilla requires login, but no credentials are configured for instance '%s'"
             (prin1-to-string instance t)))
    (message "Logging in to Bugzilla...")
    (bug--rpc-bz-rest--send
     `((resource . "User")
       (operation . "login")
       (data . ((login . ,(car creds))
                (password . ,(cadr creds))
                (remember . t))))
     instance)
    (bug--get-fields instance)
    (message "Bugzilla login successful")))

;;;###autoload
(defun bug--rpc-bz-rest (args instance)
  "Send a REST request to the given Bugzilla instance and return the parsed
response.

If an API key is configured it is passed automatically in the request.
If the server responds with a login-required error and credentials are
available, log in transparently and retry the request once."
  (condition-case err
      (bug--rpc-bz-rest--send args instance)
    (error
     (if (and (stringp (error-message-string err))
              (string-match "You must log in" (error-message-string err)))
         (progn
           (bug--rpc-bz-rest-login instance)
           (bug--rpc-bz-rest--send args instance))
       (signal (car err) (cdr err))))))

;;;###autoload
(defun bug--rpc-bz-rest-handle-error (response _instance)
  "Check data returned from Bugzilla REST for errors."
  (if (and (assoc 'error response) (assoc 'message (assoc 'error response)))
      (error (cdr (assoc 'message (assoc 'error response)))))
  response)

;;;###autoload
(defun bug--rpc-bz-rest-get-fields (object instance)
  "Delegate to shared Bugzilla field fetcher."
  (bug--bz-shared-get-fields object instance))

;;;###autoload
(defun bug--rpc-bz-rest-map-field (field-name instance)
  "Delegate to shared Bugzilla field name mapper."
  (bug--bz-shared-map-field field-name instance))

;;;###autoload
(defun bug--bz-rest-list-columns (object instance)
  "Delegate to shared Bugzilla list columns."
  (bug--bz-shared-list-columns object instance))

;;;###autoload
(defun bug--bz-rest-field-name (field-name instance)
  "Delegate to shared Bugzilla field name resolver."
  (bug--bz-shared-field-name field-name instance))

;;;;;;
;; search functions

;;;###autoload
(defun bug--do-bz-rest-search (params instance)
  "Execute a search query in Bugzilla via REST."
  (bug--bz-shared-handle-search-response
   params
   (bug-rpc `((resource . "Bug")
              (operation . "search")
              (data . ,params))
            instance)
   instance))

(defun bug--execute-bz-rest-search (params instance)
  "Execute a Bugzilla REST search and return (bugs-array . count)."
  (let ((response (bug-rpc `((resource . "Bug")
                             (operation . "search")
                             (data . ,params))
                           instance)))
    (let ((result (or (cdr (assoc 'result response)) response)))
      (if (and result (assoc 'bugs result))
          (let* ((bugs (cdr (assoc 'bugs result)))
                 (normalized (mapcar #'bug--bz-shared-normalize-bug (append bugs nil))))
            (cons (vconcat normalized) (length normalized)))
        (cons [] 0)))))

;;;###autoload
(defun bug--format-bz-rest-search-candidates (results)
  "Delegate to shared candidate formatter."
  (bug--bz-shared-format-search-candidates results))

;;;###autoload
(defun bug--parse-bz-rest-search-query (query instance)
  "Delegate to shared query parser."
  (bug--bz-shared-parse-search-query query instance))

;;;###autoload
(defun bug--parse-bz-rest-jql-query (query instance)
  "Parse a JQL query string into Bugzilla REST search params.

Delegates to the shared JQL parser in `bug-backend-bz-shared.el'."
  (bug--bz-shared-parse-jql-query query instance))

;;;;;;
;; bug-mode functions

;;;###autoload
(defun bug--fetch-bz-rest-bug (id instance)
  "Retrieve a single bug from Bugzilla via REST."
  (let ((response
         (bug-rpc `((resource . "Bug")
                    (operation . "get")
                    (data . ((id . ,id)))) instance)))
    (let ((result (or (cdr (assoc 'result response)) response)))
      (if (and result (assoc 'bugs result))
          (let ((bugs (cdr (assoc 'bugs result))))
            (cond
             ((= (length bugs) 0)
              (message (concat "Bug " id " not found.")))
             ((= (length bugs) 1)
              (bug--bz-shared-normalize-bug (aref bugs 0)))
             (t (message "You should never see this message"))))))))

;;;###autoload
(defun bug--update-bz-rest-bug (args instance)
  "Update fields in a Bugzilla bug via REST.

ARGS is a list containing (ID FIELDS) where:
  ID is the numeric bug ID
  FIELDS is an alist of field names and values to update
INSTANCE is the Bugzilla instance."
  (let* ((id (car args))
         (fields (cadr args)))
    (bug-rpc `((resource . "Bug")
               (operation . "update")
               (data . ((id . ,id)
                        ,@fields)))
             instance)))

;;;###autoload
(defun bug--browse-bz-rest-bug (id instance)
  "Open the current bugzilla bug in browser"
  (let ((url (concat (bug--instance-property :url instance) "/show_bug.cgi?id=" id)))
    (browse-url url)))

(defun bug--backend-bz-rest-get-update-id (_args _instance)
  "Return the Bugzilla bug ID for use as the update identifier."
  bug---id)

;;;###autoload
(defun bug--backend-bz-rest-create-comment (_args _instance)
  "Open a Bugzilla comment composition buffer for the current bug."
  (bug--bug-mode-open-rich-editor 'comment nil "" 'text #'bug--backend-bz-rest-commit-comment))

;;;###autoload
(defun bug--backend-bz-rest-commit-comment ()
  "Submit the current buffer content as a Bugzilla comment via REST."
  (interactive)
  (let* ((text (string-trim (buffer-string)))
         (source-buf bug--rich-edit-source-buffer))
    (when (string= text "")
      (error "Comment is empty"))
    (let ((id (with-current-buffer source-buf bug---id))
          (instance (with-current-buffer source-buf bug---instance)))
      (message "Adding comment...")
      (bug-rpc `((resource . "Bug")
                 (operation . "add_comment")
                 (data . ((id . ,id)
                          (comment . ,text))))
               instance)
      (message "Comment added.")
      (quit-window t)
      (when (buffer-live-p source-buf)
        (with-current-buffer source-buf
          (bug--bz-shared-get-comments id instance))))))

;;;###autoload
(defun bug--backend-bz-rest-show-additional-data (bug instance)
  "Delegate to shared additional-data renderer."
  (bug--bz-shared-show-additional-data bug instance))

(defun bug--backend-bz-rest-find-attachment-url (args instance)
  "Delegate to shared attachment URL finder."
  (bug--bz-shared-find-attachment-url args instance))

;;;###autoload
(defun bug--backend-bz-rest-download-attachment (args instance)
  "Delegate to shared attachment downloader."
  (bug--bz-shared-download-attachment args instance))

;;;###autoload
(defun bug--backend-bz-rest-open-thing (args instance)
  "Delegate to shared thing opener."
  (bug--bz-shared-open-thing args instance))

(provide 'bug-backend-bz-rest)
;;; bug-backend-bz-rest.el ends here
