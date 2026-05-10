;; bug-backend-bz-rpc.el --- backend implementation for Bugzilla JSON-RPC -*- lexical-binding: t; -*-
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
(defun bug--backend-bz-rpc-features (_arg _instance)
  "Features supported by Bugzilla JSON-RPC backend"
  '(:read :write :create :delete :comment :search :search-jql :projects :project-bugs))

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
      (bug--rpc-log-response 'rpc-bz-rpc)
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
      (let ((msg (cdr (assoc 'message (assoc 'error response)))))
        (bug--debug (format "Bugzilla RPC error: %s" msg) '(rpc-bz-rpc . 1))
        (error msg)))
  response)

;;;###autoload
(defun bug--rpc-bz-rpc-get-fields (object instance)
  "Delegate to shared Bugzilla field fetcher."
  (bug--bz-shared-get-fields object instance))

;;;###autoload
(defun bug--rpc-bz-rpc-map-field (field-name instance)
  "Delegate to shared Bugzilla field name mapper."
  (bug--bz-shared-map-field field-name instance))

;;;###autoload
(defun bug--bz-rpc-list-columns (object instance)
  "Delegate to shared Bugzilla list columns."
  (bug--bz-shared-list-columns object instance))

;;;###autoload
(defun bug--bz-rpc-field-name (field-name instance)
  "Delegate to shared Bugzilla field name resolver."
  (bug--bz-shared-field-name field-name instance))


;;;;;;
;;; Search support

;;;###autoload
(defun bug--do-bz-rpc-search (params instance)
  "Execute a search query in Bugzilla.

Frontend dispatch: bug--backend-function \"bug--do-%s-search\" (bug--do-search)."
  (let ((scoped-params (bug--bz-shared-search-params params)))
    (bug--bz-shared-handle-search-response
     scoped-params
     (bug-rpc `((resource . "Bug")
                (operation . "search")
                (data . ,scoped-params))
              instance)
     instance)))

(defun bug--execute-bz-rpc-search (params instance)
  "Execute a Bugzilla search RPC and return (bugs-array . count).

Frontend dispatch: bug--backend-function-optional
\"bug--execute-%s-search\" (bug--search-candidates)."
  (let ((response (bug-rpc `((resource . "Bug")
                             (operation . "search")
                             (data . ,params))
                           instance)))
    (if (and (assoc 'result response)
             (assoc 'bugs (assoc 'result response)))
        (let* ((bugs (cdr (assoc 'bugs (assoc 'result response))))
               (normalized (mapcar #'bug--bz-shared-normalize-bug (append bugs nil))))
          (cons (vconcat normalized) (length normalized)))
      (cons [] 0))))

;;;###autoload
(defun bug--format-bz-rpc-search-candidates (results)
  "Delegate to shared candidate formatter.

Frontend dispatch: bug--backend-function-optional
\"bug--format-%s-search-candidates\" (bug--search-candidates)."
  (bug--bz-shared-format-search-candidates results))

;;;###autoload
(defun bug--parse-bz-rpc-search-query (query instance)
  "Delegate to shared query parser.

Frontend dispatch: bug--backend-function
\"bug--parse-%s-search-query\" (bug--do-search)."
  (bug--bz-shared-parse-search-query query instance))

;;;###autoload
(defun bug--parse-bz-rpc-jql-query (query instance)
  "Parse a JQL query string into Bugzilla RPC search params.

Delegates to the shared JQL parser in `bug-backend-bz-shared.el'.

Frontend dispatch: bug--backend-function
\"bug--parse-%s-jql-query\" (bug-search-jql)."
  (bug--bz-shared-parse-jql-query query instance))

;;;;;;
;;; Bug display and write support

;;;###autoload
(defun bug--fetch-bz-rpc-bug (id instance)
  "Retrieve a single bug from Bugzilla.

Frontend dispatch: bug--backend-function \"bug--fetch-%s-bug\" (bug-open)."
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
            (bug--bz-shared-normalize-bug (aref bugs 0)))
           (t (message "You should never see this message")))))))

;;;###autoload
(defun bug--update-bz-rpc-bug (args instance)
  "Update fields in a Bugzilla bug.

ARGS is a list containing (ID FIELDS) where:
  ID is the numeric bug ID
  FIELDS is an alist of field names and values to update
INSTANCE is the Bugzilla instance.

Returns the response from Bugzilla's Bug.update call.

Frontend dispatch: bug--backend-function
\"bug--update-%s-bug\" (bug--bug-mode-commit)."
  (let* ((id (car args))
         (fields (cadr args))
         (fields-with-id (append fields `((ids . ,id)))))
    (bug-rpc `((resource . "Bug")
               (operation . "update")
               (data . ,fields-with-id))
             instance)))

;;;###autoload
(defun bug--browse-bz-rpc-bug (id instance)
  "Open the current bugzilla bug in browser.

Frontend dispatch: bug--backend-function
\"bug--browse-%s-bug\" (bug--browse-bug)."
  (let ((url (concat (bug--instance-property :url instance) "/show_bug.cgi?id=" id)))
    (browse-url url)))

(defun bug--backend-bz-rpc-get-update-id (_args _instance)
  "Return the Bugzilla bug ID for use as the update identifier.

Frontend dispatch: bug--backend-function
\"bug--backend-%s-get-update-id\" (bug--bug-mode-commit)."
  bug---id)

;;;###autoload
(defun bug--bz-rpc-comment-source-format (_args _instance)
  "Return the source format for Bugzilla comments.

Bugzilla comments are plain text.

Frontend dispatch: bug--backend-function-optional
\"bug--%s-comment-source-format\" (bug--bug-mode-open-rich-editor)."
  'text)

;;;###autoload
(defun bug--backend-bz-rpc-create-comment (_args instance)
  "Open a Bugzilla comment composition buffer for the current bug.

Frontend dispatch: bug--backend-function-optional
\"bug--backend-%s-create-comment\" (bug--bug-mode-create-comment)."
  (let ((fmt (bug--instance-backend-function "bug--%s-comment-source-format" nil instance)))
    (bug--bug-mode-open-rich-editor 'comment nil "" fmt #'bug--backend-bz-rpc-commit-comment)))

;;;###autoload
(defun bug--backend-bz-rpc-commit-comment ()
  "Submit the current buffer content as a Bugzilla comment."
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

;;;;;;
;;; Comments and attachments

;;;###autoload
(defun bug--backend-bz-rpc-show-additional-data (bug instance)
  "Delegate to shared additional-data renderer.

Frontend dispatch: bug--backend-function-optional
\"bug--backend-%s-show-additional-data\" (bug-show)."
  (bug--bz-shared-show-additional-data bug instance))

(defun bug--backend-bz-rpc-find-attachment-url (args instance)
  "Delegate to shared attachment URL finder.

Frontend dispatch: bug--backend-function-optional
\"bug--backend-%s-find-attachment-url\" (bug--find-attachment-url)."
  (bug--bz-shared-find-attachment-url args instance))

;;;###autoload
(defun bug--backend-bz-rpc-download-attachment (args instance)
  "Delegate to shared attachment downloader.

Frontend dispatch: bug--backend-function-optional
\"bug--backend-%s-download-attachment\" (bug--download-attachment)."
  (bug--bz-shared-download-attachment args instance))

;;;###autoload
(defun bug--backend-bz-rpc-open-thing (args instance)
  "Delegate to shared thing opener.

Frontend dispatch: bug--backend-function-optional
\"bug--backend-%s-open-thing\" (bug--open-thing)."
  (bug--bz-shared-open-thing args instance))

;;;;;;
;;; Create support

;;;###autoload
(defun bug--create-bz-rpc-bug (data instance)
  "Create a new Bugzilla bug via JSON-RPC.

`data' is an alist of field names and values.  Returns the created
bug alist on success.

Frontend dispatch: bug--backend-function
\"bug--create-%s-bug\" (bug--bug-mode-commit)."
  (let ((response (bug-rpc `((resource . "Bug")
                             (operation . "create")
                             (data . ,data))
                           instance)))
    (when (and (assoc 'result response)
               (assoc 'id (assoc 'result response)))
      (let ((id (cdr (assoc 'id (assoc 'result response)))))
        (bug--fetch-bz-rpc-bug (number-to-string id) instance)))))

;;;###autoload
(defun bug--create-bz-rpc-bug-interactive (context instance)
  "Open a draft buffer for creating a new Bugzilla bug.

Frontend dispatch: bug--backend-function
\"bug--create-%s-bug-interactive\" (bug-create)."
  (bug--bz-shared-create-bug-interactive context instance))

;;;###autoload
(defun bug--create-bz-rpc-new-artifact (args instance)
  "Create a Bugzilla bug from draft buffer data.

Frontend dispatch: bug--backend-function
\"bug--create-%s-new-artifact\" (bug--bug-mode-commit)."
  (bug--bz-shared-create-new-artifact args instance))

;;;###autoload
(defun bug--validate-draft-bz-rpc (args instance)
  "Check that required Bugzilla create fields are present.

Frontend dispatch: bug--backend-function-optional
\"bug--validate-draft-%s\" (bug--bug-mode-commit)."
  (bug--bz-shared-validate-draft args instance))

;;;;;;
;;; Delete support

;;;###autoload
(defun bug--delete-bz-rpc-bug (id instance)
  "Close a Bugzilla bug via status update.
Bugzilla does not support true deletion via the API.

Frontend dispatch: bug--backend-function
\"bug--delete-%s-bug\" (bug--bug-mode-delete-bug)."
  (bug--bz-shared-close-bug id instance))

;;;;;;
;;; Project support

;;;###autoload
(defun bug--list-bz-rpc-projects (args instance)
  "Return Bugzilla products accessible to the user.

Frontend dispatch: bug--backend-function
\"bug--list-%s-projects\" (bug-list-projects)."
  (bug--bz-shared-list-products args instance))

;;;###autoload
(defun bug--list-bz-rpc-project-bugs (project-id instance)
  "Return search params for bugs in `project-id'.

Frontend dispatch: bug--backend-function
\"bug--list-%s-project-bugs\" (bug-list-project-bugs)."
  (bug--bz-shared-list-product-bugs project-id instance))

;;;;;;
;;; Internal helpers: repo scope detection

;;;###autoload
(defun bug--backend-bz-rpc-repo-scope (parsed-remote instance)
  "Convert `parsed-remote' to a Bugzilla scope string."
  (bug--bz-shared-repo-scope parsed-remote instance))

;;;;;;
;;; Field completion

;;;###autoload
(defun bug--field-completion-bz-rpc (field-name instance)
  "Return completion candidates for `field-name' in Bugzilla bugs.

Frontend dispatch: bug--backend-function-optional
\"bug--field-completion-%s\" (bug--completing-read-field)."
  (bug--field-completion-bz-shared field-name instance))

(provide 'bug-backend-bz-rpc)
;;; bug-backend-bz-rpc.el ends here
