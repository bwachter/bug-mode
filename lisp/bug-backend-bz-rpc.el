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

(require 'bug-vars)
(require 'bug-search-common)
(require 'bug-mode)
(require 'bug-list-mode)

(require 'bug-common-functions)
(require 'bug-rpc)
(require 'bug-comment-mode)
(require 'bug-debug)
(require 'url-vars)
(require 'json)

;;;###autoload
(defun bug--backend-bz-rpc-features (_arg _instance)
  "Features supported by Bugzilla JSON-RPC backend"
  '(:read))

;;;###autoload
(defun bug--rpc-bz-rpc (args instance)
  "Send an RPC response to the given (or default) Bugzilla instance and return the
parsed response as alist"
  ;; used by url, make the byte compiler happy
  (defvar tls-program)
  (let* ((method (concat (cdr (assoc 'resource args)) "."
                         (cdr (assoc 'operation args))))
         (data (cdr (assoc 'data args)))
         (json-str (json-encode `((method . ,method) (params . [,data]) (id 11))))
         (url (concat (bug--instance-property :url instance) "/jsonrpc.cgi"))
         (url-request-method "POST")
         (tls-program '("openssl s_client -connect %h:%p -ign_eof")) ;; gnutls just hangs.. wtf?
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data json-str))
    (bug--debug (concat "request " url "\n" json-str "\n"))
    (with-current-buffer (url-retrieve-synchronously url)
      (bug--debug (concat "response: \n" (decode-coding-string (buffer-string) 'utf-8)))
      (bug--parse-rpc-response instance))))

;;;###autoload
(defun bug--rpc-bz-rpc-handle-error (response _instance)
  "Check data returned from Bugzilla for errors"
  (if (and (assoc 'error response) (assoc 'message (assoc 'error response)))
      (error (cdr (assoc 'message (assoc 'error response)))))
  response)

;;;###autoload
(defun bug--rpc-bz-rpc-get-fields (_object instance)
  "Download the field list for Bugzilla"
  (bug-rpc '((resource . "Bug")
                          (operation . "fields")) instance))

;;;###autoload
(defun bug--rpc-bz-rpc-map-field (field-name)
  "Try to guess what the definition of a field in a bug is by
either throwing away ^bug_ or looking up the key in a list.

It seems that about half of the fields in Bugzillas field query
don't match the fields found in a bug."
  (if (string-match "^bug_" field-name)
      (replace-regexp-in-string "^bug_" "" field-name)
    (cond ((string= field-name "summary")
           "short-desc"))))

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

This function takes a pre-parsed Bugzilla search query as argument.
"
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
          (if (= (length bugs) 1)
              (bug-show (aref bugs 0) instance)
            (bug-list-show query bugs instance))))
    response))

;;;###autoload
(defun bug--parse-bz-rpc-search-query (query _instance)
  "Parse search query from minibuffer for Bugzilla"
  (if (string-match "^\\([^ ]+\\):\\(.+\\)$" query)
      `((,(match-string 1 query) . ,(match-string 2 query)))
    (if (string-match "[[:space:]]*[0-9]+[:space:]*" query)
        `((id . ,(string-to-number query)))
      `((summary . ,query)))))


;;;;;;
;; bug-mode functions

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
            (aref bugs 0))
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
            (goto-char 0)
            (if (re-search-forward "^COMMENTS:$" nil t)
                (progn
                  (delete-region (point) (point-max))
                  (insert "\n")
                  (insert (mapconcat (lambda (comment)
                                       (format "[Comment #%s] %s %s:\n%s"
                                               (cdr (assoc 'count comment))
                                               (cdr (assoc 'time comment))
                                               (cdr (assoc 'creator comment))
                                               (cdr (assoc 'text comment))))
                                     comments "\n\n")))
              (error "Could not find area for comments in buffer")))
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
            (goto-char 0)
            (if (re-search-forward "^ATTACHMENTS:$" nil t)
                (progn
                  (insert "\n")
                  (insert (mapconcat (lambda (attachment)
                                       (format "attachment %s: %s; %s; %s"
                                               (cdr (assoc 'id attachment))
                                               (cdr (assoc 'description attachment))
                                               (cdr (assoc 'file_name attachment))
                                               (cdr (assoc 'content_type attachment))))
                                     attachments "\n")))
              (error "Could not find area for attachments in buffer")))
          (setq buffer-read-only t)))))

;;;###autoload
(defun bug--backend-bz-rpc-show-additional-data (_bug instance)
  "Insert comment and attachment sections and load their content."
  (insert "\nATTACHMENTS:\n")
  (insert "\nCOMMENTS:\n")
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
