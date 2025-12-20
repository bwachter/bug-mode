;; bug-backend-rally.el --- backend implementation for Bugzilla JSON-RPC -*- lexical-binding: t; -*-
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

;; Rally API documentation can be found at
;;   https://rally1.rallydev.com/slm/doc/webservice/
;; For some reason accessing the documentation requires a subscription.

(require 'bug-mode)
(require 'bug-list-mode)
(require 'bug-rpc)

(require 'bug-auth)
(require 'bug-common-functions)
(require 'bug-custom)
(require 'bug-debug)
(require 'json)
(require 'url-cookie)

;;;###autoload
(defun bug--backend-rally-features (_arg _instance)
  "Features supported by Rally backend"
  '(:read))

(defun bug--rpc-rally-auth-header (instance)
  "Generate an auth header for rally, either by using an API key, or -- if
no API key is configured -- by using basic auth with username and password"
  (if (bug--instance-property :api-key instance)
      (cons "zsessionid" (bug--instance-property :api-key instance))
    (cons "Authorization" (concat "Basic "
                                  (base64-encode-string
                                   (concat (car (bug-credentials instance))
                                           ":" (cadr (bug-credentials instance))))))))

(defun bug--rally-get-security-token (instance)
  "Obtain a security token from Rally for write operations.

Security tokens are required for POST/PUT/DELETE operations when using
basic authentication. When using API keys (zsessionid header), tokens
are managed automatically.

Returns the security token string or nil if using API key authentication.

This is experimental and not properly tested - it is always recommended
to use an API key, whenever possible."
  (if (bug--instance-property :api-key instance)
      ;; API key authentication doesn't need explicit security tokens
      nil
    ;; Basic auth requires security token
    (let* ((response (bug--rpc-rally '((resource . "security")
                                       (operation . "authorize"))
                                     instance))
           (operation-result (cdr (car response)))
           (token (cdr (assoc 'SecurityToken operation-result))))
      (unless token
        (error "Failed to obtain security token from Rally"))
      token)))

(defun bug--rally-ensure-security-token (instance)
  "Get or refresh cached security token with 24-hour TTL.

Returns the security token string, or nil if using API key authentication."
  (if (bug--instance-property :api-key instance)
      ;; API key authentication doesn't need explicit security tokens
      nil
    ;; Check cache for valid token
    (let* ((cached-token (bug--cache-get 'security-token instance))
           (token (car cached-token))
           (timestamp (cdr cached-token))
           (current-time (float-time))
           (token-ttl (* 24 60 60))) ; 24 hours in seconds
      (if (and token timestamp
               (< (- current-time timestamp) token-ttl))
          ;; Token is still valid
          token
        ;; Token expired or doesn't exist, get new one
        (let ((new-token (bug--rally-get-security-token instance)))
          (bug--cache-put 'security-token
                          (cons new-token current-time)
                          instance)
          new-token)))))

;; The following table contains supported operations, and mappins
;; to HTTP methods used as well as URL transformations.
;; +--------+--------+--------------------------+
;; | Read   | GET    | <object>/<ObjectID>      |
;; +--------+--------+--------------------------+
;; | Create | POST   | <object>/create          |
;; +--------+--------+--------------------------+
;; | Copy   | POST   | <object>/<ObjectID>/copy |
;; +--------+--------+--------------------------+
;; | Update | POST   | <object>/<ObjectID>      |
;; +--------+--------+--------------------------+
;; | Delete | DELETE | <object>/<ObjectID>      |
;; +--------+--------+--------------------------+
;; | Query  | GET    | <object>?<query>         |
;; +--------+--------+--------------------------+
(defun bug--rpc-rally-request-method (operation)
  "Return the appropriate request method (DELETE/GET/POST) for `operation'"
  (setq operation (downcase operation))
  (cond ((string= operation "delete") "DELETE")
        ((string= operation "read") "GET")
        ((string= operation "query") "GET")
        ((string= operation "authorize") "GET")
        (t "POST")))

(defun bug--rpc-rally-url-map-operation (args)
  "Create the operation specific part of the URL"
  (let ((object-id (cdr (assoc 'object-id args)))
        (object-type (cdr (assoc 'object-type args)))
        (resource (cdr (assoc 'resource args)))
        (operation (cdr (assoc 'operation args)))
        (query-string (url-build-query-string (cdr (assoc 'data args)))))
    (cond
     ((string= operation "authorize") (concat resource "/authorize"))
     ;; TODO: operations like create need to use a security key
     ;;       https://rally1.rallydev.com/slm/doc/webservice/authentication.jsp
     ((string= operation "create") (concat resource "/create"))
     ((string= operation "copy")
      (concat resource "/" object-id "/copy"))
     ((string= operation "query")
      (concat resource "?" query-string))
     (t (if object-type
            (concat resource "/" object-id "/" object-type)
          (concat resource "/" object-id))))))

;;;###autoload
(defun bug--rpc-rally (args instance)
  "Send an RPC response to the given (or default) Rally instance and return the
parsed response as alist.

The method syntax follows the Bugzilla API (<api-object>.<operation), even
though the API is different for Rally. All operations but query and create
require an additional object-id in args to work.

args is an alist, whith the following keys:
- resource: a string representing the resource to use
- operation: what to do with the resource
- object-id: a string representing the object-id
- object-type: a string describing a referenced object to retrieve
- data: an alist containing data for the POST body or query

The call to search for US1234 and return additional fields Name, Description,
Type and FormattedID would look like this:

 (bug--rpc-rally \"hierarchicalrequirement.query\"
               \='((data .
                             ((query \"( FormattedID = \"US1234\" )\")
                              (fetch \"Name,Description,Type,FormattedID\")))))

To get the full details, extract _refObjectUUID from a query, and use it as
object-id for read (or any other call requiring an object-id):

 (bug--rpc-rally \"hierarchicalrequirement.read\"
               \='((object-id . \"1a23bc45-abcd-6e78-f901-g2345hij678k\")))
"
  (let* ((_resource (cdr (assoc 'resource args)))
         (operation (cdr (assoc 'operation args)))
         (url-request-method (bug--rpc-rally-request-method operation))
         (url-str (bug--rpc-rally-url-map-operation args instance))
         (url (concat bug-rally-url url-str))
         ;; don't accept any cookie, see issue 6 for details
         (url-cookie-untrusted-urls '(".*"))
         (url-request-data (if (string= "POST" url-request-method)
                               (json-encode (list (cdr (assoc 'data args))))))
         (url-request-extra-headers `(("Content-Type" . "application/json")
                                      ,(bug--rpc-cookie-header instance)
                                      ,(bug--rpc-rally-auth-header instance))))
    (bug--debug (concat "request " url " instance " (prin1-to-string instance t) "\n"))
    (bug--debug-log-time "RPC init")
    (with-current-buffer (url-retrieve-synchronously url)
      (bug--debug (format "url: %s\ndata: %s\nheaders: %s\n" url url-request-data url-request-extra-headers))
      (bug--debug (concat "response: \n" (decode-coding-string (buffer-string) 'utf-8)))
      (bug--rpc-response-store-cookies instance)
      (bug--parse-rpc-response instance))))

;;;###autoload
(defun bug--rpc-rally-handle-error (response _instance)
  "Check data returned from Rally for errors"
  (let* (
         ;; the errors are inside the returned object, for error handling
         ;; it's easiest to just throw away the outer layer
         (return-document (cdr (car response)))
         (error-messages (assoc 'Errors return-document)))
    (if (>= (length (cdr error-messages)) 1)
        (error (aref (cdr error-messages) 0)))
    response))

;;;###autoload
(defun bug--rpc-rally-get-fields (_object _instance)
  "Return a static list of valid field names for rally

Unlike Bugzilla Rally does not have an API call to retrieve a list of
supported fields, so this function parses a json file containing field
definitions.

The syntax of the file follows the Bugzilla field definition response
as described here:
 https://www.bugzilla.org/docs/3.6/en/html/api/Bugzilla/WebService/Bug.html#Utility_Functions

The following additions are supported for Rally:

- type 98 for rally objects
- type 99 for HTML objects
- is_readonly to mark read-only fields (defaults to `false')
"
  (let ((rally-fields-file (concat
                            bug-json-data-dir
                            "/rally-fields.json")))
    (if (file-exists-p rally-fields-file)
        (json-read-file rally-fields-file)
      (error "Field definition file for Rally not found"))))

;;;###autoload
(defun bug--rally-list-columns (_object _instance)
  "Return list columns for Rally. If `object' is set object-specific columns
may be returned."
  '("FormattedID" ("State" "ScheduleState") "Name" "LastUpdateDate"))

;;;###autoload
(defun bug--rally-field-name (field-name _instance)
  "Resolve field names for rally"
  (cond ((equal :bug-uuid field-name)
         '_refObjectUUID)
        ((equal :bug-friendly-id field-name)
         'FormattedID)
        ((equal :bug-summary field-name)
         'Description)))

;;;;;;
;; search functions

;;;###autoload
(defun bug--do-rally-search (params instance)
  "Execute a search query in Rally

This function takes an alist as documented for bug--rpc-rally as argument.
Usually the parameter list is created by `bug--parse-rally-search-query'.

Default options are added to the list, if not present:

- resource: artifact
- operation: query
- data.fetch: \"FormattedID,LastUpdateDate,TaskStatus,Name,State,ScheduleState\"
- date.pagesize: 100
"
  (let* ((data (assoc 'data params)))
    (unless (listp params)
      (error "Argument not a list"))
    (unless (assoc 'pagesize (cdr data))
      (push '(pagesize 100) (cdr data)))
    (unless (assoc 'fetch (cdr data))
      (push
       '(fetch
         "FormattedID,LastUpdateDate,TaskStatus,Name,State,ScheduleState")
       (cdr data)))
    (unless (assoc 'resource params)
      (setq params (cl-pushnew '(resource . "artifact") params :test #'equal)))
    (unless (assoc 'operation params)
      (setq params (cl-pushnew '(operation . "query") params :test #'equal)))
    (bug--handle-rally-search-response
     params (bug-rpc params instance) instance)))


;; TODO: Rally strips the letters, and just queries the number, leading to
;;       duplicate results. Check the query if we were searching for a single
;;       bug, and break it down, if necessary
(defun bug--handle-rally-search-response (query response instance)
  "Parse the result of a Rally search"
  (if (and
       (assoc 'QueryResult response)
       (assoc 'TotalResultCount (assoc 'QueryResult response)))
      (let* ((query-result (assoc 'QueryResult response))
             (bugs (cdr (assoc 'TotalResultCount query-result))))
        (if (= bugs 0)
            (message "No results")
          (if (= bugs 1)
              ;; this should display the bug...
              (let* ((bug (aref (cdr (assoc 'Results query-result)) 0))
                     (bug-id (cdr (assoc '_refObjectUUID bug))))
                (bug-open bug-id instance))
            ;; ... and this should display a list
            (let* ((results
                    (cdr (assoc 'Results query-result)))
                   (stripped-query
                    (replace-regexp-in-string
                     "[ ()\"]" ""
                     (cadr (assoc 'query (cdr (assoc 'data query)))))))
              ;; check if the query was for a single bug
              (if (string-match
                   "^FormattedID=\\(\\(F\\|DE\\|TA\\|US\\)\\{1\\}[0-9]+\\)$"
                   stripped-query)
                  ;; if so, check if the bug is present in the results
                  (let* ((formatted-id (match-string 1 stripped-query))
                         (bug-position
                          (bug--position-in-array
                           results 'FormattedID formatted-id)))
                    ;; if the bug was found, return it as single bug, otherwise,
                    ;; just show the list (which shouldn't happen)
                    (if bug-position
                        (let ((bug (aref results bug-position)))
                          (bug-open (cdr (assoc '_refObjectUUID bug)) instance))
                      (bug-list-show query results instance)))
                ;; search was not for a single bug, so show the list
                (bug-list-show query results instance))))))
    ;; response didn't contain QueryResult and TotalResultCount, so just
    ;; return the response for debugging, as that should not happen.
    response))

;;;###autoload
(defun bug--parse-rally-search-query (query _instance)
  "Parse search query from minibuffer for rally"
  (cond ;; for userfriendly rally IDs, open bug directly
   ((string-match "^\\(F\\|DE\\|TA\\|US\\)[0-9]+" query)
    `((data . ((query ,(format "( FormattedID = \"%s\" )" query))))))
   ;; string contains parentheses -> assume it's a complex rally expression
   ((string-match "\\((\\|)\\)" query)
    `((data . ((query ,query)))))
   ;; search Name, Notes, Description
   ;; TODO: searching discussion seems to be problematic
   (t
    `((data . ((query
                ,(format "(((Name contains \"%s\") OR (Notes contains \"%s\")) OR (Description contains \"%s\"))"
                         query query query))))))))


;;;;;;
;; bug-mode functions

;;;###autoload
(defun bug--fetch-rally-bug (id instance)
  "Retrieve a single bug from Rally"
  (let* ((search-response (bug-rpc `((resource . "artifact")
                                     (operation . "read")
                                     (object-id . ,id)) instance))
         (return-document-type (caar search-response))
         (return-document (cdr (car search-response))))
    ;; error messages are handled in RPC backend already, and -- unlike in
    ;; bugzilla -- the query is executed with a known to exist UUID. So,
    ;; in theory, from this point on nothing should fail
    (push `(ObjectType . ,return-document-type) return-document)
    return-document))

;;;###autoload
(defun bug--browse-rally-bug (id _instance)
  "Open the current Rally bug in browser"
  ;; this probably breaks with custom hosted rally instances. If you come across
  ;; one of those please send me an email.
  (let ((url (format "https://rally1.rallydev.com/#/search?keywords=%s" id)))
    (browse-url url)))

(provide 'bug-backend-rally)
;;; bug-backend-rally.el ends here
