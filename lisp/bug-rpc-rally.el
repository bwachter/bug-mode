;; bug-rpc-rally.el --- RPC functions for Rally
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

(defun bug--rpc-rally-auth-header (&optional instance)
  "Generate an auth header for rally, either by using an API key, or -- if
no API key is configured -- by using basic auth with username and password"
  (if (bug--instance-property :api-key instance)
      (cons "zsessionid" (bug--instance-property :api-key instance))
    (cons "Authorization" (concat "Basic "
                                  (base64-encode-string
                                   (concat (car (bug-credentials instance))
                                           ":" (cadr (bug-credentials instance))))))))

;; TODO: POSTing data is currently not implemented
;;
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

(defun bug--rpc-rally-url-map-operation (operation &optional object args)
  "Create the operation specific part of the URL"
  (let ((object-id (cdr (assoc 'object-id args)))
        (object-type (cdr (assoc 'object-type args)))
        (query-string (url-build-query-string (cdr (assoc 'query-data args)))))
    (cond
     ((string= operation "authorize") (concat object "/authorize"))
     ;; TODO: operations like create need to use a security key
     ;;       https://rally1.rallydev.com/slm/doc/webservice/authentication.jsp
     ((string= operation "create") (concat object "/create"))
     ((string= operation "copy")
      (concat object "/" object-id "/copy"))
     ((string= operation "query")
      (concat object "?" query-string))
     (t (if object-type
            (concat object "/" object-id "/" object-type)
          (concat object "/" object-id))))))

;;;###autoload
(defun bug--rpc-rally (method args &optional instance)
  "Send an RPC response to the given (or default) Rally instance and return the
parsed response as alist.

The method syntax follows the Bugzilla API (<api-object>.<operation), even
though the API is different for Rally. All operations but query and create
require an additional object-id in args to work.

args is an alist, whith the following keys:
- object-id: a string representing the object-id
- object-type: a string describing a referenced object to retrieve
- query-data: an alist containing query parameters
- post-data: an alist containing data for the POST body

The call to search for US1234 and return additional fields Name, Description,
Type and FormattedID would look like this:

 (bug--rpc-rally \"hierarchicalrequirement.query\"
               '((query-data .
                             ((query \"( FormattedID = \"US1234\" )\")
                              (fetch \"Name,Description,Type,FormattedID\")))))

To get the full details, extract _refObjectUUID from a query, and use it as
object-id for read (or any other call requiring an object-id):

 (bug--rpc-rally \"hierarchicalrequirement.read\"
               '((object-id . \"1a23bc45-abcd-6e78-f901-g2345hij678k\")))
"
  (let* ((object (car (split-string method "\\." t)))
         (operation (cadr (split-string method "\\." t)))
         (url-request-method (bug--rpc-rally-request-method operation))
         (url-str (bug--rpc-rally-url-map-operation operation object args))
         (url (concat bug-rally-url url-str))
         ;; don't accept any cookie, see issue 6 for details
         (url-cookie-untrusted-urls '(".*"))
         (url-request-data (json-encode (list (cdr (assoc 'post-data args)))))
         (url-request-extra-headers `(("Content-Type" . "application/json")
                                      ,(bug--rpc-cookie-header instance)
                                      ,(bug--rpc-rally-auth-header instance))))
    (bug--debug (concat "request " url " instance " (prin1-to-string instance t) "\n"))
    (bug--debug-log-time "RPC init")
    (with-current-buffer (url-retrieve-synchronously url)
      (bug--debug (concat "response: \n" (decode-coding-string (buffer-string) 'utf-8)))
      (bug--rpc-response-store-cookies instance)
      (bug--parse-rpc-response))))

;;;###autoload
(defun bug--rpc-rally-handle-error (response)
  "Check data returned from Rally for errors"
  (let* (; the errors are inside the returned object, for error handling
         ; it's easiest to just throw away the outer layer
         (return-document (cdr (car response)))
         (error-messages (assoc 'Errors return-document)))
    (if (>= (length (cdr error-messages)) 1)
        (error (aref (cdr error-messages) 0)))
    response))

;;;###autoload
(defun bug--rpc-rally-get-fields ()
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
- is_readonly to mark read-only fields (defaults to 'false')
"
    (let ((rally-fields-file (concat
                              bug-json-data-dir
                              "/rally-fields.json")))
      (if (file-exists-p rally-fields-file)
          (json-read-file rally-fields-file)
        (error "Field definition file for Rally not found"))))

(provide 'bug-rpc-rally)
;;; bug-rpc-rally.el ends here
