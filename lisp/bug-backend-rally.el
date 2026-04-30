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

(require 'bug-vars)
(require 'bug-mode)
(require 'bug-list-mode)
(require 'bug-rpc)

(require 'bug-auth)
(require 'bug-common-functions)
(require 'bug-custom)
(require 'bug-debug)
(require 'bug-format)

(require 'vtable)
(require 'json)
(require 'url-cookie)

(defcustom bug-rally-link-flowstate t
  "When non-nil, link FlowState changes to ScheduleState automatically.

FlowState is a Rally object that mirrors ScheduleState at a finer
granularity.  When this option is t:
- FlowState cannot be edited directly in bug buffers.
- Changing ScheduleState automatically derives and sends the matching
  FlowState reference to Rally.

Set to nil to allow independent FlowState editing."
  :group 'bug
  :type 'boolean)

(defconst bug--rally-draft-fields
  '(("Defect"                   "Name" "State" "Priority" "Severity" "Owner" "Description")
    ("HierarchicalRequirement"  "Name" "ScheduleState" "Owner" "Description")
    ("Task"                     "Name" "State" "Owner" "Description")
    ("TestCase"                 "Name" "Owner" "Description")
    ("PortfolioItem"            "Name" "State" "Owner" "Description"))
  "Fallback fields for new-artifact draft buffers when the TypeDefinition
API is unavailable.  Used as the baseline in `bug--rally-get-draft-fields'.")

(defconst bug--rally-cache-ttl 86400
  "Default TTL, in seconds")

;;;###autoload
(defun bug--backend-rally-features (_arg _instance)
  "Features supported by Rally backend"
  '(:read :write :create))

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
           (token-ttl bug--rally-cache-ttl))
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

(defun bug--rpc-rally-url-map-operation (args instance)
  "Create the operation specific part of the URL.

For write operations (create, update, delete, copy), appends security token
if using basic authentication."
  (let* ((object-id (cdr (assoc 'object-id args)))
         (object-type (cdr (assoc 'object-type args)))
         (resource (cdr (assoc 'resource args)))
         (operation (cdr (assoc 'operation args)))
         (query-string (url-build-query-string (cdr (assoc 'data args))))
         ;; Check if this is a write operation that needs security token
         (needs-token (member operation '("create" "update" "delete" "copy")))
         (base-url
          (cond
           ((string= operation "authorize") (concat resource "/authorize"))
           ((string= operation "create") (concat resource "/create"))
           ((string= operation "copy")
            (concat resource "/" object-id "/copy"))
           ((string= operation "query")
            (concat resource "?" query-string))
           (t (if object-type
                  (concat resource "/" object-id "/" object-type)
                (concat resource "/" object-id))))))
    ;; Append security token for write operations if using basic auth
    (if (and needs-token (not (bug--instance-property :api-key instance)))
        (let ((token (bug--rally-ensure-security-token instance)))
          (if token
              (concat base-url "?key=" token)
            base-url))
      base-url)))

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

For GET queries `data' is a flat alist encoded into the query string:
  ((query \"(State = \='Open\=')\") (fetch \"Name,ID\") (pagesize 100))

For POST queries `data' is a nested alist, with the rally object type
name used as key:
((Defect . ((Name . \"Bug\") (Project . \"/project/123\"))))
((Artifact . ((State . \"Fixed\") (Priority . \"High\"))))

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
                               (json-encode (cdr (assoc 'data args)))))
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
(defun bug--rally-field-filters (_args _instance)
  "Return a list of field filter lists for Rally bug display.

Each inner list contains field names to display in that order. An empty list
means show all fields alphabetically. Index 0 is the default on first open.

Returns: ((all fields) (minimal fields) (normal fields) (detailed fields))"
  '(("FormattedID" "Name" "State" "ScheduleState" "Owner" "Priority" "Severity" "Description")
    ("FormattedID" "Name" "State" "ScheduleState" "Owner" "Priority" "Severity" "Iteration" "Release" "Project" "Description")
    ("FormattedID" "Name" "ObjectType" "State" "ScheduleState" "Owner" "Priority" "Severity" "Iteration" "Release" "Project" "PlanEstimate" "TaskEstimatedHours" "TaskRemainingHours" "Blocked" "BlockedReason" "Ready" "Tags" "Description")
    ()))

;;;###autoload
(defun bug--fetch-rally-discussion (bug-data instance)
  "Fetch discussion posts for a Rally artifact.

BUG-DATA is the artifact data alist containing the Discussion field.
INSTANCE is the Rally instance.

Returns a list of discussion post alists, or nil if no discussion exists."
  (let* ((discussion-ref (cdr (assoc 'Discussion bug-data)))
         (post-count (cdr (assoc 'Count discussion-ref))))
    (message "Discussion ref: %S, Count: %S" discussion-ref post-count)
    (when (and discussion-ref (> (or post-count 0) 0))
      (let* ((ref-url (cdr (assoc '_ref discussion-ref)))
             ;; Extract artifact ObjectID from URL
             ;; URL format: .../HierarchicalRequirement/839749943551/Discussion
             (artifact-oid (when (string-match "/\\([0-9]+\\)/Discussion$" ref-url)
                             (match-string 1 ref-url))))
        (message "Artifact OID: %s" artifact-oid)
        (when artifact-oid
          (let* ((response (bug--rpc-rally
                            `((resource . "conversationpost")
                              (operation . "query")
                              (data . ((query ,(format "(Artifact.ObjectID = %s)" artifact-oid))
                                       (fetch "Text,User,CreationDate,PostNumber")
                                       (order "PostNumber")
                                       (pagesize 200))))
                            instance))
                 (query-result (cdr (car response)))
                 (posts (cdr (assoc 'Results query-result))))
            (message "Discussion query result: %S" query-result)
            (message "Found %d posts" (if posts (length posts) 0))
            ;; Convert vector to list for easier handling
            (when posts (append posts nil))))))))

;;;###autoload
(defun bug--display-rally-discussion (posts)
  "Display Rally discussion POSTS in the current buffer.

POSTS is a list of post alists with Text, User, CreationDate, and PostNumber
fields."
  (when posts
    (insert "\n\nDISCUSSION:\n")
    (insert (make-string 70 ?-) "\n")
    (dolist (post posts)
      (let* ((post-num (cdr (assoc 'PostNumber post)))
             (user (cdr (assoc '_refObjectName (cdr (assoc 'User post)))))
             (date (cdr (assoc 'CreationDate post)))
             (text (cdr (assoc 'Text post))))
        (insert (propertize (format "Post #%d by %s on %s:\n"
                                    post-num
                                    (or user "Unknown")
                                    (bug--format-time-date date t))
                            'face 'bold))
        (insert (or text "") "\n")
        (insert (make-string 70 ?-) "\n")))))

;;;###autoload
(defun bug--create-rally-discussion-post (artifact-uuid text instance)
  "Create a new discussion post on a Rally artifact.

ARTIFACT-UUID is the artifact's ObjectUUID.
TEXT is the discussion post content.
INSTANCE is the Rally instance.

Returns the created post data."
  (let* ((response (bug--rpc-rally
                    `((resource . "conversationpost")
                      (operation . "create")
                      (data . ((ConversationPost .
                                                 ((Artifact . ,(concat "/artifact/" artifact-uuid))
                                                  (Text . ,text))))))
                    instance))
         (result (cdr (car response))))
    result))

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
                   (query-string (cadr (assoc 'query (cdr (assoc 'data query)))))
                   (stripped-query
                    (if query-string
                        (replace-regexp-in-string
                         "[ ()\"]" ""
                         query-string)
                      "")))  ; Empty string if no query
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
(defun bug--parse-rally-search-query (query instance)
  "Parse search query from minibuffer for rally"
  (cond
   ;; Handle empty search (when user just presses Enter)
   ;; Lists all items in the default/selected project
   ((or (eq query t) (and (stringp query) (string-empty-p query)))
    (let ((project-ref (bug--rally-get-project-ref instance)))
      (if project-ref
          `((data . ((project ,project-ref)
                     (order "FormattedID DESC")
                     (fetch "FormattedID,Name,State,ScheduleState,Owner"))))
        (error "No project specified"))))

   ;; for userfriendly rally IDs, open bug directly
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

;;;;;;
;; project and workspace management
;;
;; if we encounter other backends with a similar concept of projects we might
;; want to move some of that logic here into shared structures, but for now
;; this is good enough.

(defun bug--rally-get-workspace-oid (instance)
  "Get the workspace ObjectID for this instance.

Returns the ObjectID from instance config, or queries Rally for the
first available workspace from the subscription."
  (let ((workspace-id (bug--instance-property :workspace-id instance)))
    (if workspace-id
        workspace-id
      ;; Query subscription for workspaces
      (let ((workspaces (bug--rally-list-workspaces instance)))
        (if (null workspaces)
            (error "No workspaces found in subscription")
          ;; Return first workspace OID
          (cdr (car workspaces)))))))

(defun bug--rally-list-workspaces (instance)
  "List all workspaces accessible to the user.

Returns an alist of (name . oid) pairs."
  (let* ((response (bug--rpc-rally
                    '((resource . "workspace")
                      (operation . "query")
                      (data . ((fetch "Name,ObjectID,State")
                               (query "(State = \"Open\")")
                               (pagesize 200))))
                    instance))
         (query-result (cdr (assoc 'QueryResult response)))
         (results (cdr (assoc 'Results query-result)))
         (workspaces nil))
    (dotimes (i (length results))
      (let* ((workspace (aref results i))
             (name (cdr (assoc 'Name workspace)))
             (oid (cdr (assoc 'ObjectID workspace)))
             (state (cdr (assoc 'State workspace))))
        (when (string= state "Open")
          (push (cons name (if (numberp oid) (number-to-string oid) oid)) workspaces))))
    (nreverse workspaces)))

(defun bug--rally-list-projects (workspace-oid instance)
  "List all projects in the given workspace.

WORKSPACE-OID is the workspace ObjectID (string or number).
Returns an alist of (name . oid) pairs."
  (let* ((workspace-oid-str (if (numberp workspace-oid)
                                (number-to-string workspace-oid)
                              workspace-oid))
         (response
          (if bug-rally-projects-from-workspace
              ;; pull from workspace, with rally side filtering
              (bug--rpc-rally
               `((resource . "workspace")
                 (operation . "read")
                 (object-id . ,workspace-oid-str)
                 (object-type . "Projects")
                 (data . ((fetch "Name,ObjectID,State")
                          (query "(State = \"Open\")")
                          (pagesize 200))))
               instance)
            ;; pull projects directly, which will find all projects
            (bug--rpc-rally
             `((resource . "project")
               (operation . "query")
               (data . ((fetch "Name,ObjectID,State")
                        (query "(State = \"Open\")")
                        (pagesize 200))))
             instance)))
         (query-result (cdr (assoc 'QueryResult response)))
         (results (cdr (assoc 'Results query-result)))
         (projects nil))
    (dotimes (i (length results))
      (let* ((project (aref results i))
             (name (cdr (assoc 'Name project)))
             (oid (cdr (assoc 'ObjectID project)))
             (state (cdr (assoc 'State project))))
        (when (string= state "Open")
          (push (cons name (if (numberp oid) (number-to-string oid) oid)) projects))))
    (nreverse projects)))

;;;###autoload
(defun bug-rally-select-project (&optional instance)
  "Interactively select a Rally project from available projects.

Returns the project reference string (/project/12345) or nil if cancelled."
  (interactive (list (bug--query-instance)))
  (let* ((workspace-oid (bug--rally-get-workspace-oid instance))
         (projects (bug--rally-list-projects workspace-oid instance)))
    (if (null projects)
        (progn
          (message "No open projects found in workspace")
          nil)
      (let* ((project-names (mapcar #'car projects))
             (selected-name (completing-read "Select project: " project-names nil t))
             (selected-oid (cdr (assoc selected-name projects))))
        (when selected-oid
          (let ((project-ref (format "/project/%s" selected-oid)))
            (message "Selected project: %s (ID: %s)" selected-name selected-oid)
            project-ref))))))

;;;###autoload
(defun bug-rally-list-projects (&optional instance)
  "Display all available Rally projects in a table.

  Useful for finding project IDs to configure in instance settings."
  (interactive (list (bug--query-instance)))
  (let* ((workspace-oid (bug--rally-get-workspace-oid instance))
         (projects (bug--rally-list-projects workspace-oid instance))
         (buffer (get-buffer-create "*Rally Projects*")))
    (if (null projects)
        (message "No open projects found")
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "Rally Projects (Workspace OID: %s)\n" workspace-oid))
          (insert (format "Total: %d open projects\n\n" (length projects)))
          (insert "To use a project, add :project-id \"<ObjectID>\" to your instance config.\n")
          (insert "Use 'c' on a row to copy its ObjectID.\n\n")

          (make-vtable
           :columns '((:name "Project Name" :width 40)
                      (:name "ObjectID" :width 15)
                      (:name "State" :width 10))
           :objects projects
           :getter (lambda (project column vtable)
                     (pcase (vtable-column vtable column)
                       ("Project Name" (car project))
                       ("ObjectID" (cdr project))
                       ("State" "Open")))
           :actions `("c" ,(lambda (project)
                             (kill-new (cdr project))
                             (message "Copied ObjectID: %s" (cdr project))))))
        (special-mode)
        (goto-char (point-min)))
      (pop-to-buffer buffer))))

;;;###autoload
(defun bug-rally-create-project (name &optional instance)
  "Create a new Rally project.

NAME is the project name.
Requires Workspace Administrator or Subscription Administrator permissions."
  (interactive
   (list (read-string "Project Name: ")
         (bug--query-instance)))
  (let* ((workspace-oid (bug--rally-get-workspace-oid instance))
         (workspace-ref (format "/workspace/%s" workspace-oid))
         (created-project
          (bug--create-rally-bug
           "project"
           `((Name . ,name)
             (Workspace . ,workspace-ref)
             (State . "Open"))
           instance)))
    (when created-project
      (let ((project-name (cdr (assoc 'Name created-project)))
            (project-oid (cdr (assoc 'ObjectID created-project))))
        (message "Created project: %s (ID: %s)" project-name project-oid)
        project-oid))))

;;;;;;
;; Write operations (create, update, delete)

(defun bug--rally-project-display (project-ref instance)
  "Return a display alist for `project-ref' with _refobjectname populated.

Fetches the project name from rally the first time and caches it. Falls back to
 (_type . \"project\") when the fetch fails."
  (let* ((oid (and (string-match "/project/\\(.*\\)" project-ref)
                   (match-string 1 project-ref)))
         (cache-key (intern (concat "rally-project-name-" (or oid project-ref))))
         (cached (bug--cache-get-valid cache-key instance)))
    (or cached
        (condition-case nil
            (let* ((response (bug--rpc-rally
                              `((resource . "project")
                                (operation . "query")
                                (data . ((query ,(format "(objectid = %s)" oid))
                                         (fetch "name,objectid")
                                         (pagesize 1))))
                              instance))
                   (results (cdr (assoc 'results (cdr (assoc 'queryresult response)))))
                   (name (when (and results (> (length results) 0))
                           (cdr (assoc 'name (aref results 0)))))
                   (display `((_ref . ,project-ref)
                              (_refObjectName . ,(or name project-ref)))))
              (bug--cache-put-timed cache-key display bug--rally-cache-ttl instance)
              display)
          (error `((_ref . ,project-ref) (_type . "Project")))))))

(defun bug--rally-get-project-ref (instance)
  "Get or prompt for a Rally project reference.

Returns a project reference string in the format /project/12345.
Tries these methods in order:
1. Use :project-id from instance config
2. Interactive project selection from available projects
3. Manual input if selection fails"
  (let ((project-id (bug--instance-property :project-id instance)))
    (if project-id
        (format "/project/%s" project-id)
      ;; Try interactive project selection
      (or (bug-rally-select-project instance)
          ;; Fallback to manual input
          (let ((input-project-id (read-string "Rally Project ID: ")))
            (format "/project/%s" input-project-id))))))

(defun bug--rally-type-name (object-type)
  "Convert object-type to proper Rally API type name.

This transforms keywords to the capitalisation matching the Rally API
documentation. While it should not be needed it's better to rule that out
as source for potential errors."
  (let ((type-lower (downcase object-type)))
    (cond
     ((string= type-lower "hierarchicalrequirement") "HierarchicalRequirement")
     ((string= type-lower "defect") "Defect")
     ((string= type-lower "task") "Task")
     ((string= type-lower "project") "Project")
     ((string= type-lower "testcase") "TestCase")
     ((string= type-lower "testset") "TestSet")
     ((string= type-lower "defectsuite") "DefectSuite")
     ((string= type-lower "portfolioitem") "PortfolioItem")
     ;; PortfolioItem sub-types (TypePath like "PortfolioItem/Feature"):
     ;; the JSON body key is always the base type "PortfolioItem"
     ((string-prefix-p "portfolioitem/" type-lower) "PortfolioItem")
     ((string= type-lower "artifact") "Artifact")
     ;; Default: capitalize first letter
     (t (capitalize object-type)))))

(defun bug--create-rally-bug (object-type data instance)
  "Create a new Rally object via POST /slm/webservice/v2.0/<type>/create.

Despite its name this is the entry point for all create operations for
Rally objects.

OBJECT-TYPE is the Rally type (e.g., `defect', `hierarchicalrequirement').
DATA is an alist of field names and values.
Returns the created object from Rally's CreateResult."
  (let* ((resource (downcase object-type))
         (rally-type-name (bug--rally-type-name object-type))
         (response (bug--rpc-rally
                    `((resource . ,resource)
                      (operation . "create")
                      (data . ((,(intern rally-type-name) . ,data))))
                    instance))
         (create-result (cdr (assoc 'CreateResult response)))
         (_errors (cdr (assoc 'Errors create-result)))
         (warnings (cdr (assoc 'Warnings create-result)))
         (object (cdr (assoc 'Object create-result))))
    ;; Display warnings if any
    (when (and warnings (> (length warnings) 0))
      (message "Rally warnings: %s" (mapconcat 'identity warnings ", ")))
    ;; Error handling is done by bug--rpc-rally-handle-error
    ;; Return the created object
    object))

;;;###autoload
(defun bug-rally-create-defect (&optional instance)
  "Interactively create a new Rally Defect.

Prompts for required fields (Name, Project) and optional fields
(State, Description, Priority). Returns the created defect."
  (interactive
   (list (bug--query-instance)))
  (let* ((name (read-string "Defect Name: "))
         (project (bug--rally-get-project-ref instance))
         (state (completing-read "State (optional): "
                                 '("Submitted" "Open" "Fixed" "Closed")
                                 nil nil "Submitted"))
         (description (read-string "Description (optional): "))
         (priority (completing-read "Priority (optional): "
                                    '("Resolve Immediately" "High Attention"
                                      "Normal" "Low")
                                    nil nil))
         (data `((Name . ,name)
                 (Project . ,project))))
    ;; Add optional fields if provided
    (when (and state (not (string-empty-p state)))
      (push `(State . ,state) data))
    (when (and description (not (string-empty-p description)))
      (push `(Description . ,description) data))
    (when (and priority (not (string-empty-p priority)))
      (push `(Priority . ,priority) data))
    (let ((created-defect (bug--create-rally-bug "defect" data instance)))
      (message "Created defect: %s" (cdr (assoc '_refObjectName created-defect)))
      ;; Open the newly created defect
      (bug-open (cdr (assoc '_refObjectUUID created-defect)) instance)
      created-defect)))

;;;###autoload
(defun bug-rally-create-story (&optional instance)
  "Interactively create a new Rally User Story.

Prompts for required fields (Name, Project) and optional fields
(ScheduleState, Description). Returns the created story."
  (interactive
   (list (bug--query-instance)))
  (let* ((name (read-string "Story Name: "))
         (project (bug--rally-get-project-ref instance))
         (schedule-state (completing-read "Schedule State (optional): "
                                          '("Defined" "In-Progress" "Completed" "Accepted")
                                          nil nil "Defined"))
         (description (read-string "Description (optional): "))
         (data `((Name . ,name)
                 (Project . ,project))))
    ;; Add optional fields if provided
    (when (and schedule-state (not (string-empty-p schedule-state)))
      (push `(ScheduleState . ,schedule-state) data))
    (when (and description (not (string-empty-p description)))
      (push `(Description . ,description) data))
    (let ((created-story (bug--create-rally-bug "hierarchicalrequirement" data instance)))
      (message "Created user story: %s" (cdr (assoc '_refObjectName created-story)))
      ;; Open the newly created story
      (bug-open (cdr (assoc '_refObjectUUID created-story)) instance)
      created-story)))

;; TODO, we handle parent relationships when creating a new issue from an
;;       existing issue, but curently don't allow reparenting
(defun bug--rally-create-parent-field (new-type context-type context-ref)
  "Return (field-name . ref) for the parent link when creating `new-type' from
`context-type'.

`context-ref` is the _ref URL of the current artifact.  Returns nil when no
standard parent relationship applies between the two types."
  (when (and context-type context-ref)
    (let ((ctx (downcase context-type))
          (new (downcase new-type)))
      (cond
       ;; Defect from User Story -> Requirement field
       ((and (equal new "defect")
             (equal ctx "hierarchicalrequirement"))
        (cons "Requirement" context-ref))
       ;; Task from User Story or Defect -> WorkProduct
       ((and (equal new "task")
             (member ctx '("hierarchicalrequirement" "defect")))
        (cons "WorkProduct" context-ref))
       ;; Child User Story from User Story -> Parent
       ((and (equal new "hierarchicalrequirement")
             (equal ctx "hierarchicalrequirement"))
        (cons "Parent" context-ref))
       ;; User Story from a PortfolioItem (Feature/Epic/…) -> Feature field
       ;; Rally always calls this field "Feature" regardless of workspace naming
       ((and (equal new "hierarchicalrequirement")
             (string-prefix-p "portfolioitem/" ctx))
        (cons "Feature" context-ref))
       ;; Test Case from User Story -> WorkProduct
       ((and (equal new "testcase")
             (equal ctx "hierarchicalrequirement"))
        (cons "WorkProduct" context-ref))
       ;; Nested PortfolioItem from another PortfolioItem -> Parent
       ((and (string-prefix-p "portfolioitem/" new)
             (string-prefix-p "portfolioitem/" ctx))
        (cons "Parent" context-ref))
       (t nil)))))

(defun bug--rally-get-portfolio-item-types (instance)
  "Return an alist of (display-name . type-path) for PortfolioItem sub-types.

This is mainly relevant for creating new items as Portfolio item types (Feature,
Epic, Initiative, etc.) are workspace-specific and must be queried from the
TypeDefinition API. They're not expected to change, so the result is cached."
  (let* ((cache-key 'rally-portfolio-item-types)
         (cached (bug--cache-get-valid cache-key instance)))
    (or cached
        (condition-case err
            (let* ((workspace-oid (bug--rally-get-workspace-oid instance))
                   (workspace-ref (format "/workspace/%s" workspace-oid))
                   (response (bug--rpc-rally
                              `((resource . "typedefinition")
                                (operation . "query")
                                (data . ((query "(Creatable = true)")
                                         (fetch "Name,ElementName,TypePath")
                                         (workspace ,workspace-ref)
                                         (pagesize 200))))
                              instance))
                   (results (cdr (assoc 'Results (cdr (assoc 'QueryResult response)))))
                   (types nil))
              (when results
                (dotimes (i (length results))
                  (let* ((def (aref results i))
                         (name (cdr (assoc 'Name def)))
                         (type-path (cdr (assoc 'TypePath def))))
                    ;; Keep only PortfolioItem sub-types
                    (when (and name type-path
                               (string-prefix-p "PortfolioItem/"
                                                (if (symbolp type-path)
                                                    (symbol-name type-path)
                                                  type-path)))
                      (let ((path-str (if (symbolp type-path)
                                          (symbol-name type-path)
                                        type-path)))
                        (push (cons name path-str) types))))))
              (let ((result (nreverse types)))
                (bug--cache-put-timed cache-key result bug--rally-cache-ttl instance)
                result))
          (error
           (message "bug-mode: error fetching portfolio item types: %s"
                    (error-message-string err))
           nil)))))

(defun bug--rally-get-draft-fields (type-name instance)
  "Return field names for a new-artifact draft buffer of type `type-name'.

Merges hardcoded baseline fields with any API-Required custom fields from
the workspace TypeDefinition.  Required non-readonly non-hidden fields from
the API are appended to the baseline so workspace-specific required fields
always appear even if not in the hardcoded list.

Falls back to the baseline alone when the TypeDefinition query fails."
  (let* (;; ElementName is the last path segment: \"Feature\" from \"PortfolioItem/Feature\"
         (element-name (car (last (split-string type-name "/"))))
         ;; Hardcoded baseline: standard fields for each core type
         (baseline (or (cdr (assoc type-name bug--rally-draft-fields))
                       (when (string-prefix-p "PortfolioItem/" type-name)
                         (cdr (assoc "PortfolioItem" bug--rally-draft-fields)))
                       '("Name" "Owner" "Description")))
         (attrs (bug--rally-get-type-attributes element-name instance))
         (required-from-api '()))
    ;; Collect Required + !ReadOnly + !Hidden fields from TypeDefinition.
    ;; Only process each attribute once by matching the key against ElementName.
    (when attrs
      (maphash
       (lambda (key attr)
         (when (equal key (cdr (assoc 'ElementName attr)))
           (when (and (equal t (cdr (assoc 'Required attr)))
                      (not (equal t (cdr (assoc 'ReadOnly attr))))
                      (not (equal t (cdr (assoc 'Hidden attr)))))
             (push key required-from-api))))
       attrs))
    ;; Merge: baseline order preserved, API-required fields appended if absent
    (let ((result (copy-sequence baseline)))
      (dolist (f required-from-api)
        (unless (member f result)
          (setq result (append result (list f)))))
      result)))

(defun bug--create-rally-bug-interactive (context instance)
  "Open a draft buffer for creating a new Rally artifact.

`context' is nil for standalone creation, or the current bug's data alist (from
bug---data) when invoked from an existing bug buffer.  The parent link is
inferred automatically from the two artifact types.

Only prompts for artifact type (and project when not derivable from context or
instance config).  All other fields are edited in the draft buffer using the
normal \\[bug--bug-mode-edit-thing-near-point] key; press \\[bug--bug-mode-commit] to create."
  (let* (;; Type selection: core types hardcoded; portfolio items queried per workspace
         (core-types '(("Defect"      . "Defect")
                       ("User Story"  . "HierarchicalRequirement")
                       ("Task"        . "Task")
                       ("Test Case"   . "TestCase")))
         (portfolio-types (bug--rally-get-portfolio-item-types instance))
         (type-choices (append core-types portfolio-types))
         (type-display (completing-read "Artifact type: "
                                        (mapcar #'car type-choices) nil t))
         (rally-type (cdr (assoc type-display type-choices)))

         ;; Context analysis
         (context-type-raw
          (when context
            (let ((ot (cdr (assoc 'ObjectType context))))
              (cond ((symbolp ot) (symbol-name ot))
                    ((stringp ot) ot)
                    (t nil)))))
         (context-ref (when context (cdr (assoc '_ref context))))

         ;; Parent field from context types
         (parent-info (bug--rally-create-parent-field rally-type context-type-raw context-ref))
         (parent-field (car parent-info))
         (parent-ref   (cdr parent-info))

         ;; Project: inherit from context, or use :project-id from instance config.
         ;; No interactive prompt — user fills it in the draft buffer if needed.
         (ctx-project (when context (cdr (assoc 'Project context))))
         (project-ref
          (or (when (listp ctx-project) (cdr (assoc '_ref ctx-project)))
              (let ((pid (bug--instance-property :project-id instance)))
                (when pid (format "/project/%s" pid)))))

         ;; Fields to pre-populate in bug---changed-data (sent to create API)
         (create-alist (when project-ref `((Project . ,project-ref))))

         ;; Display alist for the draft buffer.
         ;; bug--rally-get-draft-fields merges the hardcoded baseline with any
         ;; workspace-required custom fields from the TypeDefinition API.
         (field-names (bug--rally-get-draft-fields rally-type instance))
         (display-alist
          (let ((base (list (cons 'ObjectType (intern rally-type)))))
            (when project-ref
              (let* ((obj-name (when (listp ctx-project)
                                 (cdr (assoc '_refObjectName ctx-project))))
                     (proj-display (if obj-name
                                       `((_ref . ,project-ref)
                                         (_refObjectName . ,obj-name))
                                     (bug--rally-project-display project-ref instance))))
                (push (cons 'Project proj-display) base)))
            base)))

    ;; Add pre-filled parent to both create-alist (API value) and display-alist
    (when (and parent-field parent-ref)
      (push `(,(intern parent-field) . ,parent-ref) create-alist)
      ;; Display the parent as an object reference so it renders as "-> Name"
      (let ((ctx-name (or (cdr (assoc 'FormattedID context))
                          (cdr (assoc 'Name context))
                          "")))
        (push (cons (intern parent-field)
                    `((_ref . ,parent-ref) (_refObjectName . ,ctx-name)))
              display-alist)))

    ;; Add empty draft fields; skips any already set (e.g. Project from above)
    (dolist (name field-names)
      (unless (equal name "Description")  ;; bug-show handles Description separately
        (unless (assoc (intern name) display-alist)
          (push (cons (intern name) nil) display-alist))))

    ;; Auto-populate required non-OBJECT enum fields with their first allowed value
    ;; so the user doesn't have to pick an obvious default (e.g. ScheduleState).
    (let* ((element-name (car (last (split-string rally-type "/"))))
           (attrs (bug--rally-get-type-attributes element-name instance)))
      (when attrs
        (dolist (name field-names)
          (let* ((field-sym (intern name))
                 (entry (assoc field-sym display-alist)))
            (when (and entry (null (cdr entry)))
              (let* ((attr (gethash name attrs))
                     (attr-type (when attr (cdr (assoc 'AttributeType attr))))
                     (required (when attr (equal t (cdr (assoc 'Required attr))))))
                (when (and required attr-type
                           (not (member attr-type '("OBJECT" "COLLECTION"))))
                  (let ((vals (bug--rally-field-allowed-values element-name name instance)))
                    (when vals
                      (let ((first-val (car vals)))
                        (setcdr entry first-val)
                        (unless (assoc field-sym create-alist)
                          (push (cons field-sym first-val) create-alist))))))))))))

    ;; If ScheduleState was auto-populated and FlowState linking is enabled,
    ;; pre-fill FlowState so the draft buffer shows it immediately.
    (when bug-rally-link-flowstate
      (let* ((ss-val (cdr (assoc 'ScheduleState create-alist))))
        (when (stringp ss-val)
          (let* ((mapping (bug--rally-get-flowstate-mapping instance))
                 (fs-entry (assoc ss-val mapping)))
            (when fs-entry
              (let* ((fs-name (cadr fs-entry))
                     (fs-ref  (cddr fs-entry))
                     (fs-display `((_ref . ,fs-ref) (_refObjectName . ,fs-name))))
                (unless (assoc 'FlowState create-alist)
                  (push (cons 'FlowState fs-ref) create-alist))
                (let ((fs-disp (assoc 'FlowState display-alist)))
                  (when fs-disp
                    (setcdr fs-disp fs-display)))))))))

    ;; Description must be in display-alist for bug-show to render it in drafts
    (push '(Description . nil) display-alist)

    (bug-new-draft display-alist create-alist instance)))

(defun bug--create-rally-new-artifact (args instance)
  "Create a Rally artifact from a draft buffer's committed data.

`args' is (bug-data changed-data) where `bug-data' contains the draft display
alist (with ObjectType) and `changed-data' contains the user-edited fields plus
pre-filled project/parent refs.

Called by `bug--bug-mode-commit' when bug---is-new is non-nil."
  (let* ((bug-data     (car args))
         (changed-data (cadr args))
         (object-type-raw (cdr (assoc 'ObjectType bug-data)))
         (type-path
          (cond ((symbolp object-type-raw) (symbol-name object-type-raw))
                ((stringp object-type-raw) object-type-raw)
                (t (error "No ObjectType in draft buffer"))))
         (draft-buffer (current-buffer))
         (created (bug--create-rally-bug type-path changed-data instance)))
    (if created
        (let ((uuid (cdr (assoc '_refObjectUUID created)))
              (name (cdr (assoc '_refObjectName created))))
          (message "Created: %s" (or name ""))
          (bug-open uuid instance)
          (kill-buffer draft-buffer))
      (message "Failed to create %s" type-path))))

(defun bug--update-rally-bug (args instance)
  "Update a Rally object via POST /<type>/<id>.

ARGS is a list containing (OBJECT-ID DATA) where:
  OBJECT-ID is the Rally ObjectID (UUID) or _ref string
  DATA is an alist of field names and values to update
INSTANCE is the Rally instance.

Returns the updated object from Rally's OperationResult."
  (let* ((object-id (car args))
         (data (cadr args))
         (response (bug--rpc-rally
                    `((resource . "artifact")
                      (operation . "update")
                      (object-id . ,object-id)
                      (data . ((Artifact . ,data))))
                    instance))
         (operation-result (cdr (car response)))
         (_errors (cdr (assoc 'Errors operation-result)))
         (warnings (cdr (assoc 'Warnings operation-result)))
         (object (cdr (assoc 'Object operation-result))))
    ;; Display warnings if any
    (when (and warnings (> (length warnings) 0))
      (message "Rally warnings: %s" (mapconcat 'identity warnings ", ")))
    ;; Error handling is done by bug--rpc-rally-handle-error
    ;; Return the updated object
    object))

(defun bug--delete-rally-bug (object-id instance)
  "Delete a Rally object via DELETE /<type>/<id>.

OBJECT-ID is the Rally ObjectID (UUID) or _ref string.
The object is moved to Rally's Recycle Bin, not permanently deleted.
Returns t on success."
  (let* ((response (bug--rpc-rally
                    `((resource . "artifact")
                      (operation . "delete")
                      (object-id . ,object-id))
                    instance))
         (operation-result (cdr (car response)))
         (_errors (cdr (assoc 'Errors operation-result)))
         (warnings (cdr (assoc 'Warnings operation-result))))
    ;; Display warnings if any
    (when (and warnings (> (length warnings) 0))
      (message "Rally warnings: %s" (mapconcat 'identity warnings ", ")))
    ;; Error handling is done by bug--rpc-rally-handle-error
    t))

;;;###autoload
(defun bug-rally-delete-bug (&optional id instance)
  "Interactively delete a Rally bug (moves to Recycle Bin).

If ID is not provided, uses the bug from the current buffer.
Prompts for confirmation before deleting."
  (interactive
   (list nil (bug--query-instance)))
  (unless id
    (when (boundp 'bug---uuid) (setq id bug---uuid)))
  (unless id
    (error "No bug ID found. Open a bug first or provide an ID"))
  (let ((bug-name (cdr (assoc 'Name bug---data))))
    (when (yes-or-no-p (format "Delete Rally bug '%s'? (moves to Recycle Bin) "
                               (or bug-name id)))
      (bug--delete-rally-bug id instance)
      (message "Deleted bug: %s" (or bug-name id))
      ;; Close the buffer if we're in a bug buffer
      (when (eq major-mode 'bug-mode)
        (kill-buffer))
      t)))

(defun bug-rally-discussion (&optional instance)
  "Add a discussion post to the current Rally artifact.

Opens a buffer for composing a discussion post. Use C-c C-c to submit."
  (interactive (list (bug--query-instance)))
  (unless (boundp 'bug---uuid)
    (error "Not in a bug buffer or no bug UUID found"))
  (unless bug---uuid
    (error "No bug UUID found"))
  (let ((uuid bug---uuid)
        (bug-id bug---id)
        (inst instance))
    (pop-to-buffer (format "*rally discussion: %s*" bug-id))
    (erase-buffer)
    (text-mode)
    (insert "# Enter discussion post below. Use C-c C-c to submit.\n")
    (insert "# Lines starting with # are ignored.\n\n")
    (local-set-key (kbd "C-c C-c")
                   (lambda ()
                     (interactive)
                     (let ((text (buffer-string))
                           (post-text ""))
                       ;; Filter out comment lines
                       (dolist (line (split-string text "\n"))
                         (unless (string-match-p "^#" line)
                           (setq post-text (concat post-text line "\n"))))
                       (setq post-text (string-trim post-text))
                       (when (string-empty-p post-text)
                         (error "Discussion post cannot be empty"))
                       (message "Posting discussion...")
                       (bug--create-rally-discussion-post uuid post-text inst)
                       (message "Discussion post created")
                       (kill-buffer)
                       ;; Refresh the bug view
                       (bug-open bug-id inst))))))

;;;;;;
;; Field completion for Rally

(defun bug--rally-get-type-attributes (type-name instance)
  "Fetch and cache attribute definitions for `type-name' from Rally.

Queries the workspace-scoped attributedefinition endpoint.
Returns a hash table mapping field-name (string) to attribute alist,
or nil on failure. Results are cached for 24 hours."
  (let* ((cache-key (intern (concat "rally-type-attrs-" type-name)))
         (cached (bug--cache-get-valid cache-key instance)))
    (or cached
        (condition-case err
            (progn
              (message "Fetching field definitions for %s..." type-name)
              (let* ((workspace-oid (bug--rally-get-workspace-oid instance))
                     (workspace-ref (format "/workspace/%s" workspace-oid))
                     ;; Step 1: resolve TypeDefinition OID for this type name
                     (typedef-response
                      (bug--rpc-rally
                       `((resource . "typedefinition")
                         (operation . "query")
                         (data . ((query ,(format "(ElementName = \"%s\")" type-name))
                                  (fetch "ObjectID,ElementName")
                                  (workspace ,workspace-ref)
                                  (pagesize 1))))
                       instance))
                     (typedef-results
                      (cdr (assoc 'Results (cdr (assoc 'QueryResult typedef-response)))))
                     (typedef-oid
                      (when (and typedef-results (> (length typedef-results) 0))
                        (let ((oid (cdr (assoc 'ObjectID (aref typedef-results 0)))))
                          (if (numberp oid) (number-to-string oid) oid))))
                     ;; Step 2: fetch attribute definitions for the resolved type
                     (response
                      (when typedef-oid
                        (bug--rpc-rally
                         `((resource . ,(format "typedefinition/%s/Attributes" typedef-oid))
                           (operation . "query")
                           (data . ((fetch "Name,ElementName,ObjectID,AttributeType,AllowedValues,Required,ReadOnly,Hidden,Custom")
                                    (pagesize 200))))
                         instance)))
                     (query-result (cdr (assoc 'QueryResult response)))
                     (results (cdr (assoc 'Results query-result)))
                     (attrs (make-hash-table :test 'equal)))
                (if (not typedef-oid)
                    (message "bug-mode: TypeDefinition not found for %s" type-name)
                  (when results
                    (dotimes (i (length results))
                      (let* ((attr (aref results i))
                             (name (cdr (assoc 'Name attr)))
                             (element-name (cdr (assoc 'ElementName attr))))
                        (when name
                          (puthash name attr attrs))
                        ;; Also index by ElementName (the camelCase API key, e.g.
                        ;; "ScheduleState") so lookups by buffer field-name succeed.
                        (when (and element-name (not (equal element-name name)))
                          (puthash element-name attr attrs))))))
                (bug--cache-put-timed cache-key attrs bug--rally-cache-ttl instance)
                attrs))
          (error
           (message "bug-mode: error fetching Rally field definitions: %s"
                    (error-message-string err))
           nil)))))

(defun bug--rally-field-allowed-values (type-name field-name instance)
  "Get the allowed values for `field-name' in `type-name', with caching.

`field-name' may be a string or symbol.
For STRING/STATE/RATING fields returns a list of strings.
For OBJECT fields returns an alist of (display-name . ref) so the selected
display name can be mapped back to a Rally object reference for updates."
  (let* ((field-name-str (if (symbolp field-name) (symbol-name field-name) field-name))
         (attrs (bug--rally-get-type-attributes type-name instance))
         (attr (when attrs (gethash field-name-str attrs))))
    (when attr
      (let* ((allowed-ref (cdr (assoc 'AllowedValues attr)))
             (count (cdr (assoc 'Count allowed-ref)))
             (attr-type (cdr (assoc 'AttributeType attr))))
        (when (and count (> count 0))
          (let* ((oid (cdr (assoc 'ObjectID attr)))
                 (attr-oid (if (numberp oid) (number-to-string oid) oid))
                 (cache-key (intern (concat "rally-allowed-vals-" attr-oid)))
                 (cached (bug--cache-get-valid cache-key instance)))
            (or cached
                (condition-case err
                    (let* ((response (bug--rpc-rally
                                      `((resource . ,(concat "attributedefinition/"
                                                             attr-oid
                                                             "/AllowedValues"))
                                        (operation . "query")
                                        (data . ((fetch "StringValue,Name,_refObjectName")
                                                 (pagesize 200))))
                                      instance))
                           (query-result (cdr (assoc 'QueryResult response)))
                           (results (cdr (assoc 'Results query-result)))
                           (values nil)
                           (null-entry nil))
                      (when results
                        (dotimes (i (length results))
                          (let* ((val (aref results i))
                                 (str-value (cdr (assoc 'StringValue val)))
                                 ;; Strip HTML tags from StringValue for clean display
                                 (display-name
                                  (when str-value
                                    (replace-regexp-in-string "<[^>]+>" "" str-value)))
                                 ;; For null-ref entries use Name or _refObjectName as label
                                 (item-ref (cdr (assoc '_ref val)))
                                 (null-ref-p (or (null item-ref)
                                                 (equal item-ref "null")))
                                 (display-name-plain
                                  (if (and null-ref-p
                                           (or (null display-name)
                                               (string-empty-p display-name)))
                                      (or (cdr (assoc '_refObjectName val))
                                          (cdr (assoc 'Name val)))
                                    (or (and display-name
                                             (not (string-empty-p display-name))
                                             display-name)
                                        (cdr (assoc 'Name val)))))
                                 (value-ref
                                  (when (not null-ref-p) item-ref))
                                 (push-val
                                  (if (equal attr-type "OBJECT")
                                      (cond
                                       ;; Null-ref entry: "no selection" — always record,
                                       ;; using "—" when the API provides no label.
                                       (null-ref-p
                                        (setq null-entry (cons (or display-name-plain "—") nil))
                                        nil)
                                       ;; Normal ref entry
                                       ((and value-ref display-name-plain
                                             (not (string-empty-p display-name-plain)))
                                        (cons display-name-plain value-ref)))
                                    ;; plain string for STRING/STATE/RATING etc.
                                    (when (and display-name-plain
                                               (stringp display-name-plain)
                                               (not (string-empty-p display-name-plain)))
                                      display-name-plain))))
                            (when push-val
                              (push push-val values)))))
                      (let* ((sorted-values (nreverse values))
                             ;; Prepend the null-ref "no selection" entry (e.g. "Unscheduled"
                             ;; for Iteration, "No Release" for Release) at the top so it's
                             ;; always accessible.  Its display name comes from the API.
                             (with-null (if null-entry
                                            (cons null-entry sorted-values)
                                          sorted-values)))
                        (bug--cache-put-timed cache-key with-null bug--rally-cache-ttl instance)
                        with-null))
                  (error
                   (message "bug-mode: error fetching Rally allowed values for %s: %s"
                            field-name-str (error-message-string err))
                   nil)))))))))

;;;###autoload
(defun bug--validate-draft-rally (args instance)
  "Validate a Rally draft before submission.

`args' is (bug-data changed-data). Returns a list of required field names
that are missing or empty in changed-data, or nil when everything is present."
  (let* ((bug-data (car args))
         (changed-data (cadr args))
         (object-type (cdr (assoc 'ObjectType bug-data)))
         (type-name (cond ((symbolp object-type) (symbol-name object-type))
                          ((stringp object-type) object-type)
                          (t nil)))
         (element-name (when type-name (car (last (split-string type-name "/")))))
         (attrs (when element-name (bug--rally-get-type-attributes element-name instance)))
         (missing '()))
    (when attrs
      (maphash
       (lambda (key attr)
         (when (equal key (cdr (assoc 'ElementName attr)))
           (when (and (equal t (cdr (assoc 'Required attr)))
                      (not (equal t (cdr (assoc 'ReadOnly attr))))
                      (not (equal t (cdr (assoc 'Hidden attr)))))
             (let* ((field-sym (intern key))
                    (val (or (cdr (assoc field-sym changed-data))
                             (cdr (assoc key changed-data)))))
               (when (or (null val)
                         (and (stringp val) (string-empty-p (string-trim val))))
                 (push key missing))))))
       attrs))
    (nreverse missing)))

(defun bug--available-field-names-rally (args instance)
  "Return an alist of (display-name . element-name) for addable fields.

`args' is (type-name present-keys) where present-keys is a list of symbols or
strings already in the buffer.  Hidden fields and fields already present are
excluded.  Result is sorted by display name."
  (let* ((type-name (car args))
         (present (cadr args))
         (element-name (car (last (split-string type-name "/"))))
         (attrs (bug--rally-get-type-attributes element-name instance))
         (present-strings (mapcar (lambda (k) (if (symbolp k) (symbol-name k) k)) present))
         (result '()))
    (when attrs
      (maphash
       (lambda (key attr)
         (when (equal key (cdr (assoc 'ElementName attr)))
           (unless (or (equal t (cdr (assoc 'Hidden attr)))
                       (member key present-strings))
             (let ((display-name (or (cdr (assoc 'Name attr)) key)))
               (push (cons display-name key) result)))))
       attrs))
    (sort result (lambda (a b) (string< (car a) (car b))))))

(defun bug--field-completion-rally (field-name instance)
  "Return completion candidates for `field-name' in the current Rally artifact.

Queries Rally for attribute definitions and allowed values for the artifact
type currently open in the buffer. Results are cached per type for 24 hours.
Returns a list of value strings, or nil if no completion is available."
  (when (and (boundp 'bug---data) bug---data)
    (let* ((object-type (cdr (assoc 'ObjectType bug---data)))
           (type-name (cond ((symbolp object-type) (symbol-name object-type))
                            ((stringp object-type) object-type)
                            (t nil))))
      (when type-name
        (bug--rally-field-allowed-values type-name field-name instance)))))

;;;;;;
;; FlowState / ScheduleState linking
;;
;; For now this impacts only those two fields, but implements generic logic for
;; both marking fields as non-editable (which is a frontend  feature), as well
;; as linking fields together.

(defun bug--rally-get-flowstate-mapping (instance)
  "Return an alist mapping ScheduleState strings to (name . ref) FlowState pairs.

Queries all FlowState objects, keeping the lowest-OrderIndex entry per
ScheduleStateMapping value, caching the result."
  (let* ((cache-key 'rally-flowstate-mapping)
         (cached (bug--cache-get-valid cache-key instance)))
    (or cached
        (condition-case err
            (let* ((workspace-oid (bug--rally-get-workspace-oid instance))
                   (workspace-ref (format "/workspace/%s" workspace-oid))
                   (response (bug--rpc-rally
                              `((resource . "flowstate")
                                (operation . "query")
                                (data . ((fetch "Name,_ref,ScheduleStateMapping,OrderIndex")
                                         (workspace ,workspace-ref)
                                         (order "OrderIndex")
                                         (pagesize 100))))
                              instance))
                   (results (cdr (assoc 'Results (cdr (assoc 'QueryResult response)))))
                   (mapping nil))
              (when results
                (dotimes (i (length results))
                  (let* ((fs (aref results i))
                         (name (cdr (assoc 'Name fs)))
                         (ref  (cdr (assoc '_ref fs)))
                         (ss   (let ((v (cdr (assoc 'ScheduleStateMapping fs))))
                                 (if (symbolp v) (symbol-name v) v))))
                    ;; First (lowest OrderIndex) FlowState per ScheduleState wins
                    (when (and name ref ss (not (assoc ss mapping)))
                      (push (cons ss (cons name ref)) mapping)))))
              (let ((result (nreverse mapping)))
                (bug--cache-put-timed cache-key result bug--rally-cache-ttl instance)
                result))
          (error
           (message "bug-mode: error fetching Rally FlowState mapping: %s"
                    (error-message-string err))
           nil)))))

(defun bug--field-edit-blocked-rally (field-name instance)
  "Return a message string when `field-name' must not be edited directly.

This is called by the frontend to check if a specific field can be edited
interactively.

When `bug-rally-link-flowstate' is t, FlowState is managed automatically
whenever ScheduleState changes and cannot be set by hand. Currentnly we're
not marking any other field as non-editable"
  (ignore instance)
  (when (and bug-rally-link-flowstate
             (memq field-name '(FlowState)))
    "FlowState is linked to ScheduleState (see bug-rally-link-flowstate); edit ScheduleState instead"))

(defun bug--linked-field-changes-rally (args instance)
  "Return linked field changes when `args' field edit should trigger
side-effects.

`args' is (field-name new-ref).  When `bug-rally-link-flowstate' is t and
field-name is ScheduleState, derives the matching FlowState and returns:

  ((changes . ((FlowState . ref-string)))
   (display . ((FlowState . ((_ref . ref) (_refObjectName . name))))))

Returns nil when no linked change is needed."
  (let ((field-name (car args))
        (new-ref    (cadr args)))
    (when (and bug-rally-link-flowstate
               (memq field-name '(ScheduleState))
               (stringp new-ref))
      (let* ((mapping (bug--rally-get-flowstate-mapping instance))
             (entry   (assoc new-ref mapping)))
        (when entry
          (let* ((fs-name (cadr entry))
                 (fs-ref  (cddr entry)))
            `((changes . ((FlowState . ,fs-ref)))
              (display . ((FlowState . ((_ref . ,fs-ref)
                                        (_refObjectName . ,fs-name))))))))))))

(provide 'bug-backend-rally)
;;; bug-backend-rally.el ends here
