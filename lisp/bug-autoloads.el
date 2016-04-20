;;; bug-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "bug-auth" "bug-auth.el" (22295 38790 44887
;;;;;;  270000))
;;; Generated autoloads from bug-auth.el

(autoload 'bug-logout "bug-auth" "\


\(fn &optional INSTANCE)" t nil)

(autoload 'bug-login "bug-auth" "\


\(fn &optional INSTANCE)" t nil)

;;;***

;;;### (autoloads nil "bug-list-mode" "bug-list-mode.el" (22295 39307
;;;;;;  254993 937000))
;;; Generated autoloads from bug-list-mode.el

(autoload 'bug--list-mode-info "bug-list-mode" "\
Display some information about thing at or near point

This is mostly useful for debugging text properties

\(fn)" t nil)

(autoload 'bug--list-mode-select-bug "bug-list-mode" "\
Open the current bug from the list. The bug identifier is read from text
properties at point, or -- if that fails -- from the beginning of the current
line

\(fn)" t nil)

(autoload 'bug--list-mode-update-list "bug-list-mode" "\
Update the list by running the original search query again

\(fn)" t nil)

(autoload 'bug--list-mode-quit-window "bug-list-mode" "\
Close the search result window

\(fn)" t nil)

;;;***

;;;### (autoloads nil "bug-mode" "bug-mode.el" (22295 38879 526867
;;;;;;  671000))
;;; Generated autoloads from bug-mode.el

(autoload 'bug-open "bug-mode" "\
Retrieve and show a single bug

\(fn ID &optional INSTANCE)" t nil)

(autoload 'bug--bug-format-field-value "bug-mode" "\
Format a bug field value for display, taking into account instance
specific field descriptions. Unlike bug--bug-format-field-name this function
requires both field name and content, therefore taking the complete cons
cell as argument

If the optional parameter `long' is non-nil display functions output
is formatted to take more space

\(fn FIELD &optional INSTANCE LONG)" nil nil)

(autoload 'bug--bug-get-field-property "bug-mode" "\
Return a property for a bug field from the field definition.

For example, to find the display name for the field 'foo' you could do
the following:
 (bug--bug-get-field-property 'foo 'display_name instance)

\(fn FIELD-NAME PROPERTY &optional INSTANCE)" nil nil)

(autoload 'bug--bug-mode-browse-bug "bug-mode" "\
Open the current bug in browser

\(fn)" t nil)

(autoload 'bug--bug-mode-create-comment "bug-mode" "\
Create a comment on the current bug

\(fn)" t nil)

(autoload 'bug--bug-mode-download-attachment "bug-mode" "\
Download the current attachment to the home directory

\(fn)" t nil)

(autoload 'bug--bug-mode-open-attachment "bug-mode" "\
Open the current attachment in the web browser

\(fn)" t nil)

(autoload 'bug--bug-mode-edit-field "bug-mode" "\
Edit the bug field at or near point

\(fn)" t nil)

(autoload 'bug--bug-mode-info "bug-mode" "\
Display some information about thing at or near point

This is mostly useful for debugging text properties

\(fn)" t nil)

(autoload 'bug--bug-mode-remember-bug "bug-mode" "\
Remember the current bug in a local search

\(fn LIST-NAME &optional ID INSTANCE)" t nil)

(autoload 'bug--bug-mode-resolve-bug "bug-mode" "\
Resolve the current bug

\(fn)" t nil)

(autoload 'bug--bug-mode-update-bug "bug-mode" "\
Update the bug by reloading it from the bug tracker

\(fn)" t nil)

(autoload 'bug--bug-mode-quit-window "bug-mode" "\
Close the search result window

\(fn)" t nil)

;;;***

;;;### (autoloads nil "bug-mode-bz" "bug-mode-bz.el" (22295 36973
;;;;;;  704928 2000))
;;; Generated autoloads from bug-mode-bz.el

(autoload 'bug--fetch-bz-bug "bug-mode-bz" "\
Retrieve a single bug from Bugzilla

\(fn ID &optional INSTANCE)" nil nil)

;;;***

;;;### (autoloads nil "bug-mode-rally" "bug-mode-rally.el" (22295
;;;;;;  36971 373954 901000))
;;; Generated autoloads from bug-mode-rally.el

(autoload 'bug--fetch-rally-bug "bug-mode-rally" "\
Retrieve a single bug from Rally

\(fn ID &optional INSTANCE)" nil nil)

;;;***

;;;### (autoloads nil "bug-rpc-bz" "bug-rpc-bz.el" (22295 36977 481884
;;;;;;  416000))
;;; Generated autoloads from bug-rpc-bz.el

(autoload 'bug--rpc-bz "bug-rpc-bz" "\
Send an RPC response to the given (or default) Bugzilla instance and return the
parsed response as alist

\(fn METHOD ARGS &optional INSTANCE)" nil nil)

(autoload 'bug--rpc-bug-handle-error "bug-rpc-bz" "\
Check data returned from Bugzilla for errors

\(fn RESPONSE)" nil nil)

;;;***

;;;### (autoloads nil "bug-rpc-rally" "bug-rpc-rally.el" (22295 36974
;;;;;;  151922 843000))
;;; Generated autoloads from bug-rpc-rally.el

(autoload 'bug--rpc-rally "bug-rpc-rally" "\
Send an RPC response to the given (or default) Rally instance and return the
parsed response as alist.

The method syntax follows the Bugzilla API (<api-object>.<operation), even
though the API is different for Rally. All operations but query and create
require an additional object-id in args to work.

args is an alist, whith the following keys:
- object-id: a string representing the object-id
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

\(fn METHOD ARGS &optional INSTANCE)" nil nil)

(autoload 'bug--rpc-rally-handle-error "bug-rpc-rally" "\
Check data returned from Rally for errors

\(fn RESPONSE)" nil nil)

(autoload 'bug--rpc-rally-get-fields "bug-rpc-rally" "\
Return a static list of valid field names for rally

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

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "bug-search" "bug-search.el" (22295 38936 323220
;;;;;;  507000))
;;; Generated autoloads from bug-search.el

(autoload 'bug-stored-bugs "bug-search" "\
Display a stored list of bugs

\(fn LIST-NAME &optional INSTANCE)" t nil)

(autoload 'bug-search "bug-search" "\
Take a search query from the minibuffer and execute it

\(fn QUERY &optional INSTANCE)" t nil)

(autoload 'bug-search-multiple "bug-search" "\
Take multiple details for a search query from the minibuffer in several
prompts and execute them

\(fn &optional INSTANCE)" t nil)

;;;***

;;;### (autoloads nil "bug-search-bz" "bug-search-bz.el" (22295 36975
;;;;;;  808903 722000))
;;; Generated autoloads from bug-search-bz.el

(autoload 'bug--parse-bug-search-query "bug-search-bz" "\
Parse search query from minibuffer for Bugzilla

\(fn QUERY)" nil nil)

;;;***

;;;### (autoloads nil "bug-search-rally" "bug-search-rally.el" (22295
;;;;;;  36976 849891 709000))
;;; Generated autoloads from bug-search-rally.el

(autoload 'bug--do-rally-search "bug-search-rally" "\
Execute a search query in Rally

This function takes either a query string in Rallys query string syntax,
or an alist as documented for bug--rpc-rally.

When providing just the query string additional options (like fetch, order,
pagesize, ...) can't be supplied:

 (bug--do-rally-search \"( FormattedID = \"US1234\" )\")

\(fn PARAMS &optional INSTANCE METHOD)" nil nil)

(autoload 'bug--parse-rally-search-query "bug-search-rally" "\
Parse search query from minibuffer for rally

\(fn QUERY)" nil nil)

;;;***

;;;### (autoloads nil nil ("bug-comment-mode.el" "bug-common-functions.el"
;;;;;;  "bug-rpc.el" "bug-search-common.el") (22295 39456 391176
;;;;;;  416000))

;;;***

(provide 'bug-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; bug-autoloads.el ends here
