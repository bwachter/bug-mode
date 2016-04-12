# Bugzilla mode #

A mode to interface with Bugzilla and other bugtracking systems from within Emacs. Currently somewhat working bugtrackers are:

- [Bugzilla](https://www.bugzilla.org/)
- [Rally](https://www.rallydev.com/)

## Requirements ##
* [json.el](http://cvs.savannah.gnu.org/viewvc/*checkout*/emacs/lisp/json.el?root=emacs)
* Recent release of bugzilla, probably at least 3.6
* JSON-RPC mode enabled for bugzilla

## Installing ##
* call `make` to generate autoloads
* Edit your ~/.emacs or ~/.emacs.d/init.el:

        ;; adjust the path to bz-mode
        (add-to-list 'load-path "~/.emacs.d/bz-mode/")
        (require 'bz)

* `M-x customize-group RET bz` and adjust at least the the list of instances and the name of the default instance. The instance plist configuration could look like this:

        (:foo (:url "https://foo.example.com")
         :bar (:url "https://bar.example.com" :authinfo "~/.netrc")
         :rally (:api-key "_yourapikey" :type "rally"))

* Store your credentials in authinfo format in `~/.authinfo`, or specify a database location with the `:authinfo` property
* For using Rally, generate [API keys](https://rally1.rallydev.com/login/accounts/index.html#/keys) if your subscription allows it, and set the `:api-key property`. With no API key configured username/password from authinfo are used as well.

## Functions ##
### bz-login / bz-logout###
Some bugzilla instances require explicit login. Use those functions to login/logout if that's the case for your installation.
### bz-bug ###
Open a single bug (bugzilla only)
### bz-search ###
Search for bugs. The behaviour is different, depending on the bugtracker used.

Bugzilla supports
* free form text query
* key-value (e.g. "component:Test")
* the name of a named search

Rally supports
* free form text query
* complex query in Rally query syntax

Both Bugzilla and Rally directly open a bugview instead of a list if the search term matches a bug identifier, or the search only returned a single result.
### bz-search-multiple ###
Search for bugs with multiple criterias. Same query format as bz-search (bugzilla only)
### bz-stored-bugs ###
Open a list of locally stored bugs

## Keybindings for bz-modes ##
### bz-list-mode ###
* u - execute query again
* RET - show single bug
* q - kill buffer

### bz-bug-mode ###
* RET - open attachment with browse-url function
* b - open bug in default browser
* c - add comment
* d - download attachment with w3m-download
* r - remember the bug in a locally stored list
* u - execute query again
* q - kill buffer

### bz-comment-mode ###
* C-c C-c - commit comment
