# Bugzilla mode #

## Requirements ##
* [json.el](http://cvs.savannah.gnu.org/viewvc/*checkout*/emacs/lisp/json.el?root=emacs)
* Recent release of bugzilla, probably at least 3.6
* JSON-RPC mode enabled for bugzilla

## Installing ##
* call 'make' to generate autoloads
* Edit your ~/.emacs or ~/.emacs.d/init.el:

        ;; adjust the path to bz-mode
        (add-to-list 'load-path "~/.emacs.d/bz-mode/")
        (require 'bz)

* M-x customize-group RET bz and adjust at least the the list of instances and the name of the default instance

## Functions ##
### bz-login ###
Use this to login.
### bz-search ###
Search for bugs. Query can be either free form text, key-value (e.g. "component:Test") or the name of a named search.
### bz-search-multiple ###
Search for bugs with multiple criterias. Same query format as bz-search ###

## Modes/vies ##
### bz-list-mode ###
* u - execute query again
* RET - show single bug
* q - kill buffer
### bz-single-mode ###
* RET - open attachment with browse-url function
* d - download attachment with w3m-download
* u - execute query again
* c - add comment
* q - kill buffer
### bz-comment-mode ###
* C-c C-c - commit comment
