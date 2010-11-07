# Bugzilla mode #

## Requirements ##
* [json.el](http://cvs.savannah.gnu.org/viewvc/*checkout*/emacs/lisp/json.el?root=emacs)
* Recent release of bugzilla, probably at least 3.6
* JSON-RPC mode enabled for bugzilla

## Installing ##
* Copy bz-mode.el to somewhere on your emacs load path
* Edit your ~/.emacs or ~/.emacs.d/init.el:

        ;; uncomment this to explicitly add the path to your load path:
        ;; (add-to-list 'load-path "~/.emacs.d/bz-mode/")
        (require 'bz-mode)
        
        ;; settings
        ;; debug mode?
        (setq bz-debug nil)
        ;; base url
        (setq bz-url "http://127.0.0.1/bugzilla3")
        ;; username
        (setq bz-username "henrik@localhost.localdomain")
        ;; password
        (setq bz-password "qwerty")
        ;; columns to show in list mode
        (setq bugzilla-columns '("id" "status" "summary"))

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
* u - execute query again
* c - add comment
* q - kill buffer
### bz-comment-mode ###
* C-c C-c - commit comment
