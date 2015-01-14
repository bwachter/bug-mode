# Bugzilla mode #

## Requirements ##
* [json.el](http://cvs.savannah.gnu.org/viewvc/*checkout*/emacs/lisp/json.el?root=emacs)
* Recent release of bugzilla, probably at least 3.6
* JSON-RPC mode enabled for bugzilla

## Installing ##
* call 'make' to generate autoloads
* Edit your ~/.emacs or ~/.emacs.d/init.el:

        (require 'bz-mode)
        ;; adjust the path to bz-mode
        (add-to-list 'load-path "~/.emacs.d/bz-mode/")

        ;; settings
        ;; debug mode?
        (setq bz-debug t)
        ;; set up two bugzilla instances
        ;; :work instance is either without auth, with auth-data in ~/.authinfo, or
        ;;       behind basic auth with the url-package prompting for credentials
        ;; :fun  instance uses regular bz auth, with credentials stored in ~/.netrc
        ;;       it requires a call to (bz-login "fun") before you can modify bugs
        (setq bz-instance-plist '(:work (:url "https://work-bz.example.com")
                                  :fun  (:url "https://fun-bz.example.com" :authinfo "~/.netrc")))
        ;; per default use the :work instance. All interactive functions use
        ;; prefix argument to query for the instance to use
        (setq bz-default-instance :work)
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
* RET - open attachment with browse-url function
* d - download attachment with w3m-download
* u - execute query again
* c - add comment
* q - kill buffer
### bz-comment-mode ###
* C-c C-c - commit comment
