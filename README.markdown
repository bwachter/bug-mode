Bugzilla mode
==============

Installing
----------
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

