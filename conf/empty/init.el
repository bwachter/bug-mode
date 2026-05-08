;; init.el --- minimal debug config for bug-mode  -*- lexical-binding: t; -*-
;;
;; Use this to start a clean Emacs instance for debugging bug-mode.
;;
;;   emacs --init-directory=conf/empty
;;
;;; Code:

(load-file
 (expand-file-name "../../bug.el"
                   (file-name-directory
                    (or load-file-name default-directory))))

(global-set-key "\C-cb" 'bug-menu)

(message "bug-mode loaded with empty configuration")

;;; init.el ends here
