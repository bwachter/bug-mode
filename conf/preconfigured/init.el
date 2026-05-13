;; init.el --- preconfigured debug config for bug-mode  -*- lexical-binding: t; -*-
;;
;; Use this to start an Emacs instance with demo instance configs for
;; quick interactive testing.
;;
;;   emacs --init-directory=conf/preconfigured
;;
;; Replace the placeholder credentials with real ones before use.
;;
;;; Code:

(setq native-comp-jit-compilation nil)

(load-file
 (expand-file-name "../../bug.el"
                   (file-name-directory
                    (or load-file-name default-directory))))

;; Sample instance configurations (replace with real credentials to test)
(setq bug-instances-list
      '(
        (github-demo .
                     (:type github
                            :api-key-file "~/.config/bug-mode/github-key.gpg"))
        (gitlab-demo .
                     (:type gitlab
                            :api-key-file "~/.config/bug-mode/gitlab-key.gpg"))
        (rally-demo .
                    (:type rally
                           :api-key-file "~/.config/bug-mode/rally-key.gpg"))
        (bugzilla-demo-rpc .
                       (:type bz-rpc
                              :url "http://localhost:18080"
                              :api-key-file "~/.config/bug-mode/bz-key.gpg"))
        (bugzilla-demo .
                       (:type bz-rest
                              :url "http://localhost:18080"
                              :api-key-file "~/.config/bug-mode/bz-key.gpg"))))

(global-set-key "\C-cb" 'bug-menu)

(setq bug-debug t)

(setq bug-debug-subsystems '((fields . 2)
			     (edit . 2)
                             (rpc-rally . 1)
                             (search . 1)))

(message "bug-mode loaded with preconfigured demo instances")

;;; init.el ends here
