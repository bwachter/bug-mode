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

(load-file
 (expand-file-name "../../bug.el"
                   (file-name-directory
                    (or load-file-name default-directory))))

;; Sample instance configurations (replace with real credentials to test)
(setq bug-instance-plist
      '(:github-demo
        (:type github
               :api-key-file "~/.config/bug-mode/github-key.gpg")
        :gitlab-demo
        (:type gitlab
               :api-key-file "~/.config/bug-mode/gitlab-key.gpg")
        :rally-demo
        (:type rally
               :api-key-file "~/.config/bug-mode/rally-key.gpg")
        :bugzilla-demo
        (:type bz-rpc
               :url "https://bugzilla.example.com"
               :api-key-file "~/.config/bug-mode/bz-key.gpg")))

(setq bug-default-instance :github-demo)

(global-set-key "\C-cb" 'bug-menu)

(message "bug-mode loaded with preconfigured demo instances")

;;; init.el ends here
