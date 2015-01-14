;; TODO
;; - Handle instances properly when calling helper functions
;;   Known issues are in
;;   - Commenting on bugs (goes to matching bug on default instance)

(require 'bz-autoloads)

(defvar bz-debug nil
  "Configure debugging to *bz-debug* buffer")

(defvar bz-default-instance
  "The default bugzilla to use")

(defvar bz-instance-plist nil
  "A list of bugzilla instances to use.

Example:
'(:work (:url \"https://work.example.com\")
  :fun  (:url \"https://fun.example.com\" :login \"username\" :password \"password\"))
")

(defvar bugzilla-columns '("id" "status" "summary" "last_change_time")
  "Default columns in search output")

(defmacro bz-debug (body)
  `(if (and (boundp 'bz-debug) bz-debug)
       (let ((str ,body))
         (with-current-buffer (get-buffer-create "*bz-debug*")
           (goto-char (point-max))
           (insert str)
           (insert "\n")))))

(provide 'bz-mode)
