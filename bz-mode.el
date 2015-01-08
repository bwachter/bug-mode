;; TODO
;; - Handle instances properly when calling helper functions
;;   Known issues are in
;;   - Opening bugs from a list (goes to default instance)
;;   - Commenting on bugs (goes to matching bug on default instance)

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

(require 'generic-x)

(defun bz-list-setup-keymap ()
  ;; There has to be a better way..
  (local-set-key (kbd "RET") (lambda ()
                               (interactive)
                               (save-excursion
                                 (move-beginning-of-line nil)
                                 (if (re-search-forward "^\\([0-9]+\\)" nil t)
                                     (bz-get (match-string 1))
                                   (error "WTF? No id in beginning?")))))
  (local-set-key "u" (lambda ()
                       (interactive)
                       (bz-do-search bz-query)))
  (local-set-key "q" (lambda ()
                       (interactive)
                       (kill-buffer (current-buffer)))))

(defun bz-find-attachment-url (&optional instance)
  (save-excursion
    (let ((end (re-search-forward "$" nil t)))
      (move-beginning-of-line nil)
      ;; FIXME: breaks if ; in filenames/descriptions.. heh
      (if (re-search-forward "^attachment \\([0-9]+\\): \\([^;]+\\); \\([^;]+\\);" end t)
          (format "%s/attachment.cgi?id=%s" (bz-instance-property :url instance) (match-string 1))
        (error "No attachment near point")))))

(defun bz-single-setup-keymap ()
  ;; There has to be a better way..
  ;; FIXME: send credentials in these calls?
  (local-set-key (kbd "RET") (lambda ()
                               (interactive)
                               (browse-url (bz-find-attachment-url))))
  (local-set-key "d" (lambda ()
                       (interactive)
                       (w3m-download
                        (bz-find-attachment-url)
                        (expand-file-name (concat "~/" (match-string 3))))))
  (local-set-key "c" (lambda ()
                       (interactive)
                       (bz-comment bz-id)))

  (local-set-key "u" (lambda ()
                       (interactive)
                       (bz-get bz-id)))

  (local-set-key "r" (lambda ()
                       (interactive)
                       (let ((resolution (completing-read "resolution: "
                                                          (filter (lambda (x)
                                                                    (> (length x) 0))
                                                                  (mapcar (lambda (x)
                                                                            (cdr (assoc 'name x)))
                                                                          (cdr (assoc 'values (gethash "resolution" bz-fields))))))))
                         (bz-update bz-id `((status . "RESOLVED") (resolution . ,resolution))))
                       (bz-get bz-id)))

  (local-set-key "q" (lambda ()
                       (interactive)
                       (kill-buffer (current-buffer)))))

(defun bz-comment-setup-keymap ()
  ;; There has to be a better way..
  (local-set-key "\C-c\C-c" 'bz-comment-commit))

(define-generic-mode
  'bz-list-mode
  '()
  '()
  '()
  '()
  '(bz-list-setup-keymap)
  "bugzilla list mode")

(define-generic-mode
  'bz-single-mode
  '()
  '()
  '()
  '()
  '(bz-single-setup-keymap)
  "bugzilla single mode")

(define-generic-mode
  'bz-comment-mode
  '()
  '()
  '()
  '()
  '(bz-comment-setup-keymap)
  "bugzilla comment mode")

(defmacro bz-debug (body)
  `(if (and (boundp 'bz-debug) bz-debug)
       (let ((str ,body))
         (with-current-buffer (get-buffer-create "*bz-debug*")
           (goto-char (point-max))
           (insert str)
           (insert "\n")))))

(defun bz-instance-property (property &optional instance)
  "Return the value for a PROPERTY of the instance INSTANCE, or the default
instance if INSTANCE is empty"
  (let* ( ; check if instance already is correct type, if not, check if it starts with :
          ; if it does, just convert, otherwise prepend : and assume all is fine now
          ; bz-default-instance is always assumed to be correct
         (instance (if instance
                       (cond ((symbolp instance) instance)
                             ((string-match "^:" instance) (intern instance))
                             (t (intern (concat ":" instance))))
                     bz-default-instance))
         (property-list (plist-get bz-instance-plist instance))
         )
    (plist-get property-list property)))

(defun bz-query-instance ()
  "Query for a Bugzilla instance, providing completion with the instances configured in
bz-instance-plist. Returns the entered Bugzilla instance. Instance name only needs to be
entered enough to get a match."
  (let ((completions
         (remove-if nil
                    (cl-loop for record in bz-instance-plist collect
                             (unless (listp record)
                               (replace-regexp-in-string "^:" "" (prin1-to-string record)))))))
    (completing-read "Instance: " completions nil t)))

(defun bz-rpc (method args &optional instance)
  (let* ((json-str (json-encode `((method . ,method) (params . [,args]) (id 11))))
         (url (concat (bz-instance-property :url instance) "/jsonrpc.cgi"))
         (url-request-method "POST")
         (tls-program '("openssl s_client -connect %h:%p -ign_eof")) ;; gnutls just hangs.. wtf?
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data json-str))
    (bz-debug (concat "request " url "\n" json-str "\n"))
    (with-current-buffer (url-retrieve-synchronously url)
      (bz-debug (concat "response: \n" (decode-coding-string (buffer-string) 'utf-8)))
      (bz-parse-rpc-response))))

(defun bz-parse-rpc-response ()
  (goto-char 0)
  (if (re-search-forward "\n\n" nil t)
      (let ((response (json-read-from-string (decode-coding-string (buffer-substring (point) (point-max)) 'utf-8))))
        (if (and (assoc 'error response) (assoc 'message (assoc 'error response)))
            (error (cdr (assoc 'message (assoc 'error response)))))
        response)
    (error "Failed to parse http response")))

(defun filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun bz-bug-sort-properties (bug)
  (sort bug
        (lambda (a b)
          (< (position (symbol-name (car a)) bugzilla-columns :test 'string=)
             (position (symbol-name (car b)) bugzilla-columns :test 'string=)))))

(defun bz-bug-format (bug)
  (mapconcat (lambda (property)
               (let ((hw (cdr (assoc (symbol-name (car property)) header-widths))))
                 (format (format "%%-%d.%ds"
                                 hw
                                 hw) (cdr property))))
             bug " "))

(defun bz-bug-filtered-and-sorted-properties (bug)
  (bz-bug-sort-properties (filter (lambda (property) (member (symbol-name (car property)) bugzilla-columns)) bug)))

(defun bz-header-widths (bugs)
  (mapcar* (lambda (x y)
             `(,x . ,y))
           bugzilla-columns
           (reduce (lambda (l1 l2)
                     (mapcar* 'max l1 l2))
                   (mapcar (lambda (bug)
                             (mapcar (lambda (prop) (+ (length (format "%s" (cdr prop))) 5)) bug))
                           bugs))))

(defun ht-to-alist (ht)
  (let (result)
    (maphash (lambda (key val) (setq result (cons `(,key . ,val) result))) ht)
    result))

(defun pretty-kvs (kvs)
  (if (hash-table-p kvs)
      (setq kvs (ht-to-alist kvs)))
  (mapconcat (lambda (kv)
               (format "%s: %s" (car kv) (cdr kv)))
             kvs ", "))

(defun bz-show-list (query parsed)
  (switch-to-buffer (format "*bugzilla results: %s*" (pretty-kvs query)))
  (bz-list-mode)
  (make-local-variable 'bz-query)
  (setq bz-query query)
  (setq buffer-read-only nil)
  (erase-buffer)
  (let* ((bugs (mapcar 'bz-bug-filtered-and-sorted-properties parsed)))
    (let* ((headers bugzilla-columns)
           (header-widths (bz-header-widths bugs))
           (header-item-length (/ (window-width) (length headers))))
      (setq header-line-format
            (let ((column 0)
                  (header '()))
              (mapconcat (lambda (heading)
                           (let ((result (concat
                                          (propertize " " 'display (list 'space :align-to column)
                                                      'face 'fixed-pitch)
                                          heading)))
                             (setq column (+ column (cdr (assoc heading header-widths)) 1))
                             result))
                         headers "")))
      (insert (mapconcat 'bz-bug-format bugs "\n")))
    (goto-char 0)
    (setq buffer-read-only t)))

(defun bz-json-response-from-buffer (buffer)
  (with-current-buffer (get-buffer buffer)
    (bz-parse-rpc-response)))

(defun bz-insert-hr ()
  (insert "\n")
  (insert-char ?- (floor (/ (window-width) 1.5)))
  (insert "\n"))

(defun bz-show-bug (id bug)
  (switch-to-buffer (format "*bugzilla bug: %s*" (cdr (assoc 'id bug))))
  (bz-single-mode)
  (make-local-variable 'bz-id)
  (setq bz-id id)
  (make-local-variable 'bz-bug)
  (setq bz-bug bug)
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert (mapconcat (lambda (prop)
                       (format "%s: %s"
                               (or (cdr (assoc 'display_name (gethash (symbol-name (car prop)) bz-fields)))
                                   (car prop))
                               (cdr prop)))
                     (filter (lambda (prop)
                               (not (string= (car prop) "internals"))) bug) "\n"))
  (bz-insert-hr)
  (insert "\nATTACHMENTS:\n")
  (bz-insert-hr)
  (insert "\nCOMMENTS:\n")
  (goto-char 0)
  (setq buffer-read-only t))

(defun bz-handle-search-response (query response)
  (if (and
       (assoc 'result response)
       (assoc 'bugs (assoc 'result response)))
      (let ((bugs (cdr (assoc 'bugs (assoc 'result response)))))
        (if (= (length bugs) 0)
            (message "No results")
          (if (= (length bugs) 1)
              (bz-show-bug query (aref bugs 0))
            (bz-show-list query bugs))))
    response))


(defun bz-handle-comments-response (id response)
  (if (and
       (assoc 'result response)
       (assoc 'bugs (assoc 'result response)))
      (let* ((bugs (cdr (assoc 'bugs (assoc 'result response))))
             (comments (cdr (cadr (car bugs)))))
        (save-excursion
          (switch-to-buffer (format "*bugzilla bug: %s*" id))
          (setq buffer-read-only nil)
          (goto-char 0)
          (if (re-search-forward "^COMMENTS:$" nil t)
              (progn
                (delete-region (point) (point-max))
                (insert "\n")
                (insert (mapconcat (lambda (comment)
                                     (format "[Comment #%s] %s %s:\n%s"
                                             (cdr (assoc 'count comment))
                                             (cdr (assoc 'time comment))
                                             (cdr (assoc 'creator comment))
                                             (cdr (assoc 'text comment))))
                                   comments "\n\n"))
                (setq buffer-read-only t))
            (error "Could not find area for comments in buffer"))))))

(defun bz-handle-attachments-response (id response)
  (if (and
       (assoc 'result response)
       (assoc 'bugs (assoc 'result response)))
      (let* ((bugs (cdr (assoc 'bugs (assoc 'result response))))
             (attachments (cdr (car bugs))))
        (save-excursion
          (switch-to-buffer (format "*bugzilla bug: %s*" id))
          (setq buffer-read-only nil)
          (goto-char 0)
          (if (re-search-forward "^ATTACHMENTS:$" nil t)
              (progn
                (insert "\n")
                (insert (mapconcat (lambda (attachment)
                                     (format "attachment %s: %s; %s; %s"
                                             (cdr (assoc 'id attachment))
                                             (cdr (assoc 'description attachment))
                                             (cdr (assoc 'file_name attachment))
                                             (cdr (assoc 'content_type attachment))))
                                   attachments "\n"))
                (setq buffer-read-only t))
            (error "Could not find area for attachments in buffer"))))))

(defun bz-login (&optional instance)
  (interactive
   (if current-prefix-arg
       (list (bz-query-instance))))
  (bz-rpc "User.login" `((login . ,(bz-instance-property :login instance))
                         (password . ,(bz-instance-property :password instance))
                         (remember . t)) instance)
  (setq bz-fields (make-hash-table :test 'equal))
  (let ((fields (bz-rpc "Bug.fields" '() instance)))
    (mapcar (lambda (field)
              (let ((key (cdr (assoc 'name field))))
                (puthash key field bz-fields)))
            (cdr (car (cdr (car fields))))))
  (message "Login successful"))

(defun bz-logout (&optional instance)
  (interactive
   (if current-prefix-arg
       (list (bz-query-instance))))
  (bz-rpc "User.logout" '() instance))

;; take hash table as params. todo: figure out format
(defun bz-do-search (params &optional instance)
  (bz-handle-search-response params (bz-rpc "Bug.search" params instance)))

(defun bz-search (query &optional instance)
  (interactive
   (if current-prefix-arg
       (list
        (read-string "Search query: " nil nil t)
        (bz-query-instance))
     (list (read-string "Search query: " nil nil t))))
  (bz-do-search `(,(bz-parse-query query)) instance))

(defun bz-get (id &optional instance)
  (interactive
   (if current-prefix-arg
       (list
        (read-string "Bug ID: " nil nil t)
        (bz-query-instance))
     (list (read-string "Bug ID: " nil nil t))))
  (bz-handle-search-response id (bz-rpc "Bug.get" `(("ids" . ,id)) instance))
  (bz-get-attachments id)
  (bz-get-comments id))

(defun bz-update (id fields &optional instance)
  (message (format "fields: %s" (append fields `((ids . ,id)))))
  (bz-rpc "Bug.update" (append fields `((ids . ,id))) instance))

;; TODO: make sure that a comment gets comitted to the bug on the right instance
(let ((fields '((status . "RESOLVED") (resolution . "FIXED"))))
  (append fields '((id . "123"))))
(defun bz-comment-commit (&optional instance)
  (interactive
   (if current-prefix-arg
       (list (bz-query-instance))))
  (if (not (string= major-mode "bz-comment-mode"))
      (error "not visisting a bugzilla comment buffer"))
  (let ((params (make-hash-table :test 'equal)))
    (puthash "id" bz-id params)
    (save-excursion
      (goto-char 0)
      (while (re-search-forward "^\\([^:]*\\): ?\\(.*\\)$" nil t)
        (puthash (match-string 1) (match-string 2) params))
      (re-search-forward "^[^\n]" nil t)
      (move-beginning-of-line nil)
      (puthash "comment" (buffer-substring (point) (point-max)) params)
      (let ((result (bz-rpc "Bug.add_comment" params instance)))
        (message (format "comment id: %s" (cdr (cadr (car result)))))
        (kill-buffer (current-buffer))))
    (bz-get bz-id)))

;; TODO: send to the right instance
(defun bz-comment (id)
  (interactive "nid:")
  (switch-to-buffer (format "*bugzilla add comment: %s*" id))
  (bz-comment-mode)
  (make-local-variable 'bz-id)
  (setq bz-id id)
  (erase-buffer)
  ;;(insert "is_private: false\n")
  (insert "hours_worked: 0.0\n\n")
  (goto-char (point-max)))


(defun bz-get-comments (id &optional instance)
  (bz-handle-comments-response id (bz-rpc "Bug.comments" `(("ids" . ,id)) instance)))

(defun bz-get-attachments (id &optional instance)
  (bz-handle-attachments-response id (bz-rpc "Bug.attachments" `(("ids" . ,id)) instance)))

(defun bz-search-multiple (&optional instance)
  (interactive
   (if current-prefix-arg
       (list (bz-query-instance))))
  (let ((terms (make-hash-table :test 'equal))
        (term nil))
    (while (not (string= term ""))
      (setq term (read-from-minibuffer "query term: "))
      (if (not (string= term ""))
          (let* ((parsed (bz-parse-query term))
                 (key (car parsed))
                 (value (cdr parsed))
                 (current (gethash key terms)))
            (if current
                (if (vectorp current)
                    (puthash key (vconcat current (vector value)) terms)
                  (puthash key (vector current value) terms))
              (puthash key value terms)))))
    (bz-do-search terms instance)))

(defun bz-parse-query (query)
  (if (string-match "^\\([^ ]+\\):\\(.+\\)$" query)
      `(,(match-string 1 query) . ,(match-string 2 query))
    (if (string-match "[:space:]*[0-9]+[:space:]*" query)
        `(id . ,(string-to-number query))
      `(summary . ,query))))

(provide 'bz-mode)
