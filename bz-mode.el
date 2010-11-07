(setq bz-debug t)
(setq bz-url "http://127.0.0.1/bugzilla3")
(setq bz-username "henrik@localhost.localdomain")
(setq bz-password "qwerty")
(setq bugzilla-columns '("id" "status" "summary"))

(require 'generic-x)

(defun bz-list-setup-keymap ()
  ;; There has to be a better way..
  (local-set-key (kbd "RET") (lambda ()
                               (interactive)
                               (save-excursion
                                 (move-beginning-of-line nil)
                                 (if (re-search-forward "^[0-9]+" nil t)
                                     (bz-get (buffer-substring (match-beginning 0) (match-end 0)))
                                   (error "WTF? No id in beginning?")))))
  (local-set-key "u" (lambda ()
                       (interactive)
                       (bz-do-search bz-query)))
  (local-set-key "q" (lambda ()
                       (interactive)
                       (kill-buffer (current-buffer)))))

(defun bz-single-setup-keymap ()
  ;; There has to be a better way..
  (local-set-key "c" (lambda ()
                       (interactive)
                       (bz-comment bz-id)))
  (local-set-key "u" (lambda ()
                       (interactive)
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

(defun bz-rpc (method args)
  (let* ((json-str (json-encode `((method . ,method) (params . [,args]) (id 1))))
         (url (concat bz-url "/jsonrpc.cgi"))
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
  (insert-char ?= (window-width))
  (insert "\n"))

(defun bz-show-bug (id bug)
  (switch-to-buffer (format "*bugzilla bug: %s*" (cdr (assoc 'id bug))))
  (bz-single-mode)
  (make-local-variable 'bz-id)
  (setq bz-id id)
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert (mapconcat (lambda (prop)
                       (format "%s: %s" (car prop) (cdr prop)))
                     bug "\n"))
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
                                     (message (format "%s" comment))
                                     (format "%s %s:\n%s"
                                             (cdr (assoc 'time comment))
                                             (cdr (assoc 'author comment))
                                             (cdr (assoc 'text comment))))
                                   comments "\n\n"))
                (setq buffer-read-only t))
            (error "Could not find area for comments in buffer"))))))

(defun bz-login ()
  (interactive)
  (bz-rpc "User.login" `((login . ,bz-username) (password . ,bz-password) (remember . t)))
  "Login successful")

(defun bz-do-search (params)
  (bz-handle-search-response params (bz-rpc "Bug.search" params)))

(defun bz-search (query)
  (interactive "squery: ")
  (bz-do-search `(,(bz-parse-query query))))

(defun bz-get (id)
  (interactive "nid:")
  (bz-handle-search-response id (bz-rpc "Bug.get" `(("ids" . ,id))))
  (bz-get-comments id))

(defun bz-comment-commit ()
  (interactive)
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
      (let ((result (bz-rpc "Bug.add_comment" params)))
        (message (format "comment id: %s" (cdr (cadr (car result)))))
        (kill-buffer (current-buffer))))))
  
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


(defun bz-get-comments (id)
  (bz-handle-comments-response id (bz-rpc "Bug.comments" `(("ids" . ,id)))))

(defun bz-search-multiple ()
  (interactive)
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
    (bz-do-search terms)))

(defun bz-parse-query (query)
  (if (string-match "^\\([^ ]+\\):\\(.+\\)$" query)
      `(,(match-string 1 query) . ,(match-string 2 query))
    (if (string-match "[:space:]*[0-9]+[:space:]*" query)
        `(id . ,(string-to-number query))
      `(summary . ,query))))


(provide 'bz-mode)
