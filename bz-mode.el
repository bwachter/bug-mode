;; TODO
;; - Handle instances properly when calling helper functions
;;   Known issues are in
;;   - Opening bugs from a list (goes to default instance)
;;   - Commenting on bugs (goes to matching bug on default instance)

(require 'url-parse)

;; TODO: convert to autoloads
(require 'bz-auth)
(require 'bz-list-mode)
(require 'bz-comment-mode)
(require 'bz-bug-mode)
(require 'bz-rpc)

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

(setq bz-field-cache nil)

(defmacro bz-debug (body)
  `(if (and (boundp 'bz-debug) bz-debug)
       (let ((str ,body))
         (with-current-buffer (get-buffer-create "*bz-debug*")
           (goto-char (point-max))
           (insert str)
           (insert "\n")))))

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

(defun bz-insert-hr ()
  (insert "\n")
  (insert-char ?- (floor (/ (window-width) 1.5)))
  (insert "\n"))

(defun bz-handle-search-response (query response &optional instance)
  (if (and
       (assoc 'result response)
       (assoc 'bugs (assoc 'result response)))
      (let ((bugs (cdr (assoc 'bugs (assoc 'result response)))))
        (if (= (length bugs) 0)
            (message "No results")
          (if (= (length bugs) 1)
              (bz-bug-show query (aref bugs 0) instance)
            (bz-list-show query bugs instance))))
    response))

;; take hash table as params. todo: figure out format
(defun bz-do-search (params &optional instance)
  (bz-handle-search-response params (bz-rpc "Bug.search" params instance) instance))

(defun bz-search (query &optional instance)
  (interactive
   (if current-prefix-arg
       (list
        (read-string "Search query: " nil nil t)
        (bz-query-instance))
     (list (read-string "Search query: " nil nil t))))
  (bz-do-search `(,(bz-parse-query query)) instance))

(defun bz-update (id fields &optional instance)
  (message (format "fields: %s" (append fields `((ids . ,id)))))
  (bz-rpc "Bug.update" (append fields `((ids . ,id))) instance))

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
