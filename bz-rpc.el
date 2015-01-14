;;; bz-rpc.el --- rpc and BZ instance specific code
;;
;; Copyright (c) 2010-2015 bz-mode developers
;;
;; See the AUTHORS.md file for a full list:
;; https://raw.githubusercontent.com/bwachter/bz-mode/master/AUTHORS.md
;;
;; Keywords: tools
;;
;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
;; for more details. http://www.gnu.org/copyleft/gpl.html
;;
;;; History:
;;
;; This file is maintained at https://github.com/bwachter/bz-mode/
;; Check the git history for details.
;;
;;; Code:

(defun bz-instance-to-symbolp (instance)
  "Make sure that the instance handle is symbolp; returns default instance
if instance is nil"
  (let* ( ; check if instance already is correct type, if not, check if it starts with :
          ; if it does, just convert, otherwise prepend : and assume all is fine now
          ; bz-default-instance is always assumed to be correct
         (instance (if instance
                       (cond ((symbolp instance) instance)
                             ((string-match "^:" instance) (intern instance))
                             (t (intern (concat ":" instance))))
                     bz-default-instance)))
    instance))

(defun bz-instance-property (property &optional instance)
  "Return the value for a PROPERTY of the instance INSTANCE, or the default
instance if INSTANCE is empty"
  (let* ((instance(bz-instance-to-symbolp instance))
         (property-list (plist-get bz-instance-plist instance)))
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
  "Send an RPC response to the given (or default) Bugzilla instance and return the
parsed response as alist"
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
  "Parse a JSON response from buffer and return it as alist"
  (goto-char 0)
  (if (re-search-forward "\n\n" nil t)
      (let ((response (json-read-from-string (decode-coding-string (buffer-substring (point) (point-max)) 'utf-8))))
        (if (and (assoc 'error response) (assoc 'message (assoc 'error response)))
            (error (cdr (assoc 'message (assoc 'error response)))))
        response)
    (error "Failed to parse http response")))

(defun bz-get-fields (&optional instance)
  "Download fields used by this Bugzilla instance or returns them from cache"
  (let* ((instance (bz-instance-to-symbolp instance))
         (fields (if (plist-get bz-field-cache instance) nil (bz-rpc "Bug.fields" '() instance)))
         (field-hash (make-hash-table :test 'equal)))
    (if fields
        (progn
          (mapcar (lambda (field)
                    (let ((key (cdr (assoc 'name field))))
                      (puthash key field field-hash)))
                  (cdr (car (cdr (car fields)))))
          (setq bz-field-cache (plist-put bz-field-cache instance field-hash))
          ))
    (plist-get bz-field-cache instance)))

(provide 'bz-rpc)
;;; bz-rpc.el ends here
