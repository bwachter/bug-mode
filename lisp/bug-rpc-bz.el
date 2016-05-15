;; bug-rpc-bz.el --- RPC functions for Bugzilla
;;
;; Copyright (c) 2010-2015 bug-mode developers
;;
;; See the AUTHORS.md file for a full list:
;; https://raw.githubusercontent.com/bwachter/bug-mode/master/AUTHORS.md
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
;; This file is maintained at https://github.com/bwachter/bug-mode/
;; Check the git history for details.
;;
;;; Code:

;;;###autoload
(defun bug--rpc-bz (args &optional instance)
  "Send an RPC response to the given (or default) Bugzilla instance and return the
parsed response as alist"
  (let* ((method (concat (cdr (assoc 'resource args)) "."
                         (cdr (assoc 'operation args))))
         (post-data (cdr (assoc 'post-data args)))
         (json-str (json-encode `((method . ,method) (params . [,post-data]) (id 11))))
         (url (concat (bug--instance-property :url instance) "/jsonrpc.cgi"))
         (url-request-method "POST")
         (tls-program '("openssl s_client -connect %h:%p -ign_eof")) ;; gnutls just hangs.. wtf?
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data json-str))
    (bug--debug (concat "request " url "\n" json-str "\n"))
    (with-current-buffer (url-retrieve-synchronously url)
      (bug--debug (concat "response: \n" (decode-coding-string (buffer-string) 'utf-8)))
      (bug--parse-rpc-response instance))))

;;;###autoload
(defun bug--rpc-bz-handle-error (response instance)
  "Check data returned from Bugzilla for errors"
  (if (and (assoc 'error response) (assoc 'message (assoc 'error response)))
      (error (cdr (assoc 'message (assoc 'error response)))))
  response)

;;;###autoload
(defun bug--rpc-bz-get-fields (&optional object instance)
  "Download the field list for Bugzilla"
  (bug-rpc '((resource . "Bug")
                          (operation . "fields")) instance))

;;;###autoload
(defun bug--rpc-bz-map-field (field-name)
  "Try to guess what the definition of a field in a bug is by
either throwing away ^bug_ or looking up the key in a list.

It seems that about half of the fields in Bugzillas field query
don't match the fields found in a bug."
  (if (string-match "^bug_" field-name)
      (replace-regexp-in-string "^bug_" "" field-name)
    (cond ((string= field-name "summary")
           "short-desc"))))

;;;###autoload
(defun bug--bz-field-name (field-name &optional instance)
  "Resolve field names for Bugzilla"
  (cond ((equal :bug-uuid field-name)
         'id)
        ((equal :bug-friendly-id field-name)
         'id)
        ((equal :bug-summary field-name)
         'summary)))

(provide 'bug-rpc-bz)
;;; bug-rpc-bz.el ends here
