;;; bug-rpc.el --- rpc and BZ instance specific code -*- lexical-binding: t; -*-
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

(require 'cl-lib)
(require 'bug-custom)
(require 'bug-common-functions)
(require 'bug-debug)
(require 'json)

(defun bug--rpc-cookie-header (instance)
  "Insert cookies stored for this particular instance, if any"
  (if (bug--cache-get 'cookies instance)
      (cons "Cookie" (mapconcat 'identity (bug--cache-get 'cookies instance) "; "))
    nil))

(defun bug-rpc (args instance)
  "Send an RPC response to the given (or default) bugtracker instance and return
the parsed response as alist.

The `args' alist usually has at least two elements:
- resource: a string representing the resource to use
- operation: what to do with the resource

If data needs to be sent a `data' element containing an alist with data needs
to be added.

Backends may define additional keys, check the documentation of their RPC
functions for details.
"
   (bug--backend-function "bug--rpc-%s" args instance))

(defun bug--rpc-response-store-cookies (instance)
  "Try to extract cookies from an RPC response, and store them in the cache"
  (bug--debug (concat "saving cookies for instance: " (prin1-to-string instance t) "\n"))
  (save-match-data
    (let ((cookies))
      (goto-char 0)
      (re-search-forward "\n\n" nil t)
      (while (re-search-backward "^Set-Cookie:[[:space:]]*\\([^;]+\\).*$" nil t)
        ;; A cookie header looks like this:
        ;; Set-Cookie: SUBBUCKETID=123;Path=/;Domain=rally1.rallydev.com;Secure;HttpOnly
        ;; the above regex should return `SUBBUCKETID=123'
        (let ((cookie (match-string 1)))
          (set-text-properties 0 (length cookie) nil cookie)
          (push cookie cookies)))
      (bug--cache-put 'cookies cookies instance))))

(defun bug--parse-rpc-response (instance)
  "Parse a JSON response from buffer and return it as alist"
  (bug--debug-log-time "RPC done")
  (goto-char 0)
  (if (re-search-forward "\n\n" nil t)
      (let ((response (json-read-from-string (decode-coding-string (buffer-substring (point) (point-max)) 'utf-8))))
        (bug--backend-function "bug--rpc-%s-handle-error" response instance))
    (error "Failed to parse http response")))

(provide 'bug-rpc)
;;; bug-rpc.el ends here
