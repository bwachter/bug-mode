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

(setq bz-field-cache nil)

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

(defun bz-rpc (method args &optional instance)
  "Send an RPC response to the given (or default) bugtracker instance and return the
parsed response as alist"
  (let* ((type (bz-instance-property :type instance)))
         (cond
          ((string= type "rally")
           (bz--rpc-rally method args instance))
          (t (bz--rpc-bz method args instance)))))

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
