;;; bug-html-edit.el --- inline org-mode editing for HTML fields -*- lexical-binding: t; -*-
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

(require 'bug-common-functions)
(require 'bug-format)
(require 'bug-vars)
(require 'ox)

;;;;;;
;; HTML <> org-mode conversion

(defun bug--html-to-org (html)
  "Convert HTML string to org-mode markup.

Uses pandoc if available, then libxml, then strips tags as a last resort."
  (or (when (executable-find "pandoc")
        (with-temp-buffer
          (insert (or html ""))
          (when (zerop (call-process-region (point-min) (point-max)
                                            "pandoc" t t nil
                                            "-f" "html" "-t" "org" "--no-highlight"))
            (string-trim (buffer-string)))))
      (bug--html-to-org-libxml (or html ""))))

(defun bug--html-to-org-libxml (html)
  "Convert HTML to org using Emacs's built-in libxml parser."
  (if (fboundp 'libxml-parse-html-region)
      (with-temp-buffer
        (insert html)
        (string-trim
         (bug--libxml-node-to-org
          (libxml-parse-html-region (point-min) (point-max)))))
    (replace-regexp-in-string "<[^>]+>" "" html)))

(defun bug--libxml-node-to-org (node)
  "Recursively convert a libxml parse tree NODE to org-mode markup."
  (cond
   ((null node) "")
   ((stringp node) node)
   ((listp node)
    (let* ((tag (car node))
           (attrs (cadr node))
           (children (cddr node))
           ;; Recursively process children first
           (content (mapconcat #'bug--libxml-node-to-org children "")))
      (cond
       ;; Block elements: ensure they have breathing room
       ((memq tag '(p div))    (concat "\n" (string-trim content) "\n"))
       ((memq tag '(h1 h2 h3 h4 h5 h6))
        (let ((level (string-to-number (substring (symbol-name tag) 1))))
          (format "\n%s %s\n" (make-string level ?*) (string-trim content))))
       ;; Inline elements: don't trim content, or you'll lose spaces between tags
       ((memq tag '(b strong)) (format "*%s*" content))
       ((memq tag '(i em))     (format "/%s/" content))
       ((eq tag 'u)            (format "_%s_" content))
       ((eq tag 'code)         (format "~%s~" content))
       ;; Lists
       ((eq tag 'li)           (format "- %s\n" (string-trim content)))
       ;; Links: extract href from the attrs list
       ((eq tag 'a)
        (let ((href (cdr (assoc 'href attrs))))
          (if href
              (format "[[%s][%s]]" href (string-trim content))
            content)))
       ;; Br
       ((eq tag 'br)           "\n")
       ;; Default: just pass content through (handles <html>, <body>, etc.)
       (t content))))))

(defun bug--org-to-html (org)
  "Convert org-mode markup string to HTML using ox-html."
  (require 'ox-html)
  (let ((org-export-with-toc nil)
        (org-export-with-section-numbers nil))
    (string-trim
     (org-export-string-as (or org "") 'html t '(:body-only t :with-toc nil :section-numbers nil)))))

;;;;;;
;; Edit buffer

(defvar-local bug--html-edit-field-name nil)
(defvar-local bug--html-edit-source-buffer nil)
(defvar-local bug--html-edit-field-pos nil)
(defvar-local bug--html-edit-old-value nil)

(defun bug--bug-mode-open-html-editor (field-name field-pos old-html)
  "Open a split org-mode buffer to edit the HTML field `field-name'.

`field-pos' is the field value position in the current bug buffer.
`old-html' is the raw HTML currently stored for the field."
  (let* ((source-buf (current-buffer))
         (org-content (or (bug--html-to-org old-html) ""))
         (buf (get-buffer-create (format "*bug-edit %s*" field-name))))
    (with-current-buffer buf
      (erase-buffer)
      (insert (string-trim org-content) "\n")
      (org-mode)
      (setq-local bug--html-edit-field-name field-name)
      (setq-local bug--html-edit-source-buffer source-buf)
      (setq-local bug--html-edit-field-pos field-pos)
      (setq-local bug--html-edit-old-value old-html)
      (local-set-key (kbd "C-c C-c") #'bug--html-edit-commit)
      (local-set-key (kbd "C-c C-k") #'bug--html-edit-abort)
      (message "Edit field — C-c C-c to commit, C-c C-k to abort"))
    (pop-to-buffer buf)))

(defun bug--html-edit-commit ()
  "Convert the org buffer back to HTML and apply it to the source bug buffer."
  (interactive)
  (let ((new-html   (string-trim (or (bug--org-to-html (buffer-string)) "")))
        (field-name bug--html-edit-field-name)
        (field-pos  bug--html-edit-field-pos)
        (old-html   bug--html-edit-old-value)
        (source-buf bug--html-edit-source-buffer))
    (quit-window t)
    (when (and (buffer-live-p source-buf) (not (string= new-html old-html)))
      (with-current-buffer source-buf
        (setq buffer-read-only nil)
        (let* ((fpos (text-property-any (point-min) (point-max) 'field field-name))
               (fend (when fpos
                       (or (text-property-not-all fpos (point-max) 'field field-name)
                           (point-max)))))
          (when fpos
            (delete-region fpos fend))
          (goto-char (or fpos field-pos))
          (cond
           ((and (eq bug-update-mode 'immediate) (not bug---is-new))
            (condition-case err
                (let ((update-id (bug--get-update-id bug---instance)))
                  (message "Updating field %s..." field-name)
                  (bug-update update-id `((,field-name . ,new-html)) bug---instance)
                  (if (assoc field-name bug---data)
                      (setf (cdr (assoc field-name bug---data)) new-html)
                    (push (cons field-name new-html) bug---data))
                  (insert (propertize (bug--format-field-value
                                       (cons field-name new-html) bug---instance t)
                                      'field field-name))
                  (message "Field %s updated." field-name))
              (error
               (if (assoc field-name bug---data)
                   (setf (cdr (assoc field-name bug---data)) old-html)
                 (push (cons field-name old-html) bug---data))
               (insert (propertize (bug--format-field-value
                                    (cons field-name old-html) bug---instance t)
                                   'field field-name))
               (message "Update failed: %s" (error-message-string err)))))
           (t
            (if (assoc field-name bug---changed-data)
                (setf (cdr (assoc field-name bug---changed-data)) new-html)
              (push (cons field-name new-html) bug---changed-data))
            (if (assoc field-name bug---data)
                (setf (cdr (assoc field-name bug---data)) new-html)
              (push (cons field-name new-html) bug---data))
            (insert (propertize (bug--format-field-value
                                 (cons field-name new-html) bug---instance t)
                                'field field-name))
            (message "Field %s changed (C-c C-c to commit)" field-name)))
          (when (fboundp 'bug--bug-mode-update-header)
            (bug--bug-mode-update-header)))
        (setq buffer-read-only t)))))

(defun bug--html-edit-abort ()
  "Abort the HTML field edit and close the edit buffer."
  (interactive)
  (quit-window t))

(provide 'bug-html-edit)
;;; bug-html-edit.el ends here
