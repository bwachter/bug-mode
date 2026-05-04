;;; bug-html-edit.el --- org-mode editing for rich-text fields -*- lexical-binding: t; -*-
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
                                            "-f" "html" "-t" "org" "–syntax-highlighting=none"))
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
;; Markdown <> org conversion

(defun bug--markdown-to-org (markdown)
  "Convert Markdown string to org-mode markup.

Uses pandoc if available, otherwise returns the markdown text as-is
(it is largely human-readable without conversion)."
  (or (when (executable-find "pandoc")
        (with-temp-buffer
          (insert (or markdown ""))
          (when (zerop (call-process-region (point-min) (point-max)
                                            "pandoc" t t nil
                                            "-f" "markdown" "-t" "org" "–syntax-highlighting=none"))
            (string-trim (buffer-string)))))
      (or markdown "")))

(defun bug--org-to-markdown (org)
  "Convert org-mode markup string to Markdown.

Uses pandoc if available, otherwise falls back to ox-md."
  (or (when (executable-find "pandoc")
        (with-temp-buffer
          (insert (or org ""))
          (when (zerop (call-process-region (point-min) (point-max)
                                            "pandoc" t t nil
                                            "-f" "org" "-t" "markdown" "–syntax-highlighting=none"))
            (string-trim (buffer-string)))))
      (progn
        (require 'ox-md)
        (let ((org-export-with-toc nil)
              (org-export-with-section-numbers nil))
          (string-trim
           (org-export-string-as (or org "") 'md t
                                 '(:with-toc nil :section-numbers nil)))))))

;;;;;;
;; Edit buffer

(defvar-local bug--html-edit-field-name nil)
(defvar-local bug--html-edit-source-buffer nil)
(defvar-local bug--html-edit-field-pos nil)
(defvar-local bug--html-edit-raw-value nil
  "The immutable original raw field value (HTML or markdown).
Used by revert and as the old-value baseline for commit.")
(defvar-local bug--html-edit-source-format nil
  "Source markup format: `html' or `markdown'.")
(defvar-local bug--html-edit-current-mode nil
  "Current editing view: `org' or `raw'.")
(defvar-local bug--html-edit-cached-org nil
  "Last known org content.  Updated whenever the org view is left (toggle)
or refreshed (revert/derive).")
(defvar-local bug--html-edit-cached-raw nil
  "Last known raw content.  Updated whenever the raw view is left (toggle)
or refreshed (revert/derive).")

;; TODO, entry into the transient with b obviously won't work in this
;;       edit buffer, but we'd need to figure out a better way to guid
;;       the user
(defun bug--html-edit-set-keys ()
  "Bind bug-html-edit keys in the current buffer after a mode change."
  ;; After a major-mode change the local map is often a shared symbol
  ;; (e.g. `org-mode-map').  `local-set-key' on a symbol mutates the
  ;; global map, so we must install a private copy first.
  (unless (keymapp (current-local-map))
    (use-local-map (make-sparse-keymap)))
  (when (symbolp (current-local-map))
    (use-local-map (copy-keymap (current-local-map))))
  (local-set-key (kbd "C-c C-c") #'bug--html-edit-commit)
  (local-set-key (kbd "C-c C-k") #'bug--html-edit-abort)
  (local-set-key (kbd "C-c C-t") #'bug--html-edit-toggle-mode)
  (local-set-key (kbd "C-c C-r") #'bug--html-edit-revert)
  (local-set-key (kbd "C-c C-d") #'bug--html-edit-derive))

(defun bug--html-edit-restore-locals (field-name source-buf field-pos
                                                 raw-value fmt cur-mode
                                                 cached-org cached-raw)
  "Restore all edit-buffer locals after a major-mode change wipes them."
  (setq-local bug--html-edit-field-name    field-name)
  (setq-local bug--html-edit-source-buffer source-buf)
  (setq-local bug--html-edit-field-pos     field-pos)
  (setq-local bug--html-edit-raw-value     raw-value)
  (setq-local bug--html-edit-source-format fmt)
  (setq-local bug--html-edit-current-mode  cur-mode)
  (setq-local bug--html-edit-cached-org    cached-org)
  (setq-local bug--html-edit-cached-raw    cached-raw))

;;;;;;
;; Editor entry point

(defun bug--bug-mode-open-html-editor (field-name field-pos old-value
                                                  &optional source-format)
  "Open a split buffer to edit the rich-text field FIELD-NAME.

FIELD-POS is the field value position in the current bug buffer.
OLD-VALUE is the raw value currently stored for the field.
SOURCE-FORMAT is the markup format of OLD-VALUE: `html' (default)
or `markdown'.

The buffer opens in org-mode converted from the raw value.

Key bindings:
  C-c C-c  commit (submits the active view)
  C-c C-k  abort
  C-c C-t  toggle between org and raw editing
  C-c C-r  revert active view to original content
  C-c C-d  derive active view from the other view's cached content"
  (let* ((fmt        (or source-format 'html))
         (source-buf (current-buffer))
         (org-content (string-trim
                       (or (if (eq fmt 'markdown)
                               (bug--markdown-to-org old-value)
                             (bug--html-to-org old-value))
                           "")))
         (buf (get-buffer-create (format "*bug-edit %s*" field-name))))
    (with-current-buffer buf
      (erase-buffer)
      (insert org-content "\n")
      (org-mode)
      (bug--html-edit-restore-locals field-name source-buf field-pos
                                     old-value fmt 'org
                                     org-content old-value)
      (bug--html-edit-set-keys)
      (message
       "Edit (%s→org) — C-c C-c commit, C-c C-k abort, C-c C-t toggle, C-c C-r revert, C-c C-d derive"
       (symbol-name fmt)))
    (pop-to-buffer buf)))

;;;;;;
;; Toggle

(defun bug--html-edit-toggle-mode ()
  "Toggle between org-mode and raw (HTML/markdown) editing.

Before switching, the current buffer content is saved into the cache for
the active view.  The new view is then generated from that cache.  The
two views are never kept in live sync; each toggle is a one-shot conversion."
  (interactive)
  (let ((field-name bug--html-edit-field-name)
        (source-buf bug--html-edit-source-buffer)
        (field-pos  bug--html-edit-field-pos)
        (raw-value  bug--html-edit-raw-value)
        (fmt        bug--html-edit-source-format)
        (cur-mode   bug--html-edit-current-mode))
    (cond
     ((eq cur-mode 'org)
      (let* ((new-cached-org (string-trim (buffer-string)))
             (new-raw (if (eq fmt 'markdown)
                          (bug--org-to-markdown new-cached-org)
                        (bug--org-to-html new-cached-org))))
        (erase-buffer)
        (insert new-raw)
        (if (and (eq fmt 'markdown) (fboundp 'markdown-mode))
            (markdown-mode)
          (html-mode))
        (bug--html-edit-restore-locals field-name source-buf field-pos
                                       raw-value fmt 'raw
                                       new-cached-org new-raw)
        (bug--html-edit-set-keys)
        (message "Raw %s editing — C-c C-t org, C-c C-r revert, C-c C-d derive, C-c C-c commit"
                 (symbol-name fmt))))
     (t
      (let* ((new-cached-raw (string-trim (buffer-string)))
             (new-org (if (eq fmt 'markdown)
                          (bug--markdown-to-org new-cached-raw)
                        (bug--html-to-org new-cached-raw))))
        (erase-buffer)
        (insert new-org)
        (org-mode)
        (bug--html-edit-restore-locals field-name source-buf field-pos
                                       raw-value fmt 'org
                                       new-org new-cached-raw)
        (bug--html-edit-set-keys)
        (message "Org editing — C-c C-t raw %s, C-c C-r revert, C-c C-d derive, C-c C-c commit"
                 (symbol-name fmt)))))))

;;;;;;
;; Revert and Derive

(defun bug--html-edit-revert ()
  "Revert the active view to the original field content.

In org mode: re-derives org from the original raw value and updates
the org cache.  In raw mode: restores the original raw value directly
and updates the raw cache."
  (interactive)
  (let ((field-name bug--html-edit-field-name)
        (source-buf bug--html-edit-source-buffer)
        (field-pos  bug--html-edit-field-pos)
        (raw-value  bug--html-edit-raw-value)
        (fmt        bug--html-edit-source-format)
        (cur-mode   bug--html-edit-current-mode)
        (cached-org bug--html-edit-cached-org)
        (cached-raw bug--html-edit-cached-raw))
    (cond
     ((eq cur-mode 'org)
      (let ((new-org (string-trim
                      (or (if (eq fmt 'markdown)
                              (bug--markdown-to-org raw-value)
                            (bug--html-to-org raw-value))
                          ""))))
        (erase-buffer)
        (insert new-org "\n")
        (bug--html-edit-restore-locals field-name source-buf field-pos
                                       raw-value fmt 'org
                                       new-org cached-raw)
        (bug--html-edit-set-keys)
        (message "Reverted org view from original %s" (symbol-name fmt))))
     (t
      (erase-buffer)
      (insert raw-value)
      (bug--html-edit-restore-locals field-name source-buf field-pos
                                     raw-value fmt 'raw
                                     cached-org raw-value)
      (bug--html-edit-set-keys)
      (message "Reverted to original %s" (symbol-name fmt))))))

(defun bug--html-edit-derive ()
  "Derive the active view from the other view's cached content.

In org mode: re-converts the cached raw content to org, replacing the
current buffer.  Useful to see how raw edits survive the round-trip
through the markup-to-org converter.

In raw mode: exports the cached org content to HTML/markdown, replacing
the current buffer.  Useful to inspect the output of the org exporter
before committing."
  (interactive)
  (let ((field-name bug--html-edit-field-name)
        (source-buf bug--html-edit-source-buffer)
        (field-pos  bug--html-edit-field-pos)
        (raw-value  bug--html-edit-raw-value)
        (fmt        bug--html-edit-source-format)
        (cur-mode   bug--html-edit-current-mode)
        (cached-org bug--html-edit-cached-org)
        (cached-raw bug--html-edit-cached-raw))
    (cond
     ((eq cur-mode 'org)
      (let ((new-org (string-trim
                      (or (if (eq fmt 'markdown)
                              (bug--markdown-to-org cached-raw)
                            (bug--html-to-org cached-raw))
                          ""))))
        (erase-buffer)
        (insert new-org "\n")
        (bug--html-edit-restore-locals field-name source-buf field-pos
                                       raw-value fmt 'org
                                       new-org cached-raw)
        (bug--html-edit-set-keys)
        (message "Derived org from cached %s" (symbol-name fmt))))
     (t
      (let ((new-raw (if (eq fmt 'markdown)
                         (bug--org-to-markdown cached-org)
                       (bug--org-to-html cached-org))))
        (erase-buffer)
        (insert new-raw)
        (bug--html-edit-restore-locals field-name source-buf field-pos
                                       raw-value fmt 'raw
                                       cached-org new-raw)
        (bug--html-edit-set-keys)
        (message "Derived %s from cached org" (symbol-name fmt)))))))

;;;;;;
;; Commit and Abort

(defun bug--html-edit-commit ()
  "Convert the current buffer view to the source format and apply it.

If the active view is org-mode, it is converted to HTML or markdown as
appropriate.  If the active view is raw, its content is used as-is."
  (interactive)
  (let* ((fmt       bug--html-edit-source-format)
         (cur-mode  bug--html-edit-current-mode)
         (new-value
          (string-trim
           (cond
            ((eq cur-mode 'raw)    (buffer-string))
            ((eq fmt 'markdown)    (bug--org-to-markdown (buffer-string)))
            (t                     (or (bug--org-to-html (buffer-string)) "")))))
         (field-name bug--html-edit-field-name)
         (field-pos  bug--html-edit-field-pos)
         (old-value  bug--html-edit-raw-value)
         (source-buf bug--html-edit-source-buffer))
    (quit-window t)
    (when (and (buffer-live-p source-buf) (not (string= new-value old-value)))
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
                  (bug-update update-id `((,field-name . ,new-value)) bug---instance)
                  (if (assoc field-name bug---data)
                      (setf (cdr (assoc field-name bug---data)) new-value)
                    (push (cons field-name new-value) bug---data))
                  (insert (propertize (bug--format-field-value
                                       (cons field-name new-value) bug---instance t)
                                      'field field-name))
                  (message "Field %s updated." field-name))
              (error
               (if (assoc field-name bug---data)
                   (setf (cdr (assoc field-name bug---data)) old-value)
                 (push (cons field-name old-value) bug---data))
               (insert (propertize (bug--format-field-value
                                    (cons field-name old-value) bug---instance t)
                                   'field field-name))
               (message "Update failed: %s" (error-message-string err)))))
           (t
            (if (assoc field-name bug---changed-data)
                (setf (cdr (assoc field-name bug---changed-data)) new-value)
              (push (cons field-name new-value) bug---changed-data))
            (if (assoc field-name bug---data)
                (setf (cdr (assoc field-name bug---data)) new-value)
              (push (cons field-name new-value) bug---data))
            (insert (propertize (bug--format-field-value
                                 (cons field-name new-value) bug---instance t)
                                'field field-name))
            (message "Field %s changed (C-c C-c to commit)" field-name)))
          (when (fboundp 'bug--bug-mode-update-header)
            (bug--bug-mode-update-header)))
        (setq buffer-read-only t)))))

(defun bug--html-edit-abort ()
  "Abort the rich-text field edit and close the edit buffer."
  (interactive)
  (quit-window t))

(provide 'bug-html-edit)
;;; bug-html-edit.el ends here
