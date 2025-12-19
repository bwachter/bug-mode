;;; bug-mode.el --- display a single bug -*- lexical-binding: t; -*-
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

(require 'bug-rpc)
(require 'bug-common-functions)
(require 'bug-format)
(require 'bug-debug)
(require 'bug-persistent-data)

(defvar bug---id)
(defvar bug---uuid)
(defvar bug---is-new)
(defvar bug---data)
(defvar bug---instance)
(defvar bug---changed-data)

(defvar bug-mode-map
  (let ((keymap (copy-keymap special-mode-map)))
    (define-key keymap (kbd "RET") 'bug--bug-mode-open-thing-near-point)
    (define-key keymap "b"         'bug--bug-mode-browse-bug)
    ;; TODO: change this to a 'change bug' popup
    (define-key keymap "c"         'bug--bug-mode-create-comment)
    (define-key keymap "d"         'bug--bug-mode-download-attachment)
    (define-key keymap "e"         'bug--bug-mode-edit-thing-near-point)
    (define-key keymap "i"         'bug--bug-mode-info)
    (define-key keymap "r"         'bug--bug-mode-remember-bug)
    ;; TODO: this should change to 'status change' instead of 'resolve'
    (define-key keymap "s"         'bug--bug-mode-resolve-bug)
    (define-key keymap "u"         'bug--bug-mode-update-bug)
    (define-key keymap "q"         'bug--bug-mode-quit-window)
    (define-key keymap "\C-c\C-c"  'bug--bug-mode-commit)
    keymap)
  "Keymap for BZ bug mode")

(define-derived-mode bug-mode special-mode "Bug"
  "Show a single bug"
  )

;;;###autoload
(defun bug-open (id &optional instance)
  "Retrieve and show a single bug"
  (interactive
   (if current-prefix-arg
       (nreverse (list
                  (bug--query-instance)
                  (read-string "Bug ID: " nil nil t)))
     (list (read-string "Bug ID: " nil nil t))))
  (let* ((bug-content (bug--backend-function "bug--fetch-%s-bug" id instance)))
    (if bug-content (bug-show bug-content instance))))

(defun bug--buffer-string (bug-id instance)
  "Return a buffer name string for bug-id/instance combination"
  (format "*%s bug: %s*"
          (prin1-to-string (bug--backend-type instance) t)
          bug-id))

(defun bug-show (bug instance)
  "Display an existing bug buffer in bug-mode"
  (bug--debug-log-time "bug-show")
  (let ((tmp-bug-id (cdr (assoc (bug--field-name :bug-friendly-id instance) bug))))
    (switch-to-buffer (bug--buffer-string tmp-bug-id instance))

    (bug-mode)
    ;; the tmp-bug-id bit is needed as setting the mode clears buffer-local variables
    (make-local-variable 'bug---id)
    (setq bug---id tmp-bug-id)
    (make-local-variable 'bug---uuid)
    (setq bug---uuid (cdr (assoc (bug--field-name :bug-uuid instance) bug)))
    (make-local-variable 'bug---is-new)
    (setq bug---is-new (if bug---id nil t))
    (setq bug (sort bug (lambda (a b)(string< (car a)(car b)))))
    (make-local-variable 'bug---data)
    (setq bug---data bug)
    (make-local-variable 'bug---instance)
    (setq bug---instance (bug--instance-to-symbolp instance))
    (make-local-variable 'bug---changed-data)
    (setq bug---changed-data nil)
    (setq buffer-read-only nil)
    (buffer-disable-undo)
    (erase-buffer)
    (insert
     (mapconcat
      (lambda (prop)
        (concat
         (bug--format-field-name prop instance)
         (propertize
          (bug--format-field-value prop instance t)
          'field (car prop))))
      (filter (lambda (prop)
                (and (not (equal :json-false (bug--get-field-property (car prop) 'is_visible instance)))
                     ;; referenced objects are included as a list. If there's
                     ;; a `Count' property with value `0' it's safe to assume
                     ;; we don't need to retrieve it (might be rally only)
                     (not (and
                           (listp (cdr prop))
                           (equal 0 (cdr (assoc 'Count (cdr prop))))))
                     (not (equal (cdr prop) nil))
                     (not (string-match "^[[:space:]]*$" (prin1-to-string (cdr prop) t)))
                     (not (string= (car prop) "internals")))) bug) "\n"))

    (unless (equal 'rally (bug--backend-type instance))
      ;; TODO: Rally has multiple objects which need to be loaded separately,
      ;;       the bugzilla style loading of attachements and comments won't
      ;;       scale for that.
      ;;       Additionally it'd be better to select the insertion points by
      ;;       using text properties.
      (insert "\nATTACHMENTS:\n")
      (insert "\nCOMMENTS:\n")
      (if (and bug---id bug-autoload-attachments)
          (bug-get-attachments bug---id instance))
      (if (and bug---id bug-autoload-comments)
          (bug-get-comments bug---id instance)))
    (goto-char 0)
    (setq buffer-read-only t)
    (bug--bug-mode-update-header)
    (buffer-enable-undo)
    (bug--debug-log-time "stop")))

(defun bug--bug-mode-update-header ()
  "Update the buffers headerline with bug modified status and name,
and keep the buffers modified marker accurate."
  ;; TODO: should be handled in backend stuff
  (let* ((summary (or (cdr (assoc 'summary bug---data))
                     (cdr (assoc 'Name bug---data))
                     "<Missing summary!>"))
         (face)(prefix))
    (setq summary
          (concat
           summary
           (make-string (- (window-width) (length summary)) ? )))
    (cond (bug---is-new
           (set-buffer-modified-p t)
           (setq prefix "New bug")
           (setq face 'bug-header-line-new))
          (bug---changed-data
           (set-buffer-modified-p t)
           (setq prefix (concat "*" (prin1-to-string bug---id t)))
           (setq face 'bug-header-line-modified))
          (t
           (set-buffer-modified-p nil)
           (setq prefix (prin1-to-string bug---id t))
           (setq face 'bug-header-line)))

    (setq header-line-format
          (propertize
           (concat prefix ": " summary)
           'face face))))

(defun bug-update (id fields instance)
  "Update fields in the bug on Bugzilla"
  (let ((fields (append fields `((ids . ,id)))))
    (message (format "fields: %s" fields))
    (bug-rpc `((resource . "Bug")
               (operation . "update")
               (data . ,fields)) instance)))

(defun bug-get-comments (id instance)
  "Request comments for a bug and add it to an existing(!) bug buffer
via bug-handle-comments-response"
  (bug-handle-comments-response id
                                (bug-rpc `((resource . "Bug")
                                           (operation . "comments")
                                           (data . (("ids" . ,id)))) instance)))

(defun bug-handle-comments-response (id response)
  "Add received comments into an existing bug buffer"
  (if (and
       (assoc 'result response)
       (assoc 'bugs (assoc 'result response)))
      (let* ((bugs (cdr (assoc 'bugs (assoc 'result response))))
             (comments (cdr (cadr (car bugs)))))
        (save-excursion
          (switch-to-buffer (bug--buffer-string id bug---instance))
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

;; functions usually called through keybindings in bug-mode
;;;###autoload
(defun bug--bug-mode-browse-bug ()
  "Open the current bug in browser.

Note: This passes in the user friendly ID, and assumes that the backend
function can handle it for browser display."
  (interactive)
  (bug--backend-function "bug--browse-%s-bug" bug---id bug---instance))

;;;###autoload
(defun bug--bug-mode-create-comment ()
  "Create a comment on the current bug"
  (interactive)
  (bug-comment bug---id bug---instance))

;;;###autoload
(defun bug--bug-mode-download-attachment ()
  "Download the current attachment to the home directory"
  (interactive)
  (w3m-download
   (bug-find-attachment-url bug---instance)
   (expand-file-name (concat "~/" (match-string 3)))))

;;;###autoload
(defun bug--bug-mode-open-thing-near-point ()
  "Open the current attachment in the web browser"
  (interactive)
  (browse-url (bug-find-attachment-url bug---instance)))

;;;###autoload
(defun bug--bug-mode-commit ()
  "Commit changes in the bug to the bug tracker"
  (interactive)
  (if (equal nil bug---changed-data)
      (message "No changes available.")
    (progn
      (message "Sending changes...")
      (bug-update bug---id bug---changed-data bug---instance))))

(defun bug--bug-mode-locate-field (field-name)
  "Try to locate a field `field-name' at point or at the current line. If found
a position in the field is returned -- which may be just between fields, so
the caller needs to ensure that code using this position operates on the correct
field (e.g. by using constrain-to-field).

If no (valid) field was found `nil' is returned."
  (let ((field-pos nil))

    (if (equal field-name (get-text-property (point) 'field))
        ;; the field at point is of the right type, no search required
        (setq field-pos (point))
      (save-excursion
        ;; search from beginning of line to the next field change,
        ;; and compare if it's the correct type. If not we don't
        ;; make another attempt at locating the field (most likely
        ;; that case should not happen anyway)
        (forward-line 0)
        (let ((pos (next-single-property-change (point) 'field)))
          (if (equal field-name
                     (get-text-property pos 'field))
              (setq field-pos pos)))))
    field-pos))

(defun bug--bug-mode-edit-field(field-name field-type field-value)
  "Query new value for a field, with different input methods based
on field type (minibuffer or a separate popup buffer). Returns the new field
value, or the old field value if nothing has changed."
  (unless (bug--backend-feature bug---instance :write)
    (error "Backend does not support editing"))
  (cond ((equal field-type 0)
         (let ((my-history (list field-value)))
           (read-string (concat (prin1-to-string field-name) ": ")
                        "" 'my-history)))
        (t (message (format "Editing a field of type %s is not implemented"
                            (prin1-to-string field-type t)))
           field-value)))

;;;###autoload
(defun bug--bug-mode-edit-thing-near-point ()
  "Edit the bug field at or near point"
  ;; TODO: when called with prefix argument, prompt for which field to edit
  (interactive)
  (unless (bug--backend-feature bug---instance :write)
    (error "Backend does not support editing"))
  (let ((field-name (or (get-text-property (point) 'bug-field-name)
                        (save-excursion
                          (forward-line 0)
                          (get-text-property (point) 'bug-field-name)))))
    ;; TODO: bail out if field is read-only as well
    (if (and (equal field-name nil)
             (not (equal nil (bug--bug-mode-locate-field field-name))))
        ;; no field found? Bail out.
        (message "Unable to locate an editable field near point")
      ;; field found? Make sure we're in the right field, and then query
      ;; for new values
      (let ((field-pos (bug--bug-mode-locate-field field-name)))
        (setq field-pos (constrain-to-field (+ 1 field-pos)
                                            field-pos t))
        (let* ((field-value (field-string field-pos))
               (field-type (get-text-property field-pos 'bug-field-type))
               (new-value
                (bug--bug-mode-edit-field field-name field-type field-value)))
          (unless (string= field-value new-value)
            ;; new value entered? Update buffer and internal variables
            (progn
              (setq buffer-read-only nil)
              (goto-char field-pos)
              (delete-field field-pos)
              ;; add or replace the new field in `bug---changed-data'
              (if (assoc field-name bug---changed-data)
                  (setf (cdr (assoc field-name bug---changed-data)) new-value)
                (push (cons field-name new-value) bug---changed-data))
              ;; replace old data with new ones, nicely formatted
              (insert
               (propertize
                (bug--format-field-value (cons field-name new-value)
                                         bug---instance t)
                'field field-name))
              (bug--bug-mode-update-header)
              ;(setq buffer-read-only t)
              )))))))

;;;###autoload
(defun bug--bug-mode-info ()
  "Display some information about thing at or near point

This is mostly useful for debugging text properties"
  (interactive)
  (let ((field-name (get-text-property (point) 'bug-field-name))
        (content-type (get-text-property (point) 'bug-field-type))
        (field-id (get-text-property (point) 'bug-field-id))
        (field (get-text-property (point) 'field)))
    (message
     (concat
      "object = "
      (prin1-to-string (cdr (assoc 'ObjectType bug---data)))
      "; "
      "type = "
      (prin1-to-string content-type)
      "; "
      "field-id = "
      (prin1-to-string field-id)
      "; "
      "field = "
      (prin1-to-string field)
      "; "
      (prin1-to-string field-name)
      " = "
      (prin1-to-string (cdr (assoc field-name bug---data)))
      ))))

;;;###autoload
(defun bug--bug-mode-remember-bug (list-name &optional id instance)
  "Remember the current bug in a local search"
  (interactive
   (if (and (boundp 'bug---id) (boundp 'bug---data))
       (list
        (bug--query-remembered-lists))
     (list
      (bug--query-remembered-lists)
      (read-string "Bug: " nil nil t)
      (if current-prefix-arg (bug--query-instance)))))
  (let* ((instance (bug--instance-to-symbolp instance))
         (lists-for-instance (gethash instance bug-remember-list))
         (list-entries (if lists-for-instance
                           (gethash list-name lists-for-instance)))
         (bug-id (if (boundp 'bug---id) bug---id id)))
    (add-to-list 'list-entries bug-id)
    (delete-dups list-entries)
    (if lists-for-instance
        (puthash list-name list-entries lists-for-instance)
      (let ((lists-for-instance (make-hash-table :test 'equal)))
        (puthash list-name list-entries lists-for-instance)
        (puthash instance lists-for-instance bug-remember-list)
        ))
    (bug--write-data-file)))

;;;###autoload
(defun bug--bug-mode-resolve-bug ()
  "Resolve the current bug"
  (interactive)
  (let ((resolution
         (completing-read
          "resolution: "
          (filter
           (lambda (x)
             (> (length x) 0))
           (mapcar
            (lambda (x)
              (cdr (assoc 'name x)))
            (cdr (assoc 'values (gethash "resolution" (bug--get-fields bug---instance)))))))))
    (bug-update bug---id `((status . "RESOLVED") (resolution . ,resolution)) bug---instance))
  (bug-open bug---id bug---instance))

;;;###autoload
(defun bug--bug-mode-update-bug ()
  "Update the bug by reloading it from the bug tracker"
  (interactive)
  (bug-open bug---uuid bug---instance))

;;;###autoload
(defun bug--bug-mode-quit-window ()
  "Close the search result window"
  (interactive)
  ;; TODO: check if bug---changed-data is non-nil, and prompt about losing changes
  (quit-window t))

;; attachment handling functions
(defun bug-get-attachments (id instance)
    "Request attachment details for a bug and add it to an existing(!) bug buffer
via bug-handle-attachments-response"
  (bug-handle-attachments-response id (bug-rpc `((resource . "Bug")
                                                 (operation . "attachments")
                                                 (data . (("ids" . ,id)))) instance)))

(defun bug-handle-attachments-response (id response)
  "Add received attachment info into an existing bug buffer"
  (if (and
       (assoc 'result response)
       (assoc 'bugs (assoc 'result response)))
      (let* ((bugs (cdr (assoc 'bugs (assoc 'result response))))
             (attachments (cdr (car bugs))))
        (save-excursion
          (switch-to-buffer (bug--buffer-string id bug---instance))
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

(defun bug-find-attachment-url (instance)
  "Construct the URL required to download an attachment"
  (save-excursion
    (let ((end (re-search-forward "$" nil t)))
      (move-beginning-of-line nil)
      ;; FIXME: breaks if ; in filenames/descriptions.. heh
      (if (re-search-forward "^attachment \\([0-9]+\\): \\([^;]+\\); \\([^;]+\\);" end t)
          (format "%s/attachment.cgi?id=%s" (bug--instance-property :url instance) (match-string 1))
        (error "No attachment near point")))))

(provide 'bug-mode)
;;; bug-mode.el ends here
