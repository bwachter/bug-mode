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

;;(bug-rpc "HierarchicalRequirement.create" '((data . (HierarchicalRequirement . ((Name . "Test name"))))))

(require 'cl-lib)

(require 'bug-vars)
(require 'bug-rpc)
(require 'bug-common-functions)
(require 'bug-field-completion)
(require 'bug-format)
(require 'bug-debug)
(require 'bug-persistent-data)
(require 'bug-custom)

;; Rally backend functions (loaded dynamically)
(declare-function bug--fetch-rally-discussion "bug-backend-rally" (bug-data instance))
(declare-function bug--display-rally-discussion "bug-backend-rally" (posts))
(declare-function bug-comment "bug-comment" (id &optional instance))

(defvar bug-mode-map
  (let ((keymap (copy-keymap special-mode-map)))
    (define-key keymap (kbd "RET") 'bug--bug-mode-open-thing-near-point)
    (define-key keymap (kbd "TAB") 'bug--bug-mode-peek-completions)
    (define-key keymap "a"         'bug--bug-mode-add-field)
    (define-key keymap "b"         'bug--bug-mode-browse-bug)
    ;; TODO: change this to a 'change bug' popup
    (define-key keymap "c"         'bug--bug-mode-create-comment)
    (define-key keymap "d"         'bug--bug-mode-download-attachment)
    (define-key keymap "e"         'bug--bug-mode-edit-thing-near-point)
    (define-key keymap "i"         'bug--bug-mode-info)
    (define-key keymap "n"         'bug--bug-mode-create-related)
    (define-key keymap "r"         'bug--bug-mode-remember-bug)
    ;; TODO: this should change to 'status change' instead of 'resolve'
    (define-key keymap "s"         'bug--bug-mode-resolve-bug)
    (define-key keymap "u"         'bug--bug-mode-update-bug)
    (define-key keymap "v"         'bug--bug-mode-toggle-field-filter)
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

(defun bug--field-filtered-p (instance field)
  "Return t for non-filtered fields, nil for filtered ones"
  ;; Get field filters from backend
  (let* ((field-filters (bug--backend-function-optional "bug--%s-field-filters" instance))
         (current-filter (when field-filters
                           (nth bug---field-filter-index field-filters))))

    (if (or (null current-filter)
            (not (listp current-filter)))  ;; no filter or empty = allow all
        t
      (cl-some (lambda (f)
                 (string= f field))
               current-filter))))

(defun bug--visible-field-p (prop instance)
  "Return non-nil if PROP should be included for INSTANCE."

  (let ((name (car prop))
        (value (cdr prop)))
    (and (not (equal :json-false (bug--get-field-property name 'is_visible instance)))
         ;; referenced objects are included as a list. If there's
         ;; a `Count' property with value `0' it's safe to assume
         ;; we don't need to retrieve it (might be rally only)
         (not (and (listp value)
                   (equal 0 (cdr (assoc 'Count value)))))
         ;; In new-artifact draft buffers show fields even when empty/nil,
         ;; so the user can see and fill them in before committing.
         (or (and (boundp 'bug---is-new) bug---is-new)
             (and (not (equal value nil))
                  (not (string-match "^[[:space:]]*$" (prin1-to-string value t)))))
         (bug--field-filtered-p instance name)
         ;; Apply field filter if defined and not empty
         ;;(or (null current-filter)
         ;;    (null field-filters)
         ;;    (member (car prop) current-filter))
         ;; Don't display Discussion/Description fields inline - handle separately
         (not (string= name "Discussion"))
         (not (string= name "Description"))
         (not (string= name "internals")))))

(defun bug-show (bug instance)
  "Display an existing bug buffer in bug-mode"
  (bug--debug-log-time "bug-show")
  ;; we need to save variables we want to keep over redisplay here in the let
  (let ((tmp-bug-id (cdr (assoc (bug--field-name :bug-friendly-id instance) bug)))
        (tmp-filter-index bug---field-filter-index))
    (pop-to-buffer (bug--buffer-string tmp-bug-id instance))

    (bug-mode)
    ;; the tmp-bug-id bit is needed as setting the mode clears buffer-local variables
    (make-local-variable 'bug---id)
    (setq bug---id tmp-bug-id)
    (make-local-variable 'bug---uuid)
    (setq bug---uuid (cdr (assoc (bug--field-name :bug-uuid instance) bug)))
    (make-local-variable 'bug---is-new)
    (setq bug---is-new (if bug---id nil t))
    ;; Sort: use filter-list order when a named filter is active, else alphabetical
    (let* ((instance-sym (bug--instance-to-symbolp instance))
           (all-filters (bug--backend-function-optional "bug--%s-field-filters" nil instance-sym))
           (current-filter (when all-filters
                             (nth (or tmp-filter-index 0) all-filters))))
      (setq bug
            (sort bug
                  (if (and current-filter (listp current-filter))
                      (lambda (a b)
                        (let* ((an (let ((k (car a)))
                                     (if (symbolp k) (symbol-name k) (format "%s" k))))
                               (bn (let ((k (car b)))
                                     (if (symbolp k) (symbol-name k) (format "%s" k))))
                               (ai (or (cl-position an current-filter :test #'string=)
                                       most-positive-fixnum))
                               (bi (or (cl-position bn current-filter :test #'string=)
                                       most-positive-fixnum)))
                          (if (= ai bi) (string< an bn) (< ai bi))))
                    (lambda (a b) (string< (car a) (car b)))))))
    (make-local-variable 'bug---data)
    (setq bug---data bug)
    (make-local-variable 'bug---instance)
    (setq bug---instance (bug--instance-to-symbolp instance))
    (make-local-variable 'bug---changed-data)
    (setq bug---changed-data nil)
    (make-local-variable 'bug---field-filter-index)
    ;; if the bug is new we don't want to hide any headers -> select the final
    ;; index which should be "no filter"
    (setq bug---field-filter-index
          (if bug---is-new
              (let* ((inst (bug--instance-to-symbolp instance))
                     (filters (bug--backend-function-optional "bug--%s-field-filters" nil inst)))
                (if filters (1- (length filters)) 0))
            (or tmp-filter-index 0)))
    (setq buffer-read-only nil)
    (buffer-disable-undo)
    (erase-buffer)

    (insert
     (mapconcat
      (lambda (prop)
        (concat
         (bug--format-field-name prop instance)
         (let ((fv (bug--format-field-value prop instance t)))
           (propertize (if (string-empty-p fv) " " fv) 'field (car prop)))))
      (filter (lambda (prop) (bug--visible-field-p prop instance)) bug)
      "\n"))

    ;; Display Description separately after other fields.
    ;; For new-artifact drafts, show it even when empty so the user can fill it in.
    (let ((description-prop (or (assoc "Description" bug)
                                (assoc 'Description bug))))
      (when (and description-prop
                 (or bug---is-new
                     (and (cdr description-prop)
                          (not (string-match "^[[:space:]]*$"
                                             (prin1-to-string (cdr description-prop) t))))))
        (insert "\n"
                (bug--format-field-name description-prop instance)
                "\n\n"
                (let ((fv (bug--format-field-value description-prop instance t)))
                  (propertize (if (string-empty-p fv) " " fv)
                              'field (car description-prop))))))

    ;; Load backend-specific additional data (comments, discussions, etc.)
    ;; TODO, this is a quick and dirty hack to get things working - we shouldn't
    ;; be calling backend specific specific functions directly here
    (cond
     ;; Rally: Load discussion posts
     ((equal 'rally (bug--backend-type instance))
      (let ((posts (bug--fetch-rally-discussion bug instance)))
        (when posts
          (bug--display-rally-discussion posts))))
     ;; Bugzilla: Load attachments and comments
     (t
      (insert "\nATTACHMENTS:\n")
      (insert "\nCOMMENTS:\n")
      (when (and bug---id bug-autoload-attachments)
        (bug-get-attachments bug---id instance))
      (when (and bug---id bug-autoload-comments)
        (bug-get-comments bug---id instance))))
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

(defun bug--bug-mode-toggle-field-filter ()
  "Toggle between different field filter views.

Cycles through the field filters defined by the backend. If the backend
defines no filters, this does nothing. Empty filter lists show all fields."
  (interactive)
  (let* ((instance bug---instance)
         (field-filters (bug--backend-function-optional "bug--%s-field-filters" instance)))
    (if (null field-filters)
        (message "No field filters defined for this backend")
      (setq bug---field-filter-index
            (mod (1+ bug---field-filter-index) (length field-filters)))
      (let ((current-filter (nth bug---field-filter-index field-filters)))
        (message "Field filter: %s (%d/%d)"
                 (if (null current-filter)
                     "All fields"
                   (format "%d fields" (length current-filter)))
                 (1+ bug---field-filter-index)
                 (length field-filters)))
      ;; Refresh the bug display
      (bug-show bug---data instance))))

(defun bug--get-update-id (instance)
  "Get the appropriate ID for update operations based on backend type.

For Rally, returns bug---uuid (_refObjectUUID).
For other backends, returns bug---id."
  (if (equal 'rally (bug--backend-type instance))
      bug---uuid
    bug---id))

(defun bug-update (id fields instance)
  "Update fields in a bug using the backend-specific update function.

ID is the bug identifier (UUID for Rally, numeric ID for Bugzilla).
FIELDS is an alist of field names and values to update.
INSTANCE is the bug tracker instance.

Returns the updated bug data from the backend."
  (message "Updating bug %s with fields: %s" id fields)
  (bug--backend-function "bug--update-%s-bug" (list id fields) instance))

;;;###autoload
(defun bug-create (&optional instance)
  "Create a new artifact in the bug tracker, prompting for required fields.

Required here means needed for preparing the edit buffer - typically that's just
something like artifact type. Other fields can then be edited in the buffer,
and the submission check should warn about missing required fields.

With a prefix argument, also prompts for which instance to use."
  (interactive
   (list (if current-prefix-arg
             (bug--instance-to-symbolp (bug--query-instance))
           (bug--instance-to-symbolp nil))))
  (unless (bug--backend-feature instance :create)
    (error "Backend does not support issue creation"))
  (bug--backend-function "bug--create-%s-bug-interactive" nil instance))

;;;###autoload
(defun bug-new-draft (display-alist create-alist instance)
  "Open a draft buffer for a new artifact, prefilled with `display-alist'.

This is mostly useful for backends to call at the end of the  handler for
creating new artifacts.

`display-alist' should include at minimum an ObjectType entry (needed for field
completion dispatch) and empty entries for each field the user should fill.
`create-alist' contains fields that are already decided (project ref, parent
ref, etc.) and will be pre-populated into bug---changed-data so they are
included in the create call without requiring the user to edit them.

The buffer uses the normal bug-mode layout.  Edit fields with \\[bug--bug-mode-edit-thing-near-point]
and press \\[bug--bug-mode-commit] to create the artifact."
  (bug-show display-alist instance)
  ;; bug-show reset bug---changed-data to nil; restore the pre-filled fields
  ;; (project, parent link) that should be included in the create call.
  (setq bug---changed-data create-alist)
  (bug--bug-mode-update-header))

;;;###autoload
(defun bug--bug-mode-create-related ()
  "Create a new artifact related to the bug in the current buffer.

Passes the current bug's data to the backend so it can prefill the parent
link and select a sensible default type. The exact relationship depends on
the backend and the current artifact type."
  (interactive)
  (unless (bug--backend-feature bug---instance :create)
    (error "Backend does not support issue creation"))
  (bug--backend-function "bug--create-%s-bug-interactive" bug---data bug---instance))

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
        (with-current-buffer (bug--buffer-string id bug---instance)
          (setq buffer-read-only nil)
          (save-excursion
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
                                     comments "\n\n")))
              (error "Could not find area for comments in buffer")))
          (setq buffer-read-only t)))))

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
  "Download the current attachment to the home directory."
  (interactive)
  (let ((url (bug-find-attachment-url bug---instance))
        (dest (expand-file-name (concat "~/" (match-string 3)))))
    (if url
        (url-copy-file url dest t)
      (error "No attachment URL found"))))

;;;###autoload
(defun bug--bug-mode-open-thing-near-point ()
  "Open the current attachment in the web browser"
  (interactive)
  (browse-url (bug-find-attachment-url bug---instance)))

;;;###autoload
(defun bug--bug-mode-add-field ()
  "Prompt for an additional field to add to the current bug buffer.

Uses backend-provided completion over fields not already present.  The new
field is inserted before the Description block (or at end of buffer) and can
be edited immediately with \\[bug--bug-mode-edit-thing-near-point]."
  (interactive)
  (let* ((object-type (cdr (assoc 'ObjectType bug---data)))
         (type-name (cond ((symbolp object-type) (symbol-name object-type))
                          ((stringp object-type) object-type)
                          (t nil)))
         (present (mapcar #'car bug---data))
         (candidates (when type-name
                       (bug--backend-function-optional
                        "bug--available-field-names-%s"
                        (list type-name present)
                        bug---instance))))
    (if (null candidates)
        (message "No additional fields available")
      (let* ((display-names (mapcar #'car candidates))
             (chosen-display (completing-read "Add field: " display-names nil t))
             (field-name-str (cdr (assoc chosen-display candidates)))
             (field-sym (intern field-name-str))
             (field-prop (cons field-sym nil))
             (new-line (concat
                        "\n"
                        (bug--format-field-name field-prop bug---instance)
                        (propertize " " 'field field-sym))))
        ;; Add to bug---data so filter and subsequent re-renders include it
        (push field-prop bug---data)
        ;; Insert into buffer before Description, or at end of field block
        (let ((inhibit-read-only t))
          (save-excursion
            (let ((desc-pos (or (text-property-any (point-min) (point-max)
                                                   'bug-field-name 'Description)
                                (text-property-any (point-min) (point-max)
                                                   'bug-field-name "Description"))))
              (if desc-pos
                  (goto-char (1- desc-pos))
                (goto-char (point-max)))
              (insert new-line))))
        (message "Added field %s — press e to edit it" field-name-str)))))

(defun bug--bug-mode-commit ()
  "Commit changes in the bug to the bug tracker.

If the bug is a new artifact this will create it in the backend."
  (interactive)
  (cond
   ;; New-artifact draft: validate and dispatch to backend create
   (bug---is-new
    (let ((missing (bug--backend-function-optional
                    "bug--validate-draft-%s"
                    (list bug---data bug---changed-data)
                    bug---instance)))
      (when missing
        (user-error "Cannot submit — required fields missing: %s"
                    (mapconcat #'identity missing ", "))))
    (message "Creating artifact...")
    (bug--backend-function "bug--create-%s-new-artifact"
                           (list bug---data bug---changed-data)
                           bug---instance))
   ;; Existing artifact: normal update
   ((null bug---changed-data)
    (message "No changes available."))
   (t
    (message "Sending changes...")
    (let ((update-id (bug--get-update-id bug---instance)))
      (bug-update update-id bug---changed-data bug---instance)
      (setq bug---changed-data nil)
      (bug--bug-mode-update-header)
      (message "Changes committed successfully.")))))

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
          (when (and pos
                     (equal field-name (get-text-property pos 'field)))
            (setq field-pos pos)))))
    field-pos))

(defun bug--bug-mode-edit-field (field-name field-type field-value)
  "Query new value for a field. Returns the new value, or the old
value if nothing has changed or editing is unsupported for the field type.

Tries backend-provided completion first; falls back to type-specific input."
  (unless (bug--backend-feature bug---instance :write)
    (error "Backend does not support editing"))
  (or (bug--completing-read-field field-name field-value bug---instance)
      (cond
       ((or (null field-type) (equal field-type 0))
        (let ((_my-history (list field-value)))
          (read-string (concat (prin1-to-string field-name) ": ")
                       "" 'my-history)))
       (t
        (message "Editing a field of type %s is not implemented"
                 (prin1-to-string field-type t))
        field-value))))

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
      ;; Check if the backend blocks direct editing of this field
      (let ((blocked-msg (bug--backend-function-optional
                          "bug--field-edit-blocked-%s"
                          field-name bug---instance)))
        (if blocked-msg
            (message "%s" blocked-msg)
          ;; field found and not blocked: query for a new value
          (let ((field-pos (bug--bug-mode-locate-field field-name)))
            (setq field-pos (constrain-to-field (+ 1 field-pos)
                                                field-pos t))
            (let* ((field-value (string-trim (field-string field-pos)))
                   (field-type (get-text-property field-pos 'bug-field-type))
                   (new-value
                    (bug--bug-mode-edit-field field-name field-type field-value))
                   ;; Object completions return (ref-url . display-name); normalize
                   ;; so new-ref goes to the backend and new-data-value is stored
                   ;; locally in a form that renders as "-> Name".
                   (new-ref
                    (if (consp new-value) (car new-value) new-value))
                   (new-data-value
                    (if (consp new-value)
                        `((_ref . ,new-ref) (_refObjectName . ,(cdr new-value)))
                      new-value)))
              (unless (and (stringp new-value) (string= field-value new-value))
                ;; Collect any backend-linked field changes (e.g. FlowState <> ScheduleState)
                (let* ((linked (bug--backend-function-optional
                                "bug--linked-field-changes-%s"
                                (list field-name new-ref)
                                bug---instance))
                       (extra-changes (cdr (assoc 'changes linked)))
                       (extra-display (cdr (assoc 'display linked))))
                  ;; Pre-update bug---data for linked fields so next reload is consistent
                  (dolist (disp extra-display)
                    (if (assoc (car disp) bug---data)
                        (setf (cdr (assoc (car disp) bug---data)) (cdr disp))
                      (push disp bug---data)))
                  (setq buffer-read-only nil)
                  (goto-char field-pos)
                  (delete-field field-pos)

                  (cond
                   ;; Immediate update mode: write to backend immediately.
                   ;; Bypassed for new-artifact drafts — no UUID exists yet.
                   ((and (eq bug-update-mode 'immediate) (not bug---is-new))
                    (condition-case err
                        (let ((update-id (bug--get-update-id bug---instance)))
                          (message "Updating field %s..." field-name)
                          (bug-update update-id
                                      (append `((,field-name . ,new-ref)) extra-changes)
                                      bug---instance)
                          ;; Update succeeded - update buffer and local data
                          (if (assoc field-name bug---data)
                              (setf (cdr (assoc field-name bug---data)) new-data-value)
                            (push (cons field-name new-data-value) bug---data))
                          (insert
                           (propertize
                            (bug--format-field-value (cons field-name new-data-value)
                                                     bug---instance t)
                            'field field-name))
                          (message "Field %s updated successfully." field-name))
                      (error
                       ;; Update failed - restore old value and show error
                       (insert
                        (propertize
                         (bug--format-field-value (cons field-name field-value)
                                                  bug---instance t)
                         'field field-name))
                       (message "Failed to update field: %s" (error-message-string err)))))

                   ;; On-commit mode: track changes locally
                   ((eq bug-update-mode 'on-commit)
                    ;; add or replace the new field in `bug---changed-data'
                    (if (assoc field-name bug---changed-data)
                        (setf (cdr (assoc field-name bug---changed-data)) new-ref)
                      (push (cons field-name new-ref) bug---changed-data))
                    ;; propagate linked field changes
                    (dolist (change extra-changes)
                      (if (assoc (car change) bug---changed-data)
                          (setf (cdr (assoc (car change) bug---changed-data)) (cdr change))
                        (push change bug---changed-data)))
                    ;; replace old data with new ones, nicely formatted
                    (insert
                     (propertize
                      (bug--format-field-value (cons field-name new-data-value)
                                               bug---instance t)
                      'field field-name))
                    (message "Field %s changed (use C-c C-c to commit)" field-name)))

                  ;; Live-update buffer text for linked fields visible in the buffer
                  (save-excursion
                    (dolist (disp extra-display)
                      (let* ((fname (car disp))
                             (fval  (cdr disp))
                             (fpos  (text-property-any (point-min) (point-max) 'field fname))
                             (fend  (when fpos
                                      (or (text-property-not-all fpos (point-max) 'field fname)
                                          (point-max)))))
                        (when fpos
                          (goto-char fpos)
                          (delete-region fpos fend)
                          (insert (propertize
                                   (let ((fv (bug--format-field-value
                                              (cons fname fval) bug---instance t)))
                                     (if (string-empty-p fv) " " fv))
                                   'field fname))))))
                  (bug--bug-mode-update-header)
                  (setq buffer-read-only t))))))))))

;;;###autoload
(defun bug--bug-mode-peek-completions ()
  "Fetch and display completion candidates for the field at point, bypassing cache.

Useful for debugging: shows what the backend returns for this field without
relying on any cached attribute definitions or allowed-value lists."
  (interactive)
  (let ((field-name (or (get-text-property (point) 'bug-field-name)
                        (save-excursion
                          (forward-line 0)
                          (get-text-property (point) 'bug-field-name)))))
    (if (not field-name)
        (message "No field at point")
      ;; Clear all cached completion data for this backend/instance so we always
      ;; get a fresh API response.  The prefix is the backend type name so this
      ;; works for any backend whose cache keys follow the "<backend>-" convention.
      (let ((backend-prefix (concat (prin1-to-string
                                     (bug--backend-type bug---instance) t)
                                    "-")))
        (bug-cache-clear-matching backend-prefix bug---instance))
      (let ((completions (bug--field-completion-values field-name bug---instance)))
        (if (not completions)
            (message "peek-completions for %s: nil (field has no restricted values or fetch failed)" field-name)
          (let* ((is-alist (and (consp completions) (consp (car completions))))
                 (display-items (if is-alist (mapcar #'car completions) completions)))
            (message "peek-completions for %s (%d%s): %s%s"
                     field-name
                     (length completions)
                     (if is-alist ", object refs" "")
                     (mapconcat #'identity (seq-take display-items 8) ", ")
                     (if (> (length completions) 8) ", ..." ""))
            (completing-read (format "%s (peek, RET/C-g to dismiss): " field-name)
                             display-items nil nil nil nil)))))))

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
    (setq list-entries (cl-pushnew bug-id list-entries))
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
            (cdr (assoc 'values (gethash "resolution" (bug--get-fields bug---instance))))))))
        (update-id (bug--get-update-id bug---instance)))
    (bug-update update-id `((status . "RESOLVED") (resolution . ,resolution)) bug---instance))
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
        (with-current-buffer (bug--buffer-string id bug---instance)
          (setq buffer-read-only nil)
          (save-excursion
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
                                     attachments "\n")))
              (error "Could not find area for attachments in buffer")))
          (setq buffer-read-only t)))))

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
