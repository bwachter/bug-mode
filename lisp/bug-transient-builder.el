;;; bug-transient-builder.el --- dynamic transient layout builder -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2010-2026 bug-mode developers
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
;;; Commentary:
;;
;; This module provides a dynamic transient builder for bug-mode.
;; It defines reusable blocks and assembles them into multi-row/multi-column
;; grids at invocation time based on window dimensions.
;;
;; Usage:
;;   (bug-transient-define-prefix 'bug--mode-menu
;;     :blocks '(header edit create view interact))
;;
;;; Code:

(require 'cl-lib)
(require 'transient)
;;(require 'bug-instance)
(require 'bug-vars)

(declare-function bug-search "bug-search" (query &optional instance))
(declare-function bug-search-project "bug-search" (query &optional instance))
(declare-function bug-search-jql "bug-search" (query &optional instance))
(declare-function bug-search-jql-project "bug-search" (query &optional instance))
(declare-function bug--jql-read-with-hints "bug-search" (prompt &optional initial-content default-content))
(declare-function bug-project-get-current "bug-project" (&optional instance))
(declare-function bug--instance-query "bug-instance" &optional required-feature)
(declare-function bug-instance-get-current "bug-instance")

;; ------------------------------------------------------------------------------
;; Block registry

(defvar bug-transient-blocks (make-hash-table :test 'eq)
  "Hash table mapping block names (symbols) to block definitions.

Each definition is a plist with :group-fn (function returning a
transient group vector) and :keys (list of key strings).")

(defun bug-transient-register-block (name group-fn &rest keys)
  "Register a transient block named `name'.

`group-fn' is a function returning a transient group specification.
`keys' is a list of key strings used by this block, for conflict detection."
  (puthash name (list :group-fn group-fn :keys keys) bug-transient-blocks))

(defun bug-transient-get-block (name)
  "Return the block definition for `name', or signal an error."
  (or (gethash name bug-transient-blocks)
      (error "Unknown transient block: %s" name)))

;; ------------------------------------------------------------------------------
;; Layout computation

(defun bug--transient-compute-layout (block-names)
  "Compute a transient layout vector for `block-names'.

Arranges blocks into a grid based on current frame width.
The transient popup spans the full frame, so we use `frame-width'
rather than `window-body-width'.
Returns a vector of row vectors, where each row is a vector of group specs."
  (let* ((win-width (frame-width))
         (block-count (length block-names))
         ;; Estimate: each column needs ~28 chars for key + label + padding
         ;; Leave 4 chars margin for transient borders
         (usable-width (- win-width 4))
         (max-cols (max 1 (floor (/ usable-width 28))))
         ;; Don't create more columns than blocks
         (cols (min max-cols block-count))
         (rows nil)
         (current-row nil)
         (count 0))
    (dolist (name block-names)
      (let* ((def (bug-transient-get-block name))
             (group-fn (plist-get def :group-fn)))
        (push (funcall group-fn) current-row)
        (setq count (1+ count))
        (when (>= count cols)
          (push (reverse current-row) rows)
          (setq current-row nil)
          (setq count 0))))
    (when current-row
      (push (reverse current-row) rows))
    ;; Convert to transient layout vector
    (apply #'vector
           (mapcar (lambda (row)
                     (apply #'vector row))
                   (reverse rows)))))

;; ------------------------------------------------------------------------------
;; Current-bug detection
;; ------------------------------------------------------------------------------

(defun bug--transient-detect-current-bug ()
  "Detect the current bug and return a plist `(:bug-id ID :bug-uuid UUID)'.

Checks, in order:
1. Buffer-local `bug---id' / `bug---uuid'
2. Text properties at point (`bug-id', `bug-uuid')
3. Tabulated-list entry (first column as ID, with `bug-uuid' property)
4. vtable entry (if vtable is available)

Returns nil when no bug can be detected."
  (or
   ;; 1. Buffer-local variables
   (and (boundp 'bug---uuid)
        (or bug---id bug---uuid)
        (list :bug-id (or bug---id bug---uuid)
              :bug-uuid (or bug---uuid bug---id)))
   ;; 2. Text properties at point
   (let ((uuid (get-text-property (point) 'bug-uuid))
         (id   (get-text-property (point) 'bug-id)))
     (when (or id uuid)
       (list :bug-id (or id uuid)
             :bug-uuid (or uuid id))))
   ;; 3. Tabulated-list entry
   (and (derived-mode-p 'tabulated-list-mode)
        (fboundp 'tabulated-list-get-entry)
        (let ((entry (tabulated-list-get-entry)))
          (when entry
            (list :bug-id (elt entry 0)
                  :bug-uuid (or (get-text-property (point) 'bug-uuid)
                                (elt entry 0))))))
   ;; 4. vtable entry (only when it carries :uuid -- instance/project tables
   ;;     do not have this key and should not be treated as bugs)
   (and (fboundp 'vtable-current-object)
        (let ((obj (vtable-current-object)))
          (when (and obj (listp obj))
            (let ((uuid (plist-get (cdr obj) :uuid)))
              (when uuid
                (list :bug-id (car obj)
                      :bug-uuid uuid))))))))

;; ------------------------------------------------------------------------------
;; Search dispatchers (automatic project scoping from transient)
;; ------------------------------------------------------------------------------

(defun bug--transient-search ()
  "Execute a search, automatically using project scope from the transient.
Dispatches to `bug-search-project' when a project is set in the transient
scope, otherwise to `bug-search'.  Pre-fills the current buffer's native
query string as the minibuffer default when available."
  (interactive)
  (let* ((instance (bug--transient-get-instance))
         (project (bug--transient-get-project))
         (default-query (if (boundp 'bug---query-string) bug---query-string nil))
         (query (read-string "Search query: " default-query nil nil t)))
    (unless (string-empty-p query)
      (if project
          (bug-search-project query instance)
        (bug-search query instance)))))

(defun bug--transient-search-jql ()
  "Execute a JQL search, automatically using project scope from the transient.
Dispatches to `bug-search-jql-project' when a project is set in the
transient scope, otherwise to `bug-search-jql'.  Pre-fills the current
buffer's JQL query string as the minibuffer default when available."
  (interactive)
  (let* ((instance (bug--transient-get-instance))
         (project (bug--transient-get-project))
         (default-query (if (boundp 'bug---query-jql) bug---query-jql nil))
         (query (bug--jql-read-with-hints "JQL query: " default-query)))
    (unless (string-empty-p query)
      (if project
          (bug-search-jql-project query instance)
        (bug-search-jql query instance)))))

;; ------------------------------------------------------------------------------
;; Instance / project infixes

(defun bug--transient-instance-label ()
  "Return a label string for the instance infix.

Prefers the transient-local instance (stored in the prefix scope),
failing back to the global active instance."
  (let* ((scope (and (boundp 'transient--prefix)
                     transient--prefix
                     (oref transient--prefix scope)))
         (inst (or (plist-get scope :instance)
                   (bug-instance-get-current))))
    (format "Instance: %s" (if inst (symbol-name inst) "-"))))

(defun bug--transient-project-label ()
  "Return a label string for the project infix.

Prefers the transient-local project (stored in the prefix scope),
failing back to buffer-local or global resolution."
  (let* ((scope (and (boundp 'transient--prefix)
                     transient--prefix
                     (oref transient--prefix scope)))
         (proj (or (plist-get scope :project)
                   (and (boundp 'bug---project) bug---project)
                   (and (fboundp 'bug--get-project-instance)
                        (bug--get-project-instance)))))
    (format "Project: %s" (if proj (format "%s" proj) "-"))))

(defun bug--transient-switch-instance ()
  "Switch the instance for this transient without exiting.

The selected instance is stored in the prefix scope and does not
affect the global active instance."
  (interactive)
  (let ((inst (bug--instance-query)))
    (when inst
      (let ((scope (or (and (boundp 'transient--prefix)
                            transient--prefix
                            (oref transient--prefix scope))
                       nil)))
        (oset transient--prefix scope (plist-put scope :instance inst))
        (when (fboundp 'transient--redisplay)
          (transient--redisplay))))))

(defun bug--transient-switch-project ()
  "Switch the project for this transient without exiting.

The selected project is stored in the prefix scope and does not
affect any buffer-local or global project variable."
  (interactive)
  ;; TODO: implement project selection
  (message "Project switching not yet implemented")
  (when (fboundp 'transient--redisplay)
    (transient--redisplay)))

(defun bug--transient-get-instance ()
  "Return the instance to use for the current transient.

Delegates to `bug-instance-get-current' which already checks the
transient-local scope as its first resolution step."
  (bug-instance-get-current))

(defun bug--transient-get-project ()
  "Return the project to use for the current transient.

Delegates to `bug-project-get-current' which already checks the
transient-local scope as its first resolution step."
  (bug-project-get-current (bug--transient-get-instance)))

(defun bug--transient-infix-row ()
  "Return a vector containing the single Scope infix row.

The row is a `transient-column' group labelled \"Scope\" containing the
instance and project infixes stacked vertically.  It always spans the
full width of the transient because it is the only group in its row.
The :transient property ensures pressing an infix key does not exit
the menu."
  (vector
   (vector
    `["Scope"
      ("-i" (lambda () (bug--transient-instance-label))
       bug--transient-switch-instance
       :transient t)
      ("-p" (lambda () (bug--transient-project-label))
       bug--transient-switch-project
       :transient t)])))

;; ------------------------------------------------------------------------------
;; Key validation

(defun bug--transient-block-keys (name)
  "Return the list of keys declared by block `name'."
  (plist-get (bug-transient-get-block name) :keys))

(defun bug--transient-validate-keys (block-names)
  "Validate that no two blocks in `block-names' share a key.

Signals an error if duplicates are found."
  (let ((seen (make-hash-table :test 'equal)))
    (dolist (name block-names)
      (dolist (key (bug--transient-block-keys name))
        (when (gethash key seen)
          (error "Duplicate key '%s' in blocks '%s' and '%s'"
                 key (gethash key seen) name))
        (puthash key name seen)))))

;; ------------------------------------------------------------------------------
;; Prefix definition macro

(defmacro bug-transient-define-prefix (name &rest args)
  "Define a dynamic transient prefix `name'.

`args' is a plist with:
  :blocks      - list of block names to include (required)
  :class       - transient prefix class symbol (optional)
  :init-value  - init-value function (optional)
  :infixes     - if non-nil, include instance/project infix row (default t)"
  (declare (indent defun))
  (let* ((blocks-raw (plist-get args :blocks))
         (blocks (if (and (listp blocks-raw) (eq (car blocks-raw) 'quote))
                     (cadr blocks-raw)
                   blocks-raw))
         (class (plist-get args :class))
         (init-value (plist-get args :init-value))
         (infixes (if (plist-member args :infixes)
                      (plist-get args :infixes)
                    t))
         (internal-name (intern (format "%s--internal" name))))
    ;; Validate at macro-expansion time
    (when blocks
      (dolist (block blocks)
        (unless (gethash block bug-transient-blocks)
          (error "Unknown block: %s" block)))
      (bug--transient-validate-keys blocks))
    `(progn
       ;; Set up the internal prefix manually.  We do NOT use
       ;; `transient-define-prefix' because its macro expansion stores the
       ;; parsed layout in a version-dependent vector format in the .elc.
       ;; When the .elc is loaded with a different transient version, the
       ;; formats mismatch and transient crashes with "Args out of range".
       ;; By creating the prefix object and properties at load time (not
       ;; compile time), we ensure the format always matches the currently
       ;; loaded transient.
       (put ',internal-name 'transient--prefix
            (,(or class 'transient-prefix)
             :command ',internal-name
             :init-value ,(let ((base-body '((oset obj value (transient-default-value obj))
                                             (when-let ((bug-info (bug--transient-detect-current-bug)))
                                               (oset obj scope (append (oref obj scope) bug-info))))))
                            (if init-value
                                `(lambda (obj)
                                   ,@base-body
                                   (funcall ,init-value obj))
                              `(lambda (obj)
                                 ,@base-body)))))
       (put ',internal-name 'transient--layout nil)
       (put ',internal-name 'function-documentation
            ,(format "Transient for %s" name))
       (put ',internal-name 'interactive-only t)
       (defun ,internal-name ()
         ,(format "Internal transient for %s" name)
         (interactive)
         (transient-setup ',internal-name))
       ;; Define the public command
       (defun ,name ()
         ,(format "Transient for %s" name)
         (interactive)
         (let* ((layout (bug--transient-compute-layout ',blocks))
                (infix-rows ,(if infixes `(bug--transient-infix-row) nil))
                ;; Combine infix rows (each on its own line) with the block grid.
                (all-rows (append (when infix-rows (append infix-rows nil))
                                  (append layout nil)))
                ;; Parse each row vector using the currently loaded transient.
                ;; This must happen at runtime so the internal vector format
                ;; matches whatever transient version is currently loaded.
                (parsed-layout
                 (cl-mapcan (lambda (row)
                              (mapcar #'eval (transient--parse-child ',name row)))
                            all-rows)))
           ;; Store the layout in the format expected by the currently loaded
           ;; transient version.  The bundled Emacs transient reads the layout
           ;; directly from the symbol property as a list of parsed vectors.
           ;; Newer transient versions wrap it in a version-2 vector:
           ;;   [2 nil <layout>]
           ;; We detect the newer version by the presence of
           ;; `transient--get-layout'.
           (put ',internal-name 'transient--layout
                (if (fboundp 'transient--get-layout)
                    (vector 2 nil parsed-layout)
                  parsed-layout))
           (transient-setup ',internal-name))))))

;; ------------------------------------------------------------------------------
;; Shared block definitions

(bug-transient-register-block
 'bug
 (lambda ()
   `[:description
     (lambda ()
       (let* ((scope (and (boundp 'transient--prefix)
                          transient--prefix
                          (oref transient--prefix scope)))
              (bug-id (plist-get scope :bug-id)))
         (format "Bug%s" (if bug-id
                             (format " %s"
                                     (propertize (format "%s" bug-id)
                                                 'face 'bug-field-type-6)) ""))))
     ("C" "Create related bug" bug--bug-mode-create-related)
     ("o" "Open" bug-open)
     ;; todo, also deactivate if not a bug
     ("D" "Delete this bug" bug--bug-mode-delete-bug
      :inapt-if (lambda () (not (and (bug--transient-get-instance)
                                     (bug--instance-feature (bug--transient-get-instance) :delete)))))
     ("r" "Remember bug" bug--bug-mode-remember-bug
      :inapt-if (lambda () (not (plist-get (and (boundp 'transient--prefix)
                                                transient--prefix
                                                (oref transient--prefix scope))
                                           :bug-id))))
     ("B" "Open in browser" bug--bug-mode-browse-bug
      :inapt-if (lambda () (not (plist-get (and (boundp 'transient--prefix)
                                                transient--prefix
                                                (oref transient--prefix scope))
                                           :bug-id))))])
 "o" "r" "B" "D")

(bug-transient-register-block
 'edit
 (lambda ()
   `["Edit"
     :inapt-if (lambda () (not (and (bug--transient-get-instance)
                                    (bug--instance-feature (bug--transient-get-instance) :write))))
     ("a" "Add new field" bug--bug-mode-add-field)
     ("d" "Download attachment" bug--bug-mode-download-attachment)
     ("C" "Add new comment" bug--bug-mode-create-comment)
     ("e" "Edit field" bug--bug-mode-edit-thing-near-point)])
 "a" "d" "C" "e")

(bug-transient-register-block
 'instances
 (lambda ()
   `["Instances"
     ("l" "List instances" bug-list-instances)
     ("a" "Switch active instance" bug-instance-switch)
     ("d" "Deactivate current active instance" bug-instance-deactivate)])
 "l" "a" "d")

(bug-transient-register-block
 'interact
 (lambda ()
   `["Interact"
     ("i" "Info" bug--bug-mode-info)
     ("c" "Create bug" (lambda ()
                         (interactive)
                         (let ((instance (bug--transient-get-instance)))
                           (bug-create nil instance)))
      :if (lambda ()
            (let ((instance (bug--transient-get-instance)))
              (or (not instance)
                  (bug--instance-feature instance :create)))))])
 "i" "c")

(bug-transient-register-block
 'misc
 (lambda ()
   `["Misc"
     ("-c" "Clear metadata cache" bug-cache-clear)])
 "-c")

(bug-transient-register-block
 'project
 (lambda ()
   `[:description
     (lambda ()
       (let ((project (and (fboundp 'vtable-current-object)
                           (vtable-current-object))))
         (format "Project%s"
                 (if project
                     (format " %s" (car project))
                   ""))))
     ("p" "Copy project ID" (lambda ()
                              (interactive)
                              (let* ((project (and (fboundp 'vtable-current-object)
                                                   (vtable-current-object))))
                                (when project
                                  (let ((project-id (cdr project)))
                                    (kill-new project-id)
                                    (message "Copied ID: %s" project-id)))))
      :if (lambda ()
            (and (fboundp 'vtable-current-object)
                 (vtable-current-object))))
     ("l" "List bugs for project" (lambda ()
                                    (interactive)
                                    (let* ((project (and (fboundp 'vtable-current-object)
                                                         (vtable-current-object))))
                                      (when project
                                        (bug-list-project-bugs (cdr project) bug---instance))))
      :if (lambda ()
            (and (fboundp 'vtable-current-object)
                 (vtable-current-object))))])
 "p" "l")

(bug-transient-register-block
 'repo
 (lambda ()
   `["Repository"
     :inapt-if (lambda () (not (bug--repo-matching-instances)))
     ("R" "Repo issues" (lambda ()
                          (interactive)
                          (let* ((matches (bug--repo-matching-instances))
                                 (single (and (= 1 (length matches))
                                              (car matches)))
                                 (instance (or (car single)
                                               (bug--instance-to-symbolp nil :project-bugs)))
                                 (scope (or (cdr single)
                                            (bug--repo-scope instance))))
                            (bug-list-project-bugs scope instance))))
     ("C" "Repo create" (lambda ()
                          (interactive)
                          (let* ((matches (bug--repo-matching-instances))
                                 (single (and (= 1 (length matches))
                                              (car matches)))
                                 (instance (or (car single)
                                               (bug--instance-to-symbolp nil :create)))
                                 (scope (or (cdr single)
                                            (bug--repo-scope instance))))
                            (bug-create `(:project-id ,scope) instance))))])
 "R" "C")

(bug-transient-register-block
 'search
 (lambda ()
   `["Search"
     ("s" "Search" bug--transient-search)
     ("j" "JQL search" bug--transient-search-jql
      :inapt-if (lambda () (not (and (bug--transient-get-instance)
                                     (bug--instance-feature (bug--transient-get-instance) :search-jql)))))
     ("S" "Edit search" bug-edit-search)
     ("J" "Edit JQL search" bug-edit-jql-search
      :inapt-if (lambda () (not (and (boundp 'bug---query-jql)
                                     bug---query-jql
                                     (not (string-empty-p bug---query-jql))))))])
 "s" "j" "e" "J")

(bug-transient-register-block
 'rally
 (lambda ()
   `["Rally"
     :inapt-if (lambda () (not (and (bug--transient-get-instance)
                                    (equal 'rally (bug--instance-backend-type (bug--transient-get-instance))))))
     ("s" "Subscription" (lambda ()
                           (interactive)
                           (bug-rally-subscription (bug--transient-get-instance))))])
 "s")

(bug-transient-register-block
 'view
 (lambda ()
   `["View"
     ("v" "Toggle field filter" bug--bug-mode-toggle-field-filter)])
 "v")
;;;;; TODO below

;; Instance header block (used in bug-instance-mode)
(bug-transient-register-block
 'instance-header
 (lambda ()
   `[:description
     (lambda ()
       (let ((instance (and (fboundp 'vtable-current-object)
                            (vtable-current-object))))
         (format "Instance%s"
                 (if instance
                     (format " %s" (car instance))
                   ""))))
     ("A" "Activate instance" (lambda ()
                                (interactive)
                                (let ((instance (and (fboundp 'vtable-current-object)
                                                     (car (vtable-current-object)))))
                                  (when instance
                                    (bug-instance-activate instance)
                                    (bug-list-instances))))
      :if (lambda ()
            (and (fboundp 'vtable-current-object)
                 (car (vtable-current-object)))))
     ("x" "Deactivate instance" (lambda ()
                                  (interactive)
                                  (bug-instance-deactivate)
                                  (bug-list-instances)))])
 "A" "x")

(provide 'bug-transient-builder)
;;; bug-transient-builder.el ends here
