;;; bug-field-completion.el --- field value completion for bug-mode -*- lexical-binding: t; -*-
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

(defun bug--field-completion-values (field-name instance)
  "Get completion candidates for `field-name' from the backend for `instance'.

Dispatches to the backend-specific `bug--field-completion-<backend>' function.
Returns a list of strings, or nil if no completion is available."
  (bug--backend-function-optional "bug--field-completion-%s" field-name instance))

(defun bug--completing-read-field (field-name current-value instance)
  "Prompt for a new value of `field-name' using backend-provided completion.

`current-value' is shown as the default and returned if the user enters nothing.
Returns a string if completions are available, nil otherwise (allowing
the caller to fall back to type-specific input methods).

The backend may return either a plain list of strings (for simple types like
STATE/STRING) or an alist of (display-name . update-value) pairs (for OBJECT
types). In the alist case, completing-read shows the display names and the
function returns the update-value (e.g. a Rally object ref URL)."
  (let ((completions (bug--field-completion-values field-name instance)))
    (when completions
      (let* ((is-alist (and (consp completions) (consp (car completions))))
             (display-list (if is-alist (mapcar #'car completions) completions))
             (selected (completing-read
                        (format "%s (default: %s): "
                                (if (symbolp field-name) (symbol-name field-name)
                                  (format "%s" field-name))
                                current-value)
                        display-list
                        nil            ;; predicate
                        nil            ;; require-match
                        nil            ;; initial-input
                        nil            ;; history
                        current-value)))
        (if is-alist
            (or (cdr (assoc selected completions)) selected)
          selected)))))

(provide 'bug-field-completion)
;;; bug-field-completion.el ends here
