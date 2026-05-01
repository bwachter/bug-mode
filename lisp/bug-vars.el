;;; bug-vars.el --- variables used across most of the bug codebase -*- lexical-binding: t; -*-
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

(defvar bug---id)
(make-variable-buffer-local 'bug---id)
(defvar bug---uuid)
(make-variable-buffer-local 'bug---uuid)
(defvar bug---is-new)
(make-variable-buffer-local 'bug---is-new)
(defvar bug---data)
(make-variable-buffer-local 'bug---data)
(defvar bug---instance)
(make-variable-buffer-local 'bug---instance)
(defvar bug---changed-data)
(make-variable-buffer-local 'bug---changed-data)
(defvar bug---field-filter-index nil)
(make-variable-buffer-local 'bug---field-filter-index)
(defvar bug---query)
(make-variable-buffer-local 'bug---query)
(defvar bug---query-string nil)
(make-variable-buffer-local 'bug---query-string)
(defvar bug---field-length)
(make-variable-buffer-local 'bug---field-length)

(provide 'bug-vars)

;;; bug-vars.el ends here
