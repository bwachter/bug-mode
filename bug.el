;;; bug.el --- work with bug trackers from within emacs
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

(add-to-list 'load-path
             (directory-file-name
              (concat
               (file-name-directory (or load-file-name (buffer-file-name)))
               "lisp")))

(require 'bug-autoloads)

;;;;;;
;; Customizable variables for bug-mode
;; Use M-x customize-group <ret> bug <ret> to customize

(defgroup bug nil "bug-mode related settings")

(defcustom bug-debug nil
  "Configure debugging to *bug-debug* buffer"
  :type 'string
  :group 'bug)

(defcustom bug-patched-url nil
  "Try to load url patched for https proxy support"
  :type 'string
  :group 'bug)

(defcustom bug-default-instance ""
  "The default bug tracker to use"
  :type 'string
  :group 'bug)

(defcustom bug-instance-plist nil
  "A list of bug tracker instances to use.

Example:
'(:work   (:url \"https://bz.work.example\")
  :secure (:url \"https://bz.secure.example\" :authinfo \"~/.netrc\")
  :fun    (:url \"https://bz.fun.example\"
           :login \"username\" :password \"password\"))

The :work instance is either without auth, with auth-data in ~/.authinfo, or
behind basic auth with the url-package prompting for credentials

The :secure instance uses regular bz auth, with credentials stored in ~/.netrc.
It requires a call to (bug-login \"secure\") before you can modify bugs.

The :fun instance uses regular bz auth, with credentials stored inside the
configuration, which you should try to avoid for security reasons. It also
requires a call to (bug-login \"fun\") before you can modify bugs.
"
  :type 'sexp
  :group 'bug)

(defcustom bug-autoload-attachments nil
  "Controls autoloading of attachments when opening a bug"
  :type 'sexp
  :group 'bug)

(defcustom bug-autoload-comments t
  "Controls autoloading of comments when opening a bug"
  :type 'sexp
  :group 'bug)

(defcustom bug-data-directory (locate-user-emacs-file "bug-mode/")
  "The directory containing data files for the bug-mode package"
  :type 'string
  :group 'bug)

(defcustom bug-time-date-format-short "%m.%d.%Y %T"
  "Format string describing a short time/date string (for example,
for use in a list view).

For allowed variables, see
https://www.gnu.org/software/emacs/manual/html_node/elisp/Time-Parsing.html
"
  :type 'string
  :group 'bug)

(defcustom bug-time-date-format-long "%a %b %e %T %Y"
  "Format string describing a long time/date string (for example,
for use in a bug view).

For allowed variables, see
https://www.gnu.org/software/emacs/manual/html_node/elisp/Time-Parsing.html
"
  :type 'string
  :group 'bug)

;;;;;;
;; Customizable faces for bug-mode
;; Use M-x customize-group <ret> bug-faces <ret> to customize

(defgroup bug-faces nil "Face configuration for bug-mode")

(defface bug-section-header
  '((((class color) (background light)) :foreground "DarkGoldenrod4" :weight bold)
    (((class color) (background  dark)) :foreground "LightGoldenrod2" :weight bold))
  "Face used for section headers"
  :group 'bug-faces)

;; TODO: the headerline faces are not ideal
(defface bug-header-line
  '((((class color) (background light)) :background "grey95")
    (((class color) (background  dark)) :background "grey20"))
  "Face used to display a header line for bugs"
  :group 'bug-faces)

(defface bug-header-line-modified
  '((((class color) (background light)) :background "salmon4")
    (((class color) (background  dark)) :background "LightSalmon3"))
  "Face used to display a header line for modified bugs"
  :group 'bug-faces)

(defface bug-header-line-new
  '((((class color) (background light)) :background "salmon4")
    (((class color) (background  dark)) :background "LightSalmon3"))
  "Face used to display a header line for new bugs"
  :group 'bug-faces)

(defface bug-header-field
  '((((class color) (background light)) :foreground "DarkGoldenrod4" :weight bold)
    (((class color) (background  dark)) :foreground "LightGoldenrod2" :weight bold))
  "Face used for a header field in bug-list-mode"
  :group 'bug-faces)

(defface bug-field-description
  '((((class color) (background light)) :foreground "SkyBlue4")
    (((class color) (background dark))  :foreground "LightSkyBlue1"))
  "Face used for field names in bug-mode"
  :group 'bug-faces)

(defface bug-field-type-0
  '((t :inherit default))
  "Face used for unspecified field values"
  :group 'bug-faces)

(defface bug-field-type-1
  '((t :inherit bug-field-type-0))
  "Face used for free text field values"
  :group 'bug-faces)

(defface bug-field-type-2
  '((t :inherit bug-field-type-0))
  "Face used for drop down field values"
  :group 'bug-faces)

(defface bug-field-type-3
  '((t :inherit bug-field-type-0))
  "Face used for multiple selection field values"
  :group 'bug-faces)

(defface bug-field-type-4
  '((t :inherit bug-field-type-0))
  "Face used for large text field values"
  :group 'bug-faces)

(defface bug-field-type-5
  '((t :inherit bug-field-type-0))
  "Face used for date/time field values"
  :group 'bug-faces)

(defface bug-field-type-6
  '((((class color) (background light)) :box t :foreground "DarkOliveGreen4")
    (((class color) (background dark))  :box t :foreground "DarkSeaGreen2"))
  "Face used for bug ID field values"
  :group 'bug-faces)

(defface bug-field-type-7
  '((t :inherit bug-field-type-0))
  "Face used for bug url / 'see also' field values"
  :group 'bug-faces)

(defface bug-field-type-98
  '((t :inherit bug-field-type-0))
  "Face used for rally object field values"
  :group 'bug-faces)

(defface bug-field-type-99
  '((((class color) (background light)) :background "grey95")
    (((class color) (background dark))  :background "grey20"))
  "Face used for HTML field values"
  :group 'bug-faces)

(defvar bug-data-file (concat bug-data-directory "data")
  "The file containing saved searches and similar user data. Change bug-data-directory if you don't like the storage location")

(defvar bug-rally-url "https://rally1.rallydev.com/slm/webservice/v2.0/"
  "The URL to use for rally. This should only be changed if a different port is
required for proxy circumvention")

;;;;;;
;; Debug helpers

(defmacro bug--debug (body)
  `(if (and (boundp 'bug-debug) bug-debug)
       (let ((str ,body))
         (with-current-buffer (get-buffer-create "*bug-debug*")
           (goto-char (point-max))
           (insert str)
           (insert "\n")))))

(defun bug--debug-log-time (stamp)
  "Log timestamps to debug buffer if debugging is enabled

Measurement needs to be started with passing \"start\" as argument, and may be
explicitely stopped by passing \"stop\". For measurements in between use a
descriptive string"
  (if (and (boundp 'bug-debug) bug-debug)
      (cond ((string= stamp "start")
             (setq bug-debug-timestamp (current-time))
             (setq bug-debug-last-timestamp nil)
             (bug--debug
              (format "Starting new measurement at %s"
                      (current-time-string bug-debug-timestamp))))
            ((string= stamp "stop")
             (if (and (boundp 'bug-debug-timestamp)
                      bug-debug-timestamp)
                 (bug--debug
                  (format "Stopping measurement at %s, %f seconds after start"
                          (current-time-string)
                          (time-to-seconds
                           (time-subtract (current-time) bug-debug-timestamp))))
               (bug--debug "No measurement started"))
             (setq bug-debug-timestamp nil)
             (setq bug-debug-last-timestamp nil))
            (t (if (and (boundp 'bug-debug-timestamp)
                        bug-debug-timestamp)
                   (let ((format-string
                          (if (and (boundp 'bug-debug-last-timestamp)
                                   bug-debug-last-timestamp)
                              (format "Reached '%s' after %f seconds, %f after last"
                                      stamp
                                      (time-to-seconds
                                       (time-subtract (current-time)
                                                      bug-debug-timestamp))
                                      (time-to-seconds
                                       (time-subtract (current-time)
                                                      bug-debug-last-timestamp)))
                            (format "Reached '%s' after %f seconds"
                                    stamp
                                    (time-to-seconds
                                     (time-subtract (current-time)
                                                    bug-debug-timestamp))))))
                         (bug--debug format-string))
                     (bug--debug "No measurement started"))
               (setq bug-debug-last-timestamp (current-time))))))

;;;;;;
;; helper functions and variables for reading static data and user settings
(setq bug-remember-list (make-hash-table :test 'equal))

(defconst bug-json-data-dir
  (concat
   (file-name-directory (or load-file-name (buffer-file-name)))
   "/json")
  "Location of additional JSON data files used by bug-mode.")

(defun bug--write-data-file ()
  "Write user data to disk"
  (with-temp-buffer
    (insert ";; Foo\n")
    (insert "(setq bug-remember-list ")
    (pp bug-remember-list (current-buffer))
    (insert ")\n")
    (write-file bug-data-file)))

(defun bug--read-data-file ()
  "Restore user data from disk"
  (if (file-exists-p bug-data-file)
      (load (expand-file-name bug-data-file))))

(defun bug--backend-type (&optional instance)
  "Return the backend type for the given bug tracker instance"
  (let ((type (bug--instance-property :type instance)))
    (if (equal nil type)
        'bz
      type)))

(defun bug--list-columns (&optional instance)
  "Read the list headers for a bugtracker instance.

If the given instance does not have a :list-columns property defaults
are used.
"
  (if (bug--instance-property :list-columns instance)
      (bug--instance-property :list-columns instance)
    (cond
     ((equal 'rally (bug--backend-type instance))
      '("FormattedID" "Name" "LastUpdateDate"))
     (t
      '("id" "status" "summary" "last_change_time")))))

(defun bug--uuid-field-name (&optional instance)
  "Return the field used to uniquely identify an individual bug on a
specific instance"
  (if (bug--instance-property :bug-uuid instance)
      (bug--instance-property :bug-uuid instance)
    (cond
     ((equal 'rally (bug--backend-type instance))
      '_refObjectUUID)
     (t
      'id))))

;;;;;;
;; startup code to read persistent data
(bug--read-data-file)

(provide 'bug)
;;; bug.el ends here
