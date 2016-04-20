;;; bz.el --- work with Bugzilla from within emacs
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

(add-to-list 'load-path
             (directory-file-name
              (concat
               (file-name-directory (or load-file-name (buffer-file-name)))
               "lisp")))

(require 'bz-autoloads)

;;;;;;
;; Customizable variables for bz-mode
;; Use M-x customize-group <ret> bz <ret> to customize

(defgroup bz nil "bz-mode related settings")

(defcustom bz-debug nil
  "Configure debugging to *bz-debug* buffer"
  :type 'string
  :group 'bz)

(defcustom bz-default-instance ""
  "The default bugzilla to use"
  :type 'string
  :group 'bz)

(defcustom bz-instance-plist nil
  "A list of bugzilla instances to use.

Example:
'(:work   (:url \"https://bz.work.example\")
  :secure (:url \"https://bz.secure.example\" :authinfo \"~/.netrc\")
  :fun    (:url \"https://bz.fun.example\" :login \"username\" :password \"password\"))

The :work instance is either without auth, with auth-data in ~/.authinfo, or behind basic auth with the url-package prompting for credentials

The :secure instance uses regular bz auth, with credentials stored in ~/.netrc. It requires a call to (bz-login \"secure\") before you can modify bugs

The :fun instance uses regular bz auth, with credentials stored inside the configuration, which you should try to avoid for security reasons. It also requires a call to (bz-login \"fun\") before you can modify bugs
"
  :type 'sexp
  :group 'bz)

(defcustom bz-autoload-attachments nil
  "Controls autoloading of attachments when opening a bug"
  :type 'sexp
  :group 'bz)

(defcustom bz-autoload-comments t
  "Controls autoloading of comments when opening a bug"
  :type 'sexp
  :group 'bz)

(defcustom bz-data-directory (locate-user-emacs-file "bz/")
  "The directory containing data files for the bz package"
  :type 'string
  :group 'bz)

(defcustom bz-time-date-format-short "%m.%d.%Y %T"
  "Format string describing a short time/date string (for example,
for use in a list view).

For allowed variables, see
https://www.gnu.org/software/emacs/manual/html_node/elisp/Time-Parsing.html
"
  :type 'string
  :group 'bz)

(defcustom bz-time-date-format-long "%a %b %e %T %Y"
  "Format string describing a long time/date string (for example,
for use in a bug view).

For allowed variables, see
https://www.gnu.org/software/emacs/manual/html_node/elisp/Time-Parsing.html
"
  :type 'string
  :group 'bz)

;;;;;;
;; Customizable faces for bz-mode
;; Use M-x customize-group <ret> bz-faces <ret> to customize

(defgroup bz-faces nil "Face configuration for bz-mode")

(defface bz-bug-header-field
  '((((class color) (background light)) :foreground "DarkGoldenrod4" :weight bold)
    (((class color) (background  dark)) :foreground "LightGoldenrod2" :weight bold))
  "Face used for a header field in bz-list-mode"
  :group 'bz-faces)

(defface bz-bug-field-description
  '((((class color) (background light)) :foreground "SkyBlue4")
    (((class color) (background dark))  :foreground "LightSkyBlue1"))
  "Face used for field names in bz-bug-mode"
  :group 'bz-faces)

(defface bz-bug-field-type-0
  '((t :inherit default))
  "Face used for unspecified field values"
  :group 'bz-faces)

(defface bz-bug-field-type-1
  '((t :inherit bz-bug-field-type-0))
  "Face used for free text field values"
  :group 'bz-faces)

(defface bz-bug-field-type-2
  '((t :inherit bz-bug-field-type-0))
  "Face used for drop down field values"
  :group 'bz-faces)

(defface bz-bug-field-type-3
  '((t :inherit bz-bug-field-type-0))
  "Face used for multiple selection field values"
  :group 'bz-faces)

(defface bz-bug-field-type-4
  '((t :inherit bz-bug-field-type-0))
  "Face used for large text field values"
  :group 'bz-faces)

(defface bz-bug-field-type-5
  '((t :inherit bz-bug-field-type-0))
  "Face used for date/time field values"
  :group 'bz-faces)

(defface bz-bug-field-type-6
  '((((class color) (background light)) :box t :foreground "DarkOliveGreen4")
    (((class color) (background dark))  :box t :foreground "DarkSeaGreen2"))
  "Face used for bug ID field values"
  :group 'bz-faces)

(defface bz-bug-field-type-7
  '((t :inherit bz-bug-field-type-0))
  "Face used for bug url / 'see also' field values"
  :group 'bz-faces)

(defface bz-bug-field-type-98
  '((t :inherit bz-bug-field-type-0))
  "Face used for rally object field values"
  :group 'bz-faces)

(defface bz-bug-field-type-99
  '((((class color) (background light)) :background "grey95")
    (((class color) (background dark))  :background "grey20"))
  "Face used for HTML field values"
  :group 'bz-faces)

(defvar bz-data-file (concat bz-data-directory "data")
  "The file containing saved searches and similar user data. Change bz-data-directory if you don't like the storage location")

(defvar bz-rally-url "https://rally1.rallydev.com/slm/webservice/v2.0/"
  "The URL to use for rally. This should only be changed if a different port is
required for proxy circumvention")

;;;;;;
;; Debug helpers

(defmacro bz-debug (body)
  `(if (and (boundp 'bz-debug) bz-debug)
       (let ((str ,body))
         (with-current-buffer (get-buffer-create "*bz-debug*")
           (goto-char (point-max))
           (insert str)
           (insert "\n")))))

(defun bz-debug-log-time (stamp)
  "Log timestamps to debug buffer if debugging is enabled

Measurement needs to be started with passing \"start\" as argument, and may be
explicitely stopped by passing \"stop\". For measurements in between use a
descriptive string"
  (if (and (boundp 'bz-debug) bz-debug)
      (cond ((string= stamp "start")
             (setq bz-debug-timestamp (current-time))
             (setq bz-debug-last-timestamp nil)
             (bz-debug
              (format "Starting new measurement at %s"
                      (current-time-string bz-debug-timestamp))))
            ((string= stamp "stop")
             (if bz-debug-timestamp
                 (bz-debug
                  (format "Stopping measurement at %s, %f seconds after start"
                          (current-time-string)
                          (time-to-seconds
                           (time-subtract (current-time) bz-debug-timestamp))))
               (bz-debug "No measurement started"))
             (setq bz-debug-timestamp nil)
             (setq bz-debug-last-timestamp nil))
            (t (if bz-debug-timestamp
                   (let ((format-string
                          (if (and (boundp 'bz-debug-last-timestamp)
                                   bz-debug-last-timestamp)
                              (format "Reached '%s' after %f seconds, %f after last"
                                      stamp
                                      (time-to-seconds
                                       (time-subtract (current-time)
                                                      bz-debug-timestamp))
                                      (time-to-seconds
                                       (time-subtract (current-time)
                                                      bz-debug-last-timestamp)))
                            (format "Reached '%s' after %f seconds"
                                    stamp
                                    (time-to-seconds
                                     (time-subtract (current-time)
                                                    bz-debug-timestamp))))))
                         (bz-debug format-string))
                     (bz-debug "No measurement started"))
               (setq bz-debug-last-timestamp (current-time))))))

;;;;;;
;; helper functions and variables for reading static data and user settings
(setq bz-bug-remember-list (make-hash-table :test 'equal))

(defconst bz-json-data-dir
  (concat
   (file-name-directory (or load-file-name (buffer-file-name)))
   "/json")
  "Location of additional JSON data files used by bz.")

(defun bz-write-data-file ()
  "Write user data to disk"
  (with-temp-buffer
    (insert ";; Foo\n")
    (insert "(setq bz-bug-remember-list ")
    (pp bz-bug-remember-list (current-buffer))
    (insert ")\n")
    (write-file bz-data-file)))

(defun bz-read-data-file ()
  "Restore user data from disk"
  (if (file-exists-p bz-data-file)
      (load (expand-file-name bz-data-file))))

(defun bz-list-columns (&optional instance)
  "Read the list headers for a bugtracker instance.

If the given instance does not have a :list-columns property defaults
are used.
"
  (let ((type (bz-instance-property :type instance)))
    (if (bz-instance-property :list-columns instance)
        (bz-instance-property :list-columns instance)
      (cond
       ((string= type "rally")
        '("FormattedID" "Name" "LastUpdateDate"))
       (t
        '("id" "status" "summary" "last_change_time"))))))

(defun bz--uuid-field-name (&optional instance)
  "Return the field used to uniquely identify an individual bug on a
specific instance"
  (let ((type (bz-instance-property :type instance)))
    (if (bz-instance-property :bug-uuid instance)
        (bz-instance-property :bug-uuid instance)
      (cond
       ((string= type "rally")
        '_refObjectUUID)
       (t
        'id)))))

;;;;;;
;; startup code to read persistent data
(bz-read-data-file)

(provide 'bz)
;;; bz.el ends here
