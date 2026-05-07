;; bug-debug.el --- debug helpers for bug-mode -*- lexical-binding: t; -*-
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

(require 'bug-custom)

(defvar bug-debug-last-timestamp)
(defvar bug-debug-timestamp)

(defun bug--debug-allowed-p (subsystem-level)
  "Return non-nil if a debug message for `subsystem-level' should be printed.

`subsystem-level' may be nil (always allowed when `bug-debug' is set),
a symbol `subsystem' (allowed if `subsystem' is in `bug-debug-subsystems'),
or a cons cell (subsystem . level) (allowed if `subsystem' is in
`bug-debug-subsystems' with a configured level >= `level')."
  (and (boundp 'bug-debug) bug-debug
       (or (null subsystem-level)
           (let* ((sub (if (consp subsystem-level) (car subsystem-level) subsystem-level))
                  (lvl (if (consp subsystem-level) (cdr subsystem-level) 1))
                  (cfg (assoc sub bug-debug-subsystems)))
             (and cfg (integerp (cdr cfg)) (<= lvl (cdr cfg)))))))

(defmacro bug--debug (body &optional subsystem-level)
  "Log `body' to the *bug-debug* buffer when debugging is enabled.

Optional `subsystem-level' controls filtering:
- nil          – print if `bug-debug' is non-nil (backward compatible)
- symbol       – print if subsystem is in `bug-debug-subsystems'
- (SYMBOL . N) – print if subsystem is active at level >= N"
  `(when (bug--debug-allowed-p ,subsystem-level)
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

(provide 'bug-debug)
;;; bug-debug.el ends here
