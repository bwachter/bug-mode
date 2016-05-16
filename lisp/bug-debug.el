;; bug-debug.el --- debug helpers for bug-mode
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

(provide 'bug-debug)
;;; bug-debug.el ends here
