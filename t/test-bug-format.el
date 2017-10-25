;; test-bug-format.el --- tests for bug-format
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

;; for `bug--get-fields' used by format functions
(require 'bug-rpc)
(require 'bug-format)

(ert-deftest test-bug-format-bool ()
  "Test bool formatting"
  (should (equal "no"
                 (bug--format-bool :json-false)))
  (should (equal "yes"
                 (bug--format-bool t)))
  (should (equal "whatever"
                 (bug--format-bool "whatever")))
  (should (equal "whatever"
                 (bug--format-bool 'whatever)))
   )

(ert-deftest test-bug-format-field-name ()
  "Test field name formatting"
  (bug--with-dummy-config
   (should (equal-including-properties #("(foo): " 0 7 (bug-field-name foo face bug-field-description))
                  (bug--format-field-name '(foo . bar) :rally-1)))
   (should (equal-including-properties #("ID: " 0 4 (bug-field-name FormattedID face bug-field-description))
                  (bug--format-field-name '(FormattedID . "US1234") :rally-1)))
  ))

(ert-deftest test-bug-format-field-value ()
  "Test field value formatting"
  ;; TODO: add test cases for all field types
  )

(ert-deftest test-bug-format-html ()
  "Test HTML formatting"
  (cond
   ((and (string-equal "24" (substring emacs-version 0 2))
         (< 3 (string-to-number (substring emacs-version 3 4))))
    (should (equal-including-properties "test"
                                        (bug--format-html "test")))
    (should (equal-including-properties #("test" 0 4 (face bold))
                                        (bug--format-html "<b>test</b>")))
    (should (equal-including-properties #("test" 0 4 (face italic))
                                        (bug--format-html "<i>test</i>"))))
   ;; TODO: re-implement those tests for shr in emacs >= 24.4
   ))

(ert-deftest test-bug-format-kv ()
  "Test key value formatting"
  (should (equal-including-properties
           #("foo: bar\n" 0 5 (face bug-field-description) 5 8 (face bug-field-type-0))
           (bug--format-kv "foo" "bar")))
  (should-error (bug--format-kv "foo" nil) :type 'wrong-type-argument)
  (should-error (bug--format-kv nil nil) :type 'wrong-type-argument)
  (should-error (bug--format-kv 'foo nil) :type 'wrong-type-argument)
  )

(ert-deftest test-bug-format-time-date ()
  "Test date time output"
  (should-error (bug--format-time-date "foo"))
  (should (equal "04.11.2016 13:57:39"
                 (bug--format-time-date "2016-04-11T13:57:39.939Z")))
  (should (equal "Mon Apr 11 13:57:39 2016"
                 (bug--format-time-date "2016-04-11T13:57:39.939Z" t)))
  )

(provide 'test-bug-format)
;;; test-bug-format.el ends here
