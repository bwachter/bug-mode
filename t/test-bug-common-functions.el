;; test-bug-common-functions.el --- tests for common functions
;;
;; Copyright (c) 2010-2017 bug-mode developers
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

;; common functions:
;; filter
;; bug--add-or-replace
;; bug--backend-feature
;; bug--backend-function
;; bug--backend-type
;; bug--position-in-array
;; bug--query-instance

;; caching functions:
;; bug--cache-put
;; bug--cache-get
;; bug--cache-clear
(ert-deftest test-bug-cache ()
  "Test functions for caching data"
  (bug--with-dummy-config
   (should-error (bug--cache-put 'foo "bar") :type 'wrong-number-of-arguments)
   (should (equal '(:bug-2 ((foo . "bar")))
                  (bug--cache-put 'foo "bar" nil)))
   (should (equal '(:bug-2 ((foobar . "baz") (foo . "bar")))
                  (bug--cache-put 'foobar "baz" nil)))
   (should (equal '(:bug-2 ((foobar . "baz") (foo . "barbaz")))
                  (progn (bug--cache-put 'foo "barbaz" nil) bug--cache)))
   (should (equal '(:bug-2 ((foobar . "baz") (foo . "barbaz")) :new ((bar . "baz")))
                  (bug--cache-put 'bar "baz" ':new)))
   (should (equal '(:new ((bar . "baz")))
                  (progn (bug-cache-clear ':bug-2) bug--cache)))
   (should (equal nil (progn (bug-cache-clear) bug--cache)))
   ))

;; bug--get-fields
;; bug--field-name
;; bug--get-field-property
;; bug--instance-property
;; bug--instance-to-symbolp
(ert-deftest test-bug-instance-to-symbolp ()
  "Test bug--instance-to-symbolp"
  (bug--with-dummy-config
   (should (equal ':bug-2
                  (bug--instance-to-symbolp nil)))
   (should (equal ':test
                  (bug--instance-to-symbolp ':test)))
   (should (equal 'test
                  (bug--instance-to-symbolp 'test)))
   (should (equal ':test
                  (bug--instance-to-symbolp "test")))
   (should (equal ':test
                  (bug--instance-to-symbolp ":test")))
   ))

;; bug--list-columns

;; keymap functions
;; bug--mode-default-quit-window

(provide 'test-bug-common-functions)
;;; test-bug-common-functions.el ends here
