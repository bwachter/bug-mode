;; test-bug-auth.el --- tests for bug-auth
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

(require 'bug-auth)

(ert-deftest test-bug-auth ()
  "Test functions for handling authentication"
  (bug--with-dummy-config
   (should (equal '("bz1-user" "AiNee8cu")
                  (bug-credentials :bug-1)))
   (should (equal '("bz2-user" nil)
                  (bug-credentials :bug-2)))
   (should (equal '("rally-user" "rally-password")
                  (bug-credentials :rally-2)))
   (should-error (bug-credentials :bug-nil))
   ))

;; TODO:
;; - bug-login
;; - bug-logout

(provide 'test-bug-auth)
;;; test-bug-auth.el ends here
