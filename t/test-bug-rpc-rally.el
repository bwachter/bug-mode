;; test-bug-rpc-rally.el --- tests for bug-rpc-rally
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

(require 'bug-rpc)
(require 'bug-rpc-rally)

(ert-deftest test-bug-rpc-rally-auth-header ()
  "Test functions for checking the Rally auth header"
    (bug--with-dummy-config
     (should (equal '("zsessionid" . "thisIsNotAnApiKey")
                    (bug--rpc-rally-auth-header :rally-1)))
     (should (equal '("Authorization" . "Basic cmFsbHktdXNlcjpyYWxseS1wYXNzd29yZA==")
                    (bug--rpc-rally-auth-header :rally-2)))
     ))

(ert-deftest test-bug-rpc-rally-request-method ()
  "Test request method mapping"
  (bug--with-dummy-config
   (should (equal "DELETE"
                  (bug--rpc-rally-request-method "Delete")))
   (should (equal "DELETE"
                  (bug--rpc-rally-request-method "DeLete")))
   (should (equal "DELETE"
                  (bug--rpc-rally-request-method "delete")))
   (should (equal "GET"
                  (bug--rpc-rally-request-method "read")))
   (should (equal "GET"
                  (bug--rpc-rally-request-method "query")))
   (should (equal "GET"
                  (bug--rpc-rally-request-method "authorize")))
   (should (equal "POST"
                  (bug--rpc-rally-request-method "create")))
   (should (equal "POST"
                  (bug--rpc-rally-request-method "copy")))
   (should (equal "POST"
                  (bug--rpc-rally-request-method "update")))
   (should (equal "POST"
                  (bug--rpc-rally-request-method "anything")))
   ))

(ert-deftest test-bug-rpc-rally-url-map-operation ()
  "Test url operation mapping"
  (bug--with-dummy-config
   (should (equal "security/authorize"
                  (bug--rpc-rally-url-map-operation
                   '((operation . "authorize")
                     (resource . "security")))))
   (should (equal "artifact/create"
                  (bug--rpc-rally-url-map-operation
                   '((object-id . "an-object-id")
                     (resource . "artifact")
                     (operation . "create")
                     (object-type . nil)))))
   (should (equal "artifact/an-object-id/copy"
                  (bug--rpc-rally-url-map-operation
                   '((object-id . "an-object-id")
                     (resource . "artifact")
                     (operation . "copy")
                     (object-type . nil)
                     (query-data . ((query "(FormattedID = \"foo\")")))))))
   (should (equal "artifact/an-object-id"
                  (bug--rpc-rally-url-map-operation
                   '((object-id . "an-object-id")
                     (resource . "artifact")
                     (operation . "update")
                     (object-type . nil)
                     (query-data . ((query "(FormattedID = \"foo\")")))))))
   (should (equal "artifact/an-object-id/type"
                  (bug--rpc-rally-url-map-operation
                   '((object-id . "an-object-id")
                     (resource . "artifact")
                     (operation . "update")
                     (object-type . "type")
                     (query-data . ((query "(FormattedID = \"foo\")")))))))
   (should (equal "artifact/an-object-id"
                  (bug--rpc-rally-url-map-operation
                   '((object-id . "an-object-id")
                     (resource . "artifact")
                     (operation . "delete")
                     (object-type . nil)
                     (query-data . ((query "(FormattedID = \"foo\")")))))))
   (should (equal "artifact/an-object-id/type"
                  (bug--rpc-rally-url-map-operation
                   '((object-id . "an-object-id")
                     (resource . "artifact")
                     (operation . "delete")
                     (object-type . "type")
                     (query-data . ((query "(FormattedID = \"foo\")")))))))
   (should (equal "artifact?query=%28FormattedID%20%3D%20%22foo%22%29"
                  (bug--rpc-rally-url-map-operation
                   '((object-id . "an-object-id")
                     (resource . "artifact")
                     (operation . "query")
                     (object-type . nil)
                     (query-data . ((query "(FormattedID = \"foo\")")))))))
   (should (equal "artifact?query=%28FormattedID%20%3D%20%22foo%22%29"
                  (bug--rpc-rally-url-map-operation
                   '((object-id . "an-object-id")
                     (resource . "artifact")
                     (operation . "query")
                     (object-type . "type")
                     (query-data . ((query "(FormattedID = \"foo\")")))))))
   (should (equal "artifact?query=%28FormattedID%20%3D%20%22foo%22%29"
                  (bug--rpc-rally-url-map-operation
                   '((resource . "artifact")
                     (operation . "query")
                     (query-data . ((query "(FormattedID = \"foo\")")))))))
   ))

;; TODO:
;; - bug--rpc-rally
;; - bug--rpc-rally-handle-error
;; - bug--rpc-rally-get-fields

(provide 'test-bug-rpc-rally)
;;; test-bug-rpc-rally.el ends here
