;; test-bug-backend-rally.el --- tests for bug-backend-rally
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

(load-file (concat
            (file-name-directory (or load-file-name (buffer-file-name)))
            "test-init.el"))

(require 'bug-backend-rally)

(ert-deftest test-bug-backend-rally-auth-header ()
  "Test functions for checking the Rally auth header"
  (bug--with-dummy-config
   (should (equal '("zsessionid" . "thisIsNotAnApiKey")
                  (bug--rpc-rally-auth-header :rally-1)))
   (should (equal '("Authorization" . "Basic cmFsbHktdXNlcjpyYWxseS1wYXNzd29yZA==")
                  (bug--rpc-rally-auth-header :rally-2)))
   ))

(ert-deftest test-bug-backend-rally-request-method ()
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

(ert-deftest test-bug-backend-rally-url-map-operation ()
  "Test url operation mapping"
  (bug--with-dummy-config
   ;; Test with API key instance (no security token appended)
   (should (equal "security/authorize"
                  (bug--rpc-rally-url-map-operation
                   '((operation . "authorize")
                     (resource . "security"))
                   :rally-1)))
   (should (equal "artifact/create"
                  (bug--rpc-rally-url-map-operation
                   '((object-id . "an-object-id")
                     (resource . "artifact")
                     (operation . "create")
                     (object-type . nil))
                   :rally-1)))
   (should (equal "artifact/an-object-id/copy"
                  (bug--rpc-rally-url-map-operation
                   '((object-id . "an-object-id")
                     (resource . "artifact")
                     (operation . "copy")
                     (object-type . nil)
                     (data . ((query "(FormattedID = \"foo\")"))))
                   :rally-1)))
   (should (equal "artifact/an-object-id"
                  (bug--rpc-rally-url-map-operation
                   '((object-id . "an-object-id")
                     (resource . "artifact")
                     (operation . "update")
                     (object-type . nil)
                     (data . ((query "(FormattedID = \"foo\")"))))
                   :rally-1)))
   (should (equal "artifact/an-object-id/type"
                  (bug--rpc-rally-url-map-operation
                   '((object-id . "an-object-id")
                     (resource . "artifact")
                     (operation . "update")
                     (object-type . "type")
                     (data . ((query "(FormattedID = \"foo\")"))))
                   :rally-1)))
   (should (equal "artifact/an-object-id"
                  (bug--rpc-rally-url-map-operation
                   '((object-id . "an-object-id")
                     (resource . "artifact")
                     (operation . "delete")
                     (object-type . nil)
                     (data . ((query "(FormattedID = \"foo\")"))))
                   :rally-1)))
   (should (equal "artifact/an-object-id/type"
                  (bug--rpc-rally-url-map-operation
                   '((object-id . "an-object-id")
                     (resource . "artifact")
                     (operation . "delete")
                     (object-type . "type")
                     (data . ((query "(FormattedID = \"foo\")"))))
                   :rally-1)))
   ;; Query operations encode the query string parameter
   (should (string-prefix-p "artifact?query="
                            (bug--rpc-rally-url-map-operation
                             '((object-id . "an-object-id")
                               (resource . "artifact")
                               (operation . "query")
                               (object-type . nil)
                               (data . ((query "(FormattedID = \"foo\")"))))
                             :rally-1)))
   (should (string-prefix-p "artifact?query="
                            (bug--rpc-rally-url-map-operation
                             '((object-id . "an-object-id")
                               (resource . "artifact")
                               (operation . "query")
                               (object-type . "type")
                               (data . ((query "(FormattedID = \"foo\")"))))
                             :rally-1)))
   (should (string-prefix-p "artifact?query="
                            (bug--rpc-rally-url-map-operation
                             '((resource . "artifact")
                               (operation . "query")
                               (data . ((query "(FormattedID = \"foo\")"))))
                             :rally-1)))
   ))

(ert-deftest test-bug-backend-rally-security-token-cache ()
  "Test security token caching with TTL"
  (bug--with-dummy-config
   ;; Clear cache first
   (bug-cache-clear :rally-2)

   ;; Test that API key instances don't need tokens
   (should (equal nil (bug--rally-ensure-security-token :rally-1)))

   ;; For basic auth instances, we can't test actual token retrieval without
   ;; a real Rally instance, but we can test the cache mechanism
   (let ((test-token "test-security-token-12345")
         (current-time (float-time)))
     ;; Manually set a token in cache
     (bug--cache-put 'security-token
                     (cons test-token current-time)
                     :rally-2)

     ;; Token should be returned from cache
     (should (equal test-token
                    (car (bug--cache-get 'security-token :rally-2)))))))

(ert-deftest test-bug-backend-rally-project-ref ()
  "Test project reference generation"
  (bug--with-dummy-config
   ;; Test with configured project-id
   (let ((bug-instance-plist
          '(:rally-with-project
            (:type rally
                   :api-key "test-key"
                   :project-id "12345"))))
     (should (equal "/project/12345"
                    (bug--rally-get-project-ref :rally-with-project))))))

(ert-deftest test-bug-backend-rally-url-with-security-token ()
  "Test that write operations get security token appended for basic auth"
  (bug--with-dummy-config
   ;; Set up a valid cached token for basic auth instance
   (bug--cache-put 'security-token
                   (cons "test-token-123" (float-time))
                   :rally-2)

   ;; Test create operation with basic auth (should have token)
   (let ((url (bug--rpc-rally-url-map-operation
               '((resource . "defect")
                 (operation . "create"))
               :rally-2)))
     (should (string-match "\\?key=test-token-123$" url)))

   ;; Test update operation with basic auth (should have token)
   (let ((url (bug--rpc-rally-url-map-operation
               '((resource . "defect")
                 (operation . "update")
                 (object-id . "abc123"))
               :rally-2)))
     (should (string-match "\\?key=test-token-123$" url)))

   ;; Test delete operation with basic auth (should have token)
   (let ((url (bug--rpc-rally-url-map-operation
               '((resource . "defect")
                 (operation . "delete")
                 (object-id . "abc123"))
               :rally-2)))
     (should (string-match "\\?key=test-token-123$" url)))

   ;; Test query operation (should NOT have token)
   (let ((url (bug--rpc-rally-url-map-operation
               '((resource . "defect")
                 (operation . "query")
                 (data . ((query "(Name = \"test\")"))))
               :rally-2)))
     (should-not (string-match "\\?key=" url)))))

(ert-deftest test-bug-backend-rally-type-name ()
  "Test Rally type name capitalization"
  (bug--with-dummy-config
   ;; Test proper camelCase for HierarchicalRequirement
   (should (equal "HierarchicalRequirement"
                  (bug--rally-type-name "hierarchicalrequirement")))
   (should (equal "HierarchicalRequirement"
                  (bug--rally-type-name "HierarchicalRequirement")))
   (should (equal "HierarchicalRequirement"
                  (bug--rally-type-name "HIERARCHICALREQUIREMENT")))
   ;; Test other types
   (should (equal "Defect"
                  (bug--rally-type-name "defect")))
   (should (equal "Task"
                  (bug--rally-type-name "task")))
   (should (equal "Project"
                  (bug--rally-type-name "project")))
   (should (equal "TestCase"
                  (bug--rally-type-name "testcase")))
   (should (equal "TestSet"
                  (bug--rally-type-name "testset")))
   ;; Test default behavior (capitalize only first letter)
   (should (equal "Customtype"
                  (bug--rally-type-name "customtype")))))

;; TODO:
;; - bug--rpc-rally (requires real Rally instance or mocking)
;; - bug--rpc-rally-handle-error
;; - bug--rpc-rally-get-fields
;; - bug--create-rally-bug (requires real Rally instance or mocking)
;; - bug--update-rally-bug (requires real Rally instance or mocking)
;; - bug--delete-rally-bug (requires real Rally instance or mocking)

(provide 'test-bug-backend-rally)
;;; test-bug-backend-rally.el ends here
