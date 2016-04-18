;; bz-tests.el --- tests for bz
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

(require 'cl-lib)
(require 'ert)
(require 'json)

(load-file (concat
            (file-name-directory (or load-file-name (buffer-file-name)))
            "../bz.el"))

(defconst bz-test-data-dir
  (concat
   (file-name-directory (or load-file-name (buffer-file-name)))
   "/test-data/")
  "Location of test data files.")

(defmacro bz-with-dummy-config (&rest body)
  `(let ((bz-instance-plist '(:bz-1 (:url "https://bz.tracker1.example")
                                    :bz-2 (:url "https://bz.tracker2.example")
                                    :rally-1 (:api-key "thisIsNotAnApiKey"
                                                       :type "rally")
                                     ))
         (bz-default-instance :bz-2))
     ,@body))

;; TODO: structure tests to avoid requiring explicit requires
(require 'bz-common-functions)

(ert-deftest bz-test-query-functions ()
  "Test functions for handling query data"
  (let* ((data-file (concat bz-test-data-dir
                            "rally-query-result-two-bugs.json"))
         (results (cdr (assoc 'Results
                              (assoc 'QueryResult
                                     (json-read-file data-file))))))
    (should (equal (length results) 2))
    (should (equal 0
                   (bz-bug-position-in-array results 'FormattedID "TA815")))
    (should (equal 1
                   (bz-bug-position-in-array results 'FormattedID "US815")))
    (should (equal nil
                   (bz-bug-position-in-array results 'FormattedID "US850")))
    ))


(require 'bz-rpc)

(ert-deftest bz-test-properties ()
  "Test property gathering"
  (bz-with-dummy-config
   ;; check if dummy default property is set
   (should (equal bz-default-instance :bz-2))
   ;; check if stringp->symbolp conversion behaves as expected
   (should (equal (bz-instance-to-symbolp :foo) :foo))
   (should (equal (bz-instance-to-symbolp "foo") :foo))
   (should (equal (bz-instance-to-symbolp ":foo") :foo))
   ;; check retrieving a non-default property
   (should (equal (bz-instance-property :url :bz-1)
                  "https://bz.tracker1.example"))
   (should (equal (bz-instance-property :url "bz-1")
                  "https://bz.tracker1.example"))
   ;; check if retrieving default properties works
   (should (equal (bz-instance-property :url)
                  (bz-instance-property :url :bz-2)))))

(provide 'bz-tests)
;;; bz-tests.el ends here
