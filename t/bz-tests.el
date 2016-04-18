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

(load-file (concat
            (file-name-directory (or load-file-name (buffer-file-name)))
            "../bz.el"))

(defmacro bz-with-dummy-config (&rest body)
  `(let ((bz-instance-plist '(:bz-1 (:url "https://bz1.example.com")
                                    :bz-2 (:url "https://bz2.example.com")
                                    :rally-1 (:api-key "thisIsNotAnApiKey"
                                                       :type "rally")
                                     ))
         (bz-default-instance :bz-2))
    ,@body))

;; TODO: structure tests to avoid requiring explicit requires
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
                  "https://bz1.example.com"))
   (should (equal (bz-instance-property :url "bz-1")
                  "https://bz1.example.com"))
   ;; check if retrieving default properties works
   (should (equal (bz-instance-property :url)
                  (bz-instance-property :url :bz-2)))))

(provide 'bz-tests)
;;; bz-tests.el ends here
