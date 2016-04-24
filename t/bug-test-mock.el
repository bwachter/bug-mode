;; bug-test-mock.el --- fake configuration and functions for testing
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

(defconst bug--test-data-dir
  (concat
   (file-name-directory (or load-file-name (buffer-file-name)))
   "/test-data/")
  "Location of test data files.")

(defmacro bug--with-dummy-config (&rest body)
  `(let ((bug-instance-plist `(:bug-1 (:url "https://bz.tracker1.example"
                                            :authinfo ,(concat bug--test-data-dir "netrc"))
                                      :bug-2 (:url "https://bz.tracker2.example"
                                                   :authinfo ,(concat bug--test-data-dir "netrc"))
                                    :rally-1 (:api-key "thisIsNotAnApiKey"
                                                       :type rally)
                                     ))
         (bug-default-instance :bug-2))
     ,@body))

(provide 'bug-test-mock)
;;; bug-test-mock.el ends here
