;; bug-search-bz.el --- Bugzilla specific search functions
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

;;;###autoload
(defun bug--do-bz-search (params &optional instance)
  "Execute a search query in Bugzilla.

This function takes a pre-parsed Bugzilla search query as argument.
"
  (bug--handle-search-response params
                               (bug-rpc `((resource . "Bug")
                                          (operation . "search")
                                          (post-data . ,params))
                                        instance)
                               instance))

;;;###autoload
(defun bug--parse-bz-search-query (query &optional instance)
  "Parse search query from minibuffer for Bugzilla"
  (if (string-match "^\\([^ ]+\\):\\(.+\\)$" query)
      `((,(match-string 1 query) . ,(match-string 2 query)))
    (if (string-match "[[:space:]]*[0-9]+[:space:]*" query)
        `((id . ,(string-to-number query)))
      `((summary . ,query)))))

(provide 'bug-search-bz)
;;; bug-search-bz.el ends here
