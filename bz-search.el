;;; bz-search.el --- handle Bugzilla searches
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

(require 'bz-search-common)
(require 'bz-list-mode)
(require 'bz-bug-mode)

;;;###autoload
(defun bz-search (query &optional instance)
  "Take a search query from the minibuffer and execute it"
  (interactive
   (if current-prefix-arg
       (list
        (read-string "Search query: " nil nil t)
        (bz-query-instance))
     (list (read-string "Search query: " nil nil t))))
  (bz-do-search `(,(bz-parse-query query)) instance))

;;;###autoload
(defun bz-search-multiple (&optional instance)
  "Take multiple details for a search query from the minibuffer in several
prompts and execute them"
  (interactive
   (if current-prefix-arg
       (list (bz-query-instance))))
  (let ((terms (make-hash-table :test 'equal))
        (term nil))
    (while (not (string= term ""))
      (setq term (read-from-minibuffer "query term: "))
      (if (not (string= term ""))
          (let* ((parsed (bz-parse-query term))
                 (key (car parsed))
                 (value (cdr parsed))
                 (current (gethash key terms)))
            (if current
                (if (vectorp current)
                    (puthash key (vconcat current (vector value)) terms)
                  (puthash key (vector current value) terms))
              (puthash key value terms)))))
    (bz-do-search terms instance)))

(defun bz-parse-query (query)
  "Parse the search query read from minibuffer"
  (if (string-match "^\\([^ ]+\\):\\(.+\\)$" query)
      `(,(match-string 1 query) . ,(match-string 2 query))
    (if (string-match "[:space:]*[0-9]+[:space:]*" query)
        `(id . ,(string-to-number query))
      `(summary . ,query))))

(provide 'bz-search)
;;; bz-search.el ends here
