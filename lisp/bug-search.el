;;; bug-search.el --- handle bug searches
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

(require 'bug-search-common)
(require 'bug-list-mode)
(require 'bug-mode)

;;;###autoload
(defun bug-stored-bugs (list-name &optional instance)
  "Display a stored list of bugs"
  (interactive
   (if current-prefix-arg
       (list (bug--query-remembered-lists) (bug--query-instance))
     (list
      (bug--query-remembered-lists))))
  (let* ((instance (bug--instance-to-symbolp instance))
         (lists-for-instance (gethash instance bug-remember-list))
         (list-entries (if lists-for-instance
                           (gethash list-name lists-for-instance))))
    (if list-entries
        (let ((query (make-hash-table :test 'equal)))
          (puthash "id" list-entries query)
          (bug--do-search query instance))
      (message (concat "List " list-name " not found")))
    ))

;;;###autoload
(defun bug-search (query &optional instance)
  "Take a search query from the minibuffer and execute it"
  (interactive
   (if current-prefix-arg
       (list
        (read-string "Search query: " nil nil t)
        (bug--query-instance))
     (list (read-string "Search query: " nil nil t))))
  (bug--debug-log-time "start")
  (bug--do-search
   (bug--backend-function "bug--parse-%s-search-query" query instance)
   instance))

;;;###autoload
(defun bug-search-multiple (&optional instance)
  "Take multiple details for a search query from the minibuffer in several
prompts and execute them"
  (interactive
   (if current-prefix-arg
       (list (bug--query-instance))))
  (let ((terms (make-hash-table :test 'equal))
        (term nil))
    (while (not (string= term ""))
      (setq term (read-from-minibuffer "query term: "))
      (if (not (string= term ""))
          (let* ((parsed (bug--parse-bz-search-query term))
                 (key (car parsed))
                 (value (cdr parsed))
                 (current (gethash key terms)))
            (if current
                (if (vectorp current)
                    (puthash key (vconcat current (vector value)) terms)
                  (puthash key (vector current value) terms))
              (puthash key value terms)))))
    (bug--do-search terms instance)))

(provide 'bug-search)
;;; bug-search.el ends here
