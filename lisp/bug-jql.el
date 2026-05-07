;;; bug-jql.el --- JQL parser and translator for bug-mode -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2010-2026 bug-mode developers
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

(require 'cl-lib)
(require 'bug-instance)
(require 'bug-debug)

(defvar bug-jql-font-lock-keywords
  `((,(regexp-opt '("AND" "OR" "NOT" "ORDER BY" "ASC" "DESC" "IN") 'words) . font-lock-keyword-face)
    (,(regexp-opt '("=" "!=" "~" "!~" ">" ">=" "<" "<=") t) . font-lock-function-name-face)
    ("\".*?\"" . font-lock-string-face)
    ("^\\(Examples:\\|Operators:\\|JQL Syntax:\\)" 1 'bold)))

;;;;; Tokenizer

(defun bug--jql-tokenize (query)
  "Tokenize a JQL query string into a list of tokens.

Each token is a cons cell (TYPE . VALUE) where TYPE is one of:
  STRING, WORD, NUMBER, OP, LPAREN, RPAREN, COMMA, EOF."
  (let ((pos 0)
        (len (length query))
        (tokens nil))
    (while (< pos len)
      (let ((char (aref query pos)))
        (cond
         ;; Whitespace
         ((member char '(?\s ?\t ?\n ?\r))
          (setq pos (1+ pos)))

         ;; Quoted string
         ((= char ?\")
          (let ((start (1+ pos)))
            (setq pos (1+ pos))
            (while (and (< pos len) (not (= (aref query pos) ?\")))
              (setq pos (1+ pos)))
            (push (cons 'STRING (substring query start pos)) tokens)
            (setq pos (1+ pos))))

         ;; Number
         ((and (>= char ?0) (<= char ?9))
          (let ((start pos))
            (while (and (< pos len)
                        (or (and (>= (aref query pos) ?0) (<= (aref query pos) ?9))
                            (= (aref query pos) ?.)))
              (setq pos (1+ pos)))
            (push (cons 'NUMBER (string-to-number (substring query start pos)))
                  tokens)))

         ;; Multi-char operators
         ((and (= char ?\>) (< (1+ pos) len) (= (aref query (1+ pos)) ?=))
          (push (cons 'OP ">=") tokens)
          (setq pos (+ pos 2)))
         ((and (= char ?\<) (< (1+ pos) len) (= (aref query (1+ pos)) ?=))
          (push (cons 'OP "<=") tokens)
          (setq pos (+ pos 2)))
         ((and (= char ?!) (< (1+ pos) len) (= (aref query (1+ pos)) ?=))
          (push (cons 'OP "!=") tokens)
          (setq pos (+ pos 2)))
         ((and (= char ?!) (< (1+ pos) len) (= (aref query (1+ pos)) ?~))
          (push (cons 'OP "!~") tokens)
          (setq pos (+ pos 2)))

         ;; Single-char operators
         ((member char '(?= ?> ?< ?~))
          (push (cons 'OP (string char)) tokens)
          (setq pos (1+ pos)))

         ;; Parentheses and comma
         ((= char ?\()
          (push (cons 'LPAREN "(") tokens)
          (setq pos (1+ pos)))
         ((= char ?\))
          (push (cons 'RPAREN ")") tokens)
          (setq pos (1+ pos)))
         ((= char ?,)
          (push (cons 'COMMA ",") tokens)
          (setq pos (1+ pos)))

         ;; Word (identifier, keyword, or function name)
         ((or (and (>= char ?a) (<= char ?z))
              (and (>= char ?A) (<= char ?Z))
              (= char ?_))
          (let* ((start pos)
                 (_ (while (and (< pos len)
                                (or (and (>= (aref query pos) ?a) (<= (aref query pos) ?z))
                                    (and (>= (aref query pos) ?A) (<= (aref query pos) ?Z))
                                    (and (>= (aref query pos) ?0) (<= (aref query pos) ?9))
                                    (= (aref query pos) ?_)
                                    (= (aref query pos) ?.)))
                      (setq pos (1+ pos))))
                 (word (substring query start pos)))
            (cond
             ;; NOT IN multi-word operator
             ((and (string= word "NOT")
                   (< pos len)
                   (= (aref query pos) ?\s))
              (let* ((next-pos (1+ (string-match-p "[^\s]" query pos)))
                     (next-word (and next-pos (bug--jql-next-word query next-pos))))
                (if (and next-pos (string= (downcase next-word) "in"))
                    (progn
                      (setq pos (+ next-pos (length "in")))
                      (push (cons 'OP "NOT IN") tokens))
                  ;; NOT not followed by IN -> keyword
                  (push (cons 'KEYWORD (upcase word)) tokens))))

             ;; IN operator (check before keywords)
             ((string= (downcase word) "in")
              (push (cons 'OP "IN") tokens))

             ;; Keywords
             ((member (downcase word) '("and" "or" "not" "order" "by" "asc" "desc"))
              (push (cons 'KEYWORD (upcase word)) tokens))

             ;; Regular word
             (t
              (push (cons 'WORD word) tokens)))))

         ;; Skip unknown characters
         (t
          (setq pos (1+ pos))))))
    (nreverse (cons (cons 'EOF nil) tokens))))

(defun bug--jql-next-word (query pos)
  "Return the next word starting at POS in QUERY, or empty string if none."
  (let ((start pos)
        (len (length query)))
    (while (and (< pos len)
                (or (and (>= (aref query pos) ?a) (<= (aref query pos) ?z))
                    (and (>= (aref query pos) ?A) (<= (aref query pos) ?Z))
                    (and (>= (aref query pos) ?0) (<= (aref query pos) ?9))
                    (= (aref query pos) ?_)))
      (setq pos (1+ pos)))
    (substring query start pos)))

;;;;; Parser helpers

(defun bug--jql-peek (tokens)
  "Return the type of the next token without consuming it."
  (caar tokens))

(defun bug--jql-consume (tokens expected-type)
  "Consume and return the next token if it matches EXPECTED-TYPE.
Signal an error otherwise."
  (let ((token (pop tokens)))
    (if (equal (car token) expected-type)
        (cons (cdr token) tokens)
      (error "JQL parse error: expected %s, got %s (%s)"
             expected-type (car token) (cdr token)))))

(defun bug--jql-match-keyword (tokens keywords)
  "If the next token is a KEYWORD matching one of KEYWORDS, consume it.
Return (t . remaining-tokens) or (nil . tokens)."
  (if (and (equal (caar tokens) 'KEYWORD)
           (member (cdar tokens) keywords))
      (cons t (cdr tokens))
    (cons nil tokens)))

;;;;; Recursive descent parser

;; Grammar:
;;   query        := clauses order-by?
;;   clauses      := clause ((AND | OR) clause)*
;;   clause       := NOT clause | field operator value | LPAREN clauses RPAREN
;;   field        := WORD
;;   operator     := = | != | ~ | !~ | > | < | >= | <= | IN | NOT IN
;;   value        := STRING | NUMBER | WORD | function
;;   function     := WORD LPAREN (value (COMMA value)*)? RPAREN
;;   order-by     := ORDER BY field (ASC|DESC)? (COMMA field (ASC|DESC)?)*

(defun bug--jql-parse-query (tokens)
  "Parse a JQL query from TOKENS.
Return ((:clauses CLAUSE-TREE) (:order-by ORDER-LIST)) and remaining tokens."
  (let* ((clauses-result (bug--jql-parse-clauses tokens))
         (clauses (car clauses-result))
         (tokens1 (cdr clauses-result))
         (order-result (bug--jql-parse-order-by tokens1))
         (order-by (car order-result))
         (tokens2 (cdr order-result)))
    (cons (list (cons :clauses clauses)
                (cons :order-by order-by))
          tokens2)))

(defun bug--jql-parse-clauses (tokens)
  "Parse clauses with left-associative AND/OR."
  (let* ((left-result (bug--jql-parse-clause tokens))
         (left (car left-result))
         (tokens1 (cdr left-result)))
    (catch 'bug--jql-clauses-done
      (while t
        (let ((kw-result (bug--jql-match-keyword tokens1 '("AND" "OR"))))
          (unless (car kw-result)
            (throw 'bug--jql-clauses-done (cons left tokens1)))
          (let* ((op (if (equal (cdar tokens1) "AND") :and :or))
                 (tokens2 (cdr kw-result))
                 (right-result (bug--jql-parse-clause tokens2))
                 (right (car right-result))
                 (tokens3 (cdr right-result)))
            (setq left (list op left right)
                  tokens1 tokens3)))))))

(defun bug--jql-parse-clause (tokens)
  "Parse a single clause."
  (let ((kw-result (bug--jql-match-keyword tokens '("NOT"))))
    (if (car kw-result)
        ;; NOT clause
        (let* ((tokens1 (cdr kw-result))
               (clause-result (bug--jql-parse-clause tokens1))
               (clause (car clause-result))
               (tokens2 (cdr clause-result)))
          (cons (list :not clause) tokens2))
      ;; Check for parenthesized group
      (if (equal (caar tokens) 'LPAREN)
          (let* ((tokens1 (cdr tokens)) ;; consume LPAREN
                 (group-result (bug--jql-parse-clauses tokens1))
                 (group-clauses (car group-result))
                 (tokens2 (cdr group-result))
                 (tokens3 (cdr (bug--jql-consume tokens2 'RPAREN))))
            (cons (list :group group-clauses) tokens3))
        ;; field operator value
        (let* ((field-result (bug--jql-parse-field tokens))
               (field (car field-result))
               (tokens1 (cdr field-result))
               (op-result (bug--jql-consume tokens1 'OP))
               (op (car op-result))
               (tokens2 (cdr op-result))
               (val-result (bug--jql-parse-value tokens2))
               (value (car val-result))
               (tokens3 (cdr val-result)))
          (cons (list :clause field op value) tokens3))))))

(defun bug--jql-parse-field (tokens)
  "Parse a field name."
  (let* ((token (bug--jql-consume tokens 'WORD))
         (word (car token))
         (tokens1 (cdr token)))
    (cons word tokens1)))

(defun bug--jql-parse-value (tokens)
  "Parse a value: string, number, word, or function call."
  (let ((next-type (caar tokens)))
    (cond
     ((equal next-type 'STRING)
      (let ((val (cdar tokens)))
        (cons val (cdr tokens))))
     ((equal next-type 'NUMBER)
      (let ((val (cdar tokens)))
        (cons val (cdr tokens))))
     ((equal next-type 'WORD)
      (let ((word (cdar tokens))
            (tokens1 (cdr tokens)))
        ;; Check for function call: word(
        (if (equal (caar tokens1) 'LPAREN)
            (bug--jql-parse-function-call word tokens1)
          (cons word tokens1))))
     ((equal next-type 'KEYWORD)
      ;; Some keywords can be values (e.g. "Empty" in JQL)
      (let ((kw (cdar tokens))
            (tokens1 (cdr tokens)))
        (cons kw tokens1)))
     (t
      (error "JQL parse error: unexpected token %s" (car tokens))))))

(defun bug--jql-parse-function-call (name tokens)
  "Parse a function call: NAME ( arg1, arg2, ... )."
  (let* ((tokens1 (cdr tokens)) ;; consume LPAREN
         args)
    (unless (equal (caar tokens1) 'RPAREN)
      (cl-loop
       (let* ((val-result (bug--jql-parse-value tokens1))
              (val (car val-result))
              (tokens2 (cdr val-result)))
         (push val args)
         (setq tokens1 tokens2)
         (if (equal (caar tokens1) 'COMMA)
             (setq tokens1 (cdr tokens1))
           (cl-return)))))
    (let ((tokens2 (cdr tokens1))) ;; consume RPAREN
      (cons (cons :function (cons name (nreverse args))) tokens2))))

(defun bug--jql-parse-order-by (tokens)
  "Parse ORDER BY clause, or return nil if not present."
  (let ((order-result (bug--jql-match-keyword tokens '("ORDER"))))
    (if (car order-result)
        (let* ((tokens1 (cdr order-result))
               (by-result (bug--jql-match-keyword tokens1 '("BY")))
               (tokens2 (if (car by-result)
                            (cdr by-result)
                          tokens1))
               (order-fields nil))
          (catch 'bug--jql-order-done
            (while t
              (let* ((field-result (bug--jql-parse-field tokens2))
                     (field (car field-result))
                     (tokens3 (cdr field-result))
                     (dir-result (bug--jql-match-keyword tokens3 '("ASC" "DESC"))))
                (push (cons field (if (car dir-result) (cdar tokens3) "ASC"))
                      order-fields)
                (setq tokens2 (if (car dir-result) (cdr dir-result) tokens3))
                (unless (equal (caar tokens2) 'COMMA)
                  (throw 'bug--jql-order-done (cons (nreverse order-fields) tokens2)))
                (setq tokens2 (cdr tokens2))))))
      (cons nil tokens))))

;;;;; Public parser entry point

;;;###autoload
(defun bug--parse-jql-query (query)
  "Parse a JQL query string into an AST.

Returns a list with keys :clauses and :order-by.
Clauses are represented as nested lists:
  (:clause FIELD OP VALUE)     -- single condition
  (:and LEFT RIGHT)           -- logical AND
  (:or LEFT RIGHT)            -- logical OR
  (:not CLAUSE)               -- negation
  (:group CLAUSES)            -- parenthesized group
ORDER-BY is a list of (FIELD . DIRECTION) cons cells."
  (let* ((tokens (bug--jql-tokenize query))
         (result (bug--jql-parse-query tokens)))
    (unless (equal (caar (cdr result)) 'EOF)
      (error "JQL parse error: unexpected tokens after query"))
    (let ((ast (car result)))
      (bug--debug (format "JQL AST: %S" ast) '(search . 2))
      ast)))

;;;;; Field name translation

;;;###autoload
(defun bug--jql-field-name (field-name instance)
  "Return the backend-specific field name for a JQL generic FIELD-NAME.

Generic field names follow the Jira standard field names:
  text        -- full text search (maps to summary/title/Name)
  summary     -- title/summary field
  status      -- workflow status
  assignee    -- person assigned
  reporter    -- person who created the item
  creator     -- alias for reporter (also accepted)
  priority    -- priority level
  issuetype   -- work item type (also accepts `type' for convenience)
  project     -- project name or key
  created     -- creation date/time
  updated     -- last update date/time
  description -- description field
  labels      -- tags or labels
  key         -- issue key / friendly identifier
  id          -- numeric or UUID identifier
  component   -- product/component
  resolution  -- resolution state

Native (backend-specific) field names are supported via the `native.'
prefix.  For example, `native.Iteration' passes `Iteration' through
unchanged without translation or validation."
  (if (string-prefix-p "native." field-name)
      (substring field-name (length "native."))
    (let* ((normalized (pcase (downcase field-name)
                         ("issuetype" "type")
                         ("creator" "reporter")
                         (_ field-name)))
           (generic (intern (concat ":jql-" normalized)))
           (backend-name (bug--instance-backend-function-optional
                          "bug--%s-field-name" generic instance)))
      (if backend-name
          (symbol-name backend-name)
        (error "Unknown JQL field '%s'. Use native.FIELD for backend-specific fields."
               field-name)))))

;;;;; AST translation (replace generic field names)

(defun bug--jql-translate-clause (clause instance)
  "Walk CLAUSE AST, replacing generic field names with backend-specific ones.
INSTANCE selects the backend."
  (pcase (car clause)
    (:clause
     (let ((field (nth 1 clause))
           (op (nth 2 clause))
           (value (nth 3 clause)))
       (list :clause (bug--jql-field-name field instance) op value)))
    (:and
     (list :and (bug--jql-translate-clause (nth 1 clause) instance)
           (bug--jql-translate-clause (nth 2 clause) instance)))
    (:or
     (list :or (bug--jql-translate-clause (nth 1 clause) instance)
           (bug--jql-translate-clause (nth 2 clause) instance)))
    (:not
     (list :not (bug--jql-translate-clause (nth 1 clause) instance)))
    (:group
     (list :group (bug--jql-translate-clause (nth 1 clause) instance)))
    (_ clause)))

;;;###autoload
(defun bug--jql-translate-query (jql-ast instance)
  "Translate generic field names in JQL-AST to backend-specific names.
Returns a new AST with field names translated for INSTANCE."
  (let* ((clauses (cdr (assoc :clauses jql-ast)))
         (order-by (cdr (assoc :order-by jql-ast)))
         (translated (list (cons :clauses (bug--jql-translate-clause clauses instance))
                           (cons :order-by
                                 (mapcar (lambda (o)
                                           (cons (bug--jql-field-name (car o) instance)
                                                 (cdr o)))
                                         order-by)))))
    (bug--debug (format "JQL translated AST: %S" translated) '(search . 2))
    translated))


(defun bug--jql-usage-hints ()
  "Return read-only hint text for the JQL query minibuffer."
  (concat
   "JQL Syntax:  field operator value [AND|OR ...] [ORDER BY field ASC|DESC]\n"
   "Operators: =  !=  ~  !~  >  >=  <  <=  IN  NOT IN\n"
   "Keywords: AND  OR  NOT  ORDER BY  ASC  DESC\n"
   "Generic fields: text  summary  status  assignee  reporter  priority\n"
   "                 issuetype  project  created  updated  description\n"
   "                 labels  key  id  component  resolution\n"
   "Native fields:  native.FIELD passes FIELD through unchanged\n"
   "Functions: JQL functions are transformed to native keywords\n"
   "Examples:\n"
   "  status = \"Open\" AND assignee = \"alice\"\n"
   "  text ~ \"crash\" ORDER BY updated DESC\n"
   "  project = \"MyProject\" AND issuetype = \"Bug\"\n"
   "  native.Iteration = \"Sprint 1\"\n"
   "Values can be quoted strings, numbers, or lists: IN (\"a\", \"b\")\n"))

(defun bug--jql-read-with-hints (prompt &optional initial default-value)
  "Read a JQL query from the minibuffer, showing usage hints above it.

When `bug-jql-show-usage-hints' is non-nil, a temporary buffer with
JQL syntax help is displayed at the bottom of the frame while the
minibuffer is active.  The previous window configuration is restored
automatically when the minibuffer exits.

`initial' is inserted directly into the minibuffer (e.g. the current
query when editing).  `default-value' is available via M-n but not
inserted (e.g. the sample query for a new search)."
  (let ((hints-buf nil)
        (win-config nil))
    (minibuffer-with-setup-hook
        (lambda ()
          (when bug-jql-show-usage-hints
            (setq win-config (current-window-configuration))
            (setq hints-buf (get-buffer-create " *JQL hints*"))
            (with-current-buffer hints-buf
              (erase-buffer)
              (insert (bug--jql-usage-hints))
              (setq-local font-lock-defaults '(bug-jql-font-lock-keywords t))
              (font-lock-ensure)
              (goto-char (point-min)))
            (display-buffer hints-buf '(display-buffer-in-side-window
                                        (side . bottom)
                                        (slot . -1)
                                        (window-height . fit-window-to-buffer)
                                        (preserve-size . (nil . t))))
            (add-hook 'minibuffer-exit-hook
                      (lambda ()
                        (when (buffer-live-p hints-buf)
                          (kill-buffer hints-buf))
                        (when win-config
                          (set-window-configuration win-config)))
                      nil t)))
      (read-string prompt initial nil default-value))))

(provide 'bug-jql)
;;; bug-jql.el ends here
