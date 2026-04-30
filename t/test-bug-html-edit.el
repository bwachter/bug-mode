;;; test-bug-html-edit.el --- tests for bug-html-edit -*- lexical-binding: t; -*-
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
;;; Code:

(require 'bug-html-edit)

;;;;;;
;; Helper

(defmacro bug--test-without-pandoc (&rest body)
  "Run BODY with pandoc disabled so tests use Emacs-native converters."
  `(cl-letf (((symbol-function 'executable-find)
               (lambda (prog) (if (equal prog "pandoc") nil
                               (executable-find prog)))))
     ,@body))

;;;;;;
;; bug--html-to-org tests

(ert-deftest test-html-to-org-paragraph ()
  "Plain paragraph text passes through."
  (bug--test-without-pandoc
   (when (fboundp 'libxml-parse-html-region)
     (should (equal "Hello world"
                    (bug--html-to-org "<p>Hello world</p>"))))))

(ert-deftest test-html-to-org-bold ()
  "Bold HTML becomes org bold markup."
  (bug--test-without-pandoc
   (when (fboundp 'libxml-parse-html-region)
     (should (equal "Hello *world*"
                    (bug--html-to-org "<p>Hello <b>world</b></p>"))))))

(ert-deftest test-html-to-org-italic ()
  "Italic HTML becomes org italic markup."
  (bug--test-without-pandoc
   (when (fboundp 'libxml-parse-html-region)
     (should (equal "/italic/"
                    (bug--html-to-org "<i>italic</i>"))))))

(ert-deftest test-html-to-org-underline ()
  "Underlined HTML becomes org underline markup."
  (bug--test-without-pandoc
   (when (fboundp 'libxml-parse-html-region)
     (should (equal "_underline_"
                    (bug--html-to-org "<u>underline</u>"))))))

(ert-deftest test-html-to-org-link ()
  "HTML anchor becomes org link."
  (bug--test-without-pandoc
   (when (fboundp 'libxml-parse-html-region)
     (should (equal "[[https://example.com][click here]]"
                    (bug--html-to-org "<a href=\"https://example.com\">click here</a>"))))))

(ert-deftest test-html-to-org-h1 ()
  "h1 becomes a top-level org heading."
  (bug--test-without-pandoc
   (when (fboundp 'libxml-parse-html-region)
     (should (string-prefix-p "* Title"
                              (bug--html-to-org "<h1>Title</h1>"))))))

(ert-deftest test-html-to-org-h2 ()
  "h2 becomes a second-level org heading."
  (bug--test-without-pandoc
   (when (fboundp 'libxml-parse-html-region)
     (should (string-prefix-p "** Title"
                              (bug--html-to-org "<h2>Title</h2>"))))))

(ert-deftest test-html-to-org-h3 ()
  "h3 becomes a third-level org heading."
  (bug--test-without-pandoc
   (when (fboundp 'libxml-parse-html-region)
     (should (string-prefix-p "*** Title"
                              (bug--html-to-org "<h3>Title</h3>"))))))

(ert-deftest test-html-to-org-no-strip-non-numbered ()
  "Headings without section-number pattern are unchanged."
  (bug--test-without-pandoc
   (when (fboundp 'libxml-parse-html-region)
     (should (string-prefix-p "** Title Without Number"
                              (bug--html-to-org "<h2>Title Without Number</h2>")))
     (should (string-prefix-p "** 3D Rendering"
                              (bug--html-to-org "<h2>3D Rendering</h2>"))))))

(ert-deftest test-html-to-org-list ()
  "HTML unordered list items become org list items."
  (bug--test-without-pandoc
   (when (fboundp 'libxml-parse-html-region)
     (let ((result (bug--html-to-org "<ul><li>Item 1</li><li>Item 2</li></ul>")))
       (should (string-match-p "- Item 1" result))
       (should (string-match-p "- Item 2" result))))))

(ert-deftest test-html-to-org-nil ()
  "nil input returns empty string."
  (bug--test-without-pandoc
   (should (equal "" (bug--html-to-org nil)))))

(ert-deftest test-html-to-org-empty ()
  "Empty string returns empty string."
  (bug--test-without-pandoc
   (should (equal "" (bug--html-to-org "")))))

;;;;;;
;; bug--org-to-html tests

(ert-deftest test-org-to-html-paragraph ()
  "Org text becomes an HTML paragraph."
  (bug--test-without-pandoc
   (when (require 'ox-html nil t)
     (let ((result (bug--org-to-html "Hello world")))
       (should (string-match-p "Hello world" result))
       (should (string-match-p "<p" result))))))

(ert-deftest test-org-to-html-bold ()
  "Org bold markup becomes HTML bold."
  (bug--test-without-pandoc
   (when (require 'ox-html nil t)
     (let ((result (bug--org-to-html "*bold*")))
       (should (string-match-p "<b>" result))
       (should (string-match-p "bold" result))))))

(ert-deftest test-org-to-html-italic ()
  "Org italic markup becomes HTML italic."
  (bug--test-without-pandoc
   (when (require 'ox-html nil t)
     (let ((result (bug--org-to-html "/italic/")))
       (should (string-match-p "<i>" result))
       (should (string-match-p "italic" result))))))

(ert-deftest test-org-to-html-heading-no-section-number ()
  "Org headings export without section numbers."
  (bug--test-without-pandoc
   (when (require 'ox-html nil t)
     (let ((result (bug--org-to-html "** My Heading")))
       (should (string-match-p "<h2" result))
       (should (string-match-p "My Heading" result))
       (should-not (string-match-p "section-number" result))
       (should-not (string-match-p "<span class=\"section-number" result))))))

(ert-deftest test-org-to-html-link ()
  "Org links become HTML anchors."
  (bug--test-without-pandoc
   (when (require 'ox-html nil t)
     (let ((result (bug--org-to-html "[[https://example.com][click here]]")))
       (should (string-match-p "<a " result))
       (should (string-match-p "href=\"https://example.com\"" result))
       (should (string-match-p "click here" result))))))

(ert-deftest test-org-to-html-list ()
  "Org list items become HTML list items."
  (bug--test-without-pandoc
   (when (require 'ox-html nil t)
     (let ((result (bug--org-to-html "- Item 1\n- Item 2")))
       (should (string-match-p "<li>" result))
       (should (string-match-p "Item 1" result))
       (should (string-match-p "Item 2" result))))))

(ert-deftest test-org-to-html-nil ()
  "nil input returns a minimal valid result."
  (bug--test-without-pandoc
   (let ((result (bug--org-to-html nil)))
     (should (stringp result)))))

(ert-deftest test-org-to-html-empty ()
  "Empty string returns a minimal valid result."
  (bug--test-without-pandoc
   (let ((result (bug--org-to-html "")))
     (should (stringp result)))))

;;;;;;
;; Round-trip tests

(ert-deftest test-html-roundtrip-paragraph ()
  "Plain paragraph content survives html->org->html round-trip."
  (bug--test-without-pandoc
   (when (and (fboundp 'libxml-parse-html-region)
              (require 'ox-html nil t))
     (let* ((html "<p>Hello world</p>")
            (via-org (bug--html-to-org html))
            (back-to-html (bug--org-to-html via-org)))
       (should (string-match-p "Hello world" back-to-html))))))

(ert-deftest test-html-roundtrip-bold ()
  "Bold text survives html->org->html round-trip."
  (bug--test-without-pandoc
   (when (and (fboundp 'libxml-parse-html-region)
              (require 'ox-html nil t))
     (let* ((html "<p><b>bold text</b></p>")
            (via-org (bug--html-to-org html))
            (back-to-html (bug--org-to-html via-org)))
       (should (string-match-p "<b>" back-to-html))
       (should (string-match-p "bold text" back-to-html))))))

(ert-deftest test-html-roundtrip-heading ()
  "Heading text survives html->org->html round-trip without doubled numbering."
  (bug--test-without-pandoc
   (when (and (fboundp 'libxml-parse-html-region)
              (require 'ox-html nil t))
     (let* ((html "<h2>1. My Section</h2>")
            (via-org (bug--html-to-org html))
            (back-to-html (bug--org-to-html via-org)))
       (should (string-match-p "<h2" back-to-html))
       (should (string-match-p "My Section" back-to-html))
       ;; Must not have doubled numbers like "1. 1. My Section"
       (should-not (string-match-p "1\\. 1\\." back-to-html))
       ;; Must not have org-export section numbers
       (should-not (string-match-p "section-number" back-to-html))))))

(ert-deftest test-html-roundtrip-link ()
  "Links survive html->org->html round-trip."
  (bug--test-without-pandoc
   (when (and (fboundp 'libxml-parse-html-region)
              (require 'ox-html nil t))
     (let* ((html "<a href=\"https://example.com\">Example</a>")
            (via-org (bug--html-to-org html))
            (back-to-html (bug--org-to-html via-org)))
       (should (string-match-p "https://example.com" back-to-html))
       (should (string-match-p "Example" back-to-html))))))

(provide 'test-bug-html-edit)
;;; test-bug-html-edit.el ends here
