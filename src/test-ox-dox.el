;;; test-ox-dox.el --- test suite for DOXYDOC Back-End for Org Export Engine

;; Author: Tilo Wirkner <tilo.wirkner at adtran dot com>
;; Keywords: outlines, markdown, doxygen

;;; Commentary:

;; This file implements regression test for Doxygen Document back-end
;; for Org generic exporter.

;;; Dependencies

;; load Org-test when the ORG_HOME environment variable is set
(when (getenv "ORG_HOME")
  (let ((org-test-dir (expand-file-name "testing" (getenv "ORG_HOME")))
        (org-tests-dir (expand-file-name "testing/lisp" (getenv "ORG_HOME"))))
    (when (file-directory-p org-test-dir)
      (add-to-list 'load-path org-test-dir)
      (require 'org-test))
    (when (file-directory-p org-tests-dir)
      (add-to-list 'load-path org-tests-dir)
      (require 'test-ox)))
;  (ert "\\(org\\|ob\\|ox\\)")
  (ert "\\(ox\\)"))

;;; org-dox-headline
(ert-deftest test-ox-dox/headline ()
  "Test `org-dox-headline' specifications."
  ;; If `:section-numbers' is nil, never number headlines.
  (should-not
   (org-test-with-parsed-data "* Headline"
     (org-element-map tree 'headline
       (lambda (h) (org-export-numbered-headline-p h info))
       (plist-put info :section-numbers nil))))
  ;; If `:section-numbers' is a number, only number headlines with
  ;; a level greater that it.
  (should
   (equal
    '(yes no)
    (org-test-with-parsed-data "* Headline 1\n** Headline 2"
      (org-element-map tree 'headline
	(lambda (h) (if (org-export-numbered-headline-p h info) 'yes 'no))
	(plist-put info :section-numbers 1)))))
  ;; Otherwise, headlines are always numbered.
  (should
   (org-test-with-parsed-data "* Headline"
     (org-element-map tree 'headline
       (lambda (h) (org-export-numbered-headline-p h info))
       (plist-put info :section-numbers t)))))

;;; org-dox-template
;;; org-dox-inner-template
;;; org-dox-table
;;; org-dox-table-cell
;;; org-dox-table-row
;;; org-dox-strike-through
