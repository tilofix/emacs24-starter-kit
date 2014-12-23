;;; ox-dox.el --- DOXYDOC Back-End for Org Export Engine

;; Author: Tilo Wirkner <tilo.wirkner at adtran dot com>
;; Keywords: outlines, markdown, doxygen

;;; Commentary:

;; This library implements a Doxygen Document back-end for Org generic exporter.
;; See Org manual for more information.

;;; Dependencies

(require 'ox-md)

;;; Code:


;;; User-Configurable Variables

(defgroup org-export-dox nil
  "Options specific to Doxygen Document Markdown export back-end."
  :tag "Org Doxygen Document Markdown"
  :group 'org-export
  :version "24.4"
  :package-version '(Org . "8.0"))

(defcustom org-dox-headline-style 'atx
  "Style used to format headlines.
This variable can be set to either `atx' or `setext'."
  :group 'org-export-dox
  :type '(choice
          (const :tag "Use \"atx\" style" atx)
          (const :tag "Use \"Setext\" style" setext)))



;;; Define Back-End
(org-export-define-derived-backend 'dox 'md
  :export-block '("MD")
  :menu-entry
  '(?y "Export to Doxygen Documents with Markdown"
       ((?Y "To temporary buffer"
            (lambda (a s v b) (org-dox-export-as-doxydoc a s v)))
        (?y "To file" (lambda (a s v b) (org-dox-export-to-doxydoc a s v)))
        (?o "To file and open"
            (lambda (a s v b)
              (if a (org-dox-export-to-doxydoc t s v)
                (org-open-file (org-dox-export-to-doxydoc nil s v)))))))
  :translate-alist '(
                     (headline . org-dox-headline)
                     (template . org-dox-template)
                     (inner-template . org-dox-inner-template)
                     (table . org-dox-table)
                     (table-cell . org-html-table-cell)
                     (table-row . org-html-table-row)
                     )
)


;;; Transcode Functions

;;;; Headline

(defun org-dox-headline (headline contents info)
  "Transcode HEADLINE element into Markdown format.
CONTENTS is the headline contents.  INFO is a plist used as
a communication channel."
  ;; TILO: Code from org-md-headline, extended with header-id.
  (unless (org-element-property :footnote-section-p headline)
    (let* ((level (org-export-get-relative-level headline info))
           (title (org-export-data (org-element-property :title headline) info))
           (todo (and (plist-get info :with-todo-keywords)
                      (let ((todo (org-element-property :todo-keyword
                                                        headline)))
                        (and todo (concat (org-export-data todo info) " ")))))
           (tags (and (plist-get info :with-tags)
                      (let ((tag-list (org-export-get-tags headline info)))
                        (and tag-list
                             (format "     :%s:"
                                     (mapconcat 'identity tag-list ":"))))))
           (priority (and 
                      (plist-get info :with-priority)
                      (let ((char (org-element-property :priority headline)))
                        (and char (format "[#%c] " char)))))
           ;; Headline text without tags.
           (heading (concat todo priority title))
           ;; Headline ID based on section number from org-html-headline.
           (section-number (mapconcat 'number-to-string
                                      (org-export-get-headline-number
                                       headline info) "-"))
           (ids (remove 'nil
                            (list (org-element-property :CUSTOM_ID headline)
                                  (concat "sec-" section-number)
                                  (org-element-property :ID headline))))
           (preferred-id (car ids))
           (prefix-section (file-name-base (plist-get info :input-file)))
           (header-id (format "{#%s_%s}" prefix-section preferred-id))
           )
      (cond
       ;; Cannot create a headline.  Fall-back to a list.
       ((or (org-export-low-level-p headline info)
            (not (memq org-dox-headline-style '(atx setext)))
            (and (eq org-dox-headline-style 'atx) (> level 6))
            (and (eq org-dox-headline-style 'setext) (> level 2)))
        (let ((bullet
               (if (not (org-export-numbered-headline-p headline info)) "-"
                 (concat (number-to-string
                          (car (last (org-export-get-headline-number
                                      headline info))))
                         "."))))
          (concat bullet (make-string (- 4 (length bullet)) ? ) heading tags
                  "\n\n"
                  (and contents
                       (replace-regexp-in-string "^" "    " contents)))))
       ;; Use "Setext" style.
       ((eq org-dox-headline-style 'setext)
        (concat heading tags " " header-id "\n"
                (make-string (length heading) (if (= level 1) ?= ?-))
                "\n\n"
                contents))
       ;; Use "atx" style.
       (t 
        (concat (make-string level ?#) " " heading tags " " header-id "\n\n" contents))))))


;;;; Template

(defun org-dox-template (contents info)
  "Return complete document string after Doxygen Document Markdown conversion.
CONTENTS is the transcoded contents string.  INFO is a plist used
as a communication channel."
  ;; TILO: code from org-html-template, reduced for dox.
  (concat

   ;; Opening document

   ;; Document title.

   contents

   ;; Closing document.
   )
)

;;;; Inner Template

(defun org-dox-inner-template (contents info)
  "Return body of document string after Doxy Document Markdown conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  ;; TILO: code from org-html-inner-template, reduced for dox.
  (concat

   ;; Table of contents.
   (let ((depth (plist-get info :with-toc)))
     (when depth (org-dox-toc depth info)))

   ;; Document contents.
   contents

   ;; Footnotes section.
   ;; (org-html-footnote-section info)
   )
)

;;;; Table

(defun org-dox-table-first-row-data-cells (table info)
  "Transcode the first row of TABLE.
INFO is a plist used as a communication channel."
  (let ((table-row
	 (org-element-map table 'table-row
	   (lambda (row)
	     (unless (eq (org-element-property :type row) 'rule) row))
	   info 'first-match))
	(special-column-p (org-export-table-has-special-column-p table)))
    (if (not special-column-p) (org-element-contents table-row)
      (cdr (org-element-contents table-row)))))

(defun org-dox-table (table contents info)
  "Transcode a TABLE element from Org to HTML.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
  (case (org-element-property :type table)
    ;; Case 1: table.el table.  Convert it using appropriate tools.
    (table.el (message "%s" "Case 1: table.el table is not supported."))
    ;; Case 2: Standard table.
    (t
     (let* ((label (org-element-property :name table))
	    (caption (org-export-get-caption table))
	    (number (org-export-get-ordinal
		     table info nil 'org-html--has-caption-p))
	    (attributes
	     (org-html--make-attribute-string
	      (org-combine-plists
	       (and label (list :id (org-export-solidify-link-text label)))
	       (and (not (org-html-html5-p info))
		    (plist-get info :html-table-attributes))
	       (org-export-read-attribute :attr_html table))))
	    (alignspec
	     (if (and (boundp 'org-html-format-table-no-css)
		      org-html-format-table-no-css)
		 "align=\"%s\"" "class=\"%s\""))
	    (table-column-specs
	     (function
	      (lambda (table info)
		(mapconcat
		 (lambda (table-cell)
		   (let ((alignment (org-export-table-cell-alignment
				     table-cell info)))
		     (concat
		      ;; Begin a colgroup?
		      (when (org-export-table-cell-starts-colgroup-p
			     table-cell info)
			"\n<colgroup>")
		      ;; Add a column.  Also specify it's alignment.
		      (format "\n%s"
			      (org-html-close-tag
			       "col" (concat " " (format alignspec alignment)) info))
		      ;; End a colgroup?
		      (when (org-export-table-cell-ends-colgroup-p
			     table-cell info)
			"\n</colgroup>"))))
		 (org-dox-table-first-row-data-cells table info) "\n")))))
       (format "<table%s>\n%s\n%s\n%s</table>"
	       (if (equal attributes "") "" (concat " " attributes))
	       (if (not caption) ""
		 (format (if org-html-table-caption-above
			     "<caption align=\"above\">%s</caption>"
			   "<caption align=\"bottom\">%s</caption>")
			 (concat
			  "<span class=\"table-number\">"
                          (format (org-html--translate "Table %d:" info) number)
			  "</span> " (org-export-data caption info))))
	       (funcall table-column-specs table info)
	       contents)))))




;;; Tables of Contents

(defun org-dox-toc (depth info)
  "Build a table of contents.
DEPTH is an integer specifying the depth of the table.  INFO is a
plist used as a communication channel.  Return the table of
contents as a string, or nil if it is empty."
  ;; TILO, this function will only be called by org-dox-inner-template
  ;; when with-toc is defined, so we don't need to test this.
  (concat "\n\\tableofcontents\n\n")
)



;;; Interactive function

;;;###autoload
(defun org-dox-export-as-doxydoc (&optional async subtreep visible-only)
  "Export current buffer to a Doxygen Document Markdown buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org DOXYDOC Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (org-export-to-buffer 'dox "*Org DOXYDOC Export*"
    async subtreep visible-only nil nil (lambda () (text-mode)))
)

;;;###autoload
(defun org-dox-export-to-doxydoc (&optional async subtreep visible-only)
  "Export current buffer to a Doxygen Document Markdown file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".dox" subtreep)))
    (org-export-to-file 'dox outfile async subtreep visible-only))
)

(provide 'ox-dox)
