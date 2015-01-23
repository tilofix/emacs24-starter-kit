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
;; TILO: derive directly from another backend than ox-md,
;; to overcome issue of empty lines in output of a transcoded table.
(org-export-define-derived-backend 'dox 'ascii
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
                     ;; TILO: just take transcoder functions from ox-md
		     (bold . org-md-bold)
		     (code . org-md-verbatim)
		     (comment . (lambda (&rest args) ""))
		     (comment-block . (lambda (&rest args) ""))
		     (example-block . org-md-example-block)
		     (fixed-width . org-md-example-block)
		     (footnote-definition . ignore)
		     (footnote-reference . ignore)
		     (horizontal-rule . org-md-horizontal-rule)
		     (inline-src-block . org-md-verbatim)
		     (italic . org-md-italic)
		     (item . org-md-item)
		     (line-break . org-md-line-break)
		     (link . org-md-link)
		     (paragraph . org-md-paragraph)
		     (plain-list . org-md-plain-list)
		     (plain-text . org-md-plain-text)
		     (quote-block . org-md-quote-block)
		     (quote-section . org-md-example-block)
		     (section . org-md-section)
		     (src-block . org-md-example-block)
		     (verbatim . org-md-verbatim)
                     ;; TILO: add ox-dox' own transcoder functions
                     (headline . org-dox-headline)
                     (template . org-dox-template)
                     (inner-template . org-dox-inner-template)
                     (table . org-dox-table)
                     (table-cell . org-dox-table-cell)
                     (table-row . org-dox-table-row)
                     (strike-through . org-dox-strike-through)
                     )
  )


;;; Transcode Functions

;;;; Strike-through
(defun org-dox-strike-through (strike-through contents info)
  "Transcode STRIKE-THROUGH object into Markdown format.
CONTENTS is the text within strike-through markup.  INFO is a plist used as
a communication channel."
  ;; TILO: upps, "~~%s~~" it's not working but "<s>%s</s>" should, 
  ;; accroding to http://sourceforge.net/p/doxygen/wiki/markdown_syntax/.
  ;; TILO: did not work, lets try "<strike>%s</strike>", 
  ;; accoding to http://stackoverflow.com/questions/27827966/how-can-i-use-strikethrough-in-the-doxygens-markdown.
  ;; TILO: that's bad no strike-through in doxygen.
  (format "<del>%s</del>" contents)
  )

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

;;;; Table Cell

(defun org-dox-table-cell  (table-cell contents info)
  ;; TILO: code from org-confluence-table-cell.
  (concat
   " " contents " |")
  )

;;;; Table Row
(defun org-dox--table-row-build-sep (table-row info)
  ""
  ;; TILO: code from org-ascii-table-row.
  (concat
   "|"
   (apply
    'concat
    (org-element-map table-row 'table-cell
      (lambda (cell)
        (let ((width (length (org-export-data
                              (org-element-contents cell)
                              info))))
          (concat
           ;; add 2 because of leading and trailing space.
           (make-string (+ 2 width) (string-to-char "-"))
           "|"
           )))
      info))
   "\n")
  )

(defun org-dox-table-row  (table-row contents info)
  (when (eq (org-element-property :type table-row) 'standard)
    (concat
     ;; formats the leftmost char in a row.
     (format "|%s" contents)
     (when (org-export-table-row-ends-header-p table-row info)
       (concat
        "\n"
        (org-dox--table-row-build-sep table-row info)
        ))
    ))
  )

;;;; Table

(defun org-dox-table (table contents info)
  ;; TILO: code from org-confluence-table.
  contents)

(defun org-dox-identity (blob contents info)
  "Transcode BLOB element or object back into Org syntax.
CONTENTS is its contents, as a string or nil.  INFO is ignored."
  ;; TILO, copied from function "org-org-identity" from "ox-org"
  ;; PHP-MD-Extra syntax for a table is similar to that of org-mode
  ;; https://michelf.ca/projects/php-markdown/extra/#table
  ;; but why there are new-lines in the output?
  (org-export-expand blob contents nil))


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
(defun org-dox-export-as-doxydoc
    (&optional async subtreep visible-only body-only ext-plist)
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
    async subtreep visible-only body-only ext-plist (lambda () (fundamental-mode)))
)

;;;###autoload
(defun org-dox-export-to-doxydoc
    (&optional async subtreep visible-only body-only ext-plist)
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
    (org-export-to-file 'dox outfile
      async subtreep visible-only body-only ext-plist))
)

(provide 'ox-dox)
