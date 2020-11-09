;;; zotero-fulltext.el --- Fulltext for the Zotero API  -*- lexical-binding: t; -*-

;; Author: Folkert van der Beek <folkertvanderbeek@gmail.com>

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;;;; Variables

(defvar zotero-fulltext-pandoc-mimetypes
  '(("application/epub+zip" . "epub")
    ("application/json" . "json")
    ("application/vnd.oasis.opendocument.text" . "odt")
    ("application/vnd.openxmlformats-officedocument.wordprocessingml.document" . "docx")
    ("application/x-troff-man" . "man")
    ("text/csv" ."csv")
    ("text/html" . "html")
    ("text/markdown" . "markdown")
    ("text/x-org" . "org")
    ("text/x-tex" . "latex"))
  "An alist of MIME content-types and corresponding input formats understood by pandoc.")

;;;; Customization

(defgroup zotero-fulltext nil
  "Fulltext indexing for Zotero"
  :group 'zotero)

(defcustom zotero-fulltext-pdftotext "pdftotext"
  "Executable for pdftotext.
Needed for fulltext indexing of PDF documents. It is freely
available and included by default with many Linux distributions,
and is also available for Windows as part of the Xpdf Windows
port."
  :group 'zotero-fulltext
  :type 'string)

(defcustom zotero-fulltext-pdfinfo "pdfinfo"
  "Executable for pdfinfo.
Needed for fulltext indexing of PDF documents. It is freely
available and included by default with many Linux distributions,
and is also available for Windows as part of the Xpdf Windows
port.

This variable is set by `zotero-fulltext-install-pdftools' after
downloading the PDF tools modified by Zotero. If you compile the
PDF tools from source, it should point to the \"pdfinfo-*\" binary
for your operating system."
  :group 'zotero-fulltext
  :type 'string
  :link '(url-link "https://github.com/zotero/cross-poppler"))

(defcustom zotero-fulltext-pandoc "pandoc"
  "Executable for pandoc executable."
  :group 'zotero-fulltext
  :type 'string)

(defcustom zotero-fulltext-antiword "antiword"
  "Executable for antiword executable."
  :group 'zotero-fulltext
  :type 'string)

(defcustom zotero-fulltext-max-chars 500000
  "How much text is indexed (default: 500000 characters)."
  :group 'zotero-fulltext
  :type 'integer)

(defcustom zotero-fulltext-max-pages 100
  "How much text is indexed (default: 100 pages)."
  :group 'zotero-fulltext
  :type 'integer)

;;;;; Indexing functions

(defun zotero-fulltext-index-pdf (file)
  "Convert Portable Document Format (PDF) to text.
Argument FILE is the file path to be indexed."
  (cond ((not (executable-find zotero-fulltext-pdftotext))
         (error "Executable %s not found" zotero-fulltext-pdftotext))
        ((not (executable-find zotero-fulltext-pdfinfo))
         (error "Executable %s not found" zotero-fulltext-pdfinfo))
        (t
         (let* ((total-pages (with-temp-buffer
                               (call-process zotero-fulltext-pdfinfo nil t nil file)
                               (goto-char (point-min))
                               (let* ((match (re-search-forward "Pages:[[:blank:]]+\\([[:digit:]]+\\)" nil t))
                                      (pages (match-string-no-properties 1)))
                                 (string-to-number pages))))
                (indexed-pages (if (> total-pages zotero-fulltext-max-pages) zotero-fulltext-max-pages total-pages))
                (content (with-temp-buffer
                           (call-process zotero-fulltext-pdftotext nil t nil "-l" (number-to-string indexed-pages) file "-" )
                           (buffer-string))))
           `(:content ,content :indexedPages ,indexed-pages :totalPages ,total-pages)))))

(defun zotero-fulltext-index-pandoc (file content-type)
  "Convert pandoc compatible markup format to text.

Argument FILE is the file path to be indexed. Argument
CONTENT-TYPE is the content-type."
  (cond ((not (executable-find zotero-fulltext-pandoc))
         (error "Executable %s not found" zotero-fulltext-pandoc))
        (t
         (let ((format (cdr (assoc content-type zotero-fulltext-pandoc-mimetypes))))
           (with-temp-buffer
             (call-process "pandoc" nil t nil "-f" format "-t" "plain" file)
             (let* ((content (if (> (buffer-size) zotero-fulltext-max-chars)
                                 (buffer-substring 1 (1+ zotero-fulltext-max-chars))
                               (buffer-string)))
                    (indexed-chars (length content))
                    (total-chars (buffer-size)))
               `(:content ,content :indexedChars ,indexed-chars :totalChars ,total-chars)))))))

(defun zotero-fulltext-index-antiword (file)
  "Convert MS Word version 2, 6, 7, 97, 2000 and 2003 to text.

Argument FILE is the file path to be indexed."
  (cond ((not (executable-find zotero-fulltext-antiword))
         (error "Executable %s not found" zotero-fulltext-antiword))
        (t
         (with-temp-buffer
           (call-process "antiword" nil t nil file)
           (let* ((content (if (> (buffer-size) zotero-fulltext-max-chars)
                               (buffer-substring 1 (1+ zotero-fulltext-max-chars))
                             (buffer-string)))
                  (indexed-chars (length content))
                  (total-chars (buffer-size)))
             `(:content ,content :indexedChars ,indexed-chars :totalChars ,total-chars))))))

(provide 'zotero-fulltext)

;;; zotero-fulltext.el ends here
