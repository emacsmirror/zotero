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
;; Functions to access and create full-text content of Zotero items. To index
;; documents external dependencies are needed. The pdftotext executable is
;; needed for PDFs, the antiword executable for Microsoft Word documents until
;; version 2003, and the pandoc executable for pandoc compatible markup formats.
;; See the variable `zotero-fulltext-pandoc-mimetypes' for a list of formats
;; understood by pandoc.

;;; Code:

;;;; Requirements

(require 'mailcap)
(require 'zotero)
(require 'zotero-json)

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
  "Executable for pandoc executable.
Pandoc is an open-source document converter that supports many
formats and is freely available for most operating systems."
  :group 'zotero-fulltext
  :type 'string
  :link '(url-link "http://pandoc.org/"))

(defcustom zotero-fulltext-antiword "antiword"
  "Executable for antiword executable.
Antiword is an open source reader for proprietary Microsoft Word
documents and is freely available for most operating systems."
  :group 'zotero-fulltext
  :type 'string
  :link '(url-link "http://www.winfield.demon.nl/"))

(defcustom zotero-fulltext-max-chars 500000
  "How much text is indexed (default: 500000 characters)."
  :group 'zotero-fulltext
  :type 'integer)

(defcustom zotero-fulltext-max-pages 100
  "How much text is indexed (default: 100 pages)."
  :group 'zotero-fulltext
  :type 'integer)

;;;; Functions

(defun zotero-fulltext--index-pdf (file)
  "Convert Portable Document Format (PDF) to text.
Argument FILE is the file path to be indexed.

An executable for pdftotext is needed."
  (cond ((not (executable-find zotero-fulltext-pdftotext))
         (error "Executable %s not found" zotero-fulltext-pdftotext))
        ((not (executable-find zotero-fulltext-pdfinfo))
         (error "Executable %s not found" zotero-fulltext-pdfinfo))
        (t
         (let* ((total-pages (with-temp-buffer
                               (call-process zotero-fulltext-pdfinfo nil t nil file)
                               (goto-char (point-min))
                               (re-search-forward "Pages:[[:blank:]]+\\([[:digit:]]+\\)" nil t)
                               (string-to-number (match-string-no-properties 1))))
                (indexed-pages (if (> total-pages zotero-fulltext-max-pages) zotero-fulltext-max-pages total-pages))
                (content (with-temp-buffer
                           (call-process zotero-fulltext-pdftotext nil t nil "-l" (number-to-string indexed-pages) file "-" )
                           (buffer-string))))
           `(:content ,content :indexedPages ,indexed-pages :totalPages ,total-pages)))))

(defun zotero-fulltext--index-pandoc (file content-type)
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

(defun zotero-fulltext--index-antiword (file)
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

(cl-defun zotero-fulltext-item (key &key type id api-key)
  "Return fulltext content of item KEY.

Keyword TYPE is \"user\" for your personal library, and \"group\"
for the group libraries. ID is the ID of the personal or group
library you want to access, that is the \"user ID\" or \"group
ID\". API-KEY is the Zotero API key."
  (zotero-request "GET" "item-fulltext" key :type type :id id :api-key api-key))

(cl-defun zotero-fulltext-create-item (key object &key type id api-key)
  "Create full-text content for item KEY.

OBJECT should be a a plist containing three props:
- `:content': the full-text content, and either
- `:indexedChars' and `:totalChars' for text documents, or
- `:indexedPages' and `:totalPages' for PDFs.

Keyword TYPE is \"user\" for your personal library, and \"group\"
for the group libraries. ID is the ID of the personal or group
library you want to access, that is the \"user ID\" or \"group
ID\". API-KEY is the Zotero API key."
  (let ((json (zotero-json-encode object)))
    (zotero-request "PUT" "item-fulltext" key
                    :type type
                    :id id
                    :api-key api-key
                    :headers '(("Content-Type" . "application/json"))
                    :data (encode-coding-string json 'utf-8))))

(cl-defun zotero-fulltext-index-item (key file &optional content-type &key type id api-key)
  "Create full-text content for item KEY.

FILE is the file path to be indexed. If optional argument
CONTENT-TYPE is not provided, it will be guessed.

Keyword TYPE is \"user\" for your personal library, and \"group\"
for the group libraries. ID is the ID of the personal or group
library you want to access, that is the \"user ID\" or \"group
ID\". API-KEY is the Zotero API key.

This is a convenient wrapper around `zotero-fulltext-create-item'
that is able to index a variety of file formats, including but
not limited to:
- Portable Document Format (PDF)
- OpenDocument (ODT)
- Microsoft Word version 2, 6, 7, 97, 2000 and 2003 (DOC)
- Office Open XML (DOCX)
- EPUB
- LaTeX
- Org-mode.

Return t if success, or nil if failed."
  (message "Indexing fulltext content...")
  (let* ((filename (file-name-nondirectory file))
         (path (expand-file-name file))
         (content-type (or content-type (mailcap-file-name-to-mime-type filename)))
         (object (cond ((equal content-type "application/pdf")
                        (zotero-fulltext--index-pdf path))
                       ((equal content-type "application/msword")
                        (zotero-fulltext--index-antiword path))
                       ((assoc content-type zotero-fulltext-pandoc-mimetypes)
                        (zotero-fulltext--index-pandoc path content-type))
                       (t
                        (user-error "Content-type \"%s\" is not supported" content-type)))))
    (if object
        (progn
          (message "Indexing fulltext content...done")
          (message "Updating fulltext content...")
          (let* ((result (zotero-fulltext-create-item key object :type type :id id :api-key api-key))
                 (status-code (zotero-response-status-code result)))
            ;; A status-code 204 means the item's full-text content was updated
            (if (eq status-code 204)
                (progn
                  (message "Updating fulltext content...done")
                  ;; Success: return t
                  t)
              (message "Updating fulltext content...failed")
              ;; Failed: return nil
              nil)))
      (message "Indexing fulltext content...failed")
      ;; Failed: return nil
      nil)))

(provide 'zotero-fulltext)

;;; zotero-fulltext.el ends here
