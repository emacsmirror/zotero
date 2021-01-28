;;; zotero-recognize.el --- Recognize for the Zotero API  -*- lexical-binding: t; -*-

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
;; Functions to retrieve metadata PDF documents. PDFs are recognized using an
;; undocumented Zotero web service that operates on the first few pages of text
;; using extraction algorithms and known metadata from CrossRef. The Zotero
;; lookup service doesn't require a Zotero account, and data about the content
;; or results of searches are not logged.

;;; Code:

;;;; Requirements

(require 'url)
(require 'zotero)
(require 'zotero-json)

;;;; Variables

(defconst zotero-recognize-base-url "https://recognize.zotero.org")

(defconst zotero-recognize-pdftools-version "0.0.3")

(defconst zotero-recognize-pdftools-url (concat "https://zotero-download.s3.amazonaws.com/pdftools/pdftools-" zotero-recognize-pdftools-version ".tar.gz"))

;;;;; PDF tools

(defconst zotero-recognize-pdftools-version "0.0.3")

(defconst zotero-recognize-pdftools-url (concat "https://zotero-download.s3.amazonaws.com/pdftools/pdftools-" zotero-recognize-pdftools-version ".tar.gz"))

(defcustom zotero-recognize-pdftools-dir (concat zotero-directory "pdftools/")
  "The directory were PDF tools should be installed."
  :group 'zotero-recognize
  :type 'directory)

(defvar zotero-recognize-props
  '(:type :itemType
          :authors :creators
          :language :language
          :arxiv nil
          :container
          :year nil
          :keywords :tags
          :pages :pages
          :title :title
          :doi :DOI
          :timeMs nil))

(defcustom zotero-recognize-pdftotext "pdftotext"
  "Executable for pdftotext.
Needed for fulltext indexing of PDF documents. It is freely
available and included by default with many Linux distributions,
and is also available for Windows as part of the Xpdf Windows
port.

A modified version of pdftotext is needed for recognition of
metadata. This variable is set by `zotero-recognize-install-pdftools' after
downloading the PDF tools modified by Zotero. If you compile the
PDF-tools from source, it should point to the \"pdftotext-*\"
binary for your operating system."
  :group 'zotero-recognize
  :type 'string
  :link '(url-link "https://github.com/zotero/cross-poppler"))

(defcustom zotero-recognize-pdfdata ""
  "Path to the data directory of pdftools.
This variable is set by `zotero-recognize-install-pdftools' after
downloading the PDF tools modified by Zotero. If you compile the
PDF tools from source, it should point to the \"popper-data\"
directory."
  :group 'zotero-recognize
  :type 'directory
  :link '(url-link "https://github.com/zotero/cross-poppler"))

;;;; Functions

(defun zotero-recognize--pdftojson (file)
  "Wrapper around the pdftotext executable modified by Zotero.
Return JSON with metadata, layout and rich text of FILE."
  (if (executable-find zotero-recognize-pdftotext)
      (let ((tempfile (make-nearby-temp-file "recognize-pdf-cache" nil ".json")))
        (when (file-exists-p tempfile) (delete-file tempfile))
        (let ((status (call-process zotero-recognize-pdftotext nil nil nil "-json" "-l" "5" (expand-file-name file) tempfile)))
          (if (eq status 0)
              (prog1
                  (with-temp-buffer
                    (insert-file-contents tempfile)
                    (buffer-string))
                (delete-file tempfile))
            (error "Executable %s cannot output to json" zotero-recognize-pdftotext))))
    (error "Executable %s not found" zotero-recognize-pdftotext)))

(defun zotero-recognize--submit (json)
  "Return metadata recognized from JSON returned by `zotero-recognize--pdftojson'.

PDFs are recognized using an undocumented Zotero web service that
operates on the first few pages of text using extraction
algorithms and known metadata from CrossRef. The Zotero lookup
service doesn't require a Zotero account, and data about the
content or results of searches are not logged."
  (let* ((url (concat zotero-recognize-base-url "/recognize"))
         (url-request-method "POST")
         (url-request-data json)
         (url-request-extra-headers `(("Content-Type" . "application/json"))))
    (with-current-buffer (url-retrieve-synchronously url nil nil zotero-timeout)
      (funcall #'zotero-handle-response))))

(defun zotero-recognize-install-pdftools ()
  "Install the PDF tools modified by Zotero.
The executables are modified to output a preprocessed JSON that
contains rich and structured information about the PDF and the
text extracted from it, for use with the PDF recognizer.

This function downloads and extracts the binaries available for
macOS, Windows and Linux. You can change the installation
directory by setting `zotero-recognize-pdftools-dir' to an appropriate
value before calling this function.

If there are no binaries available for your operating system, you
should compile them from source and set the variables
`zotero-recognize-pdftotext', `zotero-recognize-pdfinfo', and
`zotero-recognize-pdfdata' to the corresponding paths. The source is
available at URL `https://github.com/zotero/cross-poppler'."
  (interactive)
  (let ((filename (concat temporary-file-directory (file-name-nondirectory zotero-recognize-pdftools-url))))
    (unless (file-exists-p filename)
      (message "Downloading %s..." zotero-recognize-pdftools-url)
      (url-copy-file zotero-recognize-pdftools-url filename t)
      (message "Downloading %s...done" zotero-recognize-pdftools-url))
    (if (executable-find "tar")
        (let ((pdftotext-filename "pdftotext")
              (pdfinfo-filename "pdfinfo"))
          (pcase system-type
            ('darwin
             (setq pdftotext-filename (concat pdftotext-filename "-mac"))
             (setq pdfinfo-filename (concat pdfinfo-filename "-mac")))
            ('gnu/linux
             (let* ((arch (intern (car (split-string system-configuration "-" t))))
                    (suffix (if (eq arch 'x86_64) "-linux-x86_64" "-linux-i686")))
               (setq pdftotext-filename (concat pdftotext-filename suffix))
               (setq pdfinfo-filename (concat pdfinfo-filename suffix))))
            ('windows-nt
             (setq pdftotext-filename (concat pdftotext-filename "-win.exe"))
             (setq pdfinfo-filename (concat pdfinfo-filename "-win.exe")))
            (_
             (error "No binaries for operating system %S available" system-type)))
          (unless (file-directory-p zotero-recognize-pdftools-dir)
            (make-directory zotero-recognize-pdftools-dir))
          (dolist (member `(,pdftotext-filename ,pdfinfo-filename "poppler-data"))
            (message "Extracting %s from %s..." member filename)
            (let ((status (call-process "tar" nil nil nil "-xz" "-f" filename "-C" zotero-recognize-pdftools-dir member)))
              (if (eq status 0)
                  (message "Extracting %s from %s...done" member filename)
                (error "Extracting %s from %s...failed" member filename))))
          (customize-save-variable 'zotero-recognize-pdftotext (concat (file-name-as-directory zotero-recognize-pdftools-dir) pdftotext-filename))
          (customize-save-variable 'zotero-recognize-pdfinfo (concat (file-name-as-directory zotero-recognize-pdftools-dir) pdfinfo-filename))
          (customize-save-variable 'zotero-recognize-pdfdata (concat (file-name-as-directory zotero-recognize-pdftools-dir) "poppler-data/"))
          (delete-file filename))
      (error "Executable tar not found"))))

(defun zotero-recognize (file)
  "Return metadata recognized from PDF FILE.

The metadata can be used to create a parent item for the PDF
attachment, by looking up item metadata when supplied with a
standard identifier. Zotero uses the following databases for
looking up item metadata: Library of Congress and WorldCat for
ISBNs, CrossRef for DOIs, and NCBI PubMed for PubMed IDs."
  (let* ((json (zotero-recognize--pdftojson file))
         (result (zotero-recognize--submit json))
         (status-code (zotero-result-status-code result))
         (data (zotero-result-data result)))
    data))

;; TODO: needs testing
(defun zotero-recognize-report (metadata-pdf metadata-item &optional description)
  "Report incorrectly recognized metadata.
METADATA-PDF is the (incorrectly) recognized metadata as returned
by `zotero-recognize'. METADATA-ITEM is the attachment
item metadata. Optional argument DESCRIPTION is a
string for the report."
  (let* ((url (concat zotero-recognize-base-url "/report"))
         (data (zotero-json-encode-object `(,description ,zotero-api-version ,metadata-pdf ,metadata-item)))
         (url-request-method "POST")
         (url-request-data data)
         (url-request-extra-headers `(("Content-Type" . "application/json"))))
    (with-current-buffer (url-retrieve-synchronously url nil nil zotero-timeout)
      (funcall #'zotero-handle-response))))

(provide 'zotero-recognize)

;;; zotero-recognize.el ends here
