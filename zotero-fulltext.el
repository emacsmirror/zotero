(defvar zotero-lib-pandoc-mimetypes
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

(defcustom zotero-lib-pdftools-dir (concat zotero-lib-directory "pdftools/")
  "The directory were PDF tools should be installed."
  :group 'zotero-lib
  :type 'directory)

(defcustom zotero-lib-pdftotext "pdftotext"
  "Executable for pdftotext.
Needed for fulltext indexing of PDF documents. It is freely
available and included by default with many Linux distributions,
and is also available for Windows as part of the Xpdf Windows
port.

A modified version of pdftotext is needed for recognition of
metadata. This variable is set by `zotero-lib-install-pdftools' after
downloading the PDF tools modified by Zotero. If you compile the
PDF-tools from source, it should point to the \"pdftotext-*\"
binary for your operating system."
  :group 'zotero-lib
  :type 'string
  :link '(url-link "https://github.com/zotero/cross-poppler"))

(defcustom zotero-lib-pdfinfo "pdfinfo"
  "Executable for pdfinfo.
Needed for fulltext indexing of PDF documents. It is freely
available and included by default with many Linux distributions,
and is also available for Windows as part of the Xpdf Windows
port.

This variable is set by `zotero-lib-install-pdftools' after
downloading the PDF tools modified by Zotero. If you compile the
PDF tools from source, it should point to the \"pdfinfo-*\" binary
for your operating system."
  :group 'zotero-lib
  :type 'string
  :link '(url-link "https://github.com/zotero/cross-poppler"))

(defcustom zotero-lib-pdfdata ""
  "Path to the data directory of pdftools.
This variable is set by `zotero-lib-install-pdftools' after
downloading the PDF tools modified by Zotero. If you compile the
PDF tools from source, it should point to the \"popper-data\"
directory."
  :group 'zotero-lib
  :type 'directory
  :link '(url-link "https://github.com/zotero/cross-poppler"))

(defcustom zotero-lib-pandoc "pandoc"
  "Executable for pandoc executable."
  :group 'zotero-lib
  :type 'string)

(defcustom zotero-lib-antiword "antiword"
  "Executable for antiword executable."
  :group 'zotero-lib
  :type 'string)

(defcustom zotero-lib-max-chars 500000
  "How much text is indexed (default: 500000 characters)."
  :group 'zotero-lib
  :type 'integer)

(defcustom zotero-lib-max-pages 100
  "How much text is indexed (default: 100 pages)."
  :group 'zotero-lib
  :type 'integer)

(defconst zotero-lib-pdftools-version "0.0.3")

(defconst zotero-lib-pdftools-url (concat "https://zotero-download.s3.amazonaws.com/pdftools/pdftools-" zotero-lib-pdftools-version ".tar.gz"))

;;;;; Indexing functions

(defun zotero-lib--index-pdf (file)
  "Convert Portable Document Format (PDF) to text."
  (cond ((not (executable-find zotero-lib-pdftotext))
         (error "Executable %s not found" zotero-lib-pdftotext))
        ((not (executable-find zotero-lib-pdfinfo))
         (error "Executable %s not found" zotero-lib-pdfinfo))
        (t
         (let* ((total-pages (with-temp-buffer
                               (call-process zotero-lib-pdfinfo nil t nil file)
                               (goto-char (point-min))
                               (let* ((match (re-search-forward "Pages:[[:blank:]]+\\([[:digit:]]+\\)" nil t))
                                      (pages (match-string-no-properties 1)))
                                 (string-to-number pages))))
                (indexed-pages (if (> total-pages zotero-lib-max-pages) zotero-lib-max-pages total-pages))
                (content (with-temp-buffer
                           (call-process zotero-lib-pdftotext nil t nil "-l" (number-to-string indexed-pages) file "-" )
                           (buffer-string))))
           `(:content ,content :indexedPages ,indexed-pages :totalPages ,total-pages)))))

(defun zotero-lib--index-pandoc (file mimetype)
  "Convert pandoc compatible markup format to text."
  (cond ((not (executable-find zotero-lib-pandoc))
         (error "Executable %s not found" zotero-lib-pandoc))
        (t
         (let ((format (cdr (assoc mimetype zotero-lib-pandoc-mimetypes))))
           (with-temp-buffer
             (call-process "pandoc" nil t nil "-f" format "-t" "plain" file)
             (let* ((content (if (> (buffer-size) zotero-lib-max-chars)
                                 (buffer-substring 1 (1+ zotero-lib-max-chars))
                               (buffer-string)))
                    (indexed-chars (length content))
                    (total-chars (buffer-size)))
               `(:content ,content :indexedChars ,indexed-chars :totalChars ,total-chars)))))))

(defun zotero-lib--index-antiword (file)
  "Convert MS Word version 2, 6, 7, 97, 2000 and 2003 to text."
  (cond ((not (executable-find zotero-lib-antiword))
         (error "Executable %s not found" zotero-lib-antiword))
        (t
         (with-temp-buffer
           (call-process "antiword" nil t nil file)
           (let* ((content (if (> (buffer-size) zotero-lib-max-chars)
                               (buffer-substring 1 (1+ zotero-lib-max-chars))
                             (buffer-string)))
                  (indexed-chars (length content))
                  (total-chars (buffer-size)))
             `(:content ,content :indexedChars ,indexed-chars :totalChars ,total-chars))))))

;;;;; PDF tools
(defun zotero-lib-install-pdftools ()
  "Install the PDF tools modified by Zotero.
The executables are modified to output a preprocessed JSON that
contains rich and structured information about the PDF and the
text extracted from it, for use with the PDF recognizer.

This function downloads and extracts the binaries available for
macOS, Windows and Linux. You can change the installation
directory by setting `zotero-lib-pdftools-dir' to an appropriate
value before calling this function.

If there are no binaries available for your operating system, you
should compile them from source and set the variables
`zotero-lib-pdftotext', `zotero-lib-pdfinfo', and
`zotero-lib-pdfdata' to the corresponding paths. The source is
available at URL `https://github.com/zotero/cross-poppler'."
  (let ((filename (concat zotero-lib-directory (file-name-nondirectory zotero-lib-pdftools-url))))
    (unless (file-exists-p filename)
      (message "Downloading %s..." zotero-lib-pdftools-url)
      (url-copy-file zotero-lib-pdftools-url filename t)
      (message "Downloading %s...done" zotero-lib-pdftools-url))
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
            (system
             (error "No binaries for operating system %S available.")))
          (unless (file-directory-p zotero-lib-pdftools-dir)
            (make-directory zotero-lib-pdftools-dir))
          (dolist (member `(,pdftotext-filename ,pdfinfo-filename "poppler-data"))
            (message "Extracting %s from %s..." member filename)
            (let ((status (call-process "tar" nil nil nil "-xz" "-f" filename "-C" zotero-lib-pdftools-dir member)))
              (if (eq status 0)
                  (message "Extracting %s from %s...done" member filename)
                (error "Extracting %s from %s...failed" member filename))))
          (customize-save-variable 'zotero-lib-pdftotext (concat (file-name-as-directory zotero-lib-pdftools-dir) pdftotext-filename))
          (customize-save-variable 'zotero-lib-pdfinfo (concat (file-name-as-directory zotero-lib-pdftools-dir) pdfinfo-filename))
          (customize-save-variable 'zotero-lib-pdfdata (concat (file-name-as-directory zotero-lib-pdftools-dir) "poppler-data/"))
          (delete-file filename))
      (error "Executable tar not found"))))

(cl-defun zotero-lib-index-fulltext (file &key mimetype user group key api-key)
  "A convenient wrapper around `zotero-lib-set-item-fulltext'"
  (let* ((filename (file-name-nondirectory file))
         (mimetype (or mimetype (mailcap-file-name-to-mime-type filename)))
         (object (cond ((equal mimetype "application/pdf")
                        (zotero-lib--index-pdf file))
                       ((assoc mimetype zotero-lib-pandoc-mimetypes)
                        (zotero-lib--index-pandoc file mimetype))
                       ((equal contenttype "application/msword")
                        (zotero-lib--index-antiword file))))
         (json (zotero-lib--encode-object object)))
    (zotero-lib--submit :method PUT :resource 'item-fulltext :user user :group group :key key :data json :content-type "application/json" :expect "" :api-key api-key)))
