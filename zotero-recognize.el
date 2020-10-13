;;; zotero-recognize.el --- Recognize for the Zotero API  -*- lexical-binding: t; -*-

;;; Code:

;;;; Variables

(defconst zotero-recognize-base-url "https://recognize.zotero.org")

;;;;; Recognizer functions

(defun zotero-recognize--pdftojson (file)
  "Wrapper around the pdftotext executable modified by Zotero.
Return JSON with metadata, layout and rich text of FILE."
  (if (executable-find zotero-lib-pdftotext)
      (let ((tempfile (make-nearby-temp-file "recognize-pdf-cache" nil ".json")))
        (when (file-exists-p tempfile) (delete-file tempfile))
        (let ((status (call-process zotero-lib-pdftotext nil nil nil "-json" "-l" "5" file tempfile)))
          (if (eq status 0)
              (prog1
                  (with-temp-buffer
                    (insert-file-contents tempfile)
                    (buffer-string))
                (delete-file tempfile))
            (error "Executable %s cannot output to json" zotero-lib-pdftotext))))
    (error "Executable %s not found" zotero-lib-pdftotext)))

(defun zotero-recognize--submit (json)
  "Return metadata recognized from JSON returned by `zotero-recognize--pdftojson'.

PDFs are recognized using an undocumented Zotero web service that
operates on the first few pages of text using extraction
algorithms and known metadata from CrossRef. The Zotero lookup
service doesn't require a Zotero account, and data about the
content or results of searches are not logged."
  (let ((url (concat zotero-recognize-base-url "/recognize")))
    (zotero-lib-submit :method "POST" :url url :data json :content-type "application/json" :expect "")))

(defun zotero-recognize (file)
  "Return metadata recognized from PDF FILE.

The metadata can be used to create a parent item for the PDF
attachment, by looking up item metadata when supplied with a
standard identifier. Zotero uses the following databases for
looking up item metadata: Library of Congress and WorldCat for
ISBNs, CrossRef for DOIs, and NCBI PubMed for PubMed IDs."
  (let ((json (zotero-recognize--pdftojson file)))
    (zotero-recognize--submit json)))

;; TODO: testing
(defun zotero-recognize-report (metadata-pdf metadata-item &optional description)
  "Report incorrectly recognized metadata.
METADATA-PDF is the (incorrectly) recognized metadata as returned
by `zotero-recognize'. METADATA-ITEM is the attachment
item metadata. Optional argument DESCRIPTION is a
string for the report."
  (let ((url (concat zotero-recognize-base-url "/report"))
        (json (zotero-lib-encode-object `(,description ,zotero-lib-api-version ,metadata-pdf ,metadata-item))))
    (zotero-lib-submit :method "POST" :url url :data json :content-type "application/json" :expect "")))

(provide 'zotero-recognize)

;;; zotero-recognize.el ends here
