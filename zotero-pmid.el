;;; zotero-pmid.el --- Retrieve and parse PMID metadata -*- lexical-binding: t; -*-

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
;; Functions to retrieve metadata from the NCBI E-utilities. See
;; <https://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.ESummary>.

;;; Code:

;;;; Requirements
(require 's)
(require 'seq)
(require 'zotero)
(require 'zotero-cache)

;;;; Variables
(defconst zotero-pmid-url "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi")

(defun zotero-pmid--parse-creators (authors)
  "Parse the creators."
  (let (result)
    (seq-doseq (author authors)
      (when (equal (plist-get author :authtype) "Author")
        (push `(:creatorType "author" :name ,(plist-get author :name)) result)))
    (seq-into (nreverse result) 'vector)))

(defun zotero-pmid--parse-metadata (data)
  "Parse NCBI bibliographic metadata DATA to a Zotero item.
Return plist that could be saved to the library by passing it to
`zotero-cache-save' or uploaded by passing it to
`zotero-create-item'."
  (catch 'available
    (let (result)
      (when-let ((uids (zotero-lib-plist-get* data :result :uids))
                 (uid (unless (seq-empty-p uids) (elt uids 0)))
                 (item (zotero-lib-plist-get* data :result (zotero-lib-string->keyword uid)))
                 (seq (plist-get item :pubtype))
                 (_ (seq-some (lambda (elt) (equal elt "Journal Article")) seq)))
        (setq result (copy-tree (zotero-cache-item-template "journalArticle")))
        (setq result (plist-put result :title (plist-get item :title)))
        (setq result (plist-put result :creators (zotero-pmid--parse-creators (plist-get item :authors))))
        (setq result (plist-put result :publicationTitle (plist-get item :fulljournalname)))
        (setq result (plist-put result :volume (plist-get item :volume)))
        (setq result (plist-put result :issue (plist-get item :issue)))
        (setq result (plist-put result :pages (plist-get item :page)))
        (let* ((sortpubdate (plist-get item :sortpubdate))
               (regexp (rx string-start
                           (group (repeat 4 digit))
                           ?/
                           (group (repeat 2 digit))
                           ?/
                           (group (repeat 2 digit))
                           space
                           (repeat 2 digit) ?: (repeat 2 digit)
                           string-end))
               (_ (string-match regexp sortpubdate))
               (year (match-string 1 sortpubdate))
               (month (match-string 2 sortpubdate))
               (day (match-string 3 sortpubdate)))
          (setq result (plist-put result :date (concat year "-" month "-" day))))
        (when-let ((source (plist-get item :source))
                   (_ (not (equal source (plist-get result :publicationTitle)))))
          (setq result (plist-put result :journalAbbreviation source)))
        (let* ((articleids (plist-get item :articleids))
               (seq (seq-find (lambda (elt) (equal (plist-get elt :idtype) "doi")) articleids))
               (doi (plist-get seq :value)))
          (setq result (plist-put result :DOI doi)))
        (setq result (plist-put result :ISSN (plist-get item :issn)))
        (setq result (plist-put result :url (plist-get item :availablefromurl))))
      result)))

(defun zotero-pmid--request (id)
  "Return the response of the PMID request.
Argument ID is the PMID"
  (let* ((method "GET")
         (url zotero-pmid-url)
         (params `(("db" "pubmed")
                   ("id" ,id)
                   ("retmode" "json"))))
    (zotero-dispatch (zotero-request-create :method method
                                            :url url
                                            :params params))))

(defun zotero-pmid (id)
  "Return the parsed bibliographic metadata for ID from NCBI.
Argument ID is the PMID."
  (let* ((response (zotero-pmid--request id))
         (data (zotero-response-data response)))
    (zotero-pmid--parse-metadata data)))

(provide 'zotero-pmid)

;;; zotero-pmid.el ends here
