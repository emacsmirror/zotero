;;; zotero-crossref.el --- Retrieve and parse Crossref metadata -*- lexical-binding: t; -*-

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
;; Functions to retrieve metadata from the Crossref API. See
;; <https://github.com/CrossRef/rest-api-doc>

;;; Code:

;;;; Requirements
(require 's)
(require 'seq)
(require 'zotero)
(require 'zotero-cache)

;;;; Variables
(defconst zotero-crossref-url "https://api.crossref.org/works/")

;; Reduce network traffic by selecting only required fields
(defvar zotero-crossref-fields '("type"
		                 "container-title"
		                 "short-container-title"
		                 "volume"
		                 "issue"
		                 "ISBN"
		                 "ISSN"
		                 "publisher"
		                 "publisher-location"
		                 "event"
		                 "abstract"
		                 "issued"
		                 "page"
		                 "DOI"
		                 "link"
		                 "title"
		                 "subtitle"
		                 "author"
		                 "editor"
		                 "chair"
		                 "translator"))

(defun zotero-crossref--parse-creators (item)
  "Parse the creators."
  (let (result)
    (dolist (key '(:author :editor :chair :translator))
      (when-let ((creators (plist-get item key))
                 (creator-type (pcase key
                                 ((or :author :editor :translator)
                                  (zotero-lib-keyword->string key))
                                 (_ "contributor"))))

        (seq-doseq (creator creators)
          (if-let ((name (plist-get creator :name))) ; Organisation
              (push `(:creatorType ,creator-type :name ,name) result)
            (push `(:creatorType ,creator-type :firstName ,(plist-get creator :given) :lastName ,(plist-get creator :family)) result)))))
    (seq-into (nreverse result) 'vector)))

(defun zotero-crossref--parse-metadata (data)
  "Parse Crossref bibliographic metadata DATA to a Zotero item.
Return plist that could be saved to the library by passing it to
`zotero-cache-save' or uploaded by passing it to
`zotero-create-item'."
  (catch 'available
    (let (result)
      (when-let ((items (zotero-lib-plist-get* data :message :items))
                 (_ (> (length items) 0))
                 (item (elt items 0)))
        (pcase (plist-get item :type)
          ;; Journal article
          ("journal-article"
           (setq result (copy-tree (zotero-cache-item-template "journalArticle")))
           (when-let ((seq (plist-get item :container-title))
                      (_ (> (length seq) 0)))
             (setq result (plist-put result :publicationTitle (elt seq 0))))
           (when-let ((seq (plist-get item :short-container-title))
                      (_ (and (> (length seq) 0)
                              (not (equal (elt seq 0) (plist-get result :publicationTitle))))))
             (setq result (plist-put result :journalAbbrevation (elt seq 0))))
           (setq result (plist-put result :volume (plist-get item :volume)))
           (setq result (plist-put result :issue (plist-get item :issue)))
           (when-let ((seq (plist-get item :ISSN))
                      (_ (> (length seq) 0)))
             (setq result (plist-put result :ISSN (elt seq 0)))))
          ;; Book
          ((or "book" "book" "book-series" "book-set" "book-track" "monograph" "reference-book")
           (setq result (copy-tree (zotero-cache-item-template "book")))
           (setq result (plist-put result :publisher (plist-get item :publisher)))
           (setq result (plist-put result :place (plist-get item :publisher-location)))
           (when-let ((seq (plist-get item :ISBN))
                      (_ (> (length seq) 0)))
             (setq result (plist-put result :ISBN (elt seq 0)))))
          ;; Book secion
          ((or "book-chapter" "book-part" "book-section" "reference-entry")
           (setq result (copy-tree (zotero-cache-item-template "bookSection")))
           (setq result (plist-put result :publisher (plist-get item :publisher)))
           (setq result (plist-put result :place (plist-get item :publisher-location)))
           (when-let ((seq (plist-get item :container-title)))
             (pcase (length seq)
               (1
                (setq result (plist-put result :bookTitle (elt seq 0))))
               (2
                (setq result (plist-put result :series (elt seq 0)))
                (setq result (plist-put result :bookTitle (elt seq 1))))))
           (when-let ((seq (plist-get item :ISBN))
                      (_ (> (length seq) 0)))
             (setq result (plist-put result :ISBN (elt seq 0)))))
          ;; Report
          ((or "dataset" "posted-content" "report" "report-series" "standard")
           (setq result (copy-tree (zotero-cache-item-template "report")))
           (setq result (plist-put result :publisher (plist-get item :institution)))
           (setq result (plist-put result :place (plist-get item :publisher-location)))
           (setq result (plist-put result :seriesTitle (elt (plist-get item :container-title) 0))))
          ;; Conference paper
          ("proceedings-article"
           (setq result (copy-tree (zotero-cache-item-template "conferencePaper")))
           (setq result (plist-put result :proceedingsTitle (plist-get item :container-title)))
           (setq result (plist-put result :publisher (plist-get item :publisher)))
           (when-let ((event (plist-get item :event)))
             (setq result (plist-put result :conferenceName (plist-get event :name)))
             (setq result (plist-put result :place (plist-get event :location))))
           (when-let ((seq (plist-get item :ISBN))
                      (_ (> (length seq) 0)))
             (setq result (plist-put result :ISBN (elt seq 0)))))
          ;; Thesis
          ("dissertation"
           (setq result (copy-tree (zotero-cache-item-template "thesis")))
           (setq result (plist-put result :publisher (plist-get item :university)))
           (setq result (plist-put result :place (plist-get item :publisher-location))))
          ;; ignore, because Crossref has zero results for this type
          ((or "edited-book" "standard-series")
           (throw 'available nil))
          ;; ignore, because Zotero doesn't have equivalent item types
          ((or "component" "journal" "journal-issue" "journal-volume" "other" "proceedings"
               "proceedings-series" "peer-review")
           (throw 'available nil)))
        (when-let ((seq (plist-get item :title))
                   (_ (> (length seq) 0)))
          (setq result (plist-put result :title (elt seq 0))))
        (when-let ((subtitle (plist-get item :subtitle))
                   (title (plist-get result :title))
                   (_ (> (length subtitle) 0)))
          ;; Don't duplicate subtitle if it already exists in title
          (unless (s-contains-p (elt subtitle 0) title t)
            (setq result (plist-put result :title (concat title ": " (elt subtitle 0))))))
        (setq result (plist-put result :creators (zotero-crossref--parse-creators item)))
        (when-let ((abstract (plist-get item :abstract)))
          (setq result (plist-put result :abstractNote abstract)))
        ;; Contains the earliest of: published-online, published-print, content-created
        (setq result (plist-put result :pages (plist-get item :page)))
        (when-let ((issued (plist-get item :issued))
                   (date-parts (plist-get issued :date-parts))
                   (date (elt date-parts 0))
                   (_ (elt date 0)))
          (pcase (length date)
            (3 (let ((year (number-to-string (elt date 0)))
                     (month (number-to-string (elt date 1)))
                     (day (number-to-string (elt date 2))))
                 (setq result (plist-put result :date (concat year "-" month "-" day)))))
            (2 (let ((year (number-to-string (elt date 0)))
                     (month (number-to-string (elt date 1))))
                 (setq result (plist-put result :date (concat month "/" year)))))
            (1 (let ((year (number-to-string (elt date 0))))
                 (setq result (plist-put result :date year))))))
        (if-let ((doi (plist-get item :DOI))
                 (_ (plist-member result :DOI)))
            (setq result (plist-put result :DOI doi))
          ;; Add DOI to extra for unsupported items
          (if-let ((extra (plist-get result :extra))
                   (_ (string-empty-p extra)))
              (setq result (plist-put result :extra (concat "DOI: " doi)))
            (setq result (plist-put result :extra (concat extra "\nDOI: " doi)))))
        ;; URL is always http://dx.doi.org/..
        (when-let ((seq (plist-get item :link))
                   (_ (> (length seq) 0))
                   (url (plist-get (elt seq 0) :URL)))
          (setq result (plist-put result :url url))))
      result)))

(defun zotero-crossref--request (id)
  "Return the response of the Crossref request.
Argument ID is the Digital Object Identifier (DOI)."
  ;; TODO: Rate limits

  ;; From time to time Crossref needs to impose rate limits to ensure that the
  ;; free API is usable by all. Any rate limits that are in effect will be
  ;; advertised in the X-Rate-Limit-Limit and X-Rate-Limit-Interval HTTP
  ;; headers. For ease-of-parsing, the X-Rate-Limit-Interval will always be
  ;; expressed in seconds. So, for example the following tells you that you
  ;; should expect to be able to perform 50 requests a second:

  ;; X-Rate-Limit-Limit: 50
  ;; X-Rate-Limit-Interval: 1s

  ;; Note that if we wanted to adjust the measurement window, we could specify:

  ;; X-Rate-Limit-Limit: 3000
  ;; X-Rate-Limit-Interval: 60s
  (let* ((method "GET")
         (url zotero-crossref-url)
         (headers `(("User-Agent" . ,zotero-user-agent)))
         (params `(("filter" ,(concat "doi:" id))
                   ("select" ,(s-join "," zotero-crossref-fields)))))
    (zotero-dispatch (zotero-request-create :method method
                                            :url url
                                            :headers headers
                                            :params params))))

(defun zotero-crossref (id)
  "Return the parsed bibliographic metadata for ID from Crossref.
Argument ID is the Digital Object Identifier (DOI)."
  (let* ((response (zotero-crossref--request id))
         (data (zotero-response-data response)))
    (zotero-crossref--parse-metadata data)))

(provide 'zotero-crossref)

;;; zotero-crossref.el ends here
