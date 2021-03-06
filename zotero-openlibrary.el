;;; zotero-openlibrary.el --- Retrieve and parse Open Library metadata -*- lexical-binding: t; -*-

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
;; Functions to retrieve metadata from the Open Library API. See
;; <https://openlibrary.org/dev/docs/api/books>

;;; Code:

;;;; Requirements
(require 's)
(require 'seq)
(require 'zotero)
(require 'zotero-cache)

;;;; Variables
(defconst zotero-openlibrary-url "https://openlibrary.org/api/books")

(defun zotero-openlibrary--parse-creators (authors)
  "Parse the creators in AUTHORS."
  (let (result)
    (seq-doseq (author authors)
      (push `(:creatorType "author" :name ,(plist-get author :name)) result))
    (seq-into (nreverse result) 'vector)))

(defun zotero-openlibrary--parse-metadata (data)
  "Parse Open Library bibliographic metadata DATA to a Zotero item.
Return plist that could be saved to the library by passing it to
`zotero-cache-save' or uploaded by passing it to
`zotero-create-item'."
  (let (result)
    (when-let ((item (unless (eq data :json-empty) (cadr data))))
      (setq result (copy-tree (zotero-cache-item-template "book")))
      (setq result (plist-put result :url (plist-get item :info_url)))
      (let ((details (plist-get item :details)))
        (setq result (plist-put result :title (plist-get details :title)))
        (when-let ((title (plist-get result :title))
                   (subtitle (plist-get details :subtitle)))
          ;; Don't duplicate subtitle if it already exists in title
          (unless (s-contains-p subtitle title t)
            (setq result (plist-put result :title (concat title ": " subtitle)))))
        (let* ((seq (plist-get details :authors))
               (_ (> (length seq) 0)))
          (setq result (plist-put result :creators (zotero-openlibrary--parse-creators seq))))
        (when-let ((seq (plist-get details :series))
                   (_ (> (length seq) 0)))
          (setq result (plist-put result :series (plist-get (elt seq 0) :series))))
        (when-let ((edition (plist-get details :edition_name)))
          (setq result (plist-put result :edition edition)))
        (when-let ((seq (plist-get details :publish_places))
                   (_ (> (length seq) 0)))
          (setq result (plist-put result :places (elt seq 0))))
        (let* ((seq (plist-get details :publishers))
               (_ (> (length seq) 0)))
          (setq result (plist-put result :publisher (elt seq 0))))
        (when-let ((date (plist-get details :publish_date)))
          (setq result (plist-put result :date date)))
        (setq result (plist-put result :numPages (number-to-string (plist-get details :number_of_pages))))
        (let ((isbn (or (plist-get details :isbn_13)
                        (plist-get details :isbn_10))))
          (setq result (plist-put result :ISBN (elt isbn 0))))))
    result))

(defun zotero-openlibrary--request (id)
  "Return the response of the Open Library request.
Argument ID is the ISBN-10 or ISBN-13."
  (let* ((method "GET")
         (url zotero-openlibrary-url)
         (params `(("bibkeys" ,(concat "ISBN:" id))
                   ("format" "json")
                   ("jscmd" "details"))))
    (zotero-dispatch (zotero-request-create :method method
                                            :url url
                                            :params params))))

(defun zotero-openlibrary (id)
  "Return the parsed bibliographic metadata for ID from Open Library.
Argument ID is the ISBN-10 or ISBN-13."
  (let* ((response (zotero-openlibrary--request id))
         (data (zotero-response-data response)))
    (zotero-openlibrary--parse-metadata data)))

(provide 'zotero-openlibrary)

;;; zotero-openlibrary.el ends here
