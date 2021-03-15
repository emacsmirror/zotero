;;; zotero-arxiv.el --- Retrieve and parse Arxiv metadata -*- lexical-binding: t; -*-

;; Author: Folkert van der Beek <folkertvanderbeek@gmail.com>
;; URL: https://gitlab.com/fvdbeek/emacs-zotero

;; This file is part of Emacs-zotero.

;; Emacs-zotero is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; Emacs-zotero is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; Emacs-zotero. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Functions to retrieve metadata from the Arxiv API. See
;; <https://arxiv.org/help/api/index>.

;;; Code:

;;;; Requirements
(require 's)
(require 'seq)
(require 'iso8601)
(require 'zotero)

(declare-function zotero-cache-item-template "zotero-cache")

;;;; Variables
(defconst zotero-arxiv-url "https://export.arxiv.org/api/query")

(defun zotero-arxiv--parse-creators (authors)
  "Parse the creators in AUTHORS."
  (let (result)
    (dolist (author authors)
      (let-alist author
        (push `(:creatorType "author" :name ,(xml-node-attributes .name)) result)))
    (seq-into (nreverse result) 'vector)))

(defun zotero-arxiv--parse-metadata (dom)
  "Parse Arxiv bibliographic metadata DOM to a Zotero item.
Return plist that could be saved to the library by passing it to
`zotero-cache-save' or uploaded by passing it to
`zotero-create-item'."
  (let (result)
    (let-alist dom
      (when-let ((item .feed.entry))
        (let ((title (xml-node-attributes .feed.entry.title)) ; The title of the article.
              ;; (arxiv-id (s-chop-prefix "http://arxiv.org/abs/" (xml-node-attributes .feed.entry.id))) ; A url http://arxiv.org/abs/id
              ;; (published (xml-node-attributes .feed.entry.published)) ; The date that version 1 of the article was submitted.
              (updated (xml-node-attributes .feed.entry.updated)) ; The date that the retrieved version of the article was submitted. Same as <published> if the retrieved version is version 1.
              (summary (xml-node-attributes .feed.entry.summary)) ; The article abstract.
              (authors (xml-get-children .feed.entry 'author)) ; One for each author. Has child element <name> containing the author name.
              (links (xml-get-children .feed.entry 'link)) ; Can be up to 3 given url's associated with this article.
              ;; (arxiv:comment (xml-node-attributes .feed.entry.arxiv:comment)) ; The authors comment if present.
              (arxiv:journal_ref (xml-node-attributes .feed.entry.arxiv:journal_ref)) ; A journal reference if present.
              (arxiv:doi (xml-node-attributes .feed.entry.arxiv:doi))) ; A url for the resolved DOI to an external resource if present.
          (setq result (copy-tree (zotero-cache-item-template "journalArticle")))
          (when title (setq result (plist-put result :title title)))
          (when authors (setq result (plist-put result :creators (zotero-arxiv--parse-creators authors))))
          (when summary (setq result (plist-put result :abstractNote (s-trim summary))))
          (when updated
            (let* ((time (iso8601-parse updated))
                   (timestamp (encode-time time))
                   (time-string (format-time-string "%Y-%m-%d" timestamp)))
              (setq result (plist-put result :date time-string))))
          (dolist (link links)
            (let-alist (xml-node-attributes link)
              (when (equal .rel "alternate")
                (setq result (plist-put result :url .href)))))
          (when arxiv:journal_ref (setq result (plist-put result :publicationTitle arxiv:journal_ref)))
          (when arxiv:doi (setq result (plist-put result :DOI arxiv:doi))))))
    result))

(defun zotero-arxiv--request (id)
  "Return the response of the Arxiv request.
Argument ID is the Arxiv ID"
  (let* ((method "GET")
         (url zotero-arxiv-url)
         (params `(("id_list" ,id))))
    (zotero-dispatch (zotero-request-create :method method
                                            :url url
                                            :params params))))

(defun zotero-arxiv (id)
  "Return the parsed bibliographic metadata for ID from Arxiv.
Argument ID is the Arxiv ID."
  (let* ((response (zotero-arxiv--request id))
         (data (zotero-response-data response))
         (dom (with-temp-buffer (insert data) (xml-parse-region (point-min) (point-max)))))
    (zotero-arxiv--parse-metadata dom)))

(provide 'zotero-arxiv)

;;; zotero-arxiv.el ends here
