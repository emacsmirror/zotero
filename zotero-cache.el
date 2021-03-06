;;; zotero-cache.el --- Cache for Zotero -*- lexical-binding: t; -*-

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

;;;; Requirements
(require 'cl-lib)
(require 'ht)
(require 'iso8601)
(require 'parse-time)
(require 's)
(require 'seq)
(require 'zotero)
(require 'zotero-lib)
(require 'zotero-diff)

;;;; Variables

(defvar zotero-cache nil
  "A hash table used to cache the library data.")

;;;; Customization

(defgroup zotero-cache nil
  "Caching for Zotero"
  :group 'zotero)

(defcustom zotero-cache-enable-caching t
  "When t enables caching.
Caching is automatically enabled by default."
  :group 'zotero-cache
  :type 'boolean)

(defcustom  zotero-cache-enable-storage t
  "When t enables storage.
Storage is automatically enabled by default."
  :group 'zotero-cache
  :type 'boolean)

(defcustom zotero-cache-file
  (locate-user-emacs-file "zotero-cache")
  "The cache file."
  :group 'zotero-cache
  :type 'file)

(defcustom zotero-cache-storage-dir
  (file-name-as-directory (locate-user-emacs-file "zotero-storage"))
  "Attachment storage directory."
  :group 'zotero-cache
  :type 'file)

(defcustom zotero-cache-expire 86400
  "Number of seconds before the cache expires; default=86400 (one day).

A value of nil means the cache never expires. As schema changes
are currently rare, clients should cache type/field data for a
period of time without making further requests."
  :group 'zotero-cache
  :type '(choice (const :tag "Disabled" nil)
                 (integer :tag "Seconds")))

;;;; Serialization

(defun zotero-cache--serialize (data filename)
  "Serialize DATA to FILENAME.
The saved data can be restored with `zotero-cache--unserialize'."
  (when (file-writable-p filename)
    (with-temp-file filename
      (insert (let (print-length) (prin1-to-string data))))))

(defun zotero-cache--unserialize (filename)
  "Read data serialized by `zotero-cache--serialize' from FILENAME."
  (with-demoted-errors
      "Error during file deserialization: %S"
    (when (file-exists-p filename)
      (with-temp-buffer
        (insert-file-contents filename)
        (unless (string-empty-p (buffer-string))
          (read (buffer-string)))))))

(defun zotero-cache-serialize ()
  "Serialize the memory cache to the hard drive."
  (interactive)
  (zotero-cache--serialize zotero-cache zotero-cache-file))

(defun zotero-cache-unserialize ()
  "Serialize the hard drive to the memory cache."
  (interactive)
  (setq zotero-cache (zotero-cache--unserialize zotero-cache-file)))

(defun zotero-cache-erase (&optional no-confirm)
  "Erase the cache.
If optional argument NO-CONFIRM is non-nil, don't ask for confirmation."
  (interactive)
  (when (or no-confirm
            (y-or-n-p "This will completely erase your local Zotero cache. Are you sure? "))
    (setq zotero-cache (zotero-cache--initialize-cache))
    (zotero-cache-serialize)))

(defun zotero-cache-maybe-initialize-cache ()
  "Initialize the cache if needed."
  (interactive)
  (unless zotero-cache
    (setq zotero-cache
          (or (zotero-cache--unserialize zotero-cache-file)
              (zotero-cache--initialize-cache)))))

(defun zotero-cache--initialize-cache ()
  "Return an empty cache."
  (ht ("version" 2)
      ("libraries" (ht-create))
      ("groups" (ht-create))
      ("synccache" (ht ("collections" (ht-create))
                       ("items" (ht-create))
                       ("searches" (ht-create))))
      ("deletions" (ht ("collections" (ht-create))
                       ("items" (ht-create))
                       ("searches" (ht-create))))
      ("templates" (ht ("items" (ht-create))
                       ("attachments" (ht-create))))))

(defun zotero-cache--year (string)
  "Return year in STRING, or nil."
  (when-let ((match (s-match "[[:digit:]]\\{4\\}" string)))
    (car match)))

(defun zotero-cache--some (pred table)
  "Return non-nil if PRED is satisfied for at least one element of TABLE.
PRED is a function that takes a data element as its first
argument."
  (if (ht-find (lambda (_key value) (funcall pred (zotero-lib-plist-get* value :object :data))) table) t nil))

(defun zotero-cache--pred (field direction)
  "Return a predicate function as used by `zotero-cache-sort-by'.

FIELD is the prop of the object plist to be sorted. DIRECTION is
the sorting order: 'asc for ascending or 'desc for descending."
  (let ((pred (pcase field
                ;; REVIEW: is this necessary? The sorting function already ignores type errors.
                ;; The :tags, :collections, and :relations fields are vectors and not suitable for sorting
                ((or :tags :collections :relations)
                 (user-error "The %S field is not suitable for sorting" field))
                ;; The :creators field is a vector, and sorted by :lastName of the first creator
                (:creators
                 (lambda (a b) (string-greaterp (plist-get (seq-first a) :lastName) (plist-get (seq-first b) :lastName))))
                ;; The date fields could contain time strings in various formats that cannot be parsed reliably, so attempt to extract the year only
                ((or :date :accessDate :dateDecided :filingDate :issueDate :dateEnacted)
                 (lambda (a b) (when-let ((year-a (zotero-cache--year a))
                                          (year-b (zotero-cache--year b)))
                                 (string-greaterp year-a year-b))))
                ;; The :dateAdded and :dateModified fields are filled automatically in ISO 8601 format
                ((or :dateAdded :dateModified)
                 (lambda (a b) (time-less-p (encode-time (iso8601-parse a)) (encode-time (iso8601-parse b)))))
                ;; The :id and :owner fields are integers and only used in groups
                ;; The :version and :mtime fields are integers
                ((or :id :owner :version :mtime)
                 #'<)
                ;; The rest of the fields are strings
                (_ #'string-lessp))))
    (pcase direction
      ('asc
       pred)
      ('desc
       (lambda (a b) (funcall pred b a))))))

(defun zotero-cache-filter-data (pred table)
  "Return a table containing entries in TABLE for which PRED returns non-nil.

PRED is a function that takes a `:data' element as its first
argument."
  (ht-select (lambda (_key value)
               ;; keep the predicate nil-safe
               (ignore-error wrong-type-argument
                 (funcall pred (zotero-lib-plist-get* value :object :data)))) table))

(defun zotero-cache-read-access-p (plist)
  "Return t if read access is permitted, else return nil.

Argument PLIST is the permissions of the user or group library as
returned by `zotero-key'."
  (eq (plist-get plist :library) t))

(defun zotero-cache-write-access-p (plist)
  "Return t if write access is permitted, else return nil.

Argument PLIST is the permissions of the user or group library as
returned by `zotero-key'."
  (eq (plist-get plist :write) t))

(defun zotero-cache-read-only-p (plist)
  "Return t if read only, else return nil.

Argument PLIST is the permissions of the user or group library as
returned by `zotero-key'."
  (and (zotero-cache-read-access-p plist) (not (zotero-cache-write-access-p plist))))

(defun zotero-cache-note-access-p (plist)
  "Return t if note access is permitted, else return nil.

Argument PLIST is the permissions of the user or group library as
returned by `zotero-key'."
  (eq (plist-get plist :notes) t))

(defun zotero-cache-file-access-p (plist)
  "Return t if file access is permitted, else return nil.

Argument PLIST is the permissions of the user or group library as
returned by `zotero-key'."
  (eq (plist-get plist :files) t))

(defun zotero-cache-parentitem (key table)
  "Return the parent of KEY in TABLE, or nil."
  (let* ((value (ht-get table key))
         (parent (zotero-lib-plist-get* value :object :data :parentItem)))
    (unless (or (null parent)
                (eq parent :json-false))
      parent)))

(defun zotero-cache-parentcollection (key table)
  "Return the parent of KEY in TABLE, or nil."
  (let* ((value (ht-get table key))
         (parent (zotero-lib-plist-get* value :object :data :parentCollection)))
    (unless (or (null parent)
                (eq parent :json-false))
      parent)))

(defun zotero-cache-subitems (key table)
  "Return the subitems of KEY in TABLE."
  (zotero-cache-filter-data (lambda (elt) (equal (plist-get elt :parentItem) key)) table))

(defun zotero-cache-subcollections (key table)
  "Return the subcollections of KEY in TABLE."
  (zotero-cache-filter-data (lambda (elt) (equal (plist-get elt :parentCollection) key)) table))

(defun zotero-cache-has-subitems-p (key table)
  "Return non-nil if KEY in TABLE has subitems."
  (zotero-cache--some (lambda (elt) (equal (plist-get elt :parentItem) key)) table))

(defun zotero-cache-has-subcollections-p (key table)
  "Return non-nil if KEY in TABLE has subcollections."
  (zotero-cache--some (lambda (elt) (equal (plist-get elt :parentCollection) key)) table))

(defun zotero-cache-has-attachments-p (key table)
  "Return non-nil if KEY in TABLE has attachments."
  (zotero-cache--some (lambda (elt) (and (equal (plist-get elt :parentItem) key)
                                         (equal (plist-get elt :itemType) "attachment"))) table))

(defun zotero-cache-has-notes-p (key table)
  "Return non-nil if KEY in TABLE has attachments."
  (zotero-cache--some (lambda (elt) (and (equal (plist-get elt :parentItem) key)
                                         (equal (plist-get elt :itemType) "note"))) table))

(defun zotero-cache-group (&optional id)
  "Get groups from cache.
Return table for multiple item request or entry for a single item
request.

Optional argument ID is the ID of the group library you want to
access, i.e. the \"group ID\"."
  (if id
      (ht-get* zotero-cache "groups" id)
    (ht-get zotero-cache "groups")))

(defun zotero-cache-library (&optional type id)
  "Get libraries from cache.
Return table for multiple item request or entry for a single item
request.

Optional argument TYPE is \"user\" for your personal library, and
\"group\" for the group libraries. ID is the ID of the personal
or group library you want to access, that is the user ID or group
ID."
  (let ((table (ht-get zotero-cache "libraries")))
    (cond
     ((and type id) (ht-get (ht-select (lambda (_key value) (equal (plist-get value :type) type)) table) id))
     (type (ht-select (lambda (_key value) (equal (plist-get value :type) type)) table))
     (id (ht-get* zotero-cache "libraries" id))
     (t (ht-get* zotero-cache "libraries")))))

(defun zotero-cache-synccache (resource &optional key type id include-trashed)
  "Get RESOURCE from library in cache.
Return table for multiple item request or entry for a single item
request.

RESOURCE is one of:
  - \"collections\": collections in the library
  - \"collections-top\": top-level collections in the library
  - \"collection\": a specific collection in the library
  - \"subcollections\": subcollections within a specific collection in the library
  - \"items\": all items in the library, excluding trashed items
  - \"items-top\": top-level items in the library, excluding trashed items
  - \"trash-items\": items in the trash
  - \"item\": a specific item in the library
  - \"item-children\": child items under a specific item
  - \"publication-items\": items in My Publications
  - \"collection-items\": items within a specific collection in the library
  - \"collection-items-top\": top-level items within a specific collection in the library
  - \"searches\": all saved searches in the library
  - \"search\": a specific saved search in the library
  - \"tags\": all tags in the library, or tags of all types matching a specific name when an url encoded tag is provided
  - \"item-tags\": tags associated with a specific item
  - \"collection-tags\": tags within a specific collection in the library
  - \"items-tags\": all tags in the library, with the ability to filter based on the items
  - \"items-top-tags\": tags assigned to top-level items
  - \"trash-items-tags\": tags assigned to items in the trash
  - \"collection-items-tags\": tags assigned to items in a given collection
  - \"collection-items-top-tags\": tags assigned to top-level items in a given collection
  - \"publication-items-tags\": tags assigned to items in My Publications.

Optional argument TYPE is \"user\" for your personal library, and
\"group\" for the group libraries. ID is the ID of the personal
or group library you want to access, that is the user ID or group
ID. KEY is the item key, collection key, or search key. Which key
is needed varies by resource. If INCLUDE-TRASHED is non-nil,
items in the trash are included."
  (pcase resource
    ("collections"
     (let ((table (ht-get* zotero-cache "synccache" "collections")))
       (cond
        (key (ht-get* zotero-cache "synccache" "collections" key))
        ((and type id)
         (ht-select (lambda (_key value) (and (equal (plist-get value :type) type)
                                              (equal (plist-get value :id) id))) table))
        (t table))))
    ("collections-top"
     (let* ((table (ht-get* zotero-cache "synccache" "collections"))
            (table (if (and type id)
                       (ht-select (lambda (_key value) (and (equal (plist-get value :type) type)
                                                            (equal (plist-get value :id) id))) table)
                     table)))
       (zotero-cache-filter-data (lambda (elt) (or (not (plist-member elt :parentCollection))
                                                   (eq (plist-get elt :parentCollection) :json-false))) table)))
    ("collection"
     (ht-get* zotero-cache "synccache" "collections" key))
    ("subcollections"
     (let* ((table (ht-get* zotero-cache "synccache" "collections"))
            (table (if (and type id)
                       (ht-select (lambda (_key value) (and (equal (plist-get value :type) type)
                                                            (equal (plist-get value :id) id))) table)
                     table)))
       (zotero-cache-filter-data (lambda (elt) (equal (plist-get elt :parentCollection) key)) table)))
    ("items"
     (let* ((table (ht-get* zotero-cache "synccache" "items"))
            (table (if (and type id)
                       (ht-select (lambda (_key value) (and (equal (plist-get value :type) type)
                                                            (equal (plist-get value :id) id))) table)
                     table))
            (table (if include-trashed
                       (zotero-cache-filter-data (lambda (elt) (not (eq (plist-get elt :deleted) 1))) table)
                     table)))
       (cond
        (key (ht-get* zotero-cache "synccache" "items" key))
        ((and type id)
         (ht-select (lambda (_key value) (and (equal (plist-get value :type) type)
                                              (equal (plist-get value :id) id))) table))
        (t table))))
    ("items-top"
     (let* ((table (ht-get* zotero-cache "synccache" "items"))
            (table (if (and type id)
                       (ht-select (lambda (_key value) (and (equal (plist-get value :type) type)
                                                            (equal (plist-get value :id) id))) table)
                     table)))
       (if include-trashed
           (zotero-cache-filter-data (lambda (elt) (or (eq (plist-get elt :collections) [])
                                                       (eq (plist-get elt :collections) :json-empty))) table)
         (zotero-cache-filter-data (lambda (elt) (and (not (eq (plist-get elt :deleted) 1))
                                                      (or (eq (plist-get elt :collections) [])
                                                          (eq (plist-get elt :collections) :json-empty)))) table))))
    ("trash-items"
     (let* ((table (ht-get* zotero-cache "synccache" "items"))
            (table (if (and type id)
                       (ht-select (lambda (_key value) (and (equal (plist-get value :type) type)
                                                            (equal (plist-get value :id) id))) table)
                     table))
            (select (zotero-cache-filter-data (lambda (elt) (or (not (plist-member elt :parentItem))
                                                                (eq (plist-get elt :parentItem) :json-false))) table)))
       (zotero-cache-filter-data (lambda (elt) (eq (plist-get elt :deleted) 1)) select)))
    ("item"
     (ht-get* zotero-cache "synccache" "items" key))
    ("item-children"
     (let ((table (ht-get* zotero-cache "synccache" "items")))
       (if include-trashed
           (zotero-cache-filter-data (lambda (elt) (equal (plist-get elt :parentItem) key)) table)
         (zotero-cache-filter-data (lambda (elt) (and (not (eq (plist-get elt :deleted) 1))
                                                      (equal (plist-get elt :parentItem) key))) table))))
    ;; TODO
    ;; ("publication-items")
    ("collection-items"
     (let* ((table (ht-get* zotero-cache "synccache" "items"))
            (table (if (and type id)
                       (ht-select (lambda (_key value) (and (equal (plist-get value :type) type)
                                                            (equal (plist-get value :id) id))) table)
                     table)))
       (if include-trashed
           (zotero-cache-filter-data (lambda (elt) (seq-contains-p (plist-get elt :collections) key)) table)
         (zotero-cache-filter-data (lambda (elt) (and (not (eq (plist-get elt :deleted) 1))
                                                      (seq-contains-p (plist-get elt :collections) key))) table))))
    ("collection-items-top"
     (let* ((table (ht-get* zotero-cache "synccache" "items"))
            (table (if (and type id)
                       (ht-select (lambda (_key value) (and (equal (plist-get value :type) type)
                                                            (equal (plist-get value :id) id))) table)
                     table)))
       (if include-trashed
           (zotero-cache-filter-data (lambda (elt) (seq-contains-p (plist-get elt :collections) key)) table)
         (zotero-cache-filter-data (lambda (elt) (and (not (eq (plist-get elt :deleted) 1))
                                                      (seq-contains-p (plist-get elt :collections) key))) table))))
    ("searches"
     (let ((table (ht-get* zotero-cache "synccache" "searches")))
       (if (and type id)
           (ht-select (lambda (_key value) (and (equal (plist-get value :type) type)
                                                (equal (plist-get value :id) id))) table)
         table)))
    ("search"
     (ht-get* zotero-cache "synccache" "search" key))
    ;; TODO
    ;; ("tags")
    ;; ("tags")
    ;; ("item-tags")
    ;; ("collection-tags")
    ;; ("items-tags")
    ;; ("items-top-tags")
    ;; ("trash-items-tags")
    ;; ("collection-items-tags")
    ;; ("collection-items-top-tags")
    ;; ("publication-items-tags")
    ))

(defun zotero-cache-deletions (resource &optional key type id include-trashed)
  "Get RESOURCE from delete log in cache.
Return table for multiple item request or entry for a single item
request.

RESOURCE is one of:
  - \"collections\": collections in the library
  - \"collections-top\": top-level collections in the library
  - \"collection\": a specific collection in the library
  - \"subcollections\": subcollections within a specific collection in the library
  - \"items\": all items in the library, excluding trashed items
  - \"items-top\": top-level items in the library, excluding trashed items
  - \"trash-items\": items in the trash
  - \"item\": a specific item in the library
  - \"item-children\": child items under a specific item
  - \"publication-items\": items in My Publications
  - \"collection-items\": items within a specific collection in the library
  - \"collection-items-top\": top-level items within a specific collection in the library
  - \"searches\": all saved searches in the library
  - \"search\": a specific saved search in the library
  - \"tags\": all tags in the library, or tags of all types matching a specific name when an url encoded tag is provided
  - \"item-tags\": tags associated with a specific item
  - \"collection-tags\": tags within a specific collection in the library
  - \"items-tags\": all tags in the library, with the ability to filter based on the items
  - \"items-top-tags\": tags assigned to top-level items
  - \"trash-items-tags\": tags assigned to items in the trash
  - \"collection-items-tags\": tags assigned to items in a given collection
  - \"collection-items-top-tags\": tags assigned to top-level items in a given collection
  - \"publication-items-tags\": tags assigned to items in My Publications.

Optional argument TYPE is \"user\" for your personal library, and
\"group\" for the group libraries. ID is the ID of the personal
or group library you want to access, that is the user ID or group
ID. KEY is the item key, collection key, or search key. Which key
is needed varies by resource. If INCLUDE-TRASHED is non-nil,
items in the trash are included."
  (pcase resource
    ("collections"
     (let ((table (ht-get* zotero-cache "deletions" "collections")))
       (cond
        (key (ht-get* zotero-cache "deletions" "collections" key))
        ((and type id)
         (ht-select (lambda (_key value) (and (equal (plist-get value :type) type)
                                              (equal (plist-get value :id) id))) table))
        (t table))))
    ("collections-top"
     (let* ((table (ht-get* zotero-cache "deletions" "collections"))
            (table (if (and type id)
                       (ht-select (lambda (_key value) (and (equal (plist-get value :type) type)
                                                            (equal (plist-get value :id) id))) table)
                     table)))
       (zotero-cache-filter-data (lambda (elt) (or (not (plist-member elt :parentCollection))
                                                   (eq (plist-get elt :parentCollection) :json-false))) table)))
    ("collection"
     (ht-get* zotero-cache "deletions" "collections" key))
    ("subcollections"
     (let* ((table (ht-get* zotero-cache "deletions" "collections"))
            (table (if (and type id)
                       (ht-select (lambda (_key value) (and (equal (plist-get value :type) type)
                                                            (equal (plist-get value :id) id))) table)
                     table)))
       (zotero-cache-filter-data (lambda (elt) (equal (plist-get elt :parentCollection) key)) table)))
    ("items"
     (let* ((table (ht-get* zotero-cache "deletions" "items"))
            (table (if (and type id)
                       (ht-select (lambda (_key value) (and (equal (plist-get value :type) type)
                                                            (equal (plist-get value :id) id))) table)
                     table))
            (table (if include-trashed
                       (zotero-cache-filter-data (lambda (elt) (not (eq (plist-get elt :deleted) 1))) table)
                     table)))
       (cond
        (key (ht-get* zotero-cache "deletions" "items" key))
        ((and type id)
         (ht-select (lambda (_key value) (and (equal (plist-get value :type) type)
                                              (equal (plist-get value :id) id))) table))
        (t table))))
    ("items-top"
     (let* ((table (ht-get* zotero-cache "deletions" "items"))
            (table (if (and type id)
                       (ht-select (lambda (_key value) (and (equal (plist-get value :type) type)
                                                            (equal (plist-get value :id) id))) table)
                     table)))
       (if include-trashed
           (zotero-cache-filter-data (lambda (elt) (or (eq (plist-get elt :collections) [])
                                                       (eq (plist-get elt :collections) :json-empty))) table)
         (zotero-cache-filter-data (lambda (elt) (and (not (eq (plist-get elt :deleted) 1))
                                                      (or (eq (plist-get elt :collections) [])
                                                          (eq (plist-get elt :collections) :json-empty)))) table))))
    ("trash-items"
     (let* ((table (ht-get* zotero-cache "deletions" id "items"))
            (table (if (and type id)
                       (ht-select (lambda (_key value) (and (equal (plist-get value :type) type)
                                                            (equal (plist-get value :id) id))) table)
                     table)))
       (zotero-cache-filter-data (lambda (elt) (eq (plist-get elt :deleted) 1)) table)))
    ("item"
     (ht-get* zotero-cache "deletions" "items" key))
    ("item-children"
     (let ((table (ht-get* zotero-cache "deletions" "items")))
       (if include-trashed
           (zotero-cache-filter-data (lambda (elt) (equal (plist-get elt :parentItem) key)) table)
         (zotero-cache-filter-data (lambda (elt) (and (not (eq (plist-get elt :deleted) 1))
                                                      (equal (plist-get elt :parentItem) key))) table))))
    ;; TODO
    ;; ("publication-items")
    ("collection-items"
     (let* ((table (ht-get* zotero-cache "deletions" "items"))
            (table (if (and type id)
                       (ht-select (lambda (_key value) (and (equal (plist-get value :type) type)
                                                            (equal (plist-get value :id) id))) table)
                     table)))
       (if include-trashed
           (zotero-cache-filter-data (lambda (elt) (seq-contains-p (plist-get elt :collections) key)) table)
         (zotero-cache-filter-data (lambda (elt) (and (not (eq (plist-get elt :deleted) 1))
                                                      (seq-contains-p (plist-get elt :collections) key))) table))))
    ("collection-items-top"
     (let* ((table (ht-get* zotero-cache "deletions" id "items"))
            (table (if (and type id)
                       (ht-select (lambda (_key value) (and (equal (plist-get value :type) type)
                                                            (equal (plist-get value :id) id))) table)
                     table)))
       (if include-trashed
           (zotero-cache-filter-data (lambda (elt) (and (seq-contains-p (plist-get elt :collections) key)
                                                        (or (not (plist-member elt :parentItem))
                                                            (eq (plist-get elt :parentItem) :json-false)))) table)
         (zotero-cache-filter-data (lambda (elt) (and (not (eq (plist-get elt :deleted) 1))
                                                      (seq-contains-p (plist-get elt :collections) key)
                                                      (or (not (plist-member elt :parentItem))
                                                          (eq (plist-get elt :parentItem) :json-false)))) table))))
    ("searches"
     (let ((table (ht-get* zotero-cache "deletions" "searches")))
       (if (and type id)
           (ht-select (lambda (_key value) (and (equal (plist-get value :type) type)
                                                (equal (plist-get value :id) id))) table)
         table)))
    ("search"
     (ht-get* zotero-cache "deletions" "search" key))
    ;; TODO
    ;; ("tags")
    ;; ("tags")
    ;; ("item-tags")
    ;; ("collection-tags")
    ;; ("items-tags")
    ;; ("items-top-tags")
    ;; ("trash-items-tags")
    ;; ("collection-items-tags")
    ;; ("collection-items-top-tags")
    ;; ("publication-items-tags")
    ;; ("keys")
    ;; ("all-fulltext")
    ;; ("item-fulltext")
    ;; ("file")
    ))

(defun zotero-cache-search (query type id &optional include-trashed)
  "Search all items for QUERY.
Search titles and individual creator fields.

Argument TYPE is \"user\" for your personal library, and
\"group\" for the group libraries. ID is the ID of the personal
or group library you want to access, that is the \"user ID\" or
\"group ID\". If INCLUDE-TRASHED is non-nil, items in the trash
are included."
  (let ((table (zotero-cache-synccache "items" nil type id include-trashed)))
    (zotero-cache-filter-data (lambda (elt) (let* ((title (plist-get elt :title))
                                                   (year (plist-get elt :date))
                                                   (creators (plist-get elt :creators)))
                                              (or
                                               (s-contains-p query title t)
                                               (s-contains-p query year t)
                                               (unless (seq-empty-p creators)
                                                 (let* ((values (cl-loop for (key value) on creators by #'cddr
                                                                         unless (eq key :creatorType)
                                                                         collect value))
                                                        (string (s-join " " values)))
                                                   (s-contains-p query string t)))))) table)))

(defun zotero-cache-item-template (itemtype)
  "Return the template for ITEMTYPE from CACHE.

The template is cached for a period of time (for example one
hour) without making further requests. Conditional requests are
not (yet) implemented in the Zotero API."
  (let* ((template (ht-get* zotero-cache "templates" "items" itemtype))
         (last-sync (plist-get template :last-sync))
         (seconds-since-last-sync (float-time (time-subtract (current-time) last-sync))))
    (if (or (null template)
            (and zotero-cache-expire (> seconds-since-last-sync zotero-cache-expire)))
        (with-demoted-errors "Error downloading template: %S"
          (zotero-sync-item-template zotero-cache itemtype))
      (plist-get template :object))))

(defun zotero-cache-attachment-template (linkmode)
  "Return the attachment template for LINKMODE from CACHE.

The template is cached for a period of time (for example one
hour) without making further requests. Conditional requests are
not (yet) implemented in the Zotero API."
  (let* ((template (ht-get* zotero-cache "templates" "attachments" linkmode))
         (last-sync (plist-get template :last-sync))
         (seconds-since-last-sync (float-time (time-subtract (current-time) last-sync))))
    (if (or (null template)
            (and zotero-cache-expire (> seconds-since-last-sync zotero-cache-expire)))
        (with-demoted-errors "Error downloading template: %S"
          (zotero-sync-attachment-template zotero-cache linkmode))
      (plist-get template :object))))

(defun zotero-cache-schema ()
  "Return the schema with item types, fields, and creator types.

The schema is cached for a period of time (for example one hour)
without making further requests."
  (let* ((schema (ht-get zotero-cache "schema"))
         (last-sync (plist-get schema :last-sync))
         (seconds-since-last-sync (float-time (time-subtract (current-time) last-sync))))
    (when (or (null schema)
              (and zotero-cache-expire (> seconds-since-last-sync zotero-cache-expire)))
      (zotero-sync-schema zotero-cache))
    schema))

(defun zotero-cache-itemtype-locale (itemtype &optional locale)
  "Return translation of ITEMTYPE for LOCALE."
  (let* ((schema (zotero-cache-schema))
         (locale (zotero-lib-string->keyword (or locale zotero-locale)))
         (itemtype (zotero-lib-string->keyword itemtype))
         (itemtypes (zotero-lib-plist-get* schema :locales locale :itemTypes)))
    (plist-get itemtypes itemtype)))

(defun zotero-cache-itemfield-locale (field &optional locale)
  "Return translation of FIELD for LOCALE."
  (let* ((schema (zotero-cache-schema))
         (locale (zotero-lib-string->keyword (or locale zotero-locale)))
         (field (if (stringp field) (zotero-lib-string->keyword field) field))
         (fields (zotero-lib-plist-get* schema :locales locale :fields)))
    (plist-get fields field)))

(defun zotero-cache-creatortype-locale (creatortype &optional locale)
  "Return translation of CREATORTYPE for LOCALE."
  (let* ((schema (zotero-cache-schema))
         (locale (zotero-lib-string->keyword (or locale zotero-locale)))
         (creatortype (zotero-lib-string->keyword creatortype))
         (creatortypes (zotero-lib-plist-get* schema :locales locale :creatorTypes)))
    (plist-get creatortypes creatortype)))

(defun zotero-cache-itemtypes ()
  "Return a list of all item types."
  (let* ((schema (ht-get* zotero-cache "schema"))
         (itemtypes (plist-get schema :itemTypes)))
    (seq-map (lambda (elt) (plist-get elt :itemType)) itemtypes)))

(defun zotero-cache-itemtypefields (itemtype)
  "Return all valid fields for ITEMTYPE.
The first is the primary field."
  (let* ((schema (zotero-cache-schema))
         (itemtypes (seq-find (lambda (elt) (equal (plist-get elt :itemType) itemtype)) (plist-get schema :itemTypes)))
         (fields (seq-map (lambda (elt) (plist-get elt :field)) (plist-get itemtypes :fields))))
    fields))

(defun zotero-cache-itemtypecreatortypes (itemtype)
  "Return a list of all valid creator types for ITEMTYPE.
The first is the primary creator type."
  (let* ((schema (zotero-cache-schema))
         (itemtypes (seq-find (lambda (elt) (equal (plist-get elt :itemType) itemtype)) (plist-get schema :itemTypes)))
         (first (seq-filter (lambda (elt) (plist-member elt :primary)) (plist-get itemtypes :creatorTypes)))
         (rest (seq-remove (lambda (elt) (plist-member elt :primary)) (plist-get itemtypes :creatorTypes)))
         (all (seq-concatenate 'list first rest))
         (creatortypes (seq-map (lambda (elt) (plist-get elt :creatorType)) all)))
    creatortypes))

(defun zotero-cache-valid-field-p (field itemtype)
  "Return t if FIELD is valid for ITEMTYPE."
  (member field (zotero-cache-itemtypefields itemtype)))

(defun zotero-cache-valid-creatortype-p (creatortype itemtype)
  "Return t if CREATORTYPE is valid for ITEMTYPE."
  (member creatortype (zotero-cache-itemtypecreatortypes itemtype)))

(defun zotero-cache-sort-by (field direction table)
  "Sort TABLE and return an ordered list of the keys.

FIELD is the prop of the object plist to be sorted. DIRECTION is
the sorting order: 'asc for ascending or 'desc for descending."
  (let ((pred (zotero-cache--pred field direction)))
    (thread-last
        (ht->alist table)
      (seq-sort-by (lambda (elt)
                     (zotero-lib-plist-get* (cdr elt) :object :data field))
                   (lambda (a b)
                     ;; keep the predicate nil-safe
                     (ignore-error wrong-type-argument
                       (funcall pred a b))))
      (seq-map #'car))))

(defun zotero-cache-field (field table)
  "Get FIELD from entries in TABLE.
Return a list of the field values."
  (ht-map (lambda (key value) (cons (zotero-lib-plist-get* value :object :data field) key)) table))

(defun zotero-cache-add-to-collection (key collection type id)
  "Add item KEY to COLLECTION.

Argument TYPE is \"user\" for your personal library, and
\"group\" for the group libraries. ID is the ID of the personal
or group library you want to access, that is the user ID or group
ID."
  (let* ((entry (zotero-cache-synccache "item" key type id t))
         (data (zotero-lib-plist-get* entry :object :data))
         (collections (zotero-lib-plist-get* entry :object :data :collections))
         (updated-collections (unless (seq-contains-p collections collection) (vconcat collections (vector collection)))))
    (zotero-cache-save (plist-put data :collections updated-collections) "items" type id)))

(defun zotero-cache-remove-from-collection (key collection type id)
  "Remove item KEY from COLLECTION.

Argument TYPE is \"user\" for your personal library, and
\"group\" for the group libraries. ID is the ID of the personal
or group library you want to access, that is the user ID or group
ID."
  (let* ((entry (zotero-cache-synccache "item" key type id t))
         (data (zotero-lib-plist-get* entry :object :data))
         (collections (zotero-lib-plist-get* entry :object :data :collections))
         (updated-collections (seq-into (seq-remove (lambda (elt) (equal elt collection)) collections) 'vector)))
    (zotero-cache-save (plist-put data :collections updated-collections) "items" type id)))

(defun zotero-cache-substitute-collection (key new old type id)
  "Substitute OLD with NEW collection in item KEY.

Argument TYPE is \"user\" for your personal library, and
\"group\" for the group libraries. ID is the ID of the personal
or group library you want to access, that is the user ID or group
ID."
  (let* ((entry (zotero-cache-synccache "item" key type id t))
         (data (zotero-lib-plist-get* entry :object :data))
         (collections (zotero-lib-plist-get* entry :object :data :collections))
         (updated-collections (cl-substitute new old collections :test #'equal)))
    (zotero-cache-save (plist-put data :collections updated-collections) "items" type id)))

(defun zotero-cache-remove-from-item (key type id)
  "Remove item KEY from parent.

Argument TYPE is \"user\" for your personal library, and
\"group\" for the group libraries. ID is the ID of the personal
or group library you want to access, that is the user ID or group
ID."
  (let* ((entry (zotero-cache-synccache "item" key type id t))
         (data (zotero-lib-plist-get* entry :object :data))
         (updated-data (zotero-lib-plist-delete data :parentItem)))
    (zotero-cache-save updated-data "items" type id)))

(defun zotero-cache-delete (resource key type id)
  "Delete KEY from cache.

RESOURCE is one of:
  - \"collections\": collections in the library
  - \"items\": all items in the library, excluding trashed items
  - \"searches\": all saved searches in the library

Argument TYPE is \"user\" for your personal library, and
\"group\" for the group libraries. ID is the ID of the personal
or group library you want to access, that is the user ID or group
ID."
  (let* ((value (zotero-cache-synccache resource key type id t))
         (synccache (zotero-cache-synccache resource nil type id t))
         (deletions (zotero-cache-deletions resource nil type id t)))
    (pcase resource
      ("collections"
       (let* ((children (zotero-cache-subcollections key synccache))
              (collection key)
              (table (zotero-cache-synccache "collection-items" collection type id t)))
         ;; Remove all subcollections from the collection
         (ht-each (lambda (key _value) (zotero-cache-remove-from-collection key collection type id)) children)
         ;; Remove all items from the collection
         (ht-each (lambda (key _value)
                    (zotero-cache-remove-from-collection type id key collection))
                  table)))
      ("items"
       ;; Remove all subitems from the item
       (let ((children (zotero-cache-subitems key synccache)))
         (ht-each (lambda (key _value) (zotero-cache-remove-from-item key type id)) children))))
    (ht-set! deletions key value)
    (ht-remove! synccache key)
    (zotero-cache-serialize)))

(defun zotero-cache-trash (key type id)
  "Move item KEY to trash.

Argument TYPE is \"user\" for your personal library, and
\"group\" for the group libraries. ID is the ID of the personal
or group library you want to access, that is the user ID or group
ID."
  (let* ((entry (zotero-cache-synccache "item" key type id t))
         (data (zotero-lib-plist-get* entry :object :data))
         (updated-data (plist-put data :deleted 1)))
    (zotero-cache-save updated-data "items" type id)))

(defun zotero-cache-restore (key type id)
  "Restore item KEY from trash.

Argument TYPE is \"user\" for your personal library, and
\"group\" for the group libraries. ID is the ID of the personal
or group library you want to access, that is the user ID or group
ID."
  (let* ((entry (zotero-cache-synccache "item" key type id t))
         (data (zotero-lib-plist-get* entry :object :data))
         (updated-data (zotero-lib-plist-delete data :deleted)))
    (zotero-cache-save updated-data "items" type id)))

(defun zotero-cache-save (data resource type id)
  "Save DATA to cache.
If DATA contains a prop `:key', it already exists in cache and is
updated, else it is uploaded and a new entry is created. Return
the object if successful, or nil.

RESOURCE is one of:
  - \"collections\": collections in the library
  - \"items\": all items in the library, excluding trashed items
  - \"searches\": all saved searches in the library

Argument TYPE is \"user\" for your personal library, and
\"group\" for the group libraries. ID is the ID of the personal
or group library you want to access, that is the user ID or group
ID."
  (let ((table (ht-get* zotero-cache "synccache" resource)))
    (if-let ((key (plist-get data :key))
             (entry (ht-get table key))
             (version (plist-get entry :version))
             (object (plist-get entry :object))
             (updated-object (plist-put object :data data)))
        (progn
          (ht-set! table key `(:synced nil :version ,version :type ,type :id ,id :object ,updated-object))
          (zotero-cache-serialize)
          updated-object)
      (if-let ((object (zotero-cache-upload data resource type id))) object nil))))

(defun zotero-cache-upload (object resource type id)
  "Upload OBJECT.
Return the object if syncing was successful, or nil.

RESOURCE is one of:
  - \"collections\": collections in the library
  - \"items\": all items in the library, excluding trashed items
  - \"searches\": all saved searches in the library

Argument TYPE is \"user\" for your personal library, and
\"group\" for the group libraries. ID is the ID of the personal
or group library you want to access, that is the user ID or group
ID."
  (message "Uploading...")
  (let* ((table (zotero-cache-synccache resource nil type id t))
         (result (pcase resource
                   ("items" (zotero-create-item object :type type :id id))
                   ("collections" (zotero-create-collection object :type type :id id))
                   ("searches" (zotero-create-search object :type type :id id))))
         (status (zotero-response-data result))
         (successful (plist-get status :successful))
         ;; (success (plist-get status :success))
         (unchanged (plist-get status :unchanged))
         (failed (plist-get status :failed)))
    (cond
     ((not (eq successful :json-empty))
      (let* ((object (plist-get successful :0))
             (key (plist-get object :key))
             (version (plist-get object :version)))
        (ht-set! table key `(:synced t :version ,version :type ,type :id ,id :object ,object))
        (zotero-cache-serialize)
        (message "Uploading...done.")
        object))
     ;; Do not update the version of Zotero objects in the
     ;; unchanged object.
     ((not (eq unchanged :json-empty))
      (let* ((key (plist-get unchanged :0))
             (object (ht-get table key)))
        (ht-set! table key (plist-put object :synced t))
        (zotero-cache-serialize)
        (message "Uploading...unchanged.")
        object))
     ((not (eq failed :json-empty))
      (let ((code (zotero-lib-plist-get* failed :0 :code))
            (message (zotero-lib-plist-get* failed :0 :message)))
        (message "Uploading...failed.")
        (user-error "Error code %d: %s" code message)))
     ;; This should not happen
     (t nil))))

(provide 'zotero-cache)

;;; zotero-cache.el ends here
