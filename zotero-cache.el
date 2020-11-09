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
(require 'iso8601)
(require 'parse-time)
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
  "When t enables caching.
Caching is automatically enabled by default."
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

(defcustom zotero-cache-max-delay 3600
  "Seconds to wait before stopping sync retries; set to 0 to disable retrying."
  :group 'zotero-cache
  :type '(integer :validate
		  (lambda (widget)
                    (let ((int (widget-value widget)))
		      (unless (< int 0)
		        (widget-put widget :error
				    "Invalid value: must be a non-negative integer")
		        widget)))))

(defcustom zotero-cache-max-retries 100
  "Seconds to wait before stopping sync retries; set to 0 to disable retrying."
  :group 'zotero-cache
  :type '(integer :validate
		  (lambda (widget)
                    (let ((int (widget-value widget)))
		      (unless (< int 0)
		        (widget-put widget :error
				    "Invalid value: must be a non-negative integer")
		        widget)))))

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
  (zotero-cache--serialize zotero-cache zotero-cache-file))

(defun zotero-cache-unserialize ()
  "Serialize the hard drive to the memory cache."
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
  (unless zotero-cache
    (setq zotero-cache
          (or (zotero-cache--unserialize zotero-cache-file)
              (zotero-cache-maybe-initialize-cache)))))

(defun zotero-cache--initialize-cache ()
  "Return an empty cache."
  (ht ("version" 1)
      ("libraries" (ht-create))
      ("groups" (ht-create))
      ("synccache" (ht-create))
      ("deletions" (ht-create))
      ("templates" (ht ("items" (ht-create))
                       ("attachments" (ht-create))))))

(cl-defun zotero-cache--maybe-initialize-library (&key cache id)
  "Initialize synccache and deletions in CACHE for ID if it
doesn't exist."
  (let ((synccache (ht-get cache "synccache"))
        (deletions (ht-get cache "deletions")))
    (unless (ht-contains? synccache id)
      (ht-set! synccache id (ht ("collections" (ht-create))
                                ("items" (ht-create))
                                ("searches" (ht-create)))))
    (unless (ht-contains? deletions id)
      (ht-set! deletions id (ht ("collections" (ht-create))
                                ("items" (ht-create))
                                ("searches" (ht-create)))))
    cache))

(defun zotero-cache--mergable-plist-p (plist1 plist2)
  "Return non-nil if PLIST1 and PLIST2 can be merged without conflicts.
Two plists are considered mergable when the same keys don't have different values."
  (loop for (key val) on plist1 by #'cddr
        always (or (not (plist-member plist2 key))
                   (eq val :json-false)
                   (eq val :json-empty)
                   (eq (plist-get plist2 key) :json-false)
                   (eq (plist-get plist2 key) :json-empty)
                   (equal val (plist-get plist2 key)))))

(defun zotero-cache-merge-plist (plist1 plist2)
  "Merge PLIST2 into PLIST1."
  (loop for (key val) on plist2 by #'cddr do
        (unless (or (eq val :json-false)
                    (eq val :json-empty))
          (plist-put plist1 key val))
        finally return plist1))

(defun zotero-cache-read-access-p (plist)
  "Return t if read access is permitted, else return nil.

Argument PLIST is the permissions of the user or group library as
returned by `zotero-lib-get-key'."
  (eq (plist-get plist :library) t))

(defun zotero-cache-write-access-p (plist)
  "Return t if write access is permitted, else return nil.

Argument PLIST is the permissions of the user or group library as
returned by `zotero-lib-get-key'."
  (eq (plist-get plist :write) t))

(defun zotero-cache-read-only-p (plist)
  "Return t if read only, else return nil.

Argument PLIST is the permissions of the user or group library as
returned by `zotero-lib-get-key'."
  (and (zotero-cache-read-access-p plist) (not (zotero-cache-write-access-p plist))))

(defun zotero-cache-note-access-p (plist)
  "Return t if note access is permitted, else return nil.

Argument PLIST is the permissions of the user or group library as
returned by `zotero-lib-get-key'."
  (eq (plist-get plist :notes) t))

(defun zotero-cache-file-access-p (plist)
  "Return t if file access is permitted, else return nil.

Argument PLIST is the permissions of the user or group library as
returned by `zotero-lib-get-key'."
  (eq (plist-get plist :files) t))

(defun zotero-cache-parentitem (key table)
  "Return the parent of KEY in TABLE, or nil."
  (let ((value (ht-get table key)))
    (zotero-lib-plist-get* value :object :data :parentItem)))

(defun zotero-cache-parentcollection (key table)
  "Return the parent of KEY in TABLE, or nil."
  (let ((value (ht-get table key)))
    (zotero-lib-plist-get* value :object :data :parentCollection)))

(defun zotero-cache-subitems (key table)
  "Return the subcollections of KEY in TABLE."
  (zotero-cache--filter (lambda (elt) (equal (plist-get elt :parentItem) key)) table))

(defun zotero-cache-subcollections (key table)
  "Return the subcollections of KEY in TABLE."
  (zotero-cache--filter (lambda (elt) (equal (plist-get elt :parentCollection) key)) table))

(defun zotero-cache-has-subitems-p (key table)
  "Return non-nil if KEY in TABLE has subitems."
  (zotero-cache--some (lambda (elt) (equal (plist-get elt :parentItem) key)) table))

(defun zotero-cache-has-subcollections-p (key table)
  "Return non-nil if KEY in TABLE has subcollections."
  (zotero-cache--some (lambda (elt) (equal (plist-get elt :parentCollection) key)) table))

(defun zotero-cache-has-attachments-p (key table)
  "Return non-nil if KEY in TABLE has attachments."
  (zotero-cache--some (lambda (elt)
                        (and (equal (plist-get value :parentItem) key)
                             (equal (plist-get value :itemType) "attachment")))
                      table))

(defun zotero-cache-has-notes-p (key table)
  (zotero-cache-some (lambda (elt)
                       (and (equal (plist-get elt :parentItem) key)
                            (equal (plist-get elt :itemType) "note")))
                     table))
  "Return non-nil if KEY in TABLE has attachments."

(defun zotero-cache-itemtype-locale (itemtype &optional locale)
  "Return translation of ITEMTYPE for LOCALE."
  (let* ((schema (zotero-cache-schema))
         (locale (zotero-lib-string->keyword (or locale zotero-lib-locale)))
         (itemtype (zotero-lib-string->keyword itemtype))
         (itemtypes (zotero-lib-plist-get* schema :locales locale :itemTypes)))
    (plist-get itemtypes itemtype)))

(defun zotero-cache-itemfield-locale (field &optional locale)
  "Return translation of FIELD for LOCALE."
  (let* ((schema (zotero-cache-schema))
         (locale (zotero-lib-string->keyword (or locale zotero-lib-locale)))
         (field (if (stringp field) (zotero-lib-string->keyword field) field))
         (fields (zotero-lib-plist-get* schema :locales locale :fields)))
    (plist-get fields field)))

(defun zotero-cache-creatortype-locale (creatortype &optional locale)
  "Return translation of CREATORTYPE for LOCALE."
  (let* ((schema (zotero-cache-schema))
         (locale (zotero-lib-string->keyword (or locale zotero-lib-locale)))
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

(defun zotero-cache-item-template (itemtype)
  "Return the template for ITEMTYPE from CACHE.

The template is cached for a period of time (e.g., one hour)
without making further requests. Conditional requests are
not (yet) implemented in the Zotero API."
  (let* ((template (ht-get* zotero-cache "templates" "items" itemtype))
         (last-sync (plist-get template :last-sync))
         (seconds-since-last-sync (float-time (time-subtract (current-time) last-sync))))
    (when (or (null template) (> seconds-since-last-sync zotero-cache-expire))
      (with-demoted-errors "Error downloading template: %S"
        (zotero-cache--sync-item-template :cache zotero-cache :itemtype itemtype)))
    (plist-get template :object)))

(defun zotero-cache-attachment-template (linkmode)
  "Return the attachment template for LINKMODE from CACHE.

The template is cached for a period of time (e.g., one hour)
without making further requests. Conditional requests are
not (yet) implemented in the Zotero API."
  (let* ((template (ht-get* zotero-cache "templates" "attachments" linkmode))
         (last-sync (plist-get template :last-sync))
         (seconds-since-last-sync (float-time (time-subtract (current-time) last-sync))))
    (when (or (null template) (> seconds-since-last-sync zotero-cache-expire))
      (with-demoted-errors "Error downloading template: %S"
        (zotero-cache--sync-attachment-template :cache zotero-cache :linkmode linkmode)))
    (plist-get template :object)))

(defun zotero-cache-schema ()
  "Return the schema with item types, fields, and creator types.

The schema is cached for a period of time (e.g., one hour)
without making further requests."
  (let* ((schema (ht-get zotero-cache "schema"))
         (last-sync (plist-get schema :last-sync))
         (seconds-since-last-sync (float-time (time-subtract (current-time) last-sync))))
    (when (or (null schema) (> seconds-since-last-sync zotero-cache-expire))
      (zotero-cache--sync-schema :cache zotero-cache))
    schema))

(defun zotero-cache-valid-creatortype-p (creatortype itemtype)
  "Return t if CREATORTYPE is valid for ITEMTYPE."
  (member creatortype (zotero-cache-itemtypecreatortypes itemtype)))

;; FIXME
(cl-defun zotero-cache-search (&key type id resource keys include-trashed query mode since)
  "Search the cache.
Search titles and individual creator fields by default. Use the
MODE argument to change the mode. Default is
\"titleCreatorYear\". To include full-text content, use
\"everything\".

Keyword argument TYPE is \"user\" for your personal library, and
\"group\" for the group libraries. Keyword argument ID is the ID
of the personal or group library you want to access, e.g. the
\"user ID\" or \"group ID\"."
  (let* ((items (s-join "," keys))
         (table (ht-get* zotero-cache "synccache")))
    (plist-get response :data)))

(cl-defun zotero-cache-save (&key type id resource data)
  "Save DATA to cache.
If DATA contains a prop `:key', it already exists in cache and is
updated, else it is uploaded and a new entry is created. Return
the object if successful, or nil.

Keyword argument TYPE is \"user\" for your personal library, and
\"group\" for the group libraries. Keyword argument ID is the ID
of the personal or group library you want to access, e.g. the
\"user ID\" or \"group ID\"."
  (let ((table (ht-get* zotero-cache "synccache" id resource))
        (key (plist-get data :key)))
    (if key
        (let* ((entry (ht-get table key))
               (version (plist-get entry :version))
               (object (plist-get entry :object))
               (updated-object (plist-put object :data data)))
          (ht-set! table key `(:synced nil :version ,version :object ,updated-object))
          updated-object)
      (message "Uploading...")
      (if-let* ((object (zotero-cache-upload data :type type :id id :resource resource))
                (key (plist-get object :key))
                (version (plist-get object :version)))
          (progn
            (message "Uploading...done.")
            (ht-set! table key `(:synced t :version ,version :object ,object))
            (zotero-cache-serialize)
            object)
        (message "Uploading...failed.")
        nil))))

(cl-defun zotero-cache-upload (object &key type id resource)
  "Upload OBJECT.
Return the object if syncing was successful, or nil.

Keyword argument TYPE is \"user\" for your personal library, and
\"group\" for the group libraries. Keyword argument ID is the ID
of the personal or group library you want to access, e.g. the
\"user ID\" or \"group ID\"."
  (let* ((table (zotero-cache-get :type type :id id :resource resource))
         (token (zotero-auth-token))
         (api-key (zotero-auth-api-key token))
         (status (pcase resource
                   ("items" (zotero-lib-create-item type id api-key object))
                   ("collections" (zotero-lib-create-collection type id api-key object))
                   ("searches" (zotero-lib-create-search type id api-key object))))
         (successful (plist-get status :successful))
         (success (plist-get status :success))
         (unchanged (plist-get status :unchanged))
         (failed (plist-get status :failed)))
    (cond
     ((not (eq successful :json-empty))
      (let* ((object (plist-get successful :0))
             (key (plist-get object :key))
             (version (plist-get object :version)))
        (ht-set! table key `(:synced t :version ,version :object ,object))
        object))
     ;; Do not update the version of Zotero objects in the
     ;; unchanged object.
     ((not (eq unchanged :json-empty))
      (let* ((key (plist-get unchanged :0))
             (object (ht-get table key)))
        (ht-set! table key (plist-put object :synced t))
        object))
     ((not (eq failed :json-empty))
      (let ((code (zotero-lib-plist-get* failed :0 :code))
            (message (zotero-lib-plist-get* failed :0 :message)))
        (error "Error code %d: %s" code message)))
     ;; This should not happen
     (t nil))))

(defun zotero-cache--year (string)
  "Return year in STRING, or nil."
  (let* ((regexp "[[:digit:]]\\{4\\}")
         (match (string-match regexp string)))
    (when match (match-string 0 string))))

(defun zotero-cache--filter (pred table)
  "Return a table containing entries in TABLE for which PRED returns non-nil.
PRED is a function that takes a data element as its first
argument."
  (thread-last
      table
    (ht-select (lambda (key value)
                 ;; keep the predicate nil-safe
                 (ignore-error wrong-type-argument
                   (funcall pred (zotero-lib-plist-get* value :object :data)))))))

(defun zotero-cache--some (pred table)
  "Return non-nil if PRED is satisfied for at least one element of TABLE.
PRED is a function that takes a data element as its first
argument."
  (if (ht-find (lambda (key value) (funcall pred (zotero-lib-plist-get* value :object :data))) table) t nil))

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
                ;; The :version and :mtime fields are integers
                ((or :version :mtime)
                 #'<)
                ;; The rest of the fields are strings
                (_ #'string-lessp))))
    (pcase direction
      ('asc
       pred)
      ('desc
       (lambda (a b) (funcall pred b a))))))

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

(cl-defun zotero-cache-field (field table)
  "Get FIELD from entries in TABLE.
Return a list of the field values."
  (ht-map (lambda (key value) (cons (zotero-lib-plist-get* value :object :data field) key)) table))

(cl-defun zotero-cache-get (&key type id resource key include-trashed)
  "Get RESOURCE from library in cache.
Return table for multiple item request or entry for a single item
request.

Keyword argument TYPE is \"user\" for your personal library, and
\"group\" for the group libraries. Keyword argument ID is the ID
of the personal or group library you want to access, e.g. the
\"user ID\" or \"group ID\"."
  (pcase resource
    ("libraries"
     (ht-get* zotero-cache "libraries"))
    ("library"
     (ht-get* zotero-cache "libraries" id))
    ("groups"
     (ht-get zotero-cache "groups"))
    ("group"
     (ht-get* zotero-cache "groups" id))
    ("collections"
     (let ((table (ht-get* zotero-cache "synccache" id "collections")))
       table))
    ("collections-top"
     (let* ((table (ht-get* zotero-cache "synccache" id "collections"))
            (selection (zotero-cache--filter
                        (lambda (elt) (or (not (plist-member elt :parentCollection))
                                          (eq (plist-get elt :parentCollection) :json-false)))
                        table)))
       selection))
    ("collection"
     (ht-get* zotero-cache "synccache" id "collections" key))
    ("subcollections"
     (let* ((table (ht-get* zotero-cache "synccache" id "collections"))
            (selection (zotero-cache--filter
                        (lambda (elt) (equal (plist-get elt :parentCollection) key))
                        table)))
       selection))
    ("items"
     (let* ((table (ht-get* zotero-cache "synccache" id "items"))
            (selection (if include-trashed
                           table
                         (zotero-cache--filter (lambda (elt) (not (eq (plist-get elt :deleted) 1)))
                                               table))))
       selection))
    ("items-top"
     (let* ((table (ht-get* zotero-cache "synccache" id "items"))
            (selection (if include-trashed
                           (zotero-cache--filter (lambda (elt) (or (eq (plist-get elt :collections) [])
                                                                   (eq (plist-get elt :collections) :json-empty)))
                                                 table)
                         (zotero-cache--filter (lambda (elt) (and (not (eq (plist-get elt :deleted) 1))
                                                                  (or (eq (plist-get elt :collections) [])
                                                                      (eq (plist-get elt :collections) :json-empty))))
                                               table))))
       selection))
    ("trash-items"
     (let* ((table (ht-get* zotero-cache "synccache" id "items"))
            (selection (zotero-cache--filter (lambda (elt) (eq (plist-get elt :deleted) 1))
                                             table)))
       selection))
    ("item"
     (ht-get* zotero-cache "synccache" id "items" key))
    ("item-children"
     (let* ((table (ht-get* zotero-cache "synccache" id "items"))
            (selection (if include-trashed
                           (zotero-cache--filter
                            (lambda (elt) (equal (plist-get elt :parentItem) key))
                            table)
                         (zotero-cache--filter
                          (lambda (elt) (and (not (eq (plist-get elt :deleted) 1))
                                             (equal (plist-get elt :parentItem) key)))
                          table))))
       selection))
    ;; TODO
    ;; ("publication-items")
    ("collection-items"
     (let* ((table (ht-get* zotero-cache "synccache" id "items"))
            (selection (if include-trashed
                           (zotero-cache--filter
                            (lambda (elt) (seq-contains-p (plist-get elt :collections) key))
                            table)
                         (zotero-cache--filter
                          (lambda (elt) (and (not (eq (plist-get elt :deleted) 1))
                                             (seq-contains-p (plist-get elt :collections) key)))
                          table))))
       selection))
    ("collection-items-top"
     (let* ((table (ht-get* zotero-cache "synccache" id "items"))
            (selection (if include-trashed
                           (zotero-cache--filter
                            (lambda (elt) (and (seq-contains-p (plist-get elt :collections) key)
                                               (or (not (plist-member elt :parentItem))
                                                   (eq (plist-get elt :parentItem) :json-false))))
                            table)
                         (zotero-cache--filter
                          (lambda (elt) (and (not (eq (plist-get elt :deleted) 1))
                                             (seq-contains-p (plist-get elt :collections) key)
                                             (or (not (plist-member elt :parentItem))
                                                 (eq (plist-get elt :parentItem) :json-false))))
                          table))))
       selection))
    ("searches"
     (let ((table (ht-get* zotero-cache "synccache" id "searches")))
       table))
    ("search"
     (ht-get* zotero-cache "synccache" id "search" key))
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
    ;; ("deleted")
    ))

(cl-defun zotero-cache-add-to-collection (&key type id key collection)
  "Add item KEY to COLLECTION.

Keyword argument TYPE is \"user\" for your personal library, and
\"group\" for the group libraries. Keyword argument ID is the ID
of the personal or group library you want to access, e.g. the
\"user ID\" or \"group ID\"."
  (let* ((entry (zotero-cache-get :type type :id id :resource "item" :key key))
         (data (zotero-lib-plist-get* entry :object :data))
         (collections (zotero-lib-plist-get* entry :object :data :collections))
         (updated-collections (unless (seq-contains-p collections collection) (vconcat collections (vector collection)))))
    (zotero-cache-save :type type :id id :resource "items" :data (plist-put data :collections updated-collections))))

(cl-defun zotero-cache-remove-from-collection (&key type id key collection)
  "Remove item KEY from COLLECTION.

Keyword argument TYPE is \"user\" for your personal library, and
\"group\" for the group libraries. Keyword argument ID is the ID
of the personal or group library you want to access, e.g. the
\"user ID\" or \"group ID\"."
  (let* ((entry (zotero-cache-get :type type :id id :resource "item" :key key))
         (data (zotero-lib-plist-get* entry :object :data))
         (collections (zotero-lib-plist-get* entry :object :data :collections))
         (updated-collections (seq-into (seq-remove (lambda (elt) (equal elt collection)) collections) 'vector)))
    (zotero-cache-save :type type :id id :resource "items" :data (plist-put data :collections updated-collections))))

(cl-defun zotero-cache-substitute-collection (&key type id key new old)
  "Substitute OLD with NEW collection in item KEY.

Keyword argument TYPE is \"user\" for your personal library, and
\"group\" for the group libraries. Keyword argument ID is the ID
of the personal or group library you want to access, e.g. the
\"user ID\" or \"group ID\"."
  (let* ((entry (zotero-cache-get :type type :id id :resource "item" :key key))
         (data (zotero-lib-plist-get* entry :object :data))
         (collections (zotero-lib-plist-get* entry :object :data :collections))
         (updated-collections (cl-substitute new old collection :test #'equal)))
    (zotero-cache-save :type type :id id :resource "items" :data (plist-put data :collections updated-collections))))

(cl-defun zotero-cache-delete (&key type id resource key)
  "Delete KEY from cache.

Keyword argument TYPE is \"user\" for your personal library, and
\"group\" for the group libraries. Keyword argument ID is the ID
of the personal or group library you want to access, e.g. the
\"user ID\" or \"group ID\"."
  (let* ((value (ht-get* zotero-cache "synccache" id resource key))
         (synccache (ht-get* zotero-cache "synccache" id resource))
         (deletions (ht-get* zotero-cache "deletions" id resource)))
    ;; Remove all items from the collection
    (when (equal resource "collections")
      (let* ((collection key)
             (table (zotero-cache-get :type type :id id :resource "collection-items" :key collection :include-trashed t)))
        (ht-each (lambda (key value)
                   (zotero-cache-remove-from-collection :type type :id id :key key :collection collection))
                 table)))
    (ht-set! deletions key value)
    (ht-remove! synccache key)))

(cl-defun zotero-cache-trash (&key type id key &allow-other-keys)
  "Move item KEY to trash.

Keyword argument TYPE is \"user\" for your personal library, and
\"group\" for the group libraries. Keyword argument ID is the ID
of the personal or group library you want to access, e.g. the
\"user ID\" or \"group ID\"."
  (let* ((entry (zotero-cache-get :type type :id id :resource "item" :key key))
         (data (zotero-lib-plist-get* entry :object :data))
         (updated-data (plist-put data :deleted 1)))
    (zotero-cache-save :type type :id id :resource "items" :data updated-data)))

(cl-defun zotero-cache-restore (&key type id key &allow-other-keys)
  "Restore item KEY from trash.

Keyword argument TYPE is \"user\" for your personal library, and
\"group\" for the group libraries. Keyword argument ID is the ID
of the personal or group library you want to access, e.g. the
\"user ID\" or \"group ID\"."
  (let* ((entry (zotero-cache-get :type type :id id :resource "item" :key key))
         (data (zotero-lib-plist-get* entry :object :data))
         (updated-data (zotero-lib-plist-delete data :deleted)))
    (zotero-cache-save :type type :id id :resource "trash-items" :data updated-data)))

(defun zotero-cache-sync (&optional retries)
  "Sync the Zotero library.

Optional argument RETRIES is used to count the number of
retries."
  (interactive)
  (let* ((token (zotero-auth-token))
         (id (zotero-auth-userid token))
         (api-key (zotero-auth-api-key token)))
    (message "Syncing cache...")
    (zotero-cache-maybe-initialize-cache)
    (let* ((cache (copy-tree zotero-cache))
           (result (zotero-cache--sync :cache cache :id id :api-key api-key))
           (retries (or retries 0)))
      (pcase result
        ((pred ht?)
         (message "Syncing cache...done")
         (message "Writing cache...")
         (setq zotero-cache result)
         (zotero-cache-serialize)
         (message "Writing cache...done"))
        ;; For each response from the API, check the Last-Modified-Version to see
        ;; if it has changed since the Last-Modified-Version returned from the
        ;; first request (e.g., collections?since=). If it has, restart the
        ;; process of retrieving updated and deleted data, waiting increasing
        ;; amounts of time between restarts to give the other client the
        ;; opportunity to finish.
        ('concurrent-update
         (cond
          ((zerop zotero-cache-max-delay)
           (user-error "Syncing cache failed: concurrent update."))
          ((zerop zotero-cache-max-retries)
           (user-error "Syncing cache failed: concurrent update."))
          ((> retries zotero-cache-max-retries)
           (user-error "Syncing cache failed: concurrent update and maximum of %d retries reached" zotero-cache-max-retries))
          (t
           (let* ((intervals (seq-map (lambda (elt) (expt 2 elt)) (number-sequence 1 retries))) ; exponential increase in delay
                  (total-delay (seq-reduce #'+ intervals 0)))
             (if (> total-delay zotero-cache-max-delay)
                 (user-error "Syncing cache failed: concurrent update and maximum of %d seconds delay reached" zotero-cache-max-delay)
               (sleep-for (expt 2 retries))
               (zotero-cache-sync (1+ retries)))))))
        ('quit
         (message "Syncing cache...quit"))
        (_ ; this should not happen
         (error "Syncing cache failed: unknown error")))
      (message "Syncing schema...")
      (if-let ((result (zotero-cache--sync-schema :cache cache)))
          (progn
            (message "Syncing schema...done")
            (message "Writing cache...")
            (setq zotero-cache result)
            (zotero-cache-serialize)
            (message "Writing cache...done"))
        (message "Syncing schema...failed"))
      (message "Syncing templates...")
      (if-let ((result (zotero-cache--sync-templates :cache cache)))
          (progn
            (message "Syncing templates...done")
            (message "Writing cache...")
            (setq zotero-cache result)
            (zotero-cache-serialize)
            (message "Writing cache...done"))
        (message "Syncing templates...failed"))

      ;; TODO: check concurrent updates
      (when zotero-cache-enable-storage
        (zotero-cache-sync-attachments :cache cache :api-key api-key)))))

(cl-defun zotero-cache-sync-attachments (&key cache api-key)
  "Sync the Zotero library.
CACHE is the hash table containing the cache. API-KEY is the
Zotero API key."
  (let ((libraries (ht-get cache "libraries")))
    ;; Perform the following steps for each library:
    (cl-loop for id being the hash-keys of libraries do
             (let* ((value (ht-get libraries id))
                    (type (plist-get value :type)))

               (when zotero-cache-enable-storage
                 (message "Syncing attachments to storage for %s %s..." type id)
                 (zotero-cache--sync-attachments :cache cache :type type :id id :api-key api-key)
                 (message "Syncing attachments to storage for %s %s...done" type id))))))

(cl-defun zotero-cache--sync (&key cache id api-key)
  "Sync the Zotero library.

Keyword CACHE is the hash table containing the cache. keyword ID is the ID
of the personal or group library you want to access, e.g. the
\"user ID\" or \"group ID\". Keyword API-KEY is the
Zotero API key."
  (catch 'sync
    (message "Verifying key access...")
    (zotero-cache--sync-verify-key :cache cache :api-key api-key)
    (message "Verifying key access...done")

    (message "Syncing group metadata...")
    (zotero-cache--sync-metadata :cache cache :id id :api-key api-key)
    (message "Syncing group metadata...done")

    (let ((libraries (ht-get cache "libraries")))
      ;; Perform the following steps for each library:
      (cl-loop for id being the hash-keys of libraries do
               (let* ((value (ht-get libraries id))
                      (type (plist-get value :type))
                      (read-only (zotero-cache-read-only-p value)))

                 (zotero-cache--maybe-initialize-library :cache cache :id id)

                 (message "Syncing remotely updated data for %s %s..." type id)
                 (zotero-cache--sync-remotely-updated :cache cache :type type :id id :api-key api-key)
                 (message "Syncing remotely updated data for %s %s...done" type id)

                 (message "Syncing remotely deleted data for %s %s..." type id)
                 (zotero-cache--sync-remotely-deleted :cache cache :type type :id id :api-key api-key)
                 (message "Syncing remotely deleted data for %s %s...done" type id)

                 (unless read-only
                   (message "Syncing locally updated data for %s %s..." type id)
                   (zotero-cache--sync-locally-updated :cache cache :type type :id id :api-key api-key)
                   (message "Syncing locally updated data for %s %s...done" type id)

                   (message "Syncing locally deleted data for %s %s..." type id)
                   (zotero-cache--sync-locally-deleted :cache cache :type type :id id :api-key api-key)
                   (message "Syncing locally deleted data for %s %s...done" type id))

                 ;; After saving all remote changes without the remote version
                 ;; changing during the process, save Last-Modified-Version
                 ;; from the last run as the new local library version.
                 (let* ((value (ht-get libraries id))
                        (version (plist-get value :version)))
                   (plist-put value :storage-version version)
                   (plist-put value :last-sync (current-time))
                   (ht-set! libraries id value)))))
    cache))

(cl-defun zotero-cache--sync-schema (&key cache)
  "Store the schema in CACHE.

The schema is downloaded as a single file from \"https://api.zotero.org/schema\"."
  (let* ((schema (ht-get (or cache zotero-cache) "schema"))
         (etag (plist-get schema :etag))
         (response (zotero-lib-retrieve :url "https://api.zotero.org/schema" :if-none-match etag))
         (status-code (plist-get response :status-code)))
    (pcase status-code
      (304
       (plist-put schema :last-sync (current-time)))
      (200
       (let* ((data (plist-get response :data))
              (etag (plist-get response :etag)))
         (ht-set! cache "schema" data)
         (plist-put (ht-get cache "schema") :etag etag)
         (plist-put (ht-get cache "schema") :last-sync (current-time)))))
    cache))

(cl-defun zotero-cache--process-updates (&key table objects version)
  "Update TABLE with OBJECTS.
Return the updated table when success or nil when failed.

VERSION is the \"Last-Modified-Version\"."
  (seq-doseq (object objects)
    (let* ((key (plist-get object :key))
           (value (ht-get table key))
           (default))
      ;; FIXME: when resolving a conflict between a locally deleted object and a
      ;; remotely modified object in favor of the remote object, remove it from
      ;; the delete log.
      (pcase value
        ;; if object doesn't exist locally:
        ;; create local object with version = Last-Modified-Version and set synced = true
        ((pred null)
         (ht-set! table key `(:synced t :version ,version :object ,object)))
        ;; if object hasn't been modified locally (synced == true):
        ;; overwrite with synced = true and version = Last-Modified-Version
        ((guard (eq (plist-get value :synced) t))
         (ht-set! table key `(:synced t :version ,version :object ,object)))
        ;; if object hasn't changed:
        ;; set synced = true and version = Last-Modified-Version
        ((guard (equal (plist-get value :object) object))
         (ht-set! table key `(:synced t :version ,version :object ,object)))
        ;; if changes can be automatically merged:
        ;; apply changes from each side and set synced = true and version = Last-Modified-Version
        ((guard (zotero-cache--mergable-plist-p (plist-get value :object) object))
         (let ((merged (zotero-cache-merge-plist (plist-get value :object) object)))
           (ht-set! table key `(:synced t :version ,version :object ,merged))))
        ;; else:
        ;; prompt user to choose a side or merge conflicts
        ;; TODO: global variable to set default action: 'ask 'keep-local 'keep-remote 'manual
        (value
         (let ((choice (or default
                           (read-multiple-choice
                            "Conflict between local and remote object cannot be automatically resolved. How should this be resolved? "
                            '((?d "see the difference side by side")
                              (?l "keep the local copy")
                              (?r "keep the remote copy")
                              (?L "always keep the local copy")
                              (?R "always keep the remote copy")
                              (?q "quit"))))))
           (pcase (car choice)
             (?d
              (let* ((local-data (zotero-lib-plist-get* value :object :data))
                     (remote-data (plist-get object :data))
                     (choice (zotero-diff local-data remote-data)))
                (pcase (car choice)
                  ;; if user chooses local copy:
                  ;; synced = false and set a flag to restart the sync when finished
                  ;; REVIEW: flag to restart the sync when finished?
                  (?l
                   (ht-set! table key `(:synced nil :object ,(plist-get value :object))))
                  ;; if user chooses remote copy:
                  ;; overwrite with synced = true and version = Last-Modified-Version
                  (?r
                   (ht-set! table key `(:synced t :version ,version :object ,object)))
                  (?q
                   (throw 'sync 'quit)))))
             ;; if user chooses local copy:
             ;; synced = false and set a flag to restart the sync when finished
             ;; REVIEW: flag to restart the sync when finished?
             (?l
              (ht-set! table key `(:synced nil :object ,(plist-get value :object))))
             ;; if user chooses remote copy:
             ;; overwrite with synced = true and version = Last-Modified-Version
             (?r
              (ht-set! table key `(:synced t :version ,version :object ,object)))
             (?L
              (ht-set! table key `(:synced nil :object ,(plist-get value :object)))
              (setq default ?l))
             (?R
              (ht-set! table key `(:synced t :version ,version :object ,object))
              (setq default ?r))
             (?q
              (throw 'sync 'quit))))))))
  table)

(cl-defun zotero-cache--process-deletions (&key table keys version)
  "Delete KEYS from TABLE.
Return the updated table when success or nil when failed."
  (let (default)
    (seq-doseq (key keys)
      (pcase (ht-get table key)
        ;; if local object doesn't exist:
        ;; continue
        ((pred null))
        ;; if object hasn't been modified locally (synced == true):
        ;; delete local object, skipping delete log
        ((and value (guard (eq (plist-get value :synced) t)))
         (ht-remove! table key))
        ;; else:
        ;; perform conflict resolution
        (value
         (let ((choice (read-multiple-choice
                        "Deleted object was modified. How should this be resolved? "
                        '((?d "delete the locally modified object")
                          (?k "keep the locally modified object")
                          (?D "always delete the locally modified object")
                          (?K "always keep the locally modified object")
                          (?q "quit")))))
           (pcase (car choice)
             ;; if user chooses deletion, delete local object, skipping delete log
             (?d
              (ht-remove! table key))
             ;; if user chooses local modification, keep object and set synced = true and version = Last-Modified-Version
             (?k
              (let ((object (plist-get value :object)))
                (ht-set! table key `(:synced t :version ,version :object ,object))))
             (?D
              (ht-remove! table key)
              (setq default ?d))
             (?K
              (let ((object (plist-get value :object)))
                (ht-set! table key `(:synced t :version ,version :object ,object)))
              (setq default ?k))
             (?q
              (throw 'sync 'quit))))))))
  table)

(cl-defun zotero-cache--get-remotely-updated (&key type id resource api-key keys)
  "Return remotely updated data.
Return a plist with the props `:version' and `:data'.

Keyword argument TYPE is \"user\" for your personal library, and
\"group\" for the group libraries. Keyword argument ID is the ID
of the personal or group library you want to access, e.g. the
\"user ID\" or \"group ID\"."
  (when keys
    (let ((partitions (seq-partition keys 50))
          (number 0)
          (version)
          (data))
      (dolist (partition partitions data)
        (message "Retrieving %d-%d of %d updated %s..." (1+ number) (+ number (length partition)) (length keys) resource)
        (let* ((param-key (pcase resource
                            ("collections" :collectionkey)
                            ("items" :itemkey)
                            ("searches" :searchkey)))
               (param-value (s-join "," (seq-map #'zotero-lib-keyword->string partition)))
               (response (zotero-lib-retrieve :type type :id id :resource resource :api-key api-key :include-trashed "1" param-key param-value))
               (remote-version (plist-get response :version))
               (remote-data (plist-get response :data)))
          (message "Retrieving %d-%d of %d updated %s...done" (1+ number) (+ number (length partition)) (length keys) resource)
          (setq number (+ number (length partition)))
          (setq version remote-version)
          (setq data (seq-concatenate 'vector data remote-data))))
      `(:version ,version :data ,data))))

(cl-defun zotero-cache--get-locally-updated (&key cache id resource)
  "Return locally updated keys.

Keyword CACHE is the hash table containing the cache. keyword ID is the ID
of the personal or group library you want to access, e.g. the
\"user ID\" or \"group ID\". Keyword API-KEY is the
Zotero API key."
  (let* ((table (ht-get* cache "synccache" id resource))
         (modified (ht-reject (lambda (key value) (plist-get value :synced)) table))
         (keys (ht-keys modified)))
    keys))

(cl-defun zotero-cache--get-locally-deleted (&key cache id resource)
  "Return locally deleted keys.

Keyword CACHE is the hash table containing the cache. keyword ID is the ID
of the personal or group library you want to access, e.g. the
\"user ID\" or \"group ID\". Keyword API-KEY is the
Zotero API key."
  (let* ((table (ht-get* cache "deletions" id resource))
         (keys (ht-keys table)))
    keys))

(cl-defun zotero-cache--sync-verify-key (&key cache api-key)
  "Verify that API-KEY has the expected access to the library.
Return updated cache.

If necessary, show a warning that the user no longer has
sufficient access and offer to remove a local library or reset
local changes. Argument CACHE is the current cache, e.g. (a copy
of) `zotero-cache'."
  ;; TODO: deduplicate handling of user and group libraries
  (let* ((remote-access (zotero-lib-get-key :api-key api-key))
         (libraries (ht-get cache "libraries"))
         (groups (ht-get cache "groups"))
         (synccache (ht-get cache "synccache"))
         (deletions (ht-get cache "deletions")))
    (let ((id (number-to-string (plist-get remote-access :userID)))
          (permissions (zotero-lib-plist-get* remote-access :access :user)))
      ;; TODO: only write if changed
      ;; TODO: include note and file access of user library
      (pcase permissions
        ;; For each library without read access: offer to remove the library
        ((guard (not (zotero-cache-read-access-p permissions)))
         (if (y-or-n-p (format "User library %s has no read access. Remove from cache? " id))
             (progn
               (ht-remove! libraries id)
               (ht-remove! synccache id)
               (ht-remove! deletions id))
           (if (ht-contains? libraries id)
               (let ((value (ht-get libraries id)))
                 (plist-put value :library :json-false)
                 (plist-put value :write :json-false)
                 (ht-set! libraries id value))
             (ht-set! libraries id `(:id ,id :type "user" :library :json-false :write :json-false)))
           (message "User library %s has no read access, so cannot be synced. " id)))
        ;; For each library without write access: offer to reset local changes
        ((pred zotero-cache-read-only-p) ; read only
         (if (ht-contains? libraries id)
             (let ((value (ht-get libraries id)))
               (plist-put value :library t)
               (plist-put value :write :json-false)
               (ht-set! libraries id value))
           (ht-set! libraries id `(:id ,id :type "user" :library t :write :json-false)))
         (when (ht-contains? (ht-get* zotero-cache "synccache") id)
           (let (updated)
             (dolist (resource '("collections" "items" "searches") updated)
               (push (cons resource (zotero-cache--get-locally-updated :cache cache :id id :resource resource)) updated))
             (let ((updated-p (seq-some #'cdr updated)))
               (when (and updated-p
                          (y-or-n-p (format "Group library %s has no write access. Reset local changes? " id)))
                 (cl-loop for resource in '("collections" "items" "searches") do
                          (let ((keys (cdr (assoc resource updated)))
                                (table (ht-get* synccache id resource)))
                            (dolist (key keys)
                              (ht-remove! table key))))
                 (message "All local changes in user library %s are reset." id)))))
         (when (ht-contains? (ht-get* zotero-cache "deletions") id)
           (let (deleted)
             (dolist (resource '("collections" "items" "searches") deleted)
               (push (cons resource (zotero-cache--get-locally-deleted :cache cache :id id :resource resource)) deleted))
             (let ((deleted-p (seq-some #'cdr deleted)))
               (when (and deleted-p
                          (y-or-n-p (format "Group library %s has no write access. Reset local deletions? " id)))
                 (cl-loop for resource in '("collections" "items" "searches") do
                          (let ((keys (cdr (assoc resource deleted)))
                                (table (ht-get* deletions id resource)))
                            (dolist (key keys)
                              (ht-remove! table key))))
                 (message "All local deletions in user library %s are reset." id))))))
        ;; For each library with read and write access: continue to sync
        ((and (pred zotero-cache-read-access-p) (pred zotero-cache-write-access-p)) ; read/write
         (if (ht-contains? libraries id)
             (let ((value (ht-get libraries id)))
               (plist-put value :library t)
               (plist-put value :write t)
               (ht-set! libraries id value))
           (ht-set! libraries id `(:id ,id :type "user" :library t :write t))))))
    (let ((remote-groups (zotero-lib-plist-get* remote-access :access :groups)))
      (cl-loop for key in remote-groups by #'cddr do
               (unless (eq key :all) ; These are just the default group permissions, and is not a syncable group
                 (let ((id (zotero-lib-keyword->string key))
                       (permissions (plist-get remote-groups key)))
                   ;; TODO: only write if changed
                   (pcase permissions
                     ;; For each library without read access: offer to remove the library
                     ((guard (not (zotero-cache-read-access-p permissions))) ; none
                      (if (y-or-n-p (format "Group %s has no read access. Remove from cache? " id))
                          (progn
                            (ht-remove! libraries id)
                            (ht-remove! groups id)
                            (ht-remove! synccache id)
                            (ht-remove! deletions id))
                        (if (ht-contains? libraries id)
                            (let ((value (ht-get libraries id)))
                              (plist-put value :library :json-false)
                              (plist-put value :write :json-false)
                              (ht-set! libraries id value))
                          (ht-set! libraries id `(:id ,id :type "group" :library :json-false :write :json-false)))
                        (message "Group %s has no read access, so cannot be synced." id)))
                     ;; For each library without write access: offer to reset local changes
                     ((pred zotero-cache-read-only-p) ; read only
                      (if (ht-contains? libraries id)
                          (let ((value (ht-get libraries id)))
                            (plist-put value :library t)
                            (plist-put value :write :json-false)
                            (ht-set! libraries id value))
                        (ht-set! libraries id `(:id ,id :type "group" :library t :write :json-false)))
                      (when (ht-contains? (ht-get* zotero-cache "synccache") id)
                        (let (updated)
                          (dolist (resource '("collections" "items" "searches") updated)
                            (push (cons resource (zotero-cache--get-locally-updated :cache cache :id id :resource resource)) updated))
                          (let ((updated-p (seq-some #'cdr updated)))
                            (when (and updated-p
                                       (y-or-n-p (format "Group library %s has no write access. Reset local changes? " id)))
                              (cl-loop for resource in '("collections" "items" "searches") do
                                       (let ((keys (cdr (assoc resource updated)))
                                             (table (ht-get* synccache id resource)))
                                         (dolist (key keys)
                                           (ht-remove! table key))))
                              (message "All local changes in group library %s are reset." id)))))
                      (when (ht-contains? (ht-get* zotero-cache "deletions") id)
                        (let (deleted)
                          (dolist (resource '("collections" "items" "searches") deleted)
                            (push (cons resource (zotero-cache--get-locally-deleted :cache cache :id id :resource resource)) deleted))
                          (let ((deleted-p (seq-some #'cdr deleted)))
                            (when (and deleted-p
                                       (y-or-n-p (format "Group library %s has no write access. Reset local deletions? " id)))
                              (cl-loop for resource in '("collections" "items" "searches") do
                                       (let ((keys (cdr (assoc resource deleted)))
                                             (table (ht-get* deletions id resource)))
                                         (dolist (key keys)
                                           (ht-remove! table key))))
                              (message "All local deletions in group library %s are reset." id))))))
                     ;; For each library with read and write access: continue to sync
                     ((and (pred zotero-cache-read-access-p) (pred zotero-cache-write-access-p)) ; read/write
                      (if (ht-contains? libraries id)
                          (let ((value (ht-get libraries id)))
                            (plist-put value :library t)
                            (plist-put value :write t)
                            (ht-set! libraries id value))
                        (ht-set! libraries id `(:id ,id :type "group" :library t :write t)))))))))
    cache))

(cl-defun zotero-cache--sync-metadata (&key cache id api-key)
  "Sync metadata.

Keyword CACHE is the hash table containing the cache. keyword ID is the ID
of the personal or group library you want to access, e.g. the
\"user ID\" or \"group ID\". Keyword API-KEY is the
Zotero API key."
  ;; First, retrieve a plist of the user's groups with the group as keyword and
  ;; the version as value.
  (let* ((response (zotero-lib-retrieve :resource "groups" :type "user" :id id :api-key api-key :format "versions"))
         (remote-groups (plist-get response :data))
         (libraries (ht-get cache "libraries"))
         (groups (ht-get cache "groups"))
         (synccache (ht-get cache "synccache"))
         (deletions (ht-get cache "deletions")))
    (cl-loop for group being the hash-keys of groups do
             (let ((remote-group (zotero-lib-string->keyword group)))
               (if (plist-member remote-groups remote-group)
                   (let ((remote-version (plist-get remote-groups remote-group)))
                     ;; Update version of library
                     (if (ht-contains? libraries group)
                         (let ((value (ht-get libraries group)))
                           (plist-put value :version remote-version)
                           (ht-set! libraries group value))
                       (ht-set! libraries group '(:type "group" :version remote-version))))

                 ;; Delete any local groups not in the list, which either were deleted or
                 ;; are currently inaccessible. (The user may have been removed from a
                 ;; group, or the current API key may no longer have access.)
                 (let ((choice (read-multiple-choice (format "Local group %s is not available remotely. Remove from cache or quit syncing to transfer modified data elsewhere? " group)
                                                     '((?d "remove from cache")
                                                       (?q "quit")))))
                   (pcase (car choice)
                     (?d
                      (ht-remove! libraries group)
                      (ht-remove! groups group)
                      (ht-remove! synccache group)
                      (message "Group %s removed from cache." group))
                     (?q
                      (throw 'sync 'quit)))))))

    ;; For each group that doesn't exist locally or that has a different
    ;; version number, retrieve the group metadata
    (cl-loop for key in remote-groups by #'cddr do
             (let* ((remote-version (plist-get remote-groups key))
                    (group (zotero-lib-keyword->string key))
                    (local-version (plist-get (ht-get groups group) :version)))
               (if (eq remote-version local-version)
                   (message "Metadata of group %s already up to date." group)
                 ;; FIXME: metadata cannot be retrieved from read-only groups
                 (let* ((response (zotero-lib-get-group :id group :api-key api-key))
                        (data (plist-get response :data)))
                   (ht-set! groups group data)))))
    cache))

(cl-defun zotero-cache--sync-remotely-updated (&key cache type id api-key)
  "Sync remotely updated data.

Keyword CACHE is the hash table containing the cache. keyword ID is the ID
of the personal or group library you want to access, e.g. the
\"user ID\" or \"group ID\". Keyword API-KEY is the
Zotero API key."
  (let* ((libraries (ht-get cache "libraries"))
         (synccache (ht-get cache "synccache"))
         (storage-version (plist-get (ht-get libraries id) :storage-version))
         (version (plist-get (ht-get libraries id) :version)))
    (cl-loop for resource in '("collections" "items" "searches") do
             (let* ((response (zotero-lib-retrieve :type type :id id :resource resource :since (or storage-version 0) :format "versions" :include-trashed "1" :api-key api-key))
                    (remote-version (plist-get response :version))
                    (data (plist-get response :data))
                    ;; Collect only the keys
                    (keys (cl-loop for key in data by #'cddr collect key)))
               (when remote-version
                 ;; Store Last-Modified-Version as the
                 ;; current library version
                 (let ((value (ht-get libraries id)))
                   (plist-put value :version remote-version)
                   (ht-set! libraries id value)
                   (setq version remote-version)))
               (when keys
                 (let* ((response (zotero-cache--get-remotely-updated :type type :id id :resource resource :api-key api-key :keys keys))
                        (remote-version (plist-get response :version))
                        (objects (plist-get response :data))
                        (table (ht-get* synccache id resource)))
                   (when (and remote-version
                              (not (eq remote-version version)))
                     (throw 'sync 'concurrent-update))
                   (when-let ((updated-table (zotero-cache--process-updates :table table :objects objects :version version)))
                     (setf table updated-table))))))
    cache))

(cl-defun zotero-cache--sync-remotely-deleted (&key cache type id api-key)
  "Sync remotely deleted data.

Keyword CACHE is the hash table containing the cache. keyword ID is the ID
of the personal or group library you want to access, e.g. the
\"user ID\" or \"group ID\". Keyword API-KEY is the
Zotero API key."
  (let* ((libraries (ht-get cache "libraries"))
         (synccache (ht-get cache "synccache"))
         (storage-version (plist-get (ht-get libraries id) :storage-version))
         (version (plist-get (ht-get libraries id) :version))
         (response (zotero-lib-retrieve :type type :id id :resource "deleted" :since (or storage-version 0) :api-key api-key))
         (remote-version (plist-get response :version))
         (deletions (plist-get response :data)))
    (when (and remote-version
               (not (eq remote-version version)))
      (throw 'sync 'concurrent-update))
    ;; REVIEW: should tags and settings be synced as well?
    (cl-loop for resource in '("collections" "items" "searches") do
             (let* ((prop (pcase resource
                            ("collections" :collections)
                            ("items" :items)
                            ("searches" :searches)))
                    (keys (plist-get deletions prop))
                    (table (ht-get* synccache id resource)))
               (when-let ((updated-table (zotero-cache--process-deletions :table table :keys keys :version version)))
                 (setf table updated-table))))
    cache))

(cl-defun zotero-cache--sync-locally-updated (&key cache type id api-key)
  "Sync locally modified data.

Keyword CACHE is the hash table containing the cache."
  (let* ((libraries (ht-get cache "libraries"))
         (synccache (ht-get cache "synccache"))
         (storage-version (plist-get (ht-get libraries id) :storage-version))
         (version (plist-get (ht-get libraries id) :version))
         (failures 0))
    (cl-loop for resource in '("collections" "items" "searches") do
             (when-let ((table (ht-get* synccache id resource))
                        (modified (ht-reject (lambda (key value) (plist-get value :synced)) table))
                        (keys (ht-keys modified))
                        (objects (seq-map (lambda (elt) (zotero-lib-plist-get* elt :object :data)) (ht-values modified)))
                        (partitions (seq-partition objects 50))
                        (number 0))
               (dolist (partition partitions)
                 (message "Uploading %d-%d of %d updated %s..." (1+ number) (+ number (length partition)) (length keys) resource)
                 (let* ((param-key (pcase resource
                                     ("collections" :collectionkey)
                                     ("items" :itemkey)
                                     ("searches" :searchkey)))
                        (json (apply #'zotero-lib-encode-object partition))
                        (response (zotero-lib-submit :method "POST" :type type :id id :resource resource :data json :content-type "application/json" :expect "" :version version :api-key api-key))
                        (status-code (plist-get response :status-code))
                        (remote-version (plist-get response :version))
                        (data (plist-get response :data))
                        (successful (plist-get data :successful))
                        (success (plist-get data :success))
                        (unchanged (plist-get data :unchanged))
                        (failed (plist-get data :failed)))
                   (pcase status-code
                     (200
                      ;; On a 200 response, set synced = true and version =
                      ;; Last-Modified-Version for each successfully uploaded
                      ;; Zotero object and store Last-Modified-Version as the
                      ;; current library version to be passed with the next
                      ;; write request.
                      (when remote-version
                        ;; Store Last-Modified-Version as the current library
                        ;; version
                        (let ((value (ht-get libraries id)))
                          (plist-put value :version remote-version)
                          (ht-set! libraries id value)
                          (setq version remote-version)))
                      (unless (eq successful :json-empty)
                        (cl-loop for index in successful by #'cddr do
                                 (let* ((value (plist-get successful index))
                                        (key (plist-get value :key)))
                                   (ht-set! table key `(:synced t :version ,version :object ,value)))))
                      ;; Do not update the version of Zotero objects in the
                      ;; unchanged object.
                      (unless (eq unchanged :json-empty)
                        (cl-loop for key in unchanged do
                                 (let* ((value (ht-get table key))
                                        (updated-value (plist-put value :synced t)))
                                   (ht-set! table key updated-value))))
                      (unless (eq failed :json-empty)
                        (cl-loop for prop in failed by #'cddr do
                                 (let ((code (zotero-lib-plist-get* failed prop :code))
                                       (message (zotero-lib-plist-get* failed prop :message)))
                                   (error "Error code %d: %s" code message)))
                        (setq failures (+ failures (length failed)))))
                     ;; On a 412 Precondition Failed response, return to the
                     ;; beginning of the sync process for that library.
                     (412
                      (throw 'sync 'concurrent-update)))

                   (message "Uploading %d-%d of %d updated %s...done" (1+ number) (+ number (length partition)) (length keys) resource)
                   (setq number (+ number (length partition)))))))
    ;; Retry non-fatal failures.
    (when (and (> failures 0)
               (y-or-n-p (format "Uploading of %d local updates failed. Retry? " failures))
               (zotero-cache--sync-locally-updated :cache cache :type type :id id :api-key api-key)))
    cache))

(cl-defun zotero-cache--sync-locally-deleted (&key cache type id api-key)
  "Sync locally deleted data.

Keyword CACHE is the hash table containing the cache."
  (let* ((libraries (ht-get cache "libraries"))
         (synccache (ht-get cache "synccache"))
         (deletions (ht-get cache "deletions"))
         (storage-version (plist-get (ht-get libraries id) :storage-version))
         (version (plist-get (ht-get libraries id) :version)))
    (cl-loop for resource in '("collections" "items" "searches") do
             (when-let ((table (ht-get* deletions id resource))
                        (objects (seq-map (lambda (elt) (zotero-lib-plist-get* elt :object :data)) (ht-values table)))
                        (partitions (seq-partition objects 50))
                        (number 0))
               (dolist (partition partitions)
                 (message "Uploading %d-%d of %d deleted %s..." (1+ number) (+ number (length partition)) (length objects) resource)
                 (let* ((param-key (pcase resource
                                     ("collections" :collectionkey)
                                     ("items" :itemkey)
                                     ("searches" :searchkey)))
                        (json (apply #'zotero-lib-encode-object partition))
                        (response (zotero-lib-submit :method "DELETE" :type type :id id :resource resource :data json :content-type "application/json" :expect "" :version version :api-key api-key))
                        (status-code (plist-get response :status-code))
                        (remote-version (plist-get response :version)))
                   (pcase status-code
                     (204
                      ;; On a 204 response, store the returned
                      ;; Last-Modified-Version as the current
                      ;; library version to be passed with the next
                      ;; write request.
                      (when remote-version
                        ;; Store Last-Modified-Version as the
                        ;; current library version
                        (let ((value (ht-get libraries id)))
                          (plist-put value :version remote-version)
                          (ht-set! libraries id value)
                          (setq version remote-version)))
                      ;; Remove the keys from the delete log
                      (let ((keys (seq-map (lambda (elt) (plist-get elt :key)) partition)))
                        (dolist (key keys)
                          (ht-remove! table key))))
                     ;; On a 412 Precondition Failed response,
                     ;; return to the beginning of the sync
                     ;; process for that library.
                     (412
                      (throw 'sync 'concurrent-update)))
                   (message "Uploading %d-%d of %d deleted %s...done" (1+ number) (+ number (length partition)) (length objects) resource)
                   (setq number (+ number (length partition)))))))
    cache))

(cl-defun zotero-cache--sync-attachments (&key cache type id api-key)
  "Sync the attachments to storage.

Keyword CACHE is the hash table containing the cache."
  (let* ((synccache (ht-get cache "synccache"))
         (table (ht-get* synccache id "items"))
         (attachments (zotero-cache--filter (lambda (elt) (and (equal (plist-get elt :itemType) "attachment")
                                                               (or (equal (plist-get elt :linkMode) "imported_file")
                                                                   (equal (plist-get elt :linkMode) "imported_url"))))
                                            table)))
    (cl-loop for key being the hash-keys of attachments
             using (hash-values value) do
             (let* ((data (zotero-lib-plist-get* value :object :data))
                    (filename (plist-get data :filename))
                    (content-type (plist-get data :contentType))
                    (md5 (plist-get data :md5))
                    (dir (concat (file-name-as-directory zotero-cache-storage-dir) key))
                    (file (expand-file-name (concat (file-name-as-directory dir) filename))))
               (unless (file-exists-p dir)
                 (make-directory dir t))
               (cond
                ((string-empty-p filename)
                 (message "Attachment %s in %s %s has an empty filename." key type id))
                ((file-exists-p file)
                 (let ((attributes (zotero-lib-file-attributes file)))
                   (when (and (equal content-type "application/pdf") ; non-pdf snapshots don't have matching md5sums
                              (not (equal md5 (plist-get attributes :md5)))
                              (y-or-n-p (format "File \"%s\" is changed. Overwrite? " filename)))
                     (with-demoted-errors "Error downloading file: %S"
                       (zotero-lib-download-file :file filename :dir dir :type type :id id :key key :api-key api-key)))))
                (t
                 (with-demoted-errors "Error downloading file: %S"
                   (zotero-lib-download-file :file filename :dir dir :type type :id id :key key :api-key api-key))))))))

(cl-defun zotero-cache--sync-templates (&key cache)
  "Sync the item and attachment templates.

Keyword CACHE is the hash table containing the cache."
  (let ((itemtypes (seq-map (lambda (elt) (plist-get elt :itemType)) (zotero-lib-itemtypes)))
        (linkmodes (zotero-lib-attachment-linkmodes)))
    (dolist (itemtype itemtypes)
      (zotero-cache--sync-item-template :cache cache :itemtype itemtype))
    (dolist (linkmode linkmodes)
      (zotero-cache--sync-attachment-template :cache cache :linkmode linkmode))
    cache))

(cl-defun zotero-cache--sync-item-template (&key cache itemtype)
  "Store the template for ITEMTYPE.

Keyword CACHE is the hash table containing the cache."
  (let* ((table (ht-get* cache "templates" "items"))
         (object (zotero-lib-item-template itemtype)))
    (ht-set! table itemtype `(:last-sync ,(current-time) :object ,object))
    cache))

(cl-defun zotero-cache--sync-attachment-template (&key cache linkmode)
  "Store the attachment template for LINKMODE.

Keyword CACHE is the hash table containing the cache."
  (let* ((table (ht-get* cache "templates" "attachments"))
         (object (zotero-lib-attachment-template linkmode)))
    (ht-set! table linkmode `(:last-sync ,(current-time) :object ,object))
    cache))

(provide 'zotero-cache)

;;; zotero-cache.el ends here
