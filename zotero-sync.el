;;; zotero-sync.el --- Sync for Zotero -*- lexical-binding: t; -*-

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
(require 's)
(require 'seq)
(require 'zotero)
(require 'zotero-cache)
(require 'zotero-lib)
(require 'zotero-diff)

;;;; Variables

;;;; Customization

(defgroup zotero-sync nil
  "Syncing for Zotero"
  :group 'zotero)

(defcustom zotero-sync-max-delay 3600
  "Seconds to wait before stopping sync retries; set to 0 to disable retrying."
  :group 'zotero-cache
  :type '(integer :validate
		  (lambda (widget)
                    (let ((int (widget-value widget)))
		      (unless (< int 0)
		        (widget-put widget :error
				    "Invalid value: must be a non-negative integer")
		        widget)))))

(defcustom zotero-sync-max-retries 100
  "Seconds to wait before stopping sync retries; set to 0 to disable retrying."
  :group 'zotero-cache
  :type '(integer :validate
		  (lambda (widget)
                    (let ((int (widget-value widget)))
		      (unless (< int 0)
		        (widget-put widget :error
				    "Invalid value: must be a non-negative integer")
		        widget)))))

(defun zotero-sync--get-locally-updated (cache resource type id)
  "Return locally updated keys.

Argument CACHE is the hash table containing the cache. RESOURCE
is one of \"collections\", \"items\", or \"searches\". TYPE is
\"user\" for your personal library, and \"group\" for the group
libraries. ID is the ID of the personal or group library you want
to access, that is the \"user ID\" or \"group ID\"."
  (let* ((table (ht-get* cache "synccache" resource))
         (modified (ht-select (lambda (key value) (and (equal (plist-get value :type) type)
                                                       (equal (plist-get value :id) id)
                                                       (not (plist-get value :synced)))) table)))
    (ht-keys modified)))

(defun zotero-sync--get-locally-deleted (cache resource type id)
  "Return locally deleted keys.

Argument CACHE is the hash table containing the cache. RESOURCE
is one of \"collections\", \"items\", or \"searches\". TYPE is
\"user\" for your personal library, and \"group\" for the group
libraries. ID is the ID of the personal or group library you want
to access, that is the \"user ID\" or \"group ID\"."
  (let* ((table (ht-get* cache "deletions" resource))
         (deleted (ht-select (lambda (key value) (and (equal (plist-get value :type) type)
                                                      (equal (plist-get value :id) id))) table)))
    (ht-keys deleted)))

(defun zotero-sync--get-remotely-updated (keys resource type id api-key)
  "Return remotely updated data of KEYS.

RESOURCE is one of \"collections\", \"items\", or \"searches\".
TYPE is \"user\" for your personal library, and \"group\" for the
group libraries. ID is the ID of the personal or group library
you want to access, that is the \"user ID\" or \"group ID\". API-KEY
is the Zotero API key."
  (when keys
    (let ((partitions (seq-partition keys 50))
          (number 0)
          (version)
          (data))
      (dolist (partition partitions data)
        (message "Retrieving %d-%d of %d updated %s..." (1+ number) (+ number (length partition)) (length keys) resource)
        (let* ((param-key (pcase resource
                            ("collections" "collectionKey")
                            ("items" "itemKey")
                            ("searches" "searchKey")))
               (param-value (s-join "," (seq-map #'zotero-lib-keyword->string partition)))
               (result (zotero-request "GET" resource nil :type type :id id :api-key api-key :params `(("includeTrashed" 1)
                                                                                                       (,param-key ,param-value))))
               (remote-version (zotero-response-version result))
               (remote-data (zotero-response-data result)))
          (message "Retrieving %d-%d of %d updated %s...done" (1+ number) (+ number (length partition)) (length keys) resource)
          (setq number (+ number (length partition)))
          (setq version remote-version)
          (setq data (seq-concatenate 'vector data remote-data))))
      `(:type ,type :id ,id :version ,version :data ,data))))

(defun zotero-sync--verify-key (cache id api-key)
  "Verify that API-KEY has the expected access to the library.
Return updated cache.

Argument CACHE is the hash table containing the cache. ID is the
ID of the personal or group library you want to access, that is the
\"user ID\" or \"group ID\". API-KEY is the Zotero API key."
  (let* ((result (zotero-key api-key))
         (data (zotero-response-data result))
         (user-access (zotero-lib-plist-get* data :access :user))
         (user-access (thread-first user-access
                        (plist-put :type "user")
                        (plist-put :id id)))
         (group-access (thread-first data
                         (zotero-lib-plist-get* :access :groups)
                         (zotero-lib-plist-delete :all)))
         (group-access (cl-loop for (key value) on group-access by #'cddr
                                collect (thread-first value
                                          (plist-put :type "group")
                                          (plist-put :id (zotero-lib-keyword->string key)))))
         (access (cons user-access group-access))
         (libraries (ht-get cache "libraries"))
         (groups (ht-get cache "groups")))
    (dolist (library-access access)
      ;; TODO: only write if changed
      ;; TODO: include note and file access of user library
      (let* ((type (plist-get library-access :type))
             (id (plist-get library-access :id))
             (library (ht-get libraries id)))
        (pcase library-access
          ;; For each library without read access: offer to remove the library
          ((guard (not (zotero-cache-read-access-p library-access)))
           (if (y-or-n-p (format "%s library %s has no read access. Remove from cache? " (upcase-initials type) id))
               (progn
                 (ht-clear! (ht-get libraries id))
                 (when (equal type "group") (ht-clear! (ht-get groups id)))
                 (dolist (resource '("collections" "items" "searches"))
                   (let* ((table (ht-get* cache "synccache" resource))
                          (library (ht-select (lambda (key value) (and (equal (plist-get value :type) type)
                                                                       (equal (plist-get value :id) id))) table)))
                     (ht-clear! library))
                   (let* ((table (ht-get* cache "deletions" resource))
                          (library (ht-select (lambda (key value) (and (equal (plist-get value :type) type)
                                                                       (equal (plist-get value :id) id))) table)))
                     (ht-clear! library)))
                 (message "%s library %s removed from cache." (upcase-initials type) id))
             (let ((value (thread-first library
                            (plist-put :type type)
                            (plist-put :id id)
                            (plist-put :library :json-false)
                            (plist-put :write :json-false))))
               (ht-set! libraries id value)
               (message "%s library %s has no read access, so cannot be synced." (upcase-initials type) id))))
          ;; For each library without write access: offer to reset local changes
          ((pred zotero-cache-read-only-p) ; read only
           (let ((value (thread-first library
                          (plist-put :type type)
                          (plist-put :id id)
                          (plist-put :library t)
                          (plist-put :write :json-false))))
             (ht-set! libraries id value))
           ;; TODO: local updates are removed. Could updates be reverted in stead?
           (let (updated)
             ;; Create an alist with the resource as `car' and a list of updated keys as `cdr'
             (dolist (resource '("collections" "items" "searches") updated)
               (push (cons resource (zotero-sync--get-locally-updated cache resource type id)) updated))
             (let ((updated-p (seq-some #'cdr updated))) ; check whether any resource has non-nil updated keys
               (when (and updated-p
                          (y-or-n-p (format "%s library %s has no write access. Reset local changes? " (upcase-initials type) id)))
                 (cl-loop for resource in '("collections" "items" "searches") do
                          (let* ((table (ht-get* cache "synccache" resource))
                                 (selection (ht-select (lambda (key value) (and (equal (plist-get value :type) type)
                                                                                (equal (plist-get value :id) id))) table))
                                 (keys (cdr (assoc resource updated))))
                            (dolist (key keys)
                              (ht-remove! selection key))))
                 (message "All local changes in %s library %s are reset." type id))))
           (let (deleted)
             ;; Create an alist with the resource as `car' and a list of updated keys as `cdr'
             (dolist (resource '("collections" "items" "searches") deleted)
               (push (cons resource (zotero-sync--get-locally-deleted cache resource type id)) deleted))
             (let ((deleted-p (seq-some #'cdr deleted))) ; check whether any resource has non-nil deleted keys
               (when (and deleted-p
                          (y-or-n-p (format "%s library %s has no write access. Reset local deletions? " (upcase-initials type) id)))
                 (cl-loop for resource in '("collections" "items" "searches") do
                          (let* ((table (ht-get* cache "deletions" resource))
                                 (selection (ht-select (lambda (key value) (and (equal (plist-get value :type) type)
                                                                                (equal (plist-get value :id) id))) table))
                                 (keys (cdr (assoc resource deleted))))
                            (dolist (key keys)
                              (ht-remove! selection key))))
                 (message "All local deletions in user library %s are reset." id)))))
          ;; For each library with read and write access: continue to sync
          ((and (pred zotero-cache-read-access-p) (pred zotero-cache-write-access-p)) ; read/write
           (let ((value (thread-first library
                          (plist-put :type type)
                          (plist-put :id id)
                          (plist-put :library t)
                          (plist-put :write t))))
             (ht-set! libraries id value))))))
    cache))

(defun zotero-sync--metadata (cache id api-key)
  "Sync metadata.

Argument CACHE is the hash table containing the cache. ID is the
ID of the personal or group library you want to access, that is the
\"user ID\" or \"group ID\". API-KEY is the Zotero API key."
  ;; First, retrieve a plist of the user's groups with the group as keyword and
  ;; the version as value.
  (let* ((result (zotero-request "GET" "groups" nil :type "user" :id id :api-key api-key :params '(("format" "versions"))))
         (remote-groups (zotero-response-data result))
         (libraries (ht-get cache "libraries"))
         (groups (ht-get cache "groups")))
    (cl-loop for id being the hash-keys of groups
             using (hash-values value) do
             (let ((key (zotero-lib-string->keyword id)))
               (if (plist-member remote-groups key)
                   ;; Update version of library
                   (let ((library (ht-get libraries id))
                         (remote-version (plist-get remote-groups key)))
                     (ht-set! libraries id (plist-put library :version remote-version)))
                 ;; Delete any local groups not in the list, which either were deleted or
                 ;; are currently inaccessible. (The user may have been removed from a
                 ;; group, or the current API key may no longer have access.)
                 (let ((choice (read-multiple-choice (format "Local group %s is not available remotely. Remove from cache or quit syncing to transfer modified data elsewhere? " id)
                                                     '((?d "remove from cache")
                                                       (?q "quit")))))
                   (pcase (car choice)
                     (?d
                      (ht-remove! libraries id)
                      (ht-remove! groups id)
                      ;; FIXME: remove group items from synccache and deletions
                      (message "Group %s removed from cache." id))
                     (?q
                      (throw 'sync 'quit)))))))
    ;; For each group that doesn't exist locally or that has a different
    ;; version number, retrieve the group metadata
    (cl-loop for (key remote-version) on remote-groups by #'cddr do
             (let* ((id (zotero-lib-keyword->string key))
                    (local-version (plist-get (ht-get groups id) :version)))
               (if (eq remote-version local-version)
                   (message "Metadata of group %s already up to date." id)
                 ;; FIXME: metadata cannot be retrieved from read-only groups
                 (let* ((result (zotero-group id :api-key api-key))
                        (data (zotero-response-data result)))
                   (ht-set! groups id `(:version ,remote-version :object ,data))))))
    cache))

(defun zotero-sync--remotely-updated (cache type id api-key &optional full-sync)
  "Sync remotely updated data.

Argument CACHE is the hash table containing the cache. TYPE is
\"user\" for your personal library, and \"group\" for the group
libraries. ID is the ID of the personal or group library you want
to access, that is the \"user ID\" or \"group ID\". API-KEY is the
Zotero API key.

When optional argument FULL-SYNC is non-nil a full sync is
performed."
  (let* ((libraries (ht-get cache "libraries"))
         (library (ht-get libraries id))
         (storage-version (plist-get library :storage-version))
         (since (cond
                 (full-sync 0)
                 (storage-version storage-version)
                 (t 0)))
         (version (plist-get library :version)))
    (cl-loop for resource in '("collections" "items" "searches") do
             (let* ((result (zotero-request "GET" resource nil :type type :id id :api-key api-key :params `(("since" ,since)
                                                                                                            ("format" "versions")
                                                                                                            ("includeTrashed" 1))))
                    (remote-version (zotero-response-version result))
                    ;; Returns a plist of the form (:key version :key version ...)
                    (data (zotero-response-data result))
                    ;; Collect only the keys
                    (keys (cl-loop for key in data by #'cddr collect key)))
               (when remote-version
                 ;; Store Last-Modified-Version as the current library version
                 (ht-set! libraries id (plist-put library :version remote-version))
                 (setq version remote-version))
               (when keys
                 (let* ((response (zotero-sync--get-remotely-updated keys resource type id api-key))
                        (remote-version (plist-get response :version))
                        (objects (plist-get response :data))
                        (table (ht-get* cache "synccache" resource)))
                   (when (and remote-version
                              (not (eq remote-version version)))
                     (throw 'sync 'concurrent-update))
                   (when-let ((updated-table (zotero-sync--process-updates table objects version)))
                     (setf table updated-table))))))
    cache))

(defun zotero-sync--remotely-deleted (cache type id api-key &optional full-sync)
  "Sync remotely deleted data.

Argument CACHE is the hash table containing the cache. TYPE is
\"user\" for your personal library, and \"group\" for the group
libraries. ID is the ID of the personal or group library you want
to access, that is the \"user ID\" or \"group ID\". API-KEY is the
Zotero API key.

When optional argument FULL-SYNC is non-nil a full sync is
performed."
  (let* ((libraries (ht-get cache "libraries"))
         (library (ht-get libraries id))
         (storage-version (plist-get library :storage-version))
         (since (cond
                 (full-sync 0)
                 (storage-version storage-version)
                 (t 0)))
         (version (plist-get library :version))
         (result (zotero-request "GET" "deleted" nil :type type :id id :api-key api-key :params `(("since" ,since))))
         (remote-version (zotero-response-version result))
         ;; Returns a plist of the form (:collections ["12345678" "ABCDEFGH"
         ;; ...] :items ["87654321" "HGFEDCBA" ...] :searches [] :tags ["tag 1"
         ;; "tag 2" ...] :settings [])
         (deletions (zotero-response-data result)))
    (when (and remote-version
               (not (eq remote-version version)))
      (throw 'sync 'concurrent-update))
    ;; REVIEW: should tags and settings be synced as well?
    (cl-loop for resource in '("collections" "items" "searches") do
             (when-let ((table (ht-get* cache "synccache" resource))
                        (keys (seq-into (plist-get deletions (zotero-lib-string->keyword resource)) 'list)))
               (setf table (zotero-sync--process-deletions table keys version))))
    cache))

(defun zotero-sync--locally-updated (cache type id api-key)
  "Sync locally modified data.

Argument CACHE is the hash table containing the cache. TYPE is
\"user\" for your personal library, and \"group\" for the group
libraries. ID is the ID of the personal or group library you want
to access, that is the \"user ID\" or \"group ID\". API-KEY is the
Zotero API key."
  (let* ((libraries (ht-get cache "libraries"))
         (library (ht-get libraries id))
         (storage-version (plist-get library :storage-version))
         (version (plist-get library :version))
         (synccache (ht-get cache "synccache"))
         (failures 0))
    (cl-loop for resource being the hash-keys of synccache
             using (hash-values table) do
             (when-let ((modified (thread-last table
                                    (ht-select (lambda (key value) (and (equal (plist-get value :type) type)
                                                                        (equal (plist-get value :id) id))))
                                    (ht-reject (lambda (key value) (plist-get value :synced)))))
                        (keys (ht-keys modified))
                        (objects (seq-map (lambda (elt) (zotero-lib-plist-get* elt :object :data)) (ht-values modified)))
                        (partitions (seq-partition objects 50))
                        (number 0))
               (dolist (partition partitions)
                 (message "Uploading %d-%d of %d updated %s..." (1+ number) (+ number (length partition)) (length keys) resource)
                 (let* ((param-key (pcase resource
                                     ("collections" "collectionKey")
                                     ("items" "itemKey")
                                     ("searches" "searchKey")))
                        (json (apply #'zotero-json-encode-to-array partition))
                        (result (zotero-request "POST" resource nil :type type :id id :api-key api-key :headers `(("Content-Type" . "application/json")) :data (encode-coding-string json 'utf-8)))
                        (status-code (zotero-response-status-code result))
                        (remote-version (zotero-response-version result))
                        (data (zotero-response-data result))
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
                        (cl-loop for (index object) on successful by #'cddr do
                                 (let ((key (plist-get object :key))
                                       (type (zotero-lib-plist-get* object :library :type))
                                       (id (number-to-string (zotero-lib-plist-get* object :library :id))))
                                   (ht-set! table key `(:synced t :version ,version :type ,type :id ,id :object ,object)))))
                      ;; Do not update the version of Zotero objects in the
                      ;; unchanged object.
                      (unless (eq unchanged :json-empty)
                        (cl-loop for (number key) on unchanged do
                                 (ht-set! table key (plist-put (ht-get table key) :synced t))))
                      (unless (eq failed :json-empty)
                        (cl-loop for (_ value) on failed by #'cddr do
                                 (let ((code (plist-get value :code))
                                       (message (plist-get value :message)))
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
               (zotero-sync--locally-updated cache type id api-key)))
    cache))

(defun zotero-sync--locally-deleted (cache type id api-key)
  "Sync locally deleted data.

Argument CACHE is the hash table containing the cache. TYPE is
\"user\" for your personal library, and \"group\" for the group
libraries. ID is the ID of the personal or group library you want
to access, that is the \"user ID\" or \"group ID\". API-KEY is the
Zotero API key."
  (let* ((libraries (ht-get cache "libraries"))
         (library (ht-get libraries id))
         (storage-version (plist-get library :storage-version))
         (version (plist-get library :version))
         (deletions (ht-get cache "deletions")))
    (cl-loop for resource being the hash-keys of deletions
             using (hash-values table) do
             (when-let ((keys (thread-last table
                                (ht-select (lambda (key value) (and (equal (plist-get value :type) type)
                                                                    (equal (plist-get value :id) id))))
                                (ht-keys)))
                        (partitions (seq-partition keys 50))
                        (number 0))
               (dolist (partition partitions)
                 (message "Uploading %d-%d of %d deleted %s..." (1+ number) (+ number (length partition)) (length keys) resource)
                 (let* ((param-key (pcase resource
                                     ("collections" "collectionKey")
                                     ("items" "itemKey")
                                     ("searches" "searchKey")))
                        (param-value (s-join "," partition))
                        (result (zotero-request "DELETE" resource nil :type type :id id :api-key api-key :headers `(("If-Unmodified-Since-Version" . ,(number-to-string version))) :params `((,param-key ,param-value))))
                        (status-code (zotero-response-status-code result))
                        (remote-version (zotero-response-version result)))
                   (pcase status-code
                     (204
                      ;; On a 204 response, store the returned
                      ;; Last-Modified-Version as the current library version to
                      ;; be passed with the next write request.
                      (when remote-version
                        ;; Store Last-Modified-Version as the current library
                        ;; version
                        (ht-set! libraries id (plist-put library :version remote-version))
                        (setq version remote-version))
                      ;; Remove the keys from the delete log
                      (seq-do (lambda (key) (ht-remove! table key)) partition))
                     ;; On a 412 Precondition Failed response,
                     ;; return to the beginning of the sync
                     ;; process for that library.
                     (412
                      (throw 'sync 'concurrent-update)))
                   (message "Uploading %d-%d of %d deleted %s...done" (1+ number) (+ number (length partition)) (length keys) resource)
                   (setq number (+ number (length partition)))))))
    cache))

(defun zotero-sync--process-updates (table objects version)
  "Update TABLE with OBJECTS.
Return the updated table when success or nil when failed.

VERSION is the \"Last-Modified-Version\"."
  (seq-doseq (object objects)
    (let* ((key (plist-get object :key))
           (type (zotero-lib-plist-get* object :library :type))
           (id (number-to-string (zotero-lib-plist-get* object :library :id)))
           (value (ht-get table key))
           default)
      ;; FIXME: when resolving a conflict between a locally deleted object and a
      ;; remotely modified object in favor of the remote object, remove it from
      ;; the delete log.
      (pcase value
        ;; if object doesn't exist locally:
        ;; create local object with version = Last-Modified-Version and set synced = true
        ((pred null)
         (ht-set! table key `(:synced t :version ,version :type ,type :id ,id :object ,object)))
        ;; if object hasn't been modified locally (synced == true):
        ;; overwrite with synced = true and version = Last-Modified-Version
        ((guard (eq (plist-get value :synced) t))
         (ht-set! table key `(:synced t :version ,version :type ,type :id ,id :object ,object)))
        ;; if object hasn't changed:
        ;; set synced = true and version = Last-Modified-Version
        ((guard (equal (plist-get value :object) object))
         (ht-set! table key `(:synced t :version ,version :type ,type :id ,id :object ,object)))
        ;; if changes can be automatically merged:
        ;; apply changes from each side and set synced = true and version = Last-Modified-Version
        ((guard (zotero-lib-mergable-plist-p (plist-get value :object) object))
         (let ((merged (zotero-lib-merge-plist (plist-get value :object) object)))
           (ht-set! table key `(:synced t :version ,version :type ,type :id ,id :object ,merged))))
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
                   (ht-set! table key `(:type ,type :id ,id :synced nil :object ,(plist-get value :object))))
                  ;; if user chooses remote copy:
                  ;; overwrite with synced = true and version = Last-Modified-Version
                  (?r
                   (ht-set! table key `(:synced t :version ,version :type ,type :id ,id :object ,object)))
                  (?q
                   (throw 'sync 'quit)))))
             ;; if user chooses local copy:
             ;; synced = false and set a flag to restart the sync when finished
             ;; REVIEW: flag to restart the sync when finished?
             (?l
              (ht-set! table key `(:type ,type :id ,id :synced nil :object ,(plist-get value :object))))
             ;; if user chooses remote copy:
             ;; overwrite with synced = true and version = Last-Modified-Version
             (?r
              (ht-set! table key `(:synced t :version ,version :type ,type :id ,id :object ,object)))
             (?L
              (ht-set! table key `(:type ,type :id ,id :synced nil :object ,(plist-get value :object)))
              (setq default ?l))
             (?R
              (ht-set! table key `(:synced t :version ,version :type ,type :id ,id :object ,object))
              (setq default ?r))
             (?q
              (throw 'sync 'quit))))))))
  table)

(defun zotero-sync--process-deletions (table keys version)
  "Delete KEYS from TABLE.
Return the updated table when success or nil when failed.

VERSION is the \"Last-Modified-Version\"."
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
              (let* ((object (plist-get value :object))
                     (type (zotero-lib-plist-get* object :library :type))
                     (id (number-to-string (zotero-lib-plist-get* object :library :id))))
                (ht-set! table key `(:synced t :version ,version :type ,type :id ,id :object ,object))))
             (?D
              (ht-remove! table key)
              (setq default ?d))
             (?K
              (let* ((object (plist-get value :object))
                     (type (zotero-lib-plist-get* object :library :type))
                     (id (number-to-string (zotero-lib-plist-get* object :library :id))))
                (ht-set! table key `(:synced t :version ,version :type ,type :id ,id :object ,object)))
              (setq default ?k))
             (?q
              (throw 'sync 'quit))))))))
  table)

(defun zotero-sync-schema (cache)
  "Store the schema in CACHE.

Argument CACHE is the hash table containing the cache.

The schema is downloaded as a single file from
\"https://api.zotero.org/schema\". See URL
`https://github.com/zotero/zotero-schema' for the GitHub
repository of the schema."
  ;; The schema is large and should be retrieved by making a conditional request
  ;; using If-None-Match: <ETag>. If a 304 status code is received, the schema
  ;; has not been modified since last requested and a cached version should be
  ;; used. The function `url-http-parse-headers' already has support for
  ;; caching, and a 304 status code is never returned but implicitly redirected
  ;; to a cached version. However, this results in an error if no cached version
  ;; is found in the disk cache of the `url' library. This is unwanted
  ;; behaviour, because the schema is already cached in the `zotero-cache'. As a
  ;; workaround, `url-automatic-caching' is set to non-nil and an error is
  ;; handled by retrieving the schema again if the cache is cleared somehow.
  (let* ((schema (ht-get (or cache zotero-cache) "schema"))
         (etag (plist-get schema :etag))
         (url-automatic-caching t)
         (result (condition-case nil
                     (zotero-request "GET" "schema" nil :headers `(("If-None-Match" . ,etag)))
                   (file-missing (zotero-request "GET" "schema"))))
         (data (zotero-response-data result))
         (etag (zotero-response-etag result)))
    (ht-set! cache "schema" data)
    (plist-put (ht-get cache "schema") :etag etag)
    (plist-put (ht-get cache "schema") :last-sync (current-time))
    cache))

(defun zotero-sync-item-template (cache itemtype)
  "Store the template for ITEMTYPE.

Argument CACHE is the hash table containing the cache."
  (let* ((table (ht-get* cache "templates" "items"))
         (result (zotero-item-template itemtype))
         (object (zotero-response-data result)))
    (ht-set! table itemtype `(:last-sync ,(current-time) :object ,object))
    object))

(defun zotero-sync-attachment-template (cache linkmode)
  "Store the attachment template for LINKMODE.

Argument CACHE is the hash table containing the cache."
  (let* ((table (ht-get* cache "templates" "attachments"))
         (result (zotero-attachment-template linkmode))
         (object (zotero-response-data result)))
    (ht-set! table linkmode `(:last-sync ,(current-time) :object ,object))
    object))

(defun zotero-sync--templates (cache)
  "Sync the item and attachment templates.

Argument CACHE is the hash table containing the cache."
  (let ((itemtypes (seq-map (lambda (elt) (plist-get elt :itemType)) (zotero-item-types)))
        (linkmodes (zotero-attachment-linkmodes)))
    (dolist (itemtype itemtypes)
      (zotero-sync-item-template cache itemtype))
    (dolist (linkmode linkmodes)
      (zotero-sync-attachment-template cache linkmode))
    cache))

(defun zotero-sync--download-attachments (cache type id api-key)
  "Sync the attachments to storage.

Argument CACHE is the hash table containing the cache. TYPE is
\"user\" for your personal library, and \"group\" for the group
libraries. ID is the ID of the personal or group library you want
to access, that is the \"user ID\" or \"group ID\". API-KEY is the
Zotero API key."
  (let* ((table (ht-get* cache "synccache" "items"))
         (attachments (thread-last table
                        (ht-select (lambda (key value) (and (equal (plist-get value :type) type)
                                                            (equal (plist-get value :id) id))))
                        (zotero-cache-filter-data (lambda (elt) (and (equal (plist-get elt :itemType) "attachment")
                                                                     (or (equal (plist-get elt :linkMode) "imported_file")
                                                                         (equal (plist-get elt :linkMode) "imported_url"))))))))
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
                 (let ((attributes (zotero-file-attributes file)))
                   (when (and (equal content-type "application/pdf") ; non-pdf snapshots don't have matching md5sums
                              (not (equal md5 (plist-get attributes :md5)))
                              (y-or-n-p (format "File \"%s\" is changed. Overwrite? " filename)))
                     (with-demoted-errors "Error downloading file: %S"
                       (zotero-download-file key filename dir nil :type type :id id :api-key api-key)))))
                (t
                 (with-demoted-errors "Error downloading file: %S"
                   (zotero-download-file key filename dir nil :type type :id id :api-key api-key))))))))

(defun zotero-sync--delete-attachments (cache)
  "Sync the deleted attachments to storage.

Argument CACHE is the hash table containing the cache. API-KEY is
the Zotero API key."
  (let* ((table (ht-get* cache "synccache" "items"))
         (dir (file-name-as-directory zotero-cache-storage-dir))
         (stored-keys (directory-files dir nil "[[:alnum:]]\\{6\\}"))
         (removed-keys (thread-last table
                         (zotero-cache-filter-data (lambda (elt) (and (equal (plist-get elt :itemType) "attachment")
                                                                      (or (equal (plist-get elt :linkMode) "imported_file")
                                                                          (equal (plist-get elt :linkMode) "imported_url")))))
                         (ht-keys)))
         (keys (seq-difference stored-keys removed-keys)))
    (dolist (key keys)
      (delete-directory (concat dir key) t))))

(defun zotero-sync-attachments (cache api-key)
  "Sync the Zotero library.

Argument CACHE is the hash table containing the cache. API-KEY is
the Zotero API key."
  (let ((libraries (ht-get cache "libraries")))
    ;; Perform the following steps for each library:
    (cl-loop for id being the hash-keys of libraries
             using (hash-values value) do
             (let ((type (plist-get value :type)))
               (when zotero-cache-enable-storage
                 (message "Syncing attachments to storage for %s %s..." type id)
                 (zotero-sync--download-attachments cache type id api-key)
                 (message "Syncing attachments to storage for %s %s...done" type id))))
    (message "Deleting removed attachments from storage...")
    (zotero-sync--delete-attachments cache)
    (message "Deleting removed attachments from storage...done")))

(defun zotero-sync--sync (cache id api-key &optional full-sync)
  "Sync the Zotero library.

Argument CACHE is the hash table containing the cache. ID is the
ID of the personal or group library you want to access, that is the
\"user ID\" or \"group ID\". API-KEY is the Zotero API key.

When optional argument FULL-SYNC is non-nil a full sync is
performed."
  (catch 'sync
    (message "Verifying key access...")
    (zotero-sync--verify-key cache id api-key)
    (message "Verifying key access...done")

    (message "Syncing group metadata...")
    (zotero-sync--metadata cache id api-key)
    (message "Syncing group metadata...done")

    (let ((libraries (ht-get cache "libraries")))
      ;; Perform the following steps for each library:
      (cl-loop for id being the hash-keys of libraries
               using (hash-values value) do
               (let ((type (plist-get value :type))
                     (read-only (zotero-cache-read-only-p value)))

                 (message "Syncing remotely updated data for %s %s..." type id)
                 (zotero-sync--remotely-updated cache type id api-key full-sync)
                 (message "Syncing remotely updated data for %s %s...done" type id)

                 (message "Syncing remotely deleted data for %s %s..." type id)
                 (zotero-sync--remotely-deleted cache type id api-key full-sync)
                 (message "Syncing remotely deleted data for %s %s...done" type id)

                 (unless read-only
                   (message "Syncing locally updated data for %s %s..." type id)
                   (zotero-sync--locally-updated cache type id api-key)
                   (message "Syncing locally updated data for %s %s...done" type id)

                   (message "Syncing locally deleted data for %s %s..." type id)
                   (zotero-sync--locally-deleted cache type id api-key)
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

(defun zotero-sync (&optional full-sync retries)
  "Sync the Zotero library.

When optional argument FULL-SYNC is non-nil, or with a `C-u'
prefix, force a full sync.

Argument RETRIES is used to count the number of retries."
  (interactive "P")
  (let ((id (zotero-auth-token-userid zotero-auth-token))
        (api-key (zotero-auth-api-key zotero-auth-token)))
    (message "Syncing cache...")
    (zotero-cache-maybe-initialize-cache)
    (let* ((cache (copy-tree zotero-cache))
           (result (zotero-sync--sync cache id api-key full-sync))
           (retries (or retries 0)))
      (pcase result
        ((pred ht?)
         (message "Syncing cache...done")
         (message "Writing cache...")
         (setq zotero-cache result)
         (zotero-cache-serialize)
         (message "Writing cache...done"))
        ;; For each response from the API, check the Last-Modified-Version to
        ;; see if it has changed since the Last-Modified-Version returned from
        ;; the first request (for example "collections?since="). If it has,
        ;; restart the process of retrieving updated and deleted data, waiting
        ;; increasing amounts of time between restarts to give the other client
        ;; the opportunity to finish.
        ('concurrent-update
         (cond
          ((zerop zotero-sync-max-delay)
           (user-error "Syncing cache failed: concurrent update"))
          ((zerop zotero-sync-max-retries)
           (user-error "Syncing cache failed: concurrent update"))
          ((> retries zotero-sync-max-retries)
           (user-error "Syncing cache failed: concurrent update and maximum of %d retries reached" zotero-sync-max-retries))
          (t
           (let* ((intervals (seq-map (lambda (elt) (expt 2 elt)) (number-sequence 1 retries))) ; exponential increase in delay
                  (total-delay (seq-reduce #'+ intervals 0)))
             (if (> total-delay zotero-sync-max-delay)
                 (user-error "Syncing cache failed: concurrent update and maximum of %d seconds delay reached" zotero-sync-max-delay)
               (sleep-for (expt 2 retries))
               (zotero-sync full-sync (1+ retries)))))))
        ('quit
         (message "Syncing cache...quit"))
        (_ ; this should not happen
         (error "Syncing cache failed: unknown error")))
      (message "Syncing schema...")
      (if-let ((result (zotero-sync-schema cache)))
          (progn
            (message "Syncing schema...done")
            (message "Writing cache...")
            (setq zotero-cache result)
            (zotero-cache-serialize)
            (message "Writing cache...done"))
        (message "Syncing schema...failed"))
      (message "Syncing templates...")
      (if-let ((result (zotero-sync--templates cache)))
          (progn
            (message "Syncing templates...done")
            (message "Writing cache...")
            (setq zotero-cache result)
            (zotero-cache-serialize)
            (message "Writing cache...done"))
        (message "Syncing templates...failed"))

      ;; TODO: check concurrent updates
      (when zotero-cache-enable-storage
        (zotero-sync-attachments cache api-key)))))

(provide 'zotero-sync)

;;; zotero-sync.el ends here
