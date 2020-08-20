;;; zotero-cache.el --- Cache for Zotero -*- lexical-binding: t; -*-

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
(require 'zotero-lib)

(defvar zotero-cache-cache nil
  "A plist used to cache the library data.")

(defcustom zotero-cache-enable-caching t
  "When t enables caching.
Caching is automatically enabled by default.

For efficient usage of the API, clients should make conditional
GET requests whenever possible. If \"If-Modified-Since-Version:
<libraryVersion>\" is passed with a multi-object read
request (e.g., \"/users/1/items\") and data has not changed in the
library since the specified version, the API will return \"304 Not
Modified\". Single-object conditional requests are not currently
supported, but will be supported in the future.

While a conditional GET request that returns a \"304\" should be
fast, some clients may wish or need to perform additional caching
on their own, using stored data for a period of time before
making subsequent conditional requests to the Zotero API. This
makes particular sense when the underlying Zotero data is known
not to change frequently or when the data will be accessed
frequently. For example, a website that displayed a bibliography
from a Zotero collection might cache the returned bibliography
for an hour, after which time it would make another conditional
request to the Zotero API. If the API returned a \"304\", the website
would continue to display the cached bibliography for another
hour before retrying. This would prevent the website from making
a request to the Zotero API every time a user loaded a page."
  :group 'zotero-cache
  :type 'boolean)

(defcustom zotero-cache-cache-file
  (locate-user-emacs-file "zotero-cache-cache" ".zotero-cache-cache")
  "The name of Zotero lib's cache file."
  :group 'zotero-cache
  :type 'file)

;; ;;;;; Caching

;; (defun zotero-cache-serialize-cache ()
;;   "Serializes the memory cache to the hard drive."
;;   (zotero-cache-serialize zotero-cache-cache zotero-cache-cache-file))

;;; Serialization

(defun zotero-cache-serialize (data filename)
  "Serialize DATA to FILENAME.
The saved data can be restored with `zotero-cache-unserialize'."
  (when (file-writable-p filename)
    (with-temp-file filename
      (insert (let (print-length) (prin1-to-string data))))))

(defun zotero-cache-unserialize (filename)
  "Read data serialized by `zotero-cache-serialize' from FILENAME."
  (with-demoted-errors
      "Error during file deserialization: %S"
    (when (file-exists-p filename)
      (with-temp-buffer
        (insert-file-contents filename)
        (unless (string-empty-p (buffer-string))
          (read (buffer-string)))))))

(defun zotero-cache-unserialize-cache ()
  "Serializes the hard drive to the memory cache."
  (setq zotero-cache-cache (zotero-cache-unserialize zotero-cache-cache-file)))

;; TODO: add cache timer
(defun zotero-cache--maybe-initialize-cache ()
  "Initialize the cache if needed."
  (unless zotero-cache-cache
    (setq zotero-cache-cache
          (or (zotero-cache-unserialize zotero-cache-cache-file)
              `(libraries ,(ht-create)
                          version ,(ht-create)
                          groups ,(ht-create)
                          synccache ,(ht-create)
                          ;; collections ,(ht-create)
                          ;; items ,(ht-create)
                          ;; searches ,(ht-create)
                          deletions ,(ht-create)))))
  ;; (unless zotero-cache-enable-cache-time
  ;;   (setq zotero-cache-cache-time (ht-create)))
  )

(defun zotero-cache--group-cached-p (id)
  "Check if group ID is already in cache."
  (let ((groups (plist-get zotero-cache-cache :groups)))
    (seq-some (lambda (group) (equal (plist-get group :id) id)) groups)))

(defun zotero-cache--cache-get (resource)
  "Get cached version of RESOURCE.
RESOURCE is one of
 'version
 'groups
 'collections
 'items
 'searches."
  (zotero-cache--maybe-initialize-cache)
  (plist-get zotero-cache-cache resource))

(defun zotero-cache--cache-remove-key (resource &rest keys)
  "Remove KEY from CACHE.
The cache is created both in memory and on the hard drive."
  (let ((table (plist-get zotero-cache-cache resource)))
    (dolist (key keys)
      (ht-remove! table key))
    (setq zotero-cache-cache (plist-put zotero-cache-cache resource table))
    (zotero-cache-serialize-cache)))

(defun zotero-cache--cache-update-version (version)
  "Update VERSION in cache.
The cache is created both in memory and on the hard drive."
  (let* ((resource 'version)
         (table (plist-get zotero-cache-cache resource)))
    (ht-set! table :version version)
    (ht-set! table :lastsync (current-time))
    (setq zotero-cache-cache (plist-put zotero-cache-cache resource table))
    (zotero-cache-serialize-cache)))

(defun zotero-cache--cache-update-libraries (id data)
  "Update or add DATA in cache.
The cache is created both in memory and on the hard drive."
  (let* ((resource 'libraries)
         (table (plist-get zotero-cache-cache resource)))
    (ht-set! table id data)
    (setq zotero-cache-cache (plist-put zotero-cache-cache resource table))
    (zotero-cache-serialize-cache)))

(defun zotero-cache--cache-update-group (id data)
  "Update or add DATA in cache.
The cache is created both in memory and on the hard drive."
  (let* ((resource 'groups)
         (table (plist-get zotero-cache-cache resource)))
    (ht-set! table id data)
    (setq zotero-cache-cache (plist-put zotero-cache-cache resource table))
    (zotero-cache-serialize-cache)))

(defun zotero-cache--cache-get-library-metadata ()
  "Get library metadata from cache."
  (let* ((resource 'libraries)
         (table (plist-get zotero-cache-cache resource)))
    (ht->plist table)))

(defun zotero-cache--cache-get-groups ()
  "Get library metadata from cache."
  (let* ((resource 'groups)
         (table (plist-get zotero-cache-cache resource)))
    (ht->plist table)))

(defun zotero-cache--mergable-plist-p (plist1 plist2)
  "Return non-nil if PLIST1 and PLIST2 can be merged without conflicts.
Two plists are considered mergable when the same keys don't have different values."
  (loop for (key val) on plist1 by 'cddr
        always (or (not (plist-member plist2 key))
                   (eq val :json-false)
                   (eq val :json-empty)
                   (eq (plist-get plist2 key) :json-false)
                   (eq (plist-get plist2 key) :json-empty)
                   (equal val (plist-get plist2 key)))))

(defun zotero-cache--merge-plist (plist1 plist2)
  "Merge PLIST2 into PLIST1."
  (loop for (key val) on plist2 by 'cddr do
        (unless (or (eq val :json-false)
                    (eq val :json-empty))
          (plist-put plist1 key val))
        finally return plist1))

(cl-defun zotero-cache--process-updates (resource user group cache data version)
  "Update TABLE with DATA.
Return the updated table when success or nil when failed."
  (catch 'quit
    (seq-doseq (object data)
      (let ((key (intern (concat ":" (plist-get object :key))))
            (value (ht-get* cache group resource key)))
        (pcase value
          ;; if object doesn't exist locally:
          ;; create local object with version = Last-Modified-Version and set synced = true
          (nil
           ;; (ht-set! table key `(:synced t :version ,version :data ,object))
           (setf value `(:synced t :version ,version :data ,object)))
          ;; if object hasn't been modified locally (synced == true):
          ;; overwrite with synced = true and version = Last-Modified-Version
          ((guard (eq (plist-get value :synced) t))
           ;; (ht-set! table key `(:synced t :version ,version :data ,object))
           (setf value `(:synced t :version ,version :data ,object)))
          ;; if object hasn't changed:
          ;; set synced = true and version = Last-Modified-Version
          ((guard (equal (plist-get value :data) object))
           ;; (ht-set! table key `(:synced t :version ,version :data ,object))
           (setf value `(:synced t :version ,version :data ,object)))
          ;; if changes can be automatically merged:
          ;; apply changes from each side and set synced = true and version = Last-Modified-Version
          ((guard (zotero-cache--mergable-plist-p (plist-get value :data) object))
           (let ((merged (zotero-cache--merge-plist (plist-get value :data) object)))
             (ht-set! table key `(:synced t :version ,version :data ,merged))
             (setf value `(:synced t :version ,version :data ,merged))))
          ;; else:
          ;; prompt user to choose a side or merge conflicts
          ;; TODO: global variable to set default action: 'ask 'keep-local 'keep-remote 'manual
          (value
           ;; TODO: offer option to always delete or keep
           (let ((choice (read-multiple-choice
                          "Conflict between local and remote object cannot be automatically resolved. How should this be resolved? "
                          '((?l "keep the local copy")
                            (?r "keep the remote copy")
                            (?q "quit")))))
             (pcase (car choice)
               ;; else if user chooses local copy:
               ;; synced = false and set a flag to restart the sync when finished
               ;; TODO: flag to restart the sync when finished?
               (?l
                ;; (ht-set! table key `(:synced :json-false :data ,(plist-get value :data)))
                (setf value `(:synced :json-false :data ,(plist-get value :data))))
               ;; if user chooses remote copy:
               ;; overwrite with synced = true and version = Last-Modified-Version
               (?r
                ;; (ht-set! table key `(:synced t :version ,version :data ,object))
                (setf value `(:synced t :version ,version :data ,object)))
               (?q
                (throw 'quit nil))))))))
    cache))

(defun zotero-cache--process-deletions (table keys version)
  "Delete KEYS from TABLE.
Return the updated table when success or nil when failed."
  (catch 'break
    (seq-doseq (key keys)
      (let ((key (intern (concat ":" key))))
        (pcase (ht-get table key)
          ;; if local object doesn't exist:
          ;; continue
          ;; if object hasn't been modified locally (synced == true):
          ;; delete local object, skipping delete log
          ((and value (guard (eq (plist-get value :synced) t)))
           (ht-remove! table key))
          ;; else:
          ;; perform conflict resolution
          (value
           ;; TODO: offer option to always delete or keep
           (let ((choice (read-multiple-choice
                          "Deleted object was modified. How should this be resolved? "
                          '((?d "delete the locally modified object")
                            (?k "keep the locally modified object")
                            (?q "quit")))))
             (pcase (car choice)
               ;; if user chooses deletion, delete local object, skipping delete log
               (?d
                (ht-remove! table key))
               ;; if user chooses local modification, keep object and set synced = true and version = Last-Modified-Version
               (?k
                (let ((data (plist-get value :data)))
                  (ht-set! table key `(:synced t :version ,version :data ,data))))
               (?q
                (throw 'break nil))))))))
    table))



;; TODO: offer option to manually resolve sync conflict
(cl-defun zotero-cache--cache-update-object (resource user group cache data version)
  "Update DATA in cache.
The cache is created both in memory and on the hard drive."
  (when-let ((updated-table (zotero-cache--process-updates :resource resource :user user :group group :cache cache :data data :version version)))
    ;; TODO: handle save errors
    (setq zotero-cache-cache (plist-put zotero-cache-cache resource updated-table))
    (message "Saving updated %s to cache..." (symbol-name resource))
    (zotero-cache-serialize-cache)
    (message "Saving updated %s to cache...done" (symbol-name resource))))

(defun zotero-cache--cache-delete (resource keys version)
  "Delete KEYS in cache."
  (let* ((resource (intern (substring (symbol-name resource) 1))) ; strip the ":"
         (table (plist-get zotero-cache-cache resource)))
    (when-let ((deleted-table (zotero-cache--process-deletions table keys version)))
      (setq zotero-cache-cache (plist-put zotero-cache-cache resource deleted-table))
      (zotero-cache-serialize-cache))))

(cl-defun zotero-cache--sync-groups (&key user api-key)
  "Sync group metadata.

Group metadata includes group titles and descriptions as well as member/role/permissions information. It is separate from group library data."
  (let* ((remote-plist (zotero-sync-get-remote-versions :resource 'groups :user user :api-key api-key))
         (local-hash (zotero-cache--sync-cache-get 'groups))
         ;; Collect only the keys
         (remote (cl-loop for key in remote-plist by #'cddr collect key))
         (local (ht-keys local-hash))
         ;; The local objects not in the remote list
         (removed (seq-difference local remote))
         ;; The remote objects not in the local list
         (added (seq-difference remote local))
         ;; The objects both in the local and remote list
         (intersection (seq-intersection local remote))
         ;; The objects with different version numbers
         (updated (seq-filter (lambda (id)
                                (let* ((plist (ht-get local-hash id))
                                       (local-version (thread-first
                                                          (plist-get plist :data)
                                                        (plist-get :version)))
                                       (remote-version (plist-get remote-plist id)))
                                  (/= local-version remote-version)))
                              intersection)))
    ;; Delete any local groups not in the list, which either were
    ;; deleted or are currently inaccessible. (The user may have been
    ;; removed from a group, or the current API key may no longer have
    ;; access.)
    (when removed
      ;; TODO: If data has been modified locally in any groups that
      ;; are no longer available, offer the user the ability to cancel
      ;; and transfer modified data elsewhere before continuing.
      (apply 'zotero-cache--sync-cache-remove-key resource removed))
    ;; For each group that doesn't exist locally or that has a
    ;; different version number, retrieve the group metadata.
    ;; FIXME: upload local changes of group metadata
    (when (or added updated)
      (dolist (key (nconc added updated))
        (let* ((id (substring (symbol-name key) 1)) ; strip the ":"
               (url (zotero-cache--sync-endpoint :group id))
               (data (zotero-lib--retrieve :url url :api-key api-key)))
          (zotero-cache--sync-cache-update-group key data))))
    `(:removed ,removed :added ,added :updated ,updated)))

;; TODO: check last-modified-version with every api call
(cl-defun zotero-cache--sync-get-remotely-updated (&key resource user group (since 0) api-key)
  (let* ((url (zotero-lib--endpoint :resource resource :user user :group group))
         (handle `(:url ,url :method "GET" :api-version ,zotero-sync-api-version :api-key ,api-key :format "versions" :since ,since))
         (remote-response (zotero-lib--get-response handle))
         (remote-data (plist-get remote-response :data))
         (last-modified-version (plist-get remote-response :version))
         ;; Collect only the keys
         (remote-keys (cl-loop for key in remote-data by #'cddr collect key))
         (cache (plist-get zotero-sync-cache group))
         ;; Remotely updated keys
         (updated-keys (seq-filter (lambda (key)
                                     (let* ((value (ht-get* cache resource key))
                                            (local-version (plist-get value :version))
                                            (remote-version (plist-get remote-data key)))
                                       (/= local-version remote-version)))
                                   remote-keys)))
    ;; TODO: retrieve objects flagged as previously failed to save
    (when updated-keys
      (let* ((partitions (seq-partition updated-keys 50))
             (number 0)
             (data))
        (dolist (partition partitions data)
          (message "Retrieving %d-%d of %d updated %s..." (1+ number) (+ number (length partition)) (length updated-keys) (symbol-name resource))
          (let* ((param-key (pcase resource
                              ('collections :collectionkey)
                              ('items :itemkey)
                              ('searches :searchkey)))
                 (param-value (s-join "," (seq-map (lambda (key) (substring (symbol-name key) 1)) partition)))
                 (objects (zotero-lib--retrieve :resource resource :user user :group group :api-key api-key param-key param-value)))
            (message "Retrieving %d-%d of %d updated %s...done" (1+ number) (+ number (length partition)) (length updated-keys) (symbol-name resource))
            (setq number (+ number (length partition)))
            (setq data (seq-concatenate 'vector data objects))))
        (zotero-cache--sync-cache-update-object :resource resource :user user :group group :cache cache :data updated-data :version last-modified-version)))
    last-modified-version))

(cl-defun zotero-cache--sync-get-remotely-deleted (&key resource user group since api-key)
  (let* ((url (zotero-cache--sync-endpoint :resource 'deleted :user user :group group))
         (handle `(:url ,url :method "GET" :api-version ,zotero-sync-api-version :api-key ,api-key :since ,since))
         (remote-response (zotero-lib--get-response handle))
         (remote-data (plist-get remote-response :data))
         (last-modified-version (plist-get remote-response :version))
         (deleted-keys (plist-get remote-data resource)))
    (unless (seq-empty-p deleted-keys)
      (zotero-cache--sync-cache-delete resource deleted-keys last-modified-version))
    last-modified-version))

;; TODO: attempt to upload data first and retrieve updated data only if receiving a 412 Precondition Failed. See If-Unmodified-Since-Version for more information.
(cl-defun -zotero-cache--sync-get-locally-updated (&key resource user group since api-key)
  (let* ((url (zotero-cache--sync-endpoint :resource resource :user user :group group))
         (handle `(:url ,url :method "GET" :api-version ,zotero-sync-api-version :api-key ,api-key :since ,since))
         (last-modified-version (plist-get remote-response :version))
         (cache (zotero-cache--sync-cache-get resource))
         (keys (ht-keys cache))
         ;; Locally modified keys
         (modified-keys (seq-filter (lambda (key)
                                      (let ((plist (ht-get cache key))
                                            (synced (plist-get plist :synced)))
                                        ;; Explicit negation of true value
                                        ;; because `json.el' parses `nil' as
                                        ;; `:json-false', which evaluates to
                                        ;; `t'
                                        (not (eq synced t))))
                                    keys)))
    (when modified-keys
      (let* ((partitions (seq-partition modified-keys 50))
             (number 0)
             (data))
        (dolist (partition partitions)
          (message "Uploading %d-%d of %d updated %s..." (1+ number) (+ number (length partition)) (length modified-keys) (symbol-name resource))
          (let* ((param-key (pcase resource
                              ('collections :collectionkey)
                              ('items :itemkey)
                              ('searches :searchkey)))
                 (data (mapcar (lambda (key) (let ((plist (ht-get cache key))) (plist-get plist :data))) partition ))
                 (json (zotero-cache--sync-encode-object data)))
            (zotero-cache--sync-submit :method "POST" :resource resource :user user :group group :data json :content-type "application/json" :expect "" :version version)
            (message "Uploading %d-%d of %d updated %s...done" (1+ number) (+ number (length partition)) (length updated-keys) (symbol-name resource))
            (setq number (+ number (length partition)))
            (setq data (seq-concatenate 'vector data objects))))
        (message "Saving updated %s to cache..." (symbol-name resource))
        (zotero-cache--sync-cache-update-object resource data)
        (message "Saving updated %s to cache...done" (symbol-name resource))))
    last-modified-version))



;; TODO:
;; When an object is deleted locally during regular usage, add its library and key to a delete log.
;; When syncing, send delete requests for objects in the log and clear them from the log on successful deletion.
;; When resolving a conflict between a locally deleted object and a remotely modified object in favor of the remote object, remove it from the delete log.

;; See Deleting Multiple Collections, Deleting Multiple Searches, and Deleting Multiple Items for the specific requests. Pass the current library version as If-Unmodified-Since-Version.

;; Example request:

;; DELETE <userOrGroupPrefix>/collections?collectionKey=<key>,<key>,<key>
;; If-Unmodified-Since-Version: <version>

;; Response:

;; 204 No Content
;; Last-Modified-Version: <version>

;; On a 204 response, store the returned Last-Modified-Version as the current library version to be passed with the next write request.

;; On a 412 Precondition Failed response, return to the beginning of the sync process for that library.

(cl-defun zotero-cache--sync-get-locally-deleted (&key user group since api-key)
  (let* ((url (zotero-cache--sync-endpoint :resource resource :user user :group group))
         (handle `(:url ,url :method "GET" :api-version ,zotero-sync-api-version :api-key ,api-key :since ,since))
         (last-modified-version (plist-get remote-response :version))
         (deletions (zotero-cache--sync-cache-get deletions))
         (keys (ht-keys cache))
         ;; Locally deleted keys
         (deleted-keys (seq-filter (lambda (key)
                                     (let ((plist (ht-get cache key)))
                                       (equal (plist-get plist :group) group)))
                                   keys)))
    (when deleted-keys
      (let* ((partitions (seq-partition modified-keys 50))
             (number 0)
             (data))
        (dolist (partition partitions)
          (message "Uploading %d-%d of %d deleted %s..." (1+ number) (+ number (length partition)) (length modified-keys) (symbol-name resource))
          (let* ((param-key (pcase resource
                              ('collections :collectionkey)
                              ('items :itemkey)
                              ('searches :searchkey)))
                 (param-value (s-join "," (seq-map (lambda (key) (substring (symbol-name key) 1)) partition)))
                 (json (zotero-cache--sync-encode-object data)))
            (zotero-cache--sync-submit :method "DELETE" :resource resource :user user :group group :data json :content-type "application/json" :expect "" :version version)
            (message "Uploading %d-%d of %d deleted %s...done" (1+ number) (+ number (length partition)) (length updated-keys) (symbol-name resource))
            ;; Remove the keys from the delete log
            (mapcar (lambda (key) (ht-remove! cache key)) partition)
            (setq number (+ number (length partition)))
            (setq data (seq-concatenate 'vector data objects))))))
    last-modified-version))

(defun zotero-sync (data)
  (while
      (let* ((response (zotero-cache--sync-request handle))
             (plist (zotero-cache--sync-decider handle response)))
        (pcase (plist-get plist :data)
          ((and (pred vectorp) data)
           (setq result (seq-concatenate 'vector result data)))
          ((and (pred consp) data)
           (setq result (seq-concatenate 'list result data))))
        (setq handle (plist-get plist :handle)))))

(defun zotero-cache--sync-verify-key (handle)
  (zotero-lib--retrieve :resource 'keys :key api-key))

(defun zotero-cache--sync-group-readable-p (group data)
  (let ((group-access (plist-get data :access))
        (group (intern (concat ":" group))))
    (or (eq (thread-first group-access
              (plist-get :groups)
              (plist-get group)
              (plist-get :library))
            t)
        (eq (thread-first group-access
              (plist-get :groups)
              (plist-get :all)
              (plist-get :library))
            t))))

(defun zotero-cache--sync-group-writable-p (group response)
  (let ((group-access (thread-first response
                        (plist-get :data)
                        (plist-get :access)))
        (group (intern (concat ":" group))))
    (or (eq (thread-first group-access
              (plist-get :groups)
              (plist-get group)
              (plist-get :write))
            t)
        (eq (thread-first group-access
              (plist-get :groups)
              (plist-get :all)
              (plist-get :write))
            t))))

(defun zotero-cache--sync-cache-update-libraries (id data)
  "Update libraries in cache.
The cache is created both in memory and on the hard drive."
  (let* ((resource 'libraries)
         (table (plist-get zotero-sync-cache resource)))
    (ht-set! table :type type :read library :write write :files data :version version :lastsync (current-time))
    (setq zotero-sync-cache (plist-put zotero-sync-cache resource table))
    (zotero-sync-serialize-cache)))

(cl-defun zotero-sync-sync (&key user api-key)
  "Sync library."
  ;; 1) Verify key access
  (let* ((url (zotero-lib--endpoint :resource 'keys :key api-key))
         (handle `(:url ,url :method "GET" :api-version ,zotero-lib-api-version :api-key ,api-key))
         (response (zotero-lib--get-response handle))
         ;; (cache (zotero---cache-get-library-metadata))
         (libraries (plist-get zotero-cache-cache 'libraries)))
    ;; (ht-each (lambda (key value)
    ;;            (let* ((id key)
    ;;                   (data value))
    ;;              ;; - for each library without read access: offer to remove the library
    ;;              ;; - for each library without write access: offer to reset local changes
    ;;              ;; - for each library with read and write access: continue to sync
    ;;              )))
    )
  ;; 2) Get updated group metadata
  ;; 2a) First, retrieve a list of the user's groups, with a version indicating the current state of each group's metadata
  (let ((remote-groups (zotero-lib--retrieve :resource 'groups :user user :api-key api-key :format "versions"))
        (local-groups (zotero-cache--get 'groups))
        (synccache (zotero-cache--get 'synccache)))
    (cl-loop for group in remote-groups by #'cddr do
             ;; REVIEW: should this be postponed?
             (unless (ht-contains? synccache group)
               (ht-set! synccache group (ht-create )))
             (cl-loop for resource in '(collections items searches) do
                      ;; 2b) For each group that doesn't exist locally or that has a different version number, retrieve the group metadata
                      (let ((remote-version (plist-get remote-groups group))
                            (local-version (thread-first (ht-get local-groups group)
                                             (plist-get :version))))
                        (if (eq remote-version local-version)
                            (message "Group %s already up to date." (zotero-lib--keyword->string group))
                          ;; Perform the following steps for each library:
                          ;; i. Get updated data
                          (zotero-cache--sync-get-remotely-updated :resource resource :group group :since local-version :api-key api-key)
                          ;; ii. Get deleted data
                          ;; REVIEW: loop over resource in inside or outside function?
                          (zotero-cache--sync-get-remotely-deleted :resource resource :group group :since local-version :api-key api-key)
                          ;; FIXME: iii. Check for concurrent remote updates
                          ;; iv. Upload modified data
                          (-zotero-cache--sync-get-locally-updated :resource resource :group group :since local-version :api-key api-key)
                          ;; v. Upload local deletions
                          (-zotero-cache--sync-get-locally-deleted :resource resource :group group :since local-version :api-key api-key)))))))

;; FIXME: merge with read function
(cl-defun zotero-cache--sync-get-remote-versions (&key resource user group key since api-key)
  "Get versions of RESOURCE.
RESOURCE is one of 'groups 'collections 'items 'searches."
  (zotero-lib--retrieve :resource resource :user user :group group :key key :api-key api-key :format "versions" :since since))

;; (cl-defun zotero-sync-privileges (&key api-key)
;;   "Verify key access."
;;   (let* ((privileges (zotero-sync-get-key :api-key api-key))
;;          (access (plist-get privileges :access))
;;          (user-privileges (plist-get access :user))
;;          (user-library-access (plist-get user-privileges :library))
;;          (user-files-access (plist-get user-privileges :files))
;;          (user-notes-access (plist-get user-privileges :notes))
;;          (user-write-access (plist-get user-privileges :write)))
;;     ;; TODO: offer to remove a local library or reset local changes?
;;     ;; (cond ((not access)
;;     ;;        (warn "Insufficient privileges for access to the personal or group
;;     ;;            libraries"))
;;     ;;       ((not user-library-access)
;;     ;;        (warn "Insufficient privileges for access to the personal library"))
;;     ;;       ((not user-notes-access)
;;     ;;        (warn "Insufficient privileges for access to the notes in the personal library"))
;;     ;;       ((not user-write-access)
;;     ;;        (warn "Insufficient privileges for write access to the personal library")))
;;     user-privileges))

(provide 'zotero-cache)

;;; zotero-cache.el ends here
