;;; zotero-sync.el --- synchronization for Zotero -*- lexical-binding: t; -*-

(cl-defun zotero-sync--groups (&key user api-key)
  "Sync group metadata.

Group metadata includes group titles and descriptions as well as member/role/permissions information. It is separate from group library data."
  (let* ((remote-plist (zotero-sync-get-remote-versions :resource 'groups :user user :api-key api-key))
         (local-hash (zotero-sync--cache-get 'groups))
         ;; Collect only the keys
         (remote (cl-loop for key in remote-plist by 'cddr collect key))
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
      (apply 'zotero-sync--cache-remove-key resource removed))
    ;; For each group that doesn't exist locally or that has a
    ;; different version number, retrieve the group metadata.
    ;; FIXME: upload local changes of group metadata
    (when (or added updated)
      (dolist (key (nconc added updated))
        (let* ((id (substring (symbol-name key) 1)) ; strip the ":"
               (url (zotero-sync--endpoint :group id))
               (data (zotero-sync--retrieve :url url :api-key api-key)))
          (zotero-sync--cache-update-group key data))))
    `(:removed ,removed :added ,added :updated ,updated)))

;; TODO: check last-modified-version with every api call
(cl-defun zotero-sync--get-remotely-updated (&key resource user group (since 0) api-key)
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
                 (objects (zotero-sync--retrieve :resource resource :user user :group group :api-key api-key param-key param-value)))
            (message "Retrieving %d-%d of %d updated %s...done" (1+ number) (+ number (length partition)) (length updated-keys) (symbol-name resource))
            (setq number (+ number (length partition)))
            (setq data (seq-concatenate 'vector data objects))))
        (zotero-sync--cache-update-object :resource resource :user user :group group :cache cache :data updated-data :version last-modified-version)))
    last-modified-version))

(cl-defun zotero-sync--get-remotely-deleted (&key resource user group since api-key)
  (let* ((url (zotero-sync--endpoint :resource 'deleted :user user :group group))
         (handle `(:url ,url :method "GET" :api-version ,zotero-sync-api-version :api-key ,api-key :since ,since))
         (remote-response (zotero-sync--get-response handle))
         (remote-data (plist-get remote-response :data))
         (last-modified-version (plist-get remote-response :version))
         (deleted-keys (plist-get remote-data resource)))
    (unless (seq-empty-p deleted-keys)
      (zotero-sync--cache-delete resource deleted-keys last-modified-version))
    last-modified-version))

;; TODO: attempt to upload data first and retrieve updated data only if receiving a 412 Precondition Failed. See If-Unmodified-Since-Version for more information.
(cl-defun -zotero-sync--get-locally-updated (&key resource user group since api-key)
  (let* ((url (zotero-sync--endpoint :resource resource :user user :group group))
         (handle `(:url ,url :method "GET" :api-version ,zotero-sync-api-version :api-key ,api-key :since ,since))
         (last-modified-version (plist-get remote-response :version))
         (cache (zotero-sync--cache-get resource))
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
                 (json (zotero-sync--encode-object data)))
            (zotero-sync--submit :method "POST" :resource resource :user user :group group :data json :content-type "application/json" :expect "" :version version)
            (message "Uploading %d-%d of %d updated %s...done" (1+ number) (+ number (length partition)) (length updated-keys) (symbol-name resource))
            (setq number (+ number (length partition)))
            (setq data (seq-concatenate 'vector data objects))))
        (message "Saving updated %s to cache..." (symbol-name resource))
        (zotero-sync--cache-update-object resource data)
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

(cl-defun zotero-sync--get-locally-deleted (&key user group since api-key)
  (let* ((url (zotero-sync--endpoint :resource resource :user user :group group))
         (handle `(:url ,url :method "GET" :api-version ,zotero-sync-api-version :api-key ,api-key :since ,since))
         (last-modified-version (plist-get remote-response :version))
         (deletions (zotero-sync--cache-get deletions))
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
                 (json (zotero-sync--encode-object data)))
            (zotero-sync--submit :method "DELETE" :resource resource :user user :group group :data json :content-type "application/json" :expect "" :version version)
            (message "Uploading %d-%d of %d deleted %s...done" (1+ number) (+ number (length partition)) (length updated-keys) (symbol-name resource))
            ;; Remove the keys from the delete log
            (mapcar (lambda (key) (ht-remove! cache key)) partition)
            (setq number (+ number (length partition)))
            (setq data (seq-concatenate 'vector data objects))))))
    last-modified-version))

(defun zotero-sync (data)
  (while
      (let* ((response (zotero-sync--request handle))
             (plist (zotero-sync--decider handle response)))
        (pcase (plist-get plist :data)
          ((and (pred vectorp) data)
           (setq result (seq-concatenate 'vector result data)))
          ((and (pred consp) data)
           (setq result (seq-concatenate 'list result data))))
        (setq handle (plist-get plist :handle)))))

(defun zotero-sync--verify-key (handle)
  (zotero-sync--retrieve :resource 'keys :key api-key))

(defun zotero-sync--group-readable-p (group data)
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

(defun zotero-sync--group-writable-p (group response)
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

(defun zotero-sync--cache-update-libraries (id data)
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
        (local-groups (zotero-cache--cache-get 'groups))
        (synccache (zotero-cache--cache-get 'synccache)))
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
                          (zotero-sync--get-remotely-updated :resource resource :group group :since local-version :api-key api-key)
                          ;; ii. Get deleted data
                          ;; REVIEW: loop over resource in inside or outside function?
                          (zotero-sync--get-remotely-deleted :resource resource :group group :since local-version :api-key api-key)
                          ;; FIXME: iii. Check for concurrent remote updates
                          ;; iv. Upload modified data
                          (-zotero-sync--get-locally-updated :resource resource :group group :since local-version :api-key api-key)
                          ;; v. Upload local deletions
                          (-zotero-sync--get-locally-deleted :resource resource :group group :since local-version :api-key api-key)))))))

;; FIXME: merge with read function
(cl-defun zotero-sync--get-remote-versions (&key resource user group key since api-key)
  "Get versions of RESOURCE.
RESOURCE is one of 'groups 'collections 'items 'searches."
  (zotero-sync--retrieve :resource resource :user user :group group :key key :api-key api-key :format "versions" :since since))

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

(provide 'zotero-sync)

;;; zotero-sync.el ends here
