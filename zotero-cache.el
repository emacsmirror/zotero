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
(defun zotero-cache--initialize-cache ()
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
