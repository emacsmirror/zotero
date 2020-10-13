;;; zotero-edit.el --- Interface to Zotero  -*- lexical-binding: t; -*-

;; Author: Folkert van der Beek <folkertvanderbeek@gmail.com>
;; Created: 2020-03-27

;; URL: https://gitlab.com/fvdbeek/emacs-zotero

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

(require 'zotero-lib)
(require 'zotero-cache)
(require 'seq)
(require 'widget)

;;;; Variables

(defvar zotero-edit-doi-regexp "10\.[[:digit:]]\\{4,9\\}/[0-9A-Za-z()./:;_-]+"
  "A regular expression probably matching a modern Crossref DOI.")

(defvar zotero-edit-text-buffer-action '((display-buffer-reuse-window display-buffer-in-side-window) (side . bottom) (window-height . 0.5)(preserve-size . (nil . t))))

(defvar-local zotero-edit-type nil)
(defvar-local zotero-edit-id nil)
(defvar-local zotero-edit-data nil)
(defvar-local zotero-edit-data-copy nil)
(defvar-local zotero-edit-locale nil)
(defvar-local zotero-edit-widget nil)

;;;; Keymap

;;;; Menu

;;;; Customization

(defgroup zotero-edit nil
  "Interface to Zotero-Edit."
  :group 'external)

(defcustom zotero-edit-buffer-name "*Zotero Edit*"
  "The default buffer name."
  :group 'zotero-edit
  :type 'string)

;;;; Mode

;; (defvar zotero-edit-keymap
;;   (let ((map (copy-keymap widget-global-map)))
;;     (define-key map "\C-c\C-k" #'zotero-edit-reset)
;;     (define-key map "\C-x\C-s" #'zotero-edit-save)
;;     (define-key map (kbd "q") #'quit-window)
;;     map))

(defvar zotero-edit-text-keymap
  (let ((map (copy-keymap widget-text-keymap)))
    (define-key map (kbd "C-c '") #'zotero-edit-text)
    map))

(defvar zotero-edit-text-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c '") #'zotero-edit-text-exit)
    (define-key map (kbd "C-c C-k") #'zotero-edit-text-abort)
    (define-key map (kbd "C-x C-s") #'zotero-edit-text-save)
    map))

(defvar zotero-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-k") #'zotero-edit-reset)
    (define-key map (kbd "C-c C-s") #'zotero-edit-save)
    map)
  "Local keymap for `zotero-edit-mode'.")

;;;###autoload
(define-derived-mode zotero-edit-mode text-mode "Zotero edit"
  "Major mode for Zotero Edit buffers.

All currently available key bindings:

\\{zotero-edit-mode-map}"
  ;; Turn on highlighting
  (font-lock-mode 1)
  ;; Turn on word wrap
  (visual-line-mode 1)
  ;; Turn on buttonizing of URLs and e-mail addresses
  (goto-address-mode 1))

;;;; Commands

(eval-when-compile
  (require 'wid-edit))

;; (defun zotero-edit-fontify-doi (&optional start end)
;;   "Fontify the DOIS in the current buffer.
;; This function implements `goto-address-highlight-p'
;; and `goto-address-fontify-p'."
;;   (interactive)
;;   (save-excursion
;;     (goto-char (or start (point-min)))
;;     (when (or (eq t goto-address-fontify-maximum-size)
;; 	      (< (- (or end (point-max)) (point))
;;                  goto-address-fontify-maximum-size))
;;       (while (re-search-forward zotero-edit-doi-regexp end t)
;; 	(let* ((s (match-beginning 0))
;; 	       (e (match-end 0))
;; 	       this-overlay)
;; 	  (setq this-overlay (make-overlay s e))
;; 	  (and goto-address-fontify-p
;; 	       (overlay-put this-overlay 'face goto-address-url-face))
;; 	  (overlay-put this-overlay 'evaporate t)
;; 	  (overlay-put this-overlay
;; 		       'mouse-face goto-address-url-mouse-face)
;; 	  (overlay-put this-overlay 'follow-link t)
;; 	  (overlay-put this-overlay
;; 		       'help-echo "mouse-2, C-c RET: follow URL")
;; 	  (overlay-put this-overlay
;; 		       'keymap goto-address-highlight-keymap)
;; 	  (overlay-put this-overlay 'goto-address t))))))

;; (defun zotero-edit-find-doi-at-point ()
;;   "Find DOI around or before point.
;; Then search backwards to beginning of line for the start of DOI.
;; If no DOI found, return nil."
;;   (re-search-backward zotero-edit-doi-regexp (line-beginning-position) 'lim)
;;   (if (or (looking-at zotero-edit-doi-regexp)
;; 	  (and (re-search-forward zotero-edit-doi-regexp
;; 		                  (line-end-position) 'lim)
;; 	       (goto-char (match-beginning 0))))
;;       (match-string-no-properties 0)))

;; (defun zotero-edit-goto-doi-at-point (&optional event)
;;   "Load the DOI at point."
;;   (interactive (list last-input-event))
;;   (save-excursion
;;     (if event (posn-set-point (event-end event)))
;;     (if-let ((doi (save-excursion (zotero-edit-find-doi-at-point))))
;;         (browse-url (concat "https://doi.org/" doi))
;;       (user-error "No DOI found"))))

(cl-defun zotero-edit-create-item (&key type id itemtype locale)
  "Create a new item of ITEMTYPE."
  (let ((template (zotero-cache-item-template itemtype)))
    (zotero-edit-item :type type :id id :data template :locale locale)))

(cl-defun zotero-edit-create-attachment (&key type id parent linkmode content-type charset filename md5 mtime accessdate locale)
  "Create a new attachment of LINKTYPE."
  (let* ((template (zotero-cache-attachment-template linkmode))
         (new-template (copy-tree template)))
    (when parent (plist-put new-template :parentItem parent))
    (when content-type (plist-put new-template :contentType content-type))
    (when charset (plist-put new-template :charset charset))
    (when filename
      (plist-put new-template :title filename)
      (plist-put new-template :filename filename))
    (when md5 (plist-put new-template :md5 md5))
    (when mtime (plist-put new-template :mtime mtime))
    (when accessdate (plist-put new-template :accessDate accessdate))
    (zotero-edit-item :type type :id id :data new-template :locale locale)))

(cl-defun zotero-edit-create-note (&key type id parent itemtype locale)
  "Create a new note."
  (let* ((template (zotero-cache-item-template "note"))
         (new-template (copy-tree template)))
    (when parent (plist-put new-template :parentItem parent))
    (zotero-edit-item :type type :id id :data new-template :locale locale)))

(cl-defun zotero-edit-item (&key type id data locale)
  "Create a new item buffer."
  (let ((buffer (get-buffer-create zotero-edit-buffer-name)))
    (with-current-buffer buffer
      (zotero-edit-mode)
      (erase-buffer)
      (save-excursion
        (let* ((schema (zotero-cache-schema))
               (itemtype (plist-get data :itemType))
               (linkmode (plist-get data :linkMode))
               (itemtypes (zotero-cache-itemtypes))
               (itemfields (zotero-cache-itemtypefields itemtype))
               (creatortypes (zotero-cache-itemtypecreatortypes itemtype))
               (template (pcase itemtype
                           ("attachment" (zotero-cache-attachment-template linkmode))
                           (_ (zotero-cache-item-template itemtype)))))
          (setq zotero-edit-type type
                zotero-edit-id id
                zotero-edit-data data
                zotero-edit-data-copy (copy-tree data)
                zotero-edit-locale locale)
          ;; Key
          (when-let ((value (plist-get data :key))
                     (fieldname "Key"))
            (widget-insert (concat fieldname ": "))
            (widget-insert (concat value "\n")))
          ;; Version
          (when-let ((value (plist-get data :version))
                     (fieldname "Version"))
            (widget-insert (concat fieldname ": "))
            (widget-insert (format "%d\n" value)))
          (when-let ((value (plist-get data :parentItem))
                     (fieldname "Parent Item"))
            (widget-insert (concat fieldname ": "))
            (widget-insert (concat value "\n")))
          (cl-loop for key in template by #'cddr do
                   (pcase key
                     ;; Itemtype
                     ((and :itemType field)
                      (let* ((fieldname "Item Type")
                             (value (plist-get data key))
                             (choices (seq-map (lambda (elt) `(item :value ,elt :tag ,(zotero-cache-itemtype-locale elt locale))) itemtypes)))
                        (widget-create 'menu-choice
                                       :format (concat fieldname ": %[%v%]")
                                       :notify (lambda (widget &rest ignore)
                                                 (let* ((current-itemtype value)
                                                        (new-itemtype (widget-value widget))
                                                        (new-itemfields (zotero-cache-itemtypefields new-itemtype)))
                                                   (when-let ((missing-fields (seq-difference itemfields new-itemfields))
                                                              (localized-fields (seq-map (lambda (elt) (zotero-cache-itemfield-locale elt locale)) missing-fields))
                                                              (fields ""))
                                                     (while localized-fields
                                                       (setf fields (concat fields "- " (pop localized-fields) "\n")))
                                                     (if
                                                         (y-or-n-p (format "Are you sure you want to change the item type?\nThe following fields will be lost:\n%s" fields))
                                                         (let* ((props-to-delete (seq-map (lambda (elt) (zotero-lib-string->keyword elt)) missing-fields))
                                                                (data (plist-put zotero-edit-data-copy field new-itemtype))
                                                                (data (apply #'zotero-lib-plist-delete data props-to-delete))
                                                                (template (zotero-cache-item-template new-itemtype))
                                                                (merged (zotero-cache-merge-plist template data)))
                                                           (zotero-edit-item :type type :id id :data merged :locale locale))
                                                       (setq zotero-edit-data-copy (plist-put zotero-edit-data-copy field current-itemtype))
                                                       (widget-value-set widget current-itemtype)
                                                       (widget-setup)))))
                                       :button-prefix "▾"
                                       :value value
                                       :args choices)))
                     ;; Title
                     ((and :title field)
                      (let ((fieldname (zotero-cache-itemfield-locale key locale))
                            (value (or (plist-get data key) "")))
                        (widget-insert (concat fieldname ": "))
                        (widget-create 'editable-field
                                       :size 10
                                       :format "%v\n"
                                       :notify (lambda (widget &rest ignore)
			                         (setq zotero-edit-data-copy (plist-put zotero-edit-data-copy field (widget-value widget))))
                                       :value value)))
                     ;; Creators
                     ((and :creators field)
                      (let* ((fieldname "Creators")
                             (value (plist-get data key))
                             (values (seq-map (lambda (elt)
                                                (list (plist-get elt :creatorType)
                                                      (plist-get elt :firstName)
                                                      (plist-get elt :lastName)))
                                              value))
                             (primary (car creatortypes))
                             (choices (seq-map (lambda (elt)
                                                 `(item :format "%t" :value ,elt :tag ,(zotero-cache-creatortype-locale elt locale)))
                                               creatortypes)))
                        (widget-insert (concat fieldname "\n"))
                        (widget-create 'editable-list
                                       :entry-format "%i %d %v\n"
                                       :notify (lambda (widget &rest ignore)
                                                 (let* ((creators-list (seq-map (lambda (elt)
                                                                                  (list :creatorType (first elt) :firstName (second elt) :lastName (third elt))) (widget-value widget)))
                                                        (creators-vector (seq-into creators-list 'vector)))
                                                   (setq zotero-edit-data-copy (plist-put zotero-edit-data-copy field creators-vector))))
                                       :value values
                                       `(group
                                         :format "%v"
                                         (menu-choice
                                          :size 10
                                          :format "%[%v%]"
                                          :button-prefix "▾"
                                          :void (item :format "%t" :value ,primary :tag ,(zotero-cache-creatortype-locale primary locale))
                                          :args ,choices)
                                         (editable-field
                                          :size 10
                                          :format " %v ")
                                         (editable-field
                                          :size 10
                                          :format " %v ")))))
                     ;; Abstract
                     ((and :abstractNote field)
                      (let* ((fieldname (zotero-cache-itemfield-locale key locale))
                             (value (or (plist-get data key) "")))
                        (widget-insert (concat fieldname ":\n"))
                        (widget-create 'text
                                       :size 10
                                       :format "%v\n"
                                       :help-echo "M-TAB: complete field; RET: enter value; C-c ': edit in buffer"
                                       :notify (lambda (widget &rest ignore)
			                         (setq zotero-edit-data-copy (plist-put zotero-edit-data-copy field (widget-value widget))))
                                       :value value
                                       :keymap zotero-edit-text-keymap)))
                     ;; Note
                     ((and :note field)
                      (let* ((fieldname "Note")
                             (value (or (plist-get data key) "")))
                        (widget-insert (concat fieldname ":\n"))
                        (widget-create 'text
                                       :size 10
                                       :format "%v\n"
                                       :help-echo "M-TAB: complete field; RET: enter value; C-c ': edit in buffer"
                                       :notify (lambda (widget &rest ignore)
			                         (setq zotero-edit-data-copy (plist-put zotero-edit-data-copy field (widget-value widget))))
                                       :value value
                                       :keymap zotero-edit-text-keymap)))
                     (:mtime
                      (when-let ((fieldname "Mtime")
                                 (value (plist-get data key)))
                        (widget-insert (concat fieldname ": "))
                        (widget-insert (format "%d\n" value))))
                     ((and :tags field)
                      (let* ((fieldname "Tags")
                             (value (plist-get data key))
                             (values (unless value :json-empty (seq-map (lambda (elt) elt) value))))
                        (widget-insert (format "%d %s:\n" (length values) fieldname))
                        (widget-create 'editable-list
                                       :entry-format "%d %v"
                                       :notify (lambda (widget &rest ignore)
                                                 (let* ((value (if-let ((values (widget-value widget)))
                                                                   (seq-into values'vector)
                                                                 :json-empty)))
                                                   (setq zotero-edit-data-copy (plist-put zotero-edit-data-copy field value))))
                                       :value values
                                       '(editable-field ""))))
                     ;; Collections
                     ((and :collections field)
                      (let* ((key :collections)
                             (fieldname "Collections")
                             (value (plist-get data key))
                             (values (seq-into value 'list))
                             (table (ht-get* zotero-cache "synccache" id "collections"))
                             (choices (ht-map (lambda (key value) `(item :format "%t" :value ,key :tag ,(zotero-lib-plist-get* value :object :data :name))) table)))
                        (widget-insert (format "%d %s:\n" (length values) fieldname))
                        (widget-create 'editable-list
                                       :entry-format "%i %d %v\n"
                                       :notify (lambda (widget &rest ignore)
                                                 (let* ((value (seq-into (widget-value widget) 'vector)))
                                                   (setq zotero-edit-data-copy (plist-put zotero-edit-data-copy field value))))

                                       :value values
                                       `(menu-choice
                                         :size 10
                                         :format "%[%v%]"
                                         :button-prefix "▾"
                                         :void (item :format "%t" :value "" :tag "None")
                                         :args ,choices))))
                     ;; Relations
                     ((and :relations field)
                      (let* ((fieldname "Related")
                             (value (plist-get data key))
                             (values (unless value :json-empty (seq-map (lambda (elt) elt) value))))
                        (widget-insert (format "%d %s:\n" (length values) fieldname))
                        (widget-create 'editable-list
                                       :entry-format "%d %v"
                                       :notify (lambda (widget &rest ignore)
                                                 (let* ((value (if-let ((values (widget-value widget)))
                                                                   (seq-into values'vector)
                                                                 :json-empty)))
                                                   (setq zotero-edit-data-copy (plist-put zotero-edit-data-copy field value))))
                                       :value values
                                       '(editable-field ""))))
                     ;; Rest
                     ((and _ field)
                      (let* ((fieldname (or (zotero-cache-itemfield-locale key locale) (capitalize (zotero-lib-keyword->string key))))
                             (value (or (plist-get data key) "")))
                        (widget-insert (concat fieldname ": "))
                        (widget-create 'editable-field
                                       :size 10
                                       :format "%v\n"
                                       :notify (lambda (widget &rest ignore)
			                         (setq zotero-edit-data-copy (plist-put zotero-edit-data-copy field (widget-value widget))))
                                       :value value)))))
          ;; Date Added
          (when-let ((value (plist-get data :dateAdded))
                     (fieldname "Date Added")
                     (time (iso8601-parse value))
                     (timestamp (encode-time time))
                     (time-string (format-time-string "%c" timestamp)))
            (widget-insert (concat fieldname ": " time-string "\n")))
          ;; Date Modified
          (when-let ((value (plist-get data :dateModified))
                     (fieldname "Date Modified")
                     (time (iso8601-parse value))
                     (timestamp (encode-time time))
                     (time-string (format-time-string "%c" timestamp)))
            (widget-insert (concat fieldname ": " time-string "\n")))
          ;; Save button
          (widget-insert "\n")
          (widget-create 'push-button
		         :notify (lambda (&rest ignore)
                                   (when-let ((saved-data (zotero-edit-save-item :type type :id id :data zotero-edit-data-copy)))
                                     (message "Item saved.")
                                     (zotero-edit-item :type type :id id :data saved-data :locale locale)))
                         "Save")
          (widget-insert " ")
          ;; Reset button
          (widget-create 'push-button
		         :notify (lambda (&rest ignore)
                                   (message "Item reset.")
                                   (zotero-edit-item :type type :id id :data data :locale locale))
		         "Reset")
          (use-local-map widget-keymap)
          (widget-setup))))
    buffer))

(defun zotero-edit-reset ()
  "Reset current item."
  (interactive)
  (let ((type zotero-edit-type)
        (id zotero-edit-id)
        (data zotero-edit-data)
        (locale zotero-edit-locale))
    (zotero-edit-item :type type :id id :data data :locale locale)))

(defun zotero-edit-save ()
  "Save current item."
  (interactive)
  (let ((type zotero-edit-type)
        (id zotero-edit-id)
        (data zotero-edit-data-copy)
        (locale zotero-edit-locale))
    (when-let ((saved-data (zotero-edit-save-item :type type :id id :data data)))
      (zotero-edit-item :type type :id id :data saved-data :locale locale))))

(cl-defun zotero-edit-save-item (&key type id data)
  "Upload DATA and save to CACHE.
Return the item data when successful, else `nil'."
  (let* ((now (current-time))
         ;; convert to ISO 8601 date format
         (date (format-time-string "%FT%T%z" now))
         (data (progn
                 (unless (plist-member data :dateAdded)
                   (plist-put data :dateAdded date))
                 (plist-put data :dateModified date)))
         (key (plist-get data :key))
         (uploaded (zotero-edit-upload-item :type type :id id :data data))
         object)
    (cond
     ;; If the object was successfully uploaded
     (uploaded
      (setq object uploaded))
     ;; If the object was unchanged, it already should have a key
     (key
      (setq object (ht-get table key)))
     ;; This shouldn't happen
     (t (error "Unknown error")))
    (if (zotero-cache-update-object object :type type :id id)
        (zotero-lib-plist-get* object :object :data)
      nil)))

(cl-defun zotero-edit-upload-item (&key type id data)
  "Upload DATA.
Return the object if uploading was successful, or `nil' if failed."
  (let* ((token (zotero-auth-token))
         (api-key (zotero-auth-api-key token))
         (status (zotero-lib-create-item data :type type :id id :api-key api-key))
         (successful (plist-get status :successful))
         (success (plist-get status :success))
         (unchanged (plist-get status :unchanged))
         (failed (plist-get status :failed)))
    (cond
     ((not (eq success :json-empty))
      (plist-get successful :0))
     ((not (eq failed :json-empty))
      (let ((code (zotero-lib-plist-get* failed :0 :code))
            (message (zotero-lib-plist-get* failed :0 :message)))
        (error "Error code %d: %s" code message)))
     ((not (eq unchanged :json-empty)) nil))))

(cl-defun zotero-edit-create-collection (&key type id)
  "Create a new collection."
  (let ((template (zotero-lib-collection-template)))
    (zotero-edit-collection :type type :id id :data template)))

(cl-defun zotero-edit-collection (&key type id data)
  "Create a new collection."
  (let ((buffer (get-buffer-create zotero-edit-buffer-name)))
    (with-current-buffer buffer
      (zotero-edit-mode)
      (erase-buffer)
      ;; (remove-overlays)
      (let* ((template (zotero-lib-collection-template)))
        (setq zotero-edit-data-copy (copy-tree data))
        ;; Key
        (when-let ((value (plist-get data :key))
                   (fieldname "Key"))
          (widget-insert (concat fieldname ": "))
          (widget-insert (concat value "\n")))
        ;; Version
        (when-let ((value (plist-get data :version))
                   (fieldname "Version"))
          (widget-insert (concat fieldname ": "))
          (widget-insert (format "%d\n" value)))
        (cl-loop for key in template by #'cddr do
                 (pcase key
                   ;; Name
                   ((and :name field)
                    (let ((fieldname "Name")
                          (value (or (plist-get data key) "")))
                      (widget-insert (concat fieldname ": "))
                      (widget-create 'editable-field
                                     :size 10
                                     :format "%v\n"
                                     :notify (lambda (widget &rest ignore)
			                       (setq zotero-edit-data-copy (plist-put zotero-edit-data-copy field (widget-value widget))))
                                     :value value)))
                   ;; Parent Collection
                   ((and :parentCollection field)
                    (let* ((fieldname "Parent Collection" )
                           (value (plist-get data key))
                           (table (ht-get* zotero-cache "synccache" id "collections"))
                           (collections (ht-map (lambda (key value) `(item :format "%t" :value ,key :tag ,(zotero-lib-plist-get* value :object :data :name))) table))
                           (choices (cons `(item :format "%t" :value :json-false :tag "None") collections)))
                      (widget-create 'menu-choice
                                     :format (concat fieldname ": %[%v%]")
                                     :notify (lambda (widget &rest ignore)
                                               (setq zotero-edit-data-copy (plist-put zotero-edit-data-copy field (widget-value widget))))
                                     :button-prefix "▾"
                                     :value value
                                     :args choices)))
                   ;; Relations
                   ((and :relations field)
                    (let* ((fieldname "Related")
                           (value (plist-get data key))
                           (values (unless value :json-empty (seq-map (lambda (elt) elt) value))))
                      (widget-insert (format "%d %s:\n" (length values) fieldname))
                      (widget-create 'editable-list
                                     :entry-format "%d %v"
                                     :notify (lambda (widget &rest ignore)
                                               (let* ((value (if-let ((values (widget-value widget)))
                                                                 (seq-into values'vector)
                                                               :json-empty)))
                                                 (setq zotero-edit-data-copy (plist-put zotero-edit-data-copy field value))))
                                     :value values
                                     '(editable-field ""))))))
        ;; Save button
        (widget-insert "\n")
        (widget-create 'push-button
		       :notify (lambda (&rest ignore)
			         (zotero-cache-save-collection :type type :id id :data zotero-edit-data-copy)
                                 (zotero-edit-collection :type type :id id :data zotero-edit-data-copy))
                       "Save")
        (widget-insert " ")
        ;; Reset button
        (widget-create 'push-button
		       :notify (lambda (&rest ignore)
                                 (zotero-edit-collection :type type :id id :data data))
		       "Reset")
        (use-local-map widget-keymap)
        (widget-setup)))
    buffer))

(define-minor-mode zotero-edit-text-mode
  "Minor mode for Zotero Edit buffers.

All currently available key bindings:

\\{zotero-edit-text-mode-map}"
  nil "ZotEdit" nil)

(defun zotero-edit-text (&optional pos buffer-name)
  "Edit the text at point.
\\<zotero-edit-text-mode-map>

The text is copied to a separate buffer and the appropriate mode.
When done, exit with `\\[zotero-edit-text-exit]'. This will
remove the original text in the source buffer, and replace
it with the edited version.

When optional argument BUFFER-NAME is non-nil, use it as the
name of the sub-editing buffer."
  (interactive "d")
  (let* ((widget (widget-at (or pos (point))))
         (value (widget-value widget))
         (buffer (generate-new-buffer "*Zotero Edit Text*"))
         (tempfile (concat temporary-file-directory "zotero-edit-text")))
    (when (eq (widget-type widget) 'text)
      (with-current-buffer buffer
        (insert value)
        (set-buffer-modified-p nil))
      ;; Switch to edit buffer.
      (pop-to-buffer buffer zotero-edit-text-buffer-action)
      (zotero-edit-text-mode)
      (setq zotero-edit-widget widget))))

(defun zotero-edit-text-buffer-p (&optional buffer)
  "Non-nil when current buffer is a source editing buffer.
  If BUFFER is non-nil, test it instead."
  (let ((buffer (or buffer (current-buffer))))
    (and (buffer-live-p buffer)
         (local-variable-p 'zotero-edit-widget))))

(defun zotero-edit-text-save ()
  "Save source buffer with current state sub-editing buffer."
  (interactive)
  (unless (zotero-edit-text-buffer-p) (error "Not in a sub-editing buffer"))
  (let* ((edit-buffer (current-buffer))
         (widget zotero-edit-widget)
         (src-buffer (widget-field-buffer widget))
         (contents (save-excursion (widen) (buffer-string))))
    (set-buffer-modified-p nil)
    (if (buffer-live-p src-buffer)
        (with-current-buffer src-buffer
          (widget-value-set widget contents)
          (widget-setup))
      (error "Source buffer disappeared. Aborting"))))

(defun zotero-edit-text-abort ()
  "Abort editing text and return to the source buffer."
  (interactive)
  (unless (zotero-edit-text-buffer-p) (error "Not in an editing buffer"))
  (let* ((edit-buffer (current-buffer))
         (edit-window (get-buffer-window edit-buffer))
         (widget zotero-edit-widget)
         (src-buffer (widget-field-buffer widget)))
    (set-buffer-modified-p nil)
    (if (buffer-live-p src-buffer)
        (progn
          (switch-to-buffer src-buffer)
          (delete-window edit-window)
          (kill-buffer edit-buffer))
      (error "Source buffer disappeared. Aborting"))))

(defun zotero-edit-text-exit ()
  "Kill current sub-editing buffer and return to source buffer."
  (interactive)
  (unless (zotero-edit-text-buffer-p) (error "Not in an editing buffer"))
  (let* ((edit-buffer (current-buffer))
         (edit-window (get-buffer-window edit-buffer))
         (widget zotero-edit-widget)
         (src-buffer (widget-field-buffer widget))
         (contents (save-excursion (widen) (buffer-string))))
    (set-buffer-modified-p nil)
    (if (buffer-live-p src-buffer)
        (progn
          (with-current-buffer src-buffer
            (widget-value-set widget contents)
            (widget-setup))
          (pop-to-buffer src-buffer)
          (delete-window edit-window)
          (kill-buffer edit-buffer))
      (error "Source buffer disappeared. Aborting"))))

(provide 'zotero-edit)

;;; zotero-edit.el ends here
