;;; zotero-edit.el --- Interface to Zotero  -*- lexical-binding: t; -*-

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

(require 'ht)
(require 'iso8601)
(require 's)
(require 'seq)
(require 'wid-edit)
(require 'zotero)
(require 'zotero-cache)
(require 'zotero-lib)

(declare-function zotero-browser-revert "zotero-browser")

;;;; Variables

(defvar zotero-edit-text-buffer-action '((display-buffer-reuse-window display-buffer-in-side-window)
                                         (side . bottom)
                                         (window-height . 0.5)
                                         (preserve-size . (nil . t))))

(defvar-local zotero-edit-resource nil)
(defvar-local zotero-edit-type nil)
(defvar-local zotero-edit-id nil)
(defvar-local zotero-edit-data nil)
(defvar-local zotero-edit-data-copy nil)
(defvar-local zotero-edit-widget nil)
(defvar-local zotero-edit-creators-widget nil)

(defconst zotero-edit-help-echo
  "\\[widget-complete]: complete field; \\[widget-button-press]: \
enter value; \\[zotero-edit-text]: edit in buffer")

(defconst zotero-edit-usage-message
  "Type \\[zotero-edit-text-exit] to finish, \
\\[zotero-edit-text-save] to save, or \
\\[zotero-edit-text-abort] to abort.")

;;;; Keymap

(defvar zotero-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-keymap)
    (define-key map (kbd "C-x C-s") #'zotero-edit-save)
    (define-key map (kbd "C-c C-k") #'zotero-edit-reset)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Local keymap for `zotero-edit-mode'.")

(defvar zotero-edit-text-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'zotero-edit-text-exit)
    (define-key map (kbd "C-x C-s") #'zotero-edit-text-save)
    (define-key map (kbd "C-c C-k") #'zotero-edit-text-abort)
    map)
  "Local keymap for `zotero-edit-text-mode'.")

(defvar zotero-edit-text-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-text-keymap)
    (define-key map (kbd "C-c C-c") #'zotero-edit-text)
    map)
  "Keymap for multiline text fields in `zotero-edit-mode'.")

;;;; Menu

(easy-menu-define zotero-edit-mode-menu zotero-edit-mode-map
  "Menu for `zotero-edit-mode'."
  `("Zotero-Edit"
    "--"
    ["Save" zotero-edit-save :help "Save item"]
    ["Reset" zotero-edit-reset :help "Reset item"]
    "--"
    ["Quit" quit-window]
    ["Customize" (customize-group 'zotero-browser)]))

(easy-menu-define zotero-edit-text-mode-menu zotero-edit-text-mode-map
  "Menu for `zotero-edit-text-mode'."
  `("Zotero-Edit"
    "--"
    ["Exit" zotero-edit-text-exit :help "Save current text and exit"]
    ["Save" zotero-edit-text-save :help "Save current text"]
    ["Abort" zotero-edit-text-abort :help "Discard current text and exit"]
    "--"
    ["Quit" quit-window]
    ["Customize" (customize-group 'zotero-browser)]))

;;;; Customization

(defgroup zotero-edit nil
  "Interface to Zotero-Edit."
  :group 'zotero)

(defcustom zotero-edit-buffer-name "*Zotero Edit*"
  "The default buffer name."
  :group 'zotero-edit
  :type 'string)

;;;; Mode

;;;###autoload
(define-derived-mode zotero-edit-mode fundamental-mode "Zotero edit"
  "Major mode for Zotero Edit buffers.

All currently available key bindings:

\\{zotero-edit-mode-map}"
  ;; Turn on highlighting
  (font-lock-mode 1)
  ;; Turn on word wrap
  (visual-line-mode 1)
  ;; Turn on buttonizing of URLs and e-mail addresses
  (goto-address-mode 1))

;;;###autoload
(define-minor-mode zotero-edit-text-mode
  "Minor mode for Zotero Edit buffers.

All currently available key bindings:

\\{zotero-edit-text-mode-map}"
  nil "ZotEdit" nil)

;;;; Functions

(defun zotero-edit--toggle-notify (widget &rest _ignore)
  "Toggle mode of name textfields in WIDGET."
  (let* ((parent (widget-get widget :parent))
         (siblings (widget-get parent :children))
         (single-field-p (widget-value widget))
         (list (seq-find (lambda (sibling) (eq (widget-type sibling) 'editable-list)) siblings))
         (namefields (widget-get list :children)))
    ;; If switched to single-field mode
    (if single-field-p
        ;; Join the first and last name
        (let* ((first-name (nth 0 namefields))
               (last-name (nth 1 namefields))
               (full-name (cond
                           ((string-empty-p (widget-value first-name)) (widget-value last-name))
                           ((string-empty-p (widget-value last-name)) (widget-value first-name))
                           (t (s-join " " (list (widget-value first-name) (widget-value last-name)))))))
          (widget-value-set list (list full-name)))
      ;; Else if switched to dual-field mode
      (let* ((full-name (nth 0 namefields))
             ;; Split the full name to a first and last name
             (words (s-split-words (widget-value full-name)))
             (first-name (or (s-join " " (butlast words)) ""))
             (last-name (or (car (last words)) "")))
        (widget-value-set list (list first-name last-name))))
    (widget-setup)
    (widget-apply parent :notify parent)))

(defun zotero-edit-item (data type id)
  "Create an item edit buffer with DATA.

Argument TYPE is \"user\" for your personal library, and
\"group\" for the group libraries. ID is the ID of the personal
or group library you want to access, that is the user ID or group
ID."
  ;; TODO: use schema for internationalisation
  (let ((buffer (get-buffer-create zotero-edit-buffer-name)))
    (with-current-buffer buffer
      (zotero-edit-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (remove-overlays))
      (save-excursion
        (let* ((itemtype (plist-get data :itemType))
               (linkmode (plist-get data :linkMode))
               (itemtypes (zotero-cache-itemtypes))
               (itemfields (zotero-cache-itemtypefields itemtype))
               (creatortypes (zotero-cache-itemtypecreatortypes itemtype))
               (template (pcase itemtype
                           ("attachment" (zotero-cache-attachment-template linkmode))
                           (_ (zotero-cache-item-template itemtype)))))
          (setq zotero-edit-resource "items"
                zotero-edit-type type
                zotero-edit-id id
                zotero-edit-data data
                zotero-edit-data-copy (copy-tree data))
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
                     (_ (not(eq value :json-false)))
                     (fieldname "Parent Item"))
            (widget-insert (concat fieldname ": "))
            (widget-insert (concat value "\n")))
          (cl-loop for key in template by #'cddr do
                   (pcase key
                     ;; Itemtype
                     ((and :itemType field)
                      (let* ((fieldname "Item Type")
                             (value (plist-get data key))
                             (choices (seq-map (lambda (elt) `(item :value ,elt :tag ,(zotero-cache-itemtype-locale elt))) itemtypes)))
                        (widget-create 'menu-choice
                                       :format (concat fieldname ": %[%v%]")
                                       :notify (lambda (widget &rest _ignore)
                                                 (let* ((current-itemtype value)
                                                        (new-itemtype (widget-value widget))
                                                        (new-itemfields (zotero-cache-itemtypefields new-itemtype)))
                                                   (when-let ((missing-fields (seq-difference itemfields new-itemfields))
                                                              (localized-fields (seq-map (lambda (elt) (zotero-cache-itemfield-locale elt)) missing-fields))
                                                              (fields ""))
                                                     (while localized-fields
                                                       (setf fields (concat fields "- " (pop localized-fields) "\n")))
                                                     (if
                                                         (y-or-n-p (format "Are you sure you want to change the item type?\nThe following fields will be lost:\n%s" fields))
                                                         (let* ((props-to-delete (seq-map (lambda (elt) (zotero-lib-string->keyword elt)) missing-fields))
                                                                (data (plist-put zotero-edit-data-copy field new-itemtype))
                                                                (data (apply #'zotero-lib-plist-delete data props-to-delete))
                                                                (template (zotero-cache-item-template new-itemtype))
                                                                (merged (zotero-lib-merge-plist template data)))
                                                           (zotero-edit-item merged type id))
                                                       (setq zotero-edit-data-copy (plist-put zotero-edit-data-copy field current-itemtype))
                                                       (widget-value-set widget current-itemtype)
                                                       (widget-setup)))))
                                       :button-prefix "▾"
                                       :value value
                                       :args choices)))
                     ;; Title
                     ((and :title field)
                      (let ((fieldname (zotero-cache-itemfield-locale key))
                            (value (or (plist-get data key) "")))
                        (widget-insert (concat fieldname ":"))
                        (widget-create 'editable-field
                                       :size 10
                                       :format " %v "
                                       :notify (lambda (widget &rest _ignore)
			                         (setq zotero-edit-data-copy (plist-put zotero-edit-data-copy field (widget-value widget))))
                                       :value value)
                        (widget-insert "\n")))
                     ;; Creators
                     ((and :creators field)
                      (let* ((fieldname "Creators")
                             (value (plist-get data key))
                             (values (seq-map (lambda (elt)
                                                (let ((type (plist-get elt :creatorType))
                                                      (single-field-p (plist-member elt :name)))
                                                  (list
                                                   type
                                                   (if single-field-p
                                                       (list (or (plist-get elt :name) ""))
                                                     (list (or (plist-get elt :firstName) "")
                                                           (or (plist-get elt :lastName) "")))
                                                   ;; nil means dual textfield, t single textfield
                                                   (if single-field-p t nil))))
                                              value))
                             (primary (car creatortypes))
                             (choices (seq-map (lambda (elt)
                                                 `(item :format "%t" :value ,elt :tag ,(zotero-cache-creatortype-locale elt)))
                                               creatortypes)))
                        (widget-insert (concat fieldname ":\n"))
                        (setq zotero-edit-creators-widget
                              (widget-create 'editable-list
                                             :entry-format "%i %d %v\n"
                                             :notify (lambda (widget &rest _ignore)
                                                       (let* ((creators-list (seq-map (lambda (elt)
                                                                                        (pcase elt
                                                                                          (`(,creator-type (,full-name) ,_fieldmode)
                                                                                           (list :creatorType creator-type
                                                                                                 :name full-name))
                                                                                          (`(,creator-type (,first-name ,last-name) ,_fieldmode)
                                                                                           (list :creatorType creator-type
                                                                                                 :firstName first-name
                                                                                                 :lastName last-name))
                                                                                          (_ (error "Invalid value")))) (widget-value widget)))
                                                              (creators-vector (seq-into creators-list 'vector)))
                                                         (setq zotero-edit-data-copy (plist-put zotero-edit-data-copy :creators creators-vector))))
                                             :value values
                                             `(group
                                               :format "%v"
                                               (menu-choice
                                                :size 10
                                                :format "Type: %[%v%]\n"
                                                :button-prefix "▾"
                                                :void (item :format "%t" :value ,primary :tag ,(zotero-cache-creatortype-locale primary))
                                                :args ,choices)
                                               (editable-list
                                                :format "%v\n" ; Omit `insert-button' widget
                                                :entry-format "%v" ; Omit `insert-button' and `delete-button' widgets
                                                (editable-field
                                                 :size 10
                                                 :format " %v "))
                                               (toggle
                                                :format "Switch to %[%v%]"
                                                :on "Dual field"
                                                :off "Single field"
                                                :notify zotero-edit--toggle-notify))))))
                     ;; Abstract
                     ((and :abstractNote field)
                      (let* ((fieldname (zotero-cache-itemfield-locale key))
                             (value (or (plist-get data key) "")))
                        (widget-insert (concat fieldname ":\n"))
                        (widget-create 'text
                                       :size 10
                                       :format " %v "
                                       :help-echo "M-TAB: complete field; RET: enter value; C-c ': edit in buffer"
                                       :notify (lambda (widget &rest _ignore)
			                         (setq zotero-edit-data-copy (plist-put zotero-edit-data-copy field (widget-value widget))))
                                       :value value
                                       :keymap zotero-edit-text-keymap)
                        (widget-insert "\n")))
                     ;; Note
                     ((and :note field)
                      (let* ((fieldname "Note")
                             (value (or (plist-get data key) "")))
                        (widget-insert (concat fieldname ":\n"))
                        (widget-create 'text
                                       :size 10
                                       :format " %v "
                                       :help-echo "M-TAB: complete field; RET: enter value; C-c ': edit in buffer"
                                       :notify (lambda (widget &rest _ignore)
			                         (setq zotero-edit-data-copy (plist-put zotero-edit-data-copy field (widget-value widget))))
                                       :value value
                                       :keymap zotero-edit-text-keymap)
                        (widget-insert "\n")))
                     (:md5
                      (when-let ((fieldname "MD5")
                                 (value (plist-get data key)))
                        (unless (eq value :json-false)
                          (widget-insert (concat fieldname ": "))
                          (widget-insert (format "%s\n" value)))))
                     (:mtime
                      (when-let ((fieldname "Mtime")
                                 (value (plist-get data key)))
                        (unless (eq value :json-false)
                          (widget-insert (concat fieldname ": "))
                          (widget-insert (format "%s\n" value)))))
                     ((and :tags field)
                      (let* ((fieldname "Tags")
                             (value (plist-get data key))
                             (values (unless (eq value :json-empty) (seq-map (lambda (elt) elt) value))))
                        (widget-insert (format "%d %s:\n" (length values) fieldname))
                        (widget-create 'editable-list
                                       :entry-format "%d %v"
                                       :notify (lambda (widget &rest _ignore)
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
                             (table (zotero-cache-synccache "collections" nil type id))
                             (choices (ht-map (lambda (key value) `(item :format "%t" :value ,key :tag ,(zotero-lib-plist-get* value :object :data :name))) table)))
                        (widget-insert (format "%d %s:\n" (length values) fieldname))
                        (widget-create 'editable-list
                                       :entry-format "%i %d %v\n"
                                       :notify (lambda (widget &rest _ignore)
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
                                       :notify (lambda (widget &rest _ignore)
                                                 (let* ((value (if-let ((values (widget-value widget)))
                                                                   (seq-into values'vector)
                                                                 :json-empty)))
                                                   (setq zotero-edit-data-copy (plist-put zotero-edit-data-copy field value))))
                                       :value values
                                       '(editable-field ""))))
                     ;; Rest
                     ((and _ field)
                      (let* ((fieldname (or (zotero-cache-itemfield-locale key) (capitalize (zotero-lib-keyword->string key))))
                             (value (or (plist-get data key) "")))
                        (widget-insert (concat fieldname ":"))
                        (widget-create 'editable-field
                                       :size 10
                                       :format " %v "
                                       :notify (lambda (widget &rest _ignore)
			                         (setq zotero-edit-data-copy (plist-put zotero-edit-data-copy field (widget-value widget))))
                                       :value value)
                        (widget-insert "\n")))))
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
		         :notify (lambda (&rest _ignore)
                                   (message "Saving...")
                                   (if-let ((object (zotero-cache-save zotero-edit-data-copy "items" type id)))
                                       (progn
                                         (message "Saving...done.")
                                         (zotero-edit-item (plist-get object :data) type id))
                                     (message "Saving...failed.")))
                         "Save")
          (widget-insert " ")
          ;; Reset button
          (widget-create 'push-button
		         :notify (lambda (&rest _ignore)
                                   (message "Item reset.")
                                   (zotero-edit-item data type id))
		         "Reset")
          (widget-setup))))
    buffer))

(defun zotero-edit-collection (data type id)
  "Create an collection edit buffer with DATA.

Argument TYPE is \"user\" for your personal library, and
\"group\" for the group libraries. ID is the ID of the personal
or group library you want to access, that is the user ID or group
ID."
  (let ((buffer (get-buffer-create zotero-edit-buffer-name)))
    (with-current-buffer buffer
      (zotero-edit-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (remove-overlays))
      (save-excursion
        (let ((template (zotero-collection-template)))
          (setq zotero-edit-resource "collections"
                zotero-edit-type type
                zotero-edit-id id
                zotero-edit-data data
                zotero-edit-data-copy (copy-tree data))
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
                        (widget-insert (concat fieldname ":"))
                        (widget-create 'editable-field
                                       :size 10
                                       :format " %v "
                                       :notify (lambda (widget &rest _ignore)
			                         (setq zotero-edit-data-copy (plist-put zotero-edit-data-copy field (widget-value widget))))
                                       :value value)
                        (widget-insert "\n")))
                     ;; Parent Collection
                     ((and :parentCollection field)
                      (let* ((fieldname "Parent Collection" )
                             (value (plist-get data key))
                             (table (zotero-cache-synccache "collections" nil type id))
                             (collections (ht-map (lambda (key value) `(item :format "%t" :value ,key :tag ,(zotero-lib-plist-get* value :object :data :name))) table))
                             (choices (cons `(item :format "%t" :value :json-false :tag "None") collections)))
                        (widget-create 'menu-choice
                                       :format (concat fieldname ": %[%v%]\n")
                                       :notify (lambda (widget &rest _ignore)
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
                                       :notify (lambda (widget &rest _ignore)
                                                 (let* ((value (if-let ((values (widget-value widget)))
                                                                   (seq-into values'vector)
                                                                 :json-empty)))
                                                   (setq zotero-edit-data-copy (plist-put zotero-edit-data-copy field value))))
                                       :value values
                                       '(editable-field ""))))))
          ;; Save button
          (widget-insert "\n")
          (widget-create 'push-button
		         :notify (lambda (&rest _ignore)
	                           (message "Saving...")
                                   (if-let ((object (zotero-cache-save zotero-edit-data-copy "collections" type id)))
                                       (progn
                                         (message "Saving...done.")
                                         (zotero-edit-collection (plist-get object :data) type id))
                                     (message "Saving...failed.")))
                         "Save")
          (widget-insert " ")
          ;; Reset button
          (widget-create 'push-button
		         :notify (lambda (&rest _ignore)
                                   (zotero-edit-collection data type id))
		         "Reset")
          (widget-setup))))
    buffer))

(defun zotero-edit-create-item (itemtype type id collection)
  "Create an empty item edit buffer of ITEMTYPE.

Argument TYPE is \"user\" for your personal library, and
\"group\" for the group libraries. ID is the ID of the personal
or group library you want to access, that is the user ID or group
ID. COLLECTION is the collection key."
  (let* ((template (zotero-cache-item-template itemtype))
         (data (if collection (plist-put template :collections (vector collection)) template)))
    (zotero-edit-item data type id)))

(defun zotero-edit-create-note (type id &optional parent)
  "Create an empty note edit buffer.

Argument TYPE is \"user\" for your personal library, and
\"group\" for the group libraries. ID is the ID of the personal
or group library you want to access, that is the user ID or group
ID.

Optional argument PARENT is the key of the parent item."
  (let* ((template (zotero-cache-item-template "note"))
         (new-template (copy-tree template)))
    (when parent (plist-put new-template :parentItem parent))
    (zotero-edit-item new-template type id)))

;;;; Commands

(defun zotero-edit-reset ()
  "Reset current item."
  (interactive)
  (pcase zotero-edit-resource
    ("items" (zotero-edit-item zotero-edit-data zotero-edit-type zotero-edit-id))
    ("collections" (zotero-edit-collection zotero-edit-data zotero-edit-type zotero-edit-id))))

(defun zotero-edit-save ()
  "Save current item."
  (interactive)
  (message "Saving item...")
  (let ((resource zotero-edit-resource)
        (type zotero-edit-type)
        (id zotero-edit-id)
        (data zotero-edit-data-copy))
    (if-let ((object (zotero-cache-save data resource type id)))
        (progn
          (message "Saving item...done")
          (pcase zotero-edit-resource
            ("items" (zotero-edit-item (plist-get object :data) type id))
            ("collections" (zotero-edit-collection (plist-get object :data) type id))))
      (message "Saving item...failed"))))

(defun zotero-edit-ensure-edit-buffer ()
  "Check if the current buffer is a text edit buffer."
  (unless (local-variable-p 'zotero-edit-widget)
    (user-error "Current buffer is not a Zotero editing buffer")))

(defun zotero-edit-text ()
  "Edit the text at point.

The text is copied to a separate buffer. When done, exit with
`\\[zotero-edit-text-exit]'. This will remove the original text
in the source buffer, and replace it with the edited version.

When optional argument BUFFER-NAME is non-nil, use it as the
name of the sub-editing buffer."
  (interactive)
  (let* ((widget (widget-at (point)))
         (value (widget-value widget)))
    (when (eq (widget-type widget) 'text)
      (let ((buffer (generate-new-buffer "*Zotero Edit Text*")))
        (with-current-buffer buffer
          (insert value)
          (set-buffer-modified-p nil))
        ;; Switch to edit buffer.
        (pop-to-buffer buffer zotero-edit-text-buffer-action)
        (zotero-edit-text-mode 1)
        (visual-line-mode 1)
        (setq zotero-edit-widget widget)
        (message (substitute-command-keys zotero-edit-usage-message))))))

(defun zotero-edit-text-save ()
  "Save source buffer with current state sub-editing buffer."
  (interactive)
  (zotero-edit-ensure-edit-buffer)
  (let* ((widget zotero-edit-widget)
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
  (zotero-edit-ensure-edit-buffer)
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
