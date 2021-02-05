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

(require 'zotero-lib)
(require 'zotero-cache)
(require 'zotero-sync)
(require 'seq)
(require 'widget)

;;;; Variables

(defvar zotero-edit-doi-regexp "10\.[[:digit:]]\\{4,9\\}/[0-9A-Za-z()./:;_-]+"
  "A regular expression probably matching a modern Crossref DOI.")

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

(defconst zotero-edit-usage-message "Type
\\[zotero-edit-text-exit] to finish, \\[zotero-edit-text-save] to
save, or \\[zotero-edit-text-abort] to abort.")

;;;; Keymap

;;;; Menu

;;;; Customization

(defgroup zotero-edit nil
  "Interface to Zotero-Edit."
  :group 'zotero)

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

;;;###autoload
(define-minor-mode zotero-edit-text-mode
  "Minor mode for Zotero Edit buffers.

All currently available key bindings:

\\{zotero-edit-text-mode-map}"
  nil "ZotEdit" nil)

;;;; Commands

(eval-when-compile
  (require 'wid-edit))

(defun zotero-edit--apply-field-mode (widget &rest ignore)
  "Apply the appropriate field mode of WIDGET."
  ;; This function is called after initializing the widgets, and every time the
  ;; `creator-edit-creator-widget' is notified. This is necessary to deactivate
  ;; the appropriate fields in a newly inserted widget. As an undesirable side
  ;; effect, this function is called every time the field mode is toggled, which
  ;; would interfere with changing values by `zotero-edit--toggle-notify'. If
  ;; the conditions below do not apply, the field mode is toggled and the widget
  ;; should not be initialized.
  (let ((groups (widget-get widget :children)))
    (dolist (group groups)
      (let* ((siblings (widget-get group :children))
             (field-mode (seq-find (lambda (sibling) (eq (widget-type sibling) 'toggle)) siblings))
             (single-field-p (widget-value field-mode))
             (namefields (seq-filter (lambda (sibling) (eq (widget-type sibling) 'editable-field)) siblings))
             (first-name (first namefields))
             (last-name (second namefields))
             (full-name (third namefields)))
        (cond
         ;; Initialize the field mode if all namefields are empty
         ((and (string-empty-p (widget-value first-name))
               (string-empty-p (widget-value last-name))
               (string-empty-p (widget-value full-name)))
          (if single-field-p
              (progn
                ;; Activate only the full-name widget
                (widget-apply first-name :deactivate)
                (widget-apply last-name :deactivate)
                (widget-apply full-name :activate))
            ;; Activate only the full-name widget
            (widget-apply first-name :activate)
            (widget-apply last-name :activate)
            (widget-apply full-name :deactivate)))
         ;; Single-field mode and first-name and last-name widgets are active
         ((and single-field-p
               (string-empty-p (widget-value first-name))
               (string-empty-p (widget-value last-name)))
          ;; Activate only the full-name widget
          (widget-apply first-name :deactivate)
          (widget-apply last-name :deactivate)
          (widget-apply full-name :activate))
         ;; Dual-field mode and full-name widget is active
         ((and (not single-field-p) (string-empty-p (widget-value full-name)))
          ;; Activate only the full-name widget
          (widget-apply first-name :activate)
          (widget-apply last-name :activate)
          (widget-apply full-name :deactivate)))))))

(defun zotero-edit--toggle-notify (widget &rest ignore)
  "Toggle the field mode of WIDGET between single and dual.
If switched to single-field mode, the first and last name are
joined. If switched to dual-mode, the first name is set to all
but the last word of the full name, and the last name is set to
the last word."
  (let* ((single-field-p (widget-value widget))
         (parent (widget-get widget :parent))
         (siblings (widget-get parent :children))
         (namefields (seq-filter (lambda (sibling) (eq (widget-type sibling) 'editable-field)) siblings))
         (first-name (first namefields))
         (last-name (second namefields))
         (full-name (third namefields)))
    ;; If switched to single-field mode
    (if single-field-p
        ;; Join the first and last name
        (let* ((first (widget-value first-name))
               (last (widget-value last-name))
               (value (cond
                       ((string-empty-p first) last)
                       ((string-empty-p last) first)
                       (t (s-join " " (list first last))))))
          ;; Make the full-name widget active
          (widget-apply full-name :activate)
          ;; And set the value to the full name
          (widget-value-set full-name value)
          ;; Erase the first and last name
          (widget-value-set first-name "")
          (widget-value-set last-name "")
          ;; Make the widgets inactive
          (widget-apply first-name :deactivate)
          (widget-apply last-name :deactivate))
      ;; Else if switched to dual-field mode
      ;; Split the full name to a first and last name
      (let* ((words (s-split-words (widget-value full-name)))
             (first (or (s-join " " (butlast words)) ""))
             (last (or (car (last words)) "")))
        ;; Make the first-name and last-name widgets active
        (widget-apply first-name :activate)
        (widget-apply last-name :activate)
        ;; Set the value of the first name to all but last word
        (widget-value-set first-name first)
        ;; And set the value of the last name to the last word
        (widget-value-set last-name last)
        ;; Erase the full name
        (widget-value-set full-name "")
        ;; Make the widget inactive
        (widget-apply full-name :deactivate)))
    (widget-setup)))

(defun zotero-edit-item (data type id)
  "Create an item edit buffer with DATA.

Argument TYPE is \"user\" for your personal library, and
\"group\" for the group libraries. ID is the ID of the personal
or group library you want to access, e.g. the user ID or group
ID."
  (let ((buffer (get-buffer-create zotero-edit-buffer-name)))
    (with-current-buffer buffer
      (zotero-edit-mode)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (remove-overlays)
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
                                       :notify (lambda (widget &rest ignore)
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
                                                (let ((type (plist-get elt :creatorType))
                                                      (first (plist-get elt :firstName))
                                                      (last (plist-get elt :lastName))
                                                      (name (plist-get elt :name)))
                                                  (list
                                                   type
                                                   (or first "")
                                                   (or last "")
                                                   (or name "")
                                                   ;; nil means dual textfield, t single textfield
                                                   (if name t nil))))
                                              value))
                             (primary (car creatortypes))
                             (choices (seq-map (lambda (elt)
                                                 `(item :format "%t" :value ,elt :tag ,(zotero-cache-creatortype-locale elt)))
                                               creatortypes)))
                        (widget-insert (concat fieldname "\n"))
                        (setq zotero-edit-creators-widget
                              (widget-create 'editable-list
                                             :entry-format "%i %d %v\n"
                                             :notify (lambda (widget &rest ignore)
                                                       (zotero-edit--apply-field-mode widget)
                                                       (let* ((creators-list (seq-map (lambda (elt)
                                                                                        (if (string-empty-p (fourth elt))
                                                                                            (list :creatorType (first elt)
                                                                                                  :firstName (second elt)
                                                                                                  :lastName (third elt))
                                                                                          (list :creatorType (first elt)
                                                                                                :name (fourth elt)))) (widget-value widget)))
                                                              (creators-vector (seq-into creators-list 'vector)))
                                                         (setq zotero-edit-data-copy (plist-put zotero-edit-data-copy field creators-vector))))

                                             :value values
                                             `(group
                                               :format "%v"
                                               (menu-choice
                                                :size 10
                                                :format "Type: %[%v%]\n"
                                                :button-prefix "▾"
                                                :void (item :format "%t" :value ,primary :tag ,(zotero-cache-creatortype-locale primary))
                                                :args ,choices)
                                               (editable-field
                                                :size 10
                                                :format "First name: %v\n")
                                               (editable-field
                                                :size 10
                                                :format "Last name: %v\n")
                                               (editable-field
                                                :size 20
                                                :format "Full name: %v\n")
                                               (toggle
                                                :format "Switch to %[%v%]\n"
                                                :on "Dual field"
                                                :off "Single field"
                                                :notify zotero-edit--toggle-notify))))
                        (zotero-edit--apply-field-mode zotero-edit-creators-widget)))
                     ;; Abstract
                     ((and :abstractNote field)
                      (let* ((fieldname (zotero-cache-itemfield-locale key))
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
                             (table (zotero-cache-synccache "collections" nil type id))
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
                      (let* ((fieldname (or (zotero-cache-itemfield-locale key) (capitalize (zotero-lib-keyword->string key))))
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
                                   (message "Saving...")
                                   (if-let ((object (zotero-cache-save zotero-edit-data-copy "items" type id)))
                                       (progn
                                         (message "Saving...done.")
                                         (with-current-buffer zotero-browser-items-buffer-name
                                           (zotero-browser-revert))
                                         (zotero-edit-item (plist-get object :data) type id))
                                     (message "Saving...failed.")))
                         "Save")
          (widget-insert " ")
          ;; Reset button
          (widget-create 'push-button
		         :notify (lambda (&rest ignore)
                                   (message "Item reset.")
                                   (zotero-edit-item data type id))
		         "Reset")
          (use-local-map widget-keymap)
          (widget-setup))))
    buffer))

(defun zotero-edit-collection (data type id)
  "Create an collection edit buffer with DATA.

Argument TYPE is \"user\" for your personal library, and
\"group\" for the group libraries. ID is the ID of the personal
or group library you want to access, e.g. the user ID or group
ID."
  (let ((buffer (get-buffer-create zotero-edit-buffer-name)))
    (with-current-buffer buffer
      (zotero-edit-mode)
      (erase-buffer)
      ;; (remove-overlays)
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
                           (table (zotero-cache-synccache "collections" nil type id))
                           (collections (ht-map (lambda (key value) `(item :format "%t" :value ,key :tag ,(zotero-lib-plist-get* value :object :data :name))) table))
                           (choices (cons `(item :format "%t" :value :json-false :tag "None") collections)))
                      (widget-create 'menu-choice
                                     :format (concat fieldname ": %[%v%]\n")
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
	                         (message "Saving...")
                                 (if-let ((object (zotero-cache-save zotero-edit-data-copy "collections" type id)))
                                     (progn
                                       (message "Saving...done.")
                                       (with-current-buffer zotero-browser-collections-buffer-name
                                         (zotero-browser-revert))
                                       (zotero-edit-collection (plist-get object :data) type id))
                                   (message "Saving...failed.")))
                       "Save")
        (widget-insert " ")
        ;; Reset button
        (widget-create 'push-button
		       :notify (lambda (&rest ignore)
                                 (zotero-edit-collection data type id))
		       "Reset")
        (use-local-map widget-keymap)
        (widget-setup)))
    buffer))

(defun zotero-edit-create-item (itemtype type id collection)
  "Create an empty item edit buffer of ITEMTYPE.

Argument TYPE is \"user\" for your personal library, and
\"group\" for the group libraries. ID is the ID of the personal
or group library you want to access, e.g. the user ID or group
ID."
  (let* ((template (zotero-cache-item-template itemtype))
         (data (if collection (plist-put template :collections (vector collection)) template)))
    (zotero-edit-item template type id)))

(defun zotero-edit-create-collection (type id)
  "Create an empty collection edit buffer.

Argument TYPE is \"user\" for your personal library, and
\"group\" for the group libraries. ID is the ID of the personal
or group library you want to access, e.g. the user ID or group
ID."
  (zotero-edit-collection (zotero-collection-template) type id))

(cl-defun zotero-edit-create-attachment (linkmode type id &optional parent &key content-type charset filename md5 mtime accessdate)
  "Create an empty attachment edit buffer of LINKMODE.

LINKMODE is one of:
  - \"imported_file\"
  - \"imported_url\"
  - \"linked_file\"
  - \"linked_url\"

Argument TYPE is \"user\" for your personal library, and
\"group\" for the group libraries. ID is the ID of the personal
or group library you want to access, e.g. the user ID or group
ID.

Optional argument PARENT is the key of the parent item.

Keyword arguments CONTENT-TYPE, CHARSET FILENAME, FILESIZE, MD5,
and MTIME are attributes of the file and can be obtained by
`zotero-file-attributes', except for the charset that cannot be
determined without external tools."
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
    (zotero-edit-item new-template type id)))

(defun zotero-edit-create-note (type id &optional parent)
  "Create an empty note edit buffer.

Argument TYPE is \"user\" for your personal library, and
\"group\" for the group libraries. ID is the ID of the personal
or group library you want to access, e.g. the user ID or group
ID.

Optional argument PARENT is the key of the parent item."
  (let* ((template (zotero-cache-item-template "note"))
         (new-template (copy-tree template)))
    (when parent (plist-put new-template :parentItem parent))
    (zotero-edit-item new-template type id)))

(defun zotero-edit-reset ()
  "Reset current item."
  (interactive)
  (pcase zotero-edit-resource
    ("items" (zotero-edit-item zotero-edit-data zotero-edit-type zotero-edit-id))
    ("collections" (zotero-edit-collection zotero-edit-data zotero-edit-type zotero-edit-id))))

(defun zotero-edit-save ()
  "Save current item."
  (message "Saving item...")
  (interactive)
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

(defun zotero-edit-text (&optional pos buffer-name)
  "Edit the text at point.
\\<zotero-edit-text-mode-map>

The text is copied to a separate buffer. When done, exit with
`\\[zotero-edit-text-exit]'. This will remove the original text
in the source buffer, and replace it with the edited version.

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
      (zotero-edit-text-mode 1)
      (visual-line-mode 1)
      (setq zotero-edit-widget widget)
      (message (substitute-command-keys zotero-edit-usage-message)))))

(defun zotero-edit-text-save ()
  "Save source buffer with current state sub-editing buffer."
  (interactive)
  (zotero-edit-ensure-edit-buffer)
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
