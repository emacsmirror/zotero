;;; zotero-browser.el --- Interface to Zotero  -*- lexical-binding: t; -*-

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
(require 'zotero-edit)
(require 'zotero-fulltext)
(require 'zotero-recognize)
(require 'cl-lib)
(require 'ewoc)
(require 'ht)
(require 'mailcap)
(require 'seq)
(require 'subr-x)

;;;; Variables

(defvar zotero-browser-libraries-buffer-action
  '((display-buffer-reuse-window display-buffer-in-side-window) . ((side . left)
                                                                   (slot . -1)
                                                                   (window-width . 0.3)
                                                                   (window-height . fit-window-to-buffer)
                                                                   (preserve-size . (t . nil))
                                                                   (reusable-frames . nil))))

(defvar zotero-browser-collections-buffer-action
  '((display-buffer-reuse-window display-buffer-in-side-window) . ((side . left)
                                                                   (slot . 0)
                                                                   (window-width . 0.3)
                                                                   (preserve-size . (t . nil))
                                                                   (reusable-frames . nil))))

(defvar zotero-browser-items-buffer-action
  '((display-buffer-same-window) . ((window-width . 0.4)
                                    (preserve-size . (t . nil)))))

(defvar zotero-browser-edit-buffer-action
  '((display-buffer-reuse-window display-buffer-in-side-window) . ((side . right)
                                                                   (slot . 0)
                                                                   (window-width . 0.3)
                                                                   (preserve-size . (t . nil))
                                                                   (reusable-frames . nil))))

(defconst zotero-browser-base (file-name-directory load-file-name))

(defvar zotero-browser-padding 1
  "Set the number of characters preceding each entry")

(defvar zotero-browser-default-itemtypes nil
  "Default itemtypes when creating a new item.")

(defvar zotero-browser-default-linkmodes nil
  "Default linkmodes when creating a new item.")

(defvar-local zotero-browser-ewoc nil)

(defvar-local zotero-browser-type nil)

(defvar-local zotero-browser-id nil)

(defvar-local zotero-browser-resource nil)

(defvar-local zotero-browser-keys nil)

(defvar-local zotero-browser-collection nil)

(defvar-local zotero-browser-table nil)

(defvar-local zotero-browser-status nil)

;;;; Keymap

(defvar zotero-browser-libraries-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'zotero-browser-display)
    (define-key map (kbd "g") #'zotero-browser-revert)
    (define-key map (kbd "n") #'zotero-browser-goto-next)
    (define-key map (kbd "p") #'zotero-browser-goto-prev)
    (define-key map (kbd "C-c C-n") #'zotero-browser-next-collection)
    (define-key map (kbd "C-c C-p") #'zotero-browser-prev-collection)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Local keymap for `zotero-browser-libraries-mode'.")

(defvar zotero-browser-collections-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'zotero-browser-display)
    (define-key map (kbd "TAB") #'zotero-browser-toggle)
    (define-key map (kbd "<backtab>") #'zotero-browser-cycle)
    (define-key map (kbd "$") #'zotero-browser-expand-all)
    (define-key map (kbd "M-$") #'zotero-browser-collapse-all)
    (define-key map (kbd "n") #'zotero-browser-goto-next)
    (define-key map (kbd "p") #'zotero-browser-goto-prev)
    (define-key map (kbd "u") #'zotero-browser-goto-parent)
    (define-key map (kbd "C-c C-n") #'zotero-browser-next-collection)
    (define-key map (kbd "C-c C-p") #'zotero-browser-prev-collection)
    (define-key map (kbd "C-c C-u") #'zotero-browser-parent-collection)
    (define-key map (kbd "g") #'zotero-browser-revert)
    (define-key map (kbd "D") #'zotero-browser-delete)
    (define-key map (kbd "+") #'zotero-browser-create)
    (define-key map (kbd "e") #'zotero-browser-edit)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Local keymap for `zotero-browser-collections-mode'.")

(defvar zotero-browser-items-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'zotero-browser-open-attachment)
    (define-key map (kbd "TAB") #'zotero-browser-toggle)
    (define-key map (kbd "<backtab>") #'zotero-browser-cycle)
    (define-key map (kbd "$") #'zotero-browser-expand-all)
    (define-key map (kbd "M-$") #'zotero-browser-collapse-all)
    (define-key map (kbd "n") #'zotero-browser-goto-next)
    (define-key map (kbd "p") #'zotero-browser-goto-prev)
    (define-key map (kbd "u") #'zotero-browser-goto-parent)
    (define-key map (kbd "C-c C-n") #'zotero-browser-next-collection)
    (define-key map (kbd "C-c C-p") #'zotero-browser-prev-collection)
    (define-key map (kbd "C-c C-u") #'zotero-browser-parent-collection)
    (define-key map (kbd "g") #'zotero-browser-revert)
    (define-key map (kbd "D") #'zotero-browser-delete)
    (define-key map (kbd "R") #'zotero-browser-remove-from-collection)
    (define-key map (kbd "M") #'zotero-browser-move-to-parent)
    (define-key map (kbd "C") #'zotero-browser-copy-to-collection)
    (define-key map (kbd "+") #'zotero-browser-create)
    ;; (define-key map (kbd "+") #'zotero-browser-create-note)
    ;; (define-key map (kbd "+") #'zotero-browser-create-attachment)
    ;; (define-key map (kbd "+") #'zotero-browser-update-attachment)
    (define-key map (kbd "e") #'zotero-browser-edit)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Local keymap for `zotero-items-mode'.")

;;;; Menu

(easy-menu-define zotero-browser-items-mode-menu zotero-browser-items-mode-map
  "Menu for `zotero-browser-items-mode'."
  `("Zotero-Browser"
    ["Toggle" zotero-browser-toggle :help "Expand or collapse the children of the current item"]
    ["Cycle" zotero-browser-cycle :help "Cycle the visibility of children"]
    ["Expand all" zotero-browser-expand-all :help "Expand all children"]
    ["Collapse all" zotero-browser-collapse-all :help "Collapse all children"]
    "--"
    ["Edit" zotero-browser-edit :help "Edit current entry"]
    ["Create" zotero-browser-create :help "Create a new item"]
    ["Create note" zotero-browser-create-note :help "Create a new note."]
    ["Create attachment" zotero-browser-create-attachment :help "Create a new attachment"]
    ["Update attachment" zotero-browser-update-attachment :help "Update current attachment"]
    "--"
    ["Delete" zotero-browser-delete :help "Delete current entry or entries in active region"]
    ["Restore" zotero-browser-restore :help "Restore current entry or entries in active region"]
    ["Remove from collection" zotero-browser-remove-from-collection :help "Remove current entry from a collection"]
    ["Copy to collection" zotero-browser-copy-to-collection :help "Copy current entry to a collection"]
    ["Move to parent" zotero-browser-move-to-parent :help "Move current entry to a parent item"]
    "--"
    ["Open attachment" zotero-browser-open-attachment]
    "--"
    ["Revert" zotero-browser-revert]
    ["Quit" quit-window]
    ["Customize" (customize-group 'zotero-browser)]))

(easy-menu-define zotero-browser-collections-mode-menu zotero-browser-collections-mode-map
  "Menu for `zotero-browser-collections-mode'."
  `("Zotero-Browser"
    ["Toggle" zotero-browser-toggle :help "Expand or collapse the children of the current item"]
    ["Cycle" zotero-browser-cycle :help "Cycle the visibility of children"]
    ["Expand all" zotero-browser-expand-all :help "Expand all children"]
    ["Collapse all" zotero-browser-collapse-all :help "Collapse all children"]
    "--"
    ["Edit" zotero-browser-edit :help "Edit current entry"]
    ["Create" zotero-browser-create :help "Create a new collection"]
    "--"
    ["Revert" zotero-browser-revert]
    ["Quit" quit-window]
    ["Customize" (customize-group 'zotero-browser)]))

;;;; Customization

(defgroup zotero-browser nil
  "Interface to Zotero-Browser."
  :group 'external)

(defcustom zotero-browser-libraries-buffer-name "*Zotero Libraries*"
  "The default buffer name."
  :group 'zotero-browser
  :type 'string)

(defcustom zotero-browser-collections-buffer-name "*Zotero Collections*"
  "The default buffer name."
  :group 'zotero-browser
  :type 'string)

(defcustom zotero-browser-items-buffer-name "*Zotero Items*"
  "The default name of the items buffer."
  :group 'zotero-browser
  :type 'string)

(defcustom zotero-browser-default-collection-level 1
  "The default expansion level for collections."
  :group 'zotero-browser
  :type 'integer)

(defcustom zotero-browser-default-item-level 1
  "The default expansion level for items."
  :group 'zotero-browser
  :type 'integer)

(defcustom zotero-browser-collection-keys '(:name)
  "Keys to show in the collections browser"
  :group 'zotero-browser
  :type '(repeat (choice
                  (const :tag "Key" :key)
                  (const :tag "Version" :version)
                  (const :tag "Name" :name))))

(defcustom zotero-browser-item-keys '(itemtype title creators)
  "Keys to show in the items browser"
  :group 'zotero-browser
  :type
  '(repeat (choice
            (const :tag "Key" key)
            (const :tag "Version" version)
            (const :tag "Item Type" itemtype)
            (const :tag "Title" title)
            (const :tag "Creators" creators)
            (const :tag "Date" date)
            (const :tag "Year" year)
            (const :tag "Publisher" publisher)
            (const :tag "Publication Title" publication-title)
            (const :tag "Date Added" date-added)
            (const :tag "Date Modified" date-modified)
            (const :tag "Extra" extra)
            (const :tag "Note" note)
            (const :tag "Attachments" attachments)
            (const :tag "Notes" notes))))

(defcustom zotero-browser-preferred-application 'mailcap
  "Preferred application to open files."
  :group 'zotero-browser
  :type '(choice
          (const :tag "Emacs" emacs)
          (const :tag "External" external)
          (const :tag "Mailcap" mailcap)))

;;;; Mode

;;;###autoload
(define-derived-mode zotero-browser-libraries-mode special-mode "Zotero libraries"
  "Major mode for the Zotero browser.

All currently available key bindings:

\\{zotero-browser-libraries-mode-map}"
  ;; Turn on highlighting
  (font-lock-mode 1)
  ;; Turn on word wrap
  (visual-line-mode 1))

;;;###autoload
(define-derived-mode zotero-browser-collections-mode special-mode "Zotero collections"
  "Major mode for the Zotero browser.

All currently available key bindings:

\\{zotero-browser-collections-mode-map}"
  ;; Turn on highlighting
  (font-lock-mode 1)
  ;; Turn on word wrap
  (visual-line-mode 1))

;;;###autoload
(define-derived-mode zotero-browser-items-mode special-mode "Zotero items"
  "Major mode for the Zotero items browser.

All currently available key bindings:

\\{zotero-browser-items-mode-map}"
  ;; Turn on highlighting
  (font-lock-mode 1)
  ;; Turn on word wrap
  (visual-line-mode 1))

;;;; Commands

(defun zotero-browser ()
  "Create a new browser buffer, or switch."
  (interactive)
  (zotero-cache-maybe-initialize-cache)
  (let ((items-buffer (or (get-buffer zotero-browser-items-buffer-name)
                          (zotero-browser-items)))
        (libraries-buffer (or (get-buffer zotero-browser-libraries-buffer-name)
                              (zotero-browser-libraries)))
        (collections-buffer (or (get-buffer zotero-browser-collections-buffer-name)
                                (zotero-browser-collections))))
    (pop-to-buffer items-buffer zotero-browser-items-buffer-action)
    (display-buffer libraries-buffer zotero-browser-libraries-buffer-action)
    (display-buffer collections-buffer zotero-browser-collections-buffer-action)
    ;; Display the currently selected library
    (with-current-buffer libraries-buffer
      (zotero-browser-display))))

(defun zotero-browser-libraries ()
  "Create a libraries browser buffer."
  (let ((buffer (get-buffer-create "*Zotero Libraries*")))
    (with-current-buffer buffer
      (zotero-browser-libraries-mode)
      (let* ((table (zotero-cache-get :resource "libraries"))
             (user (ht-select (lambda (key value) (equal (plist-get value :type) "user")) table))
             (groups (ht-select (lambda (key value) (equal (plist-get value :type) "group")) table))
             (ewoc (ewoc-create #'zotero-browser--library-pp nil nil))
             (inhibit-read-only t))
        (erase-buffer)
        (setq zotero-browser-table table
              zotero-browser-ewoc ewoc)
        (thread-last user
          (ht-keys)
          (seq-do (lambda (key) (ewoc-enter-last ewoc key))))
        (thread-last groups
          (ht-keys)
          (seq-do (lambda (key) (ewoc-enter-last ewoc key))))))
    buffer))

(cl-defun zotero-browser-collections (&key type id resource)
  "Create a collections browser buffer."
  (let ((buffer (get-buffer-create zotero-browser-collections-buffer-name)))
    (with-current-buffer buffer
      (zotero-browser-collections-mode)
      (let ((ewoc (ewoc-create #'zotero-browser--collection-pp nil nil nil))
            (inhibit-read-only t))
        ;; Remove previous entries
        (erase-buffer)
        (when (and type id resource)
          (let* ((table (zotero-cache-get :type type :id id :resource resource))
                 (keys (zotero-cache-sort-by :name 'asc table)))
            (setq zotero-browser-ewoc ewoc
                  zotero-browser-type type
                  zotero-browser-resource resource
                  zotero-browser-id id
                  zotero-browser-keys keys)
            (seq-do (lambda (key) (ewoc-enter-last ewoc key)) keys)
            (zotero-browser-expand-level zotero-browser-default-collection-level)
            (zotero-browser-items :type type :id id :resource "items-top")))))
    buffer))

(cl-defun zotero-browser-items (&key type id resource key)
  "Create an items buffer."
  (let ((buffer (get-buffer-create zotero-browser-items-buffer-name)))
    (with-current-buffer buffer
      (zotero-browser-items-mode)
      (let ((ewoc (ewoc-create #'zotero-browser--item-pp nil nil nil))
            (inhibit-read-only t))
        ;; Remove previous entries
        (erase-buffer)
        (when (and type id resource)
          (let* ((table (zotero-cache-get :type type :id id :resource resource :key key))
                 (keys (zotero-cache-sort-by :date 'asc table)))
            (setq zotero-browser-ewoc ewoc
                  zotero-browser-type type
                  zotero-browser-id id
                  zotero-browser-resource resource
                  zotero-browser-collection key
                  zotero-browser-keys keys)
            (seq-do (lambda (key) (ewoc-enter-last ewoc key)) keys)
            (zotero-browser-expand-level zotero-browser-default-item-level)))))
    buffer))

(defun zotero-browser-ensure-browser-buffer ()
  (unless (or (eq major-mode 'zotero-browser-libraries-mode)
              (eq major-mode 'zotero-browser-collections-mode)
              (eq major-mode 'zotero-browser-items-mode))
    (error "Current buffer is not a Zotero browser buffer")))

(defun zotero-browser-display ()
  "Display current library or collection."
  (interactive)
  (zotero-browser-ensure-browser-buffer)
  (pcase major-mode
    ('zotero-browser-libraries-mode
     (let* ((node (ewoc-locate zotero-browser-ewoc))
            (key (ewoc-data node))
            (table zotero-browser-table)
            (type (plist-get (ht-get table key) :type))
            (id (plist-get (ht-get table key) :id)))
       (setq zotero-browser-type type
             zotero-browser-id id)
       (display-buffer (zotero-browser-collections :type type :id id :resource "collections-top"))
       (display-buffer (zotero-browser-items :type type :id id :resource "items-top"))))
    ('zotero-browser-collections-mode
     (let* ((ewoc zotero-browser-ewoc)
            (node (ewoc-locate ewoc))
            (key (ewoc-data node))
            (type zotero-browser-type)
            (id zotero-browser-id)
            (table (zotero-cache-get :type type :id id :resource "collections")))
       (display-buffer (zotero-browser-items :type type :id id :resource "collection-items" :key key))))
    ('zotero-browser-items-mode
     (let* ((ewoc zotero-browser-ewoc)
            (node (ewoc-locate ewoc))
            (key (ewoc-data node))
            (type zotero-browser-type)
            (id zotero-browser-id)
            (entry (zotero-cache-get :type type :id id :resource "item" :key key))
            (data (zotero-lib-plist-get* entry :object :data)))
       (display-buffer (zotero-edit-item :type type :id id :data data :locale zotero-lib-locale) zotero-browser-edit-buffer-action)))))

(defun zotero-browser-revert ()
  "Revert the buffer."
  (interactive)
  (zotero-browser-ensure-browser-buffer)
  (let ((pos (point))
        (ewoc zotero-browser-ewoc))
    (ewoc-refresh ewoc)
    (goto-char pos)))

(defun zotero-browser-goto-next ()
  "Move point to the next item."
  (interactive)
  (zotero-browser-ensure-browser-buffer)
  (let ((ewoc zotero-browser-ewoc))
    (when (ewoc-nth ewoc 0)
      (ewoc-goto-next ewoc 1)
      (zotero-browser-display))))

(defun zotero-browser-goto-prev ()
  "Move point to the previous item."
  (interactive)
  (zotero-browser-ensure-browser-buffer)
  (let ((ewoc zotero-browser-ewoc))
    (when (ewoc-nth ewoc 0)
      (ewoc-goto-prev ewoc 1)
      (zotero-browser-display))))

(defun zotero-browser-goto-parent ()
  "Move point to the parent item."
  (interactive)
  (zotero-browser-ensure-browser-buffer)
  (let ((ewoc zotero-browser-ewoc))
    (when (ewoc-nth ewoc 0)
      (let* ((node (ewoc-locate ewoc))
             (key (ewoc-data node))
             (parent (zotero-browser--parent key))
             (n 0))
        (unless (or (null parent)
                    (eq parent :json-false))
          (while
              (let* ((node (ewoc-nth ewoc n))
                     (key (ewoc-data node)))
                (ewoc-goto-node ewoc node)
                (prog1
                    (not (equal key parent))
                  (setq n (1+ n)))))
          (zotero-browser-display))))))

(defun zotero-browser-next-collection ()
  "Move point to the next collection."
  (interactive)
  (zotero-browser-ensure-browser-buffer)
  (let ((buffer (current-buffer)))
    (pop-to-buffer zotero-browser-collections-buffer-name)
    (zotero-browser-goto-next)
    (pop-to-buffer buffer)))

(defun zotero-browser-prev-collection ()
  "Move point to the previous collection."
  (interactive)
  (zotero-browser-ensure-browser-buffer)
  (let ((buffer (current-buffer)))
    (pop-to-buffer zotero-browser-collections-buffer-name)
    (zotero-browser-goto-prev)
    (pop-to-buffer buffer)))

(defun zotero-browser-parent-collection ()
  "Move point to the parent collection."
  (interactive)
  (zotero-browser-ensure-browser-buffer)
  (let ((buffer (current-buffer)))
    (pop-to-buffer zotero-browser-collections-buffer-name)
    (zotero-browser-goto-parent)
    (pop-to-buffer buffer)))

(defun zotero-browser-all-items ()
  "Show all items."
  (interactive)
  (zotero-browser-ensure-browser-buffer)
  (zotero-browser-items :type zotero-browser-type :id zotero-browser-id :resource "items"))

(defun zotero-browser-unfiled-items ()
  "Show unfiled items."
  (interactive)
  (zotero-browser-ensure-browser-buffer)
  (zotero-browser-items :type zotero-browser-type :id zotero-browser-id :resource "items-top"))

(defun zotero-browser-trash-items ()
  "Show trashed items."
  (interactive)
  (zotero-browser-ensure-browser-buffer)
  (zotero-browser-items :type zotero-browser-type :id zotero-browser-id :resource "trash-items"))

(defun zotero-browser-toggle ()
  "Expand or collapse the children of the current item."
  (interactive)
  (zotero-browser-ensure-browser-buffer)
  (let* ((inhibit-read-only t)
         (ewoc zotero-browser-ewoc)
         (node (ewoc-locate ewoc)))
    (when (zotero-browser--has-children-p node)
      (if (zotero-browser--expanded-p ewoc node)
          (progn
            (zotero-browser--collapse ewoc node))
        (zotero-browser--expand ewoc node)))))

(defun zotero-browser-cycle ()
  "Cycle the visibility of children."
  (interactive)
  (zotero-browser-ensure-browser-buffer)
  ;; Only cycle if the ewoc is not empty
  (when (ewoc-nth zotero-browser-ewoc 0)
    (cond
     ((and (eq last-command this-command)
	   (eq zotero-browser-status 'collapsed))
      (zotero-browser-expand-all)
      (setq zotero-browser-status 'expanded))
     ((and (eq last-command this-command)
	   (eq zotero-browser-status 'expanded))
      (zotero-browser-collapse-all)
      (setq zotero-browser-status 'collapsed))
     (t
      (zotero-browser-expand-all)
      (setq zotero-browser-status 'expanded)))))

(defun zotero-browser-expand-all ()
  "Expand all children."
  (interactive)
  (zotero-browser-ensure-browser-buffer)
  (let ((ewoc zotero-browser-ewoc)
        (inhibit-read-only t))
    (save-excursion
      (when-let ((node (ewoc-nth ewoc 0)))
        (ewoc-goto-node ewoc node)
        (while
            (let ((node (ewoc-locate ewoc)))
              (when (and (zotero-browser--has-children-p node)
                         (not (zotero-browser--expanded-p ewoc node)))
                (zotero-browser--expand ewoc node))
              (prog1
                  ;; End-test of while loop
                  (ewoc-next ewoc node)
                (ewoc-goto-next ewoc 1))))))))

(defun zotero-browser-collapse-all ()
  "Collapse all children."
  (interactive)
  (zotero-browser-ensure-browser-buffer)
  (let ((ewoc zotero-browser-ewoc)
        (inhibit-read-only t))
    (save-excursion
      (when-let ((node (ewoc-nth ewoc 0)))
        (ewoc-goto-node ewoc node)
        (while
            (let ((node (ewoc-locate ewoc)))
              (when (and (zotero-browser--has-children-p node)
                         (zotero-browser--expanded-p ewoc node))
                (zotero-browser--collapse ewoc node))
              (prog1
                  (ewoc-next ewoc node)
                (ewoc-goto-next ewoc 1))))))))

(defun zotero-browser-expand-level (&optional num)
  "Expand children till LEVEL."
  (interactive "P")
  (zotero-browser-ensure-browser-buffer)
  (let* ((ewoc zotero-browser-ewoc)
         (type zotero-browser-type)
         (id zotero-browser-id)
         (table (pcase major-mode
                  ('zotero-browser-collections-mode
                   (zotero-cache-get :type type :id id :resource "collections"))
                  ('zotero-browser-items-mode
                   (zotero-cache-get :type type :id id :resource "items"))))
         (num (or num 1))
         (inhibit-read-only t))
    (save-excursion
      (when-let ((node (ewoc-nth ewoc 0)))
        (ewoc-goto-node ewoc node)
        (while
            (let* ((node (ewoc-locate ewoc))
                   (key (ewoc-data node))
                   (level (zotero-cache-level key table)))
              (when (zotero-browser--has-children-p node)
                (cond
                 ((and (zotero-browser--expanded-p ewoc node)
                       (>= level num))
                  (zotero-browser--collapse ewoc node))
                 ((and (not (zotero-browser--expanded-p ewoc node))
                       (or (< level num) (eq num 0)))
                  (zotero-browser--expand ewoc node))
                 (t
                  (zotero-browser--prefix "▸" (ewoc-location node)))))
              (prog1
                  ;; End-test of while loop
                  (ewoc-next ewoc node)
                (ewoc-goto-next ewoc 1))))))))

(defun zotero-browser-edit ()
  "Edit current entry."
  (interactive)
  (zotero-browser-ensure-browser-buffer)
  (let* ((type zotero-browser-type)
         (id zotero-browser-id)
         (library (zotero-cache-get :id id :resource "library")))
    (if (zotero-cache-write-access-p library)
        (pcase major-mode
          ('zotero-browser-collections-mode
           (let* ((ewoc zotero-browser-ewoc)
                  (node (ewoc-locate ewoc))
                  (key (ewoc-data node))
                  (entry (zotero-cache-get :type type :id id :resource "collection" :key key))
                  (data (zotero-lib-plist-get* entry :object :data)))
             (pop-to-buffer (zotero-edit-collection :type type :id id :data data :locale zotero-lib-locale) zotero-browser-edit-buffer-action)))
          ('zotero-browser-items-mode
           (let* ((ewoc zotero-browser-ewoc)
                  (node (ewoc-locate ewoc))
                  (key (ewoc-data node))
                  (entry (zotero-cache-get :type type :id id :resource "item" :key key))
                  (data (zotero-lib-plist-get* entry :object :data)))
             (pop-to-buffer (zotero-edit-item :type type :id id :data data :locale zotero-lib-locale) zotero-browser-edit-buffer-action))))
      (user-error "Library %s had no write access" id))))

(defun zotero-browser-create ()
  "Create a new collection or item."
  (interactive)
  (zotero-browser-ensure-browser-buffer)
  (let* ((type zotero-browser-type)
         (id zotero-browser-id)
         (library (zotero-cache-get :resource "library" :id id)))
    (if (zotero-cache-write-access-p library)
        (pcase major-mode
          ('zotero-browser-libraries-mode
           ;; TODO
           )
          ('zotero-browser-collections-mode
           (display-buffer (zotero-edit-create-collection :type type :id id) zotero-browser-edit-buffer-action))
          ('zotero-browser-items-mode
           (let ((itemtype (completing-read "Select an item type: " (zotero-cache-itemtypes) nil t nil nil zotero-browser-default-itemtypes )))
             (cl-pushnew itemtype zotero-browser-default-itemtypes :test #'equal)
             (display-buffer (zotero-edit-create-item :type type :id id :itemtype itemtype :locale zotero-lib-locale) zotero-browser-edit-buffer-action))))
      (user-error "Library %s had no write access" id))))

(defun zotero-browser-move-to-parent (&optional arg)
  "Move current entry to a parent item.
With a `C-u' prefix, move to top level."
  (interactive "P")
  (zotero-browser-ensure-browser-buffer)
  (let* ((type zotero-browser-type)
         (id zotero-browser-id)
         (library (zotero-cache-get :resource "library" :id id)))
    (if (zotero-cache-write-access-p library)
        (when (eq major-mode 'zotero-browser-items-mode)
          (let* ((inhibit-read-only t)
                 (node (ewoc-locate zotero-browser-ewoc))
                 (key (ewoc-data node))
                 (entry (zotero-cache-get :type type :id id :resource "item" :key key))
                 (data (zotero-lib-plist-get* entry :object :data))
                 (itemtype (zotero-lib-plist-get* entry :object :data :itemType))
                 updated-data)
            (unless (or (equal itemtype "attachment") (equal itemtype "note"))
              (user-error "Item type %s cannot be moved to a parent"))
            (if (equal arg '(4))
                (setq updated-data (zotero-lib-plist-delete data :parentItem))
              (let* ((table (zotero-cache-get :type type :id id :resource "items"))
                     (choices (zotero-cache-field :title table))
                     (name (completing-read "Select parent item:" choices nil t))
                     (parent (cdr (assoc name choices)))
                     (entry (ht-get table parent))
                     (itemtype (zotero-lib-plist-get* entry :object :data :itemType)))
                (if (or (equal itemtype "attachment") (equal itemtype "note"))
                    (user-error "Parent item cannot be a note or attachment")
                  (setq updated-data (plist-put data :parentItem parent)))))
            (delete key zotero-browser-keys)
            (ewoc-delete ewoc node)
            (zotero-cache-save :type type :id id :resource "items" :data updated-data)))
      (user-error "Library %s had no write access" id))))

(defun zotero-browser-move-to-collection ()
  "Move current entry to a collection."
  (interactive)
  (zotero-browser-ensure-browser-buffer)
  (let* ((ewoc zotero-browser-ewoc)
         (type zotero-browser-type)
         (id zotero-browser-id)
         (library (zotero-cache-get :resource "library" :id id)))
    (if (zotero-cache-write-access-p library)
        (when (eq major-mode 'zotero-browser-items-mode)
          (let* ((inhibit-read-only t)
                 (node (ewoc-locate ewoc))
                 (key (ewoc-data node))
                 (table (zotero-cache-get :type type :id id :resource "collections"))
                 (choices (zotero-cache-field :name table))
                 (name (completing-read "Select collection:" choices nil t))
                 (new (cdr (assoc name choices)))
                 (old zotero-browser-collection))
            (delete key zotero-browser-keys)
            (ewoc-delete ewoc node)
            (zotero-cache-substitute-collection :type type :id id :key key :new new :old old)))
      (user-error "Library %s had no write access" id))))

(defun zotero-browser-copy-to-collection ()
  "Copy current entry to a collection."
  (interactive)
  (zotero-browser-ensure-browser-buffer)
  (let* ((ewoc zotero-browser-ewoc)
         (type zotero-browser-type)
         (id zotero-browser-id)
         (resource zotero-browser-resource)
         (library (zotero-cache-get :resource "library" :id id)))
    (if (zotero-cache-write-access-p library)
        (when (eq major-mode 'zotero-browser-items-mode)
          (let* ((inhibit-read-only t)
                 (node (ewoc-locate ewoc))
                 (key (ewoc-data node))
                 (table (zotero-cache-get :type type :id id :resource "collections"))
                 (choices (zotero-cache-field :name table))
                 (name (completing-read "Select collection:" choices nil t))
                 (collection (cdr (assoc name choices))))
            (when (equal resource "items-top")
              (delete key zotero-browser-keys)
              (ewoc-delete ewoc node))
            (zotero-cache-add-to-collection :type type :id id :key key :collection collection)))
      (user-error "Library %s had no write access" id))))

(defun zotero-browser-remove-from-collection ()
  "Remove current entry from a collection."
  (interactive)
  (zotero-browser-ensure-browser-buffer)
  (let* ((ewoc zotero-browser-ewoc)
         (type zotero-browser-type)
         (id zotero-browser-id)
         (library (zotero-cache-get :resource "library" :id id)))
    (if (zotero-cache-write-access-p library)
        (when (eq major-mode 'zotero-browser-items-mode)
          (let* ((inhibit-read-only t)
                 (node (ewoc-locate ewoc))
                 (key (ewoc-data node))
                 (collection zotero-browser-collection))
            (delete key zotero-browser-keys)
            (ewoc-delete ewoc node)
            (zotero-cache-remove-from-collection :type type :id id :key key :collection collection)))
      (user-error "Library %s had no write access" id))))

(defun zotero-browser-move-to-trash ()
  "Move current entry to trash.
If region is active, trash entries in active region instead."
  (interactive)
  (let* ((ewoc zotero-browser-ewoc)
         (type zotero-browser-type)
         (id zotero-browser-id)
         (resource (pcase major-mode
                     ('zotero-browser-collections-mode "collections")
                     ('zotero-browser-items-mode "items")))
         (library (zotero-cache-get :resource "library" :id id)))
    (if (zotero-cache-write-access-p library)
        (let ((inhibit-read-only t)
              (keys (zotero-browser--keys ewoc)))
          (dolist (key keys)
            (delete key zotero-browser-keys)
            (ewoc-delete ewoc node)
            (zotero-cache-trash :type type :id id :resource resource :key key)))
      (user-error "Library %s had no write access" id))))

(defun zotero-browser-restore-from-trash ()
  "Restore current entry from trash.
If region is active, restore entries in active region instead."
  (interactive)
  (let* ((ewoc zotero-browser-ewoc)
         (type zotero-browser-type)
         (id zotero-browser-id)
         (resource (pcase major-mode
                     ('zotero-browser-collections-mode "collections")
                     ('zotero-browser-items-mode "items")))
         (library (zotero-cache-get :resource "library" :id id)))
    (if (zotero-cache-write-access-p library)
        (let ((inhibit-read-only t)
              (keys (zotero-browser--keys ewoc)))
          (dolist (key keys)
            (delete key zotero-browser-keys)
            (ewoc-delete ewoc node)
            (zotero-cache-restore :type type :id id :resource resource :key key)))
      (user-error "Library %s had no write access" id))))

(defun zotero-browser-delete ()
  "Delete current entry.
If region is active, delete entries in active region instead."
  (interactive)
  (let* ((ewoc zotero-browser-ewoc)
         (type zotero-browser-type)
         (id zotero-browser-id)
         (resource (pcase major-mode
                     ('zotero-browser-collections-mode "collections")
                     ('zotero-browser-items-mode "items")))
         (library (zotero-cache-get :resource "library" :id id)))
    (if (zotero-cache-write-access-p library)
        (let ((inhibit-read-only t)
              (keys (zotero-browser--keys ewoc)))
          (dolist (key keys)
            (delete key zotero-browser-keys)
            (ewoc-delete ewoc node)
            (zotero-cache-delete :type type :id id :resource resource :key key)))
      (user-error "Library %s had no write access" id))))

(defun zotero-browser--keys (ewoc)
  "Return a list with the key of the current entry.
If region is active, return a list of the keys in the active region instead."
  (if (use-region-p)
      (let ((nodes (zotero-browser--region-nodes (region-beginning) (region-end) ewoc)))
        (seq-map (lambda (node) (ewoc-data node)) nodes))
    (let ((node (ewoc-locate ewoc)))
      (list (ewoc-data node)))))

(defun zotero-browser--region-nodes (beg end ewoc)
  "Return nodes of EWOC in region."
  (save-excursion
    (let* ((first-node (ewoc-locate ewoc beg))
           (last-node (ewoc-locate ewoc end))
           (node first-node)
           nodes)
      (while
          (progn
            (ewoc-goto-node ewoc node)
            (push node nodes)
            (setq node (ewoc-next ewoc node))
            (not (eq node (ewoc-next ewoc last-node)))))
      nodes)))

(defun zotero-browser--node-keys (nodes)
  "Return keys of NODES."
  (seq-map (lambda (node) (ewoc-data node)) nodes))

(defun zotero-browser-open-attachment ()
  "Open attachment at point."
  (interactive)
  (zotero-browser-ensure-browser-buffer)
  (let* ((ewoc zotero-browser-ewoc)
         (type zotero-browser-type)
         (id zotero-browser-id)
         (key (ewoc-data (ewoc-locate ewoc)))
         (entry (zotero-cache-get :type type :id id :resource "item" :key key))
         (itemtype (zotero-lib-plist-get* entry :object :data :itemType)))
    (when (equal itemtype "attachment")
      (let ((linkmode (zotero-lib-plist-get* entry :object :data :linkMode)))
        (pcase linkmode
          ("imported_file" (zotero-browser-open-imported-file entry))
          ("imported_url" (zotero-browser-open-imported-url entry))
          ("linked_file" (zotero-browser-open-linked-file entry))
          ("linked_url" (zotero-browser-open-linked-url entry)))))))

(defun zotero-browser-find-attachment ()
  "Return the path of attachment at point."
  (zotero-browser-ensure-browser-buffer)
  (when-let ((ewoc zotero-browser-ewoc)
             (type zotero-browser-type)
             (id zotero-browser-id)
             (key (ewoc-data (ewoc-locate ewoc)))
             (entry (zotero-cache-get :type type :id id :resource "item" :key key))
             (filename (zotero-lib-plist-get* entry :object :data :filename))
             (dir (concat (file-name-as-directory zotero-cache-storage-dir) key))
             (file (concat (file-name-as-directory dir) filename)))
    (cond
     ;; If the file exists, return path
     ((file-exists-p file)
      file)
     ;; If storage is enabled, download to storage
     (zotero-cache-enable-storage
      (zotero-browser-download-attachment))
     ;; If storage is disabled, download to temp directory
     (t
      (zotero-browser-download-attachment temporary-file-directory)))))

(defun zotero-browser-download-attachment (&optional dir)
  "Download the attachment at point."
  (zotero-browser-ensure-browser-buffer)
  (let* ((ewoc zotero-browser-ewoc)
         (key (ewoc-data (ewoc-locate ewoc)))
         (entry (zotero-cache-get :type type :id id :resource "item" :key key))
         (filename (zotero-lib-plist-get* entry :object :data :filename))
         (dir (or dir (concat (file-name-as-directory zotero-cache-storage-dir) key)))
         (type zotero-browser-type)
         (id zotero-browser-id)
         (token (zotero-auth-token))
         (api-key (zotero-auth-api-key token)))
    (unless (file-exists-p dir)
      (make-directory dir t))
    (zotero-lib-download-file :file filename :dir dir :type type :id id :key key :api-key api-key)))

(defun zotero-browser-create-note (&optional arg)
  "Create a new note.
With a `C-u' prefix, create a new top level note."
  (interactive "P")
  (zotero-browser-ensure-browser-buffer)
  (let* ((type zotero-browser-type)
         (id zotero-browser-id)
         (library (zotero-cache-get :resource "library" :id id)))
    (if (zotero-cache-write-access-p library)
        (when (eq major-mode 'zotero-browser-items-mode)
          (let* ((ewoc zotero-browser-ewoc)
                 (node (ewoc-locate ewoc))
                 (key (ewoc-data node))
                 (parent (if (equal arg '(4)) :json-false key)))
            (display-buffer (zotero-edit-create-note :type type :id id :parent parent :itemtype "note" :locale zotero-lib-locale) zotero-browser-edit-buffer-action)))
      (user-error "Library %s had no write access" id))))

(defun zotero-browser-create-attachment (&optional arg)
  "Create a new attachment with the current entry as parent.
With a `C-u' prefix, create a new top level attachment.

Only file attachments (imported_file/linked_file) and PDF
imported web attachments (imported_url with content type
application/pdf) are allowed as top-level items, as in the Zotero
client."
  (interactive "P")
  (zotero-browser-ensure-browser-buffer)
  (let* ((type zotero-browser-type)
         (id zotero-browser-id)
         (library (zotero-cache-get :resource "library" :id id)))
    (when (eq major-mode 'zotero-browser-items-mode)
      (if (zotero-cache-write-access-p library)
          (let* ((ewoc zotero-browser-ewoc)
                 (node (ewoc-locate ewoc))
                 (key (ewoc-data node))
                 ;; Top-level attachments can be created by excluding the
                 ;; parentItem property or setting it to false.
                 (parent (if (equal arg '(4)) :json-false key))
                 (linkmode (completing-read "Select a linkmode: " (zotero-lib-attachment-linkmodes) nil t nil nil zotero-browser-default-linkmodes))
                 (template (copy-tree (zotero-cache-attachment-template linkmode))))
            (cl-pushnew linkmode zotero-browser-default-linkmodes :test #'equal)
            (pcase linkmode
              ("imported_file"
               (let* ((file (expand-file-name (read-file-name "Select file: " nil nil t)))
                      (attributes (zotero-lib-file-attributes file))
                      (filename (file-name-nondirectory file))
                      (filesize (plist-get attributes :filesize))
                      (content-type (plist-get attributes :content-type))
                      (md5 (plist-get attributes :md5))
                      (mtime (plist-get attributes :mtime))
                      (accessdate (plist-get attributes :accessdate))
                      (data (thread-first template
                              (plist-put :parentItem parent)
                              (plist-put :title filename)
                              (plist-put :accessDate accessdate)
                              (plist-put :contentType content-type)
                              ;; (plist-put :charset charset) ; charset cannot be determined without external tools
                              (plist-put :filename filename)
                              ;; md5 and mtime can be edited directly in
                              ;; personal libraries for WebDAV-based file
                              ;; syncing. They should not be edited directly
                              ;; when using Zotero File Storage, which provides
                              ;; an atomic method for setting the properties
                              ;; along with the corresponding file.
                              (plist-put :md5 nil)
                              (plist-put :mtime nil))))
                 (when-let ((object (zotero-cache-save :type type :id id :resource "items" :data data))
                            (key (plist-get object :key))
                            (token (zotero-auth-token))
                            (api-key (zotero-auth-api-key token)))
                   (unless (zotero-lib-upload-attachment :type type :id id :key key :file file :api-key api-key)
                     (error "Failed to associate attachment with item %s" key))
                   (display-buffer (zotero-edit-item :type type :id id :data data :locale zotero-lib-locale) zotero-browser-edit-buffer-action))))
              ("imported_url"
               (user-error "Creating a snapshot is not supported"))
              ("linked_file"
               (unless (equal zotero-browser-type "user")
                 (user-error "Linked files can only be added to user library"))
               (let* ((file (expand-file-name (read-file-name "Select file: " nil nil t)))
                      (attributes (zotero-lib-file-attributes file))
                      (filename (file-name-nondirectory file))
                      (content-type (plist-get attributes :content-type))
                      (accessdate (plist-get attributes :accessdate))
                      (data (thread-first template
                              (plist-put :parentItem parent)
                              (plist-put :title filename)
                              (plist-put :accessDate accessdate)
                              (plist-put :contentType content-type)
                              ;; (plist-put :charset charset) ; charset cannot be determined without external tools
                              (plist-put :path file))))
                 (when-let ((object (zotero-cache-save :type type :id id :resource "items" :data data))
                            (key (plist-get object :key))
                            (token (zotero-auth-token))
                            (api-key (zotero-auth-api-key token)))
                   (display-buffer (zotero-edit-item :type type :id id :data (plist-get object :data) :locale zotero-lib-locale) zotero-browser-edit-buffer-action))))
              ("linked_url"
               (if (or (null parent) (eq parent :json-false))
                   (user-error "Links to URLs are not allowed as top-level items")
                 (display-buffer (zotero-edit-item :type type :id id :data template :locale zotero-lib-locale) zotero-browser-edit-buffer-action)))))
        (user-error "Library %s had no write access" id)))))

(defun zotero-browser-update-attachment ()
  "Update attachment of the current entry."
  (interactive)
  (zotero-browser-ensure-browser-buffer)
  (let* ((type zotero-browser-type)
         (id zotero-browser-id)
         (library (zotero-cache-get :resource "library" :id id)))
    (when (eq major-mode 'zotero-browser-items-mode)
      (if (zotero-cache-write-access-p library)
          (let* ((ewoc zotero-browser-ewoc)
                 (node (ewoc-locate ewoc))
                 (key (ewoc-data node))
                 (entry (zotero-cache-get :type type :id id :resource "item" :key key))
                 (data (zotero-lib-plist-get* entry :object :data))
                 (itemtype (zotero-lib-plist-get* entry :object :data :itemType))
                 (linkmode (zotero-lib-plist-get* entry :object :data :linkMode))
                 (filename (zotero-lib-plist-get* entry :object :data :filename))
                 (dir (concat (file-name-as-directory zotero-cache-storage-dir) key))
                 (path (concat (file-name-as-directory dir) filename))
                 (token (zotero-auth-token))
                 (api-key (zotero-auth-api-key token))
                 (hash (zotero-lib-get-file-hash :type type :id id :key key :api-key api-key)))
            (when (and (equal itemtype "attachment")
                       (equal linkmode "imported_file"))
              (let* ((file (expand-file-name (read-file-name "Select file: " dir nil t path)))
                     (attributes (zotero-lib-file-attributes file))
                     (filename (file-name-nondirectory file))
                     (filesize (plist-get attributes :filesize))
                     (content-type (plist-get attributes :content-type))
                     (md5 (plist-get attributes :md5))
                     (mtime (plist-get attributes :mtime))
                     (accessdate (plist-get attributes :accessdate))
                     (data (thread-first data
                             (plist-put :title filename)
                             (plist-put :accessDate accessdate)
                             (plist-put :contentType content-type)
                             ;; (plist-put :charset charset) ; charset cannot be determined without external tools
                             (plist-put :filename filename)
                             ;; md5 and mtime can be edited directly in
                             ;; personal libraries for WebDAV-based file
                             ;; syncing. They should not be edited directly
                             ;; when using Zotero File Storage, which provides
                             ;; an atomic method for setting the properties
                             ;; along with the corresponding file.
                             (plist-put :md5 nil)
                             (plist-put :mtime nil))))
                (unless (zotero-lib-upload-attachment :type type :id id :key key :file file :hash hash :api-key api-key)
                  (error "Failed to associate attachment with item %s" key))
                (display-buffer (zotero-edit-item :type type :id id :data data :locale zotero-lib-locale) zotero-browser-edit-buffer-action))))
        (user-error "Library %s had no write access" id)))))

(defun zotero-browser-recognize-attachment ()
  "Recognize content of the current entry."
  (interactive)
  (zotero-browser-ensure-browser-buffer)
  (let* ((type zotero-browser-type)
         (id zotero-browser-id)
         (library (zotero-cache-get :resource "library" :id id)))
    (when (eq major-mode 'zotero-browser-items-mode)
      (if (zotero-cache-write-access-p library)
          (let* ((ewoc zotero-browser-ewoc)
                 (node (ewoc-locate ewoc))
                 (key (ewoc-data node))
                 (entry (zotero-cache-get :type type :id id :resource "item" :key key))
                 (itemtype (zotero-lib-plist-get* entry :object :data :itemType))
                 (linkmode (zotero-lib-plist-get* entry :object :data :linkMode))
                 (filename (zotero-lib-plist-get* entry :object :data :filename)))
            (when (and (equal itemtype "attachment")
                       (equal linkmode "imported_file"))
              (let* ((file (zotero-browser-find-attachment))
                     (attributes (zotero-lib-file-attributes file))
                     (content-type (plist-get attributes :content-type)))
                (zotero-recognize file))))
        (user-error "Library %s had no write access" id)))))

(defun zotero-browser-set-fulltext ()
  "Set the full-text content of the current entry."
  (interactive)
  (zotero-browser-ensure-browser-buffer)
  (let* ((type zotero-browser-type)
         (id zotero-browser-id)
         (library (zotero-cache-get :resource "library" :id id)))
    (when (eq major-mode 'zotero-browser-items-mode)
      (if (zotero-cache-write-access-p library)
          (let* ((ewoc zotero-browser-ewoc)
                 (node (ewoc-locate ewoc))
                 (key (ewoc-data node))
                 (entry (zotero-cache-get :type type :id id :resource "item" :key key))
                 (itemtype (zotero-lib-plist-get* entry :object :data :itemType))
                 (linkmode (zotero-lib-plist-get* entry :object :data :linkMode))
                 (filename (zotero-lib-plist-get* entry :object :data :filename)))
            (when (and (equal itemtype "attachment")
                       (equal linkmode "imported_file"))
              (let* ((file (zotero-browser-find-attachment))
                     (attributes (zotero-lib-file-attributes file))
                     (content-type (plist-get attributes :content-type)))
                (zotero-fulltext-index-fulltext :type type :id id :key key :file file :content-type content-type))))
        (user-error "Library %s had no write access" id)))))

(defun zotero-browser-open-file (path)
  "Open the file at PATH.

The preferred method of opening is customizable by setting the
variable `zotero-browser-preferred-application'. If no
application is found, Emacs simply visits the file."
  (let* ((file (expand-file-name path)))
    (pcase zotero-browser-preferred-application
      ('emacs
       (find-file-other-frame file))
      ('external
       (zotero-browser-open-externally file))
      ('mailcap
       (mailcap-parse-mailcaps)
       (let* ((mime-type (mailcap-extension-to-mime (file-name-extension file)))
	      (command (mailcap-mime-info mime-type))
              cmd)
         (if (stringp command)
	     (setq cmd command)
	   (setq cmd 'emacs))
         (cond
          ((and (stringp cmd) (not (string-match "^[[:space:]]*$" cmd)))
           ;; Remove quotes around the file name - we'll use shell-quote-argument.
           (while (string-match "['\"]%s['\"]" cmd)
             (setq cmd (replace-match "%s" t t cmd)))
           (setq cmd (replace-regexp-in-string
	              "%s"
	              (shell-quote-argument file)
	              cmd
	              nil t))
           (start-process-shell-command cmd nil cmd))
          ((or (stringp cmd)
	       (eq cmd 'emacs))
           (find-file-other-frame file))))))))

(defun zotero-browser-open-externally (path)
  "Open PATH in an external program.

This function is intented for graphical desktop environments on GNU/Linux, macOS, or Microsoft Windows."
  (pcase system-type
    ('cygwin
     (start-process-shell-command "zotero-browser-open-externally" nil (concat "cygstart" " " (shell-quote-argument path))))
    ('darwin
     (start-process-shell-command "zotero-browser-open-externally" nil (concat "open" " " (shell-quote-argument path))))
    ('gnu/linux
     (start-process-shell-command "zotero-browser-open-externally" nil (concat "xdg-open" " " (shell-quote-argument path))))
    ('windows-nt
     (w32-shell-execute "open" path))
    (system
     (error "Unable to determine default application on operating system %S."))))

(defun zotero-browser-open-imported-file (entry)
  (let ((path (expand-file-name (zotero-browser-find-attachment))))
    (zotero-browser-open-file path)))

(defun zotero-browser-open-imported-url (entry)
  (let ((path (expand-file-name (zotero-browser-find-attachment)))
        (contenttype (zotero-lib-plist-get* entry :object :data :contentType))
        (key (zotero-lib-plist-get* entry :object :data :key))
        (filename (zotero-lib-plist-get* entry :object :data :filename))
        (dir (concat temporary-file-directory key)))
    (if (equal contenttype "application/pdf")
        (zotero-browser-open-file path)
      (let* ((unzip (or (executable-find "unzip")
                        (error "Unable to find executable \"unzip\"")))
             (exit-status (call-process unzip nil nil nil "-o" "-d" dir path)))
        (if (eq exit-status 0)
            (let ((path (concat (file-name-as-directory dir) filename)))
              (browse-url-file-url path))
          (error "Error extracting snapshot"))))))

(defun zotero-browser-open-linked-file (entry)
  (let ((path (zotero-lib-plist-get* entry :object :data :path)))
    (zotero-browser-open-file path)))

(defun zotero-browser-open-linked-url (entry)
  (let ((url (zotero-lib-plist-get* entry :object :data :url)))
    (browse-url url)))

(defun zotero-browser--library-pp (key)
  "Pretty print KEY."
  (let* ((table (zotero-cache-get :resource "libraries"))
         (value (ht-get table key))
         (id (zotero-lib-plist-get* value :id))
         (type (zotero-lib-plist-get* value :type))
         (name (pcase type
                 ("group"
                  (let* ((group (zotero-cache-get :id id :resource "group"))
                         (name (zotero-lib-plist-get* group :data :name)))
                    name))
                 ("user" "User library")))
         (icon "treesource-library.png")
         (dir (file-name-as-directory "img"))
         (filename (expand-file-name (concat dir icon) zotero-browser-base)))
    (when (file-readable-p filename)
      (let ((image (create-image filename 'png)))
        (insert-image image)
        (insert " ")))
    (insert name)))

(defun zotero-browser--collection-pp (key)
  "Pretty print KEY."
  (let* ((table (zotero-cache-get :type zotero-browser-type :id zotero-browser-id :resource "collections"))
         (entry (zotero-cache-get :type zotero-browser-type :id zotero-browser-id :resource "collection" :key key))
         (level (zotero-cache-level key table))
         (indentation (+ zotero-browser-padding level))
         (prefix (make-string indentation ?\s))
         (values (seq-map (lambda (key) (zotero-lib-plist-get* entry :object :data key)) zotero-browser-collection-keys))
         (text (string-join values " ")))
    (add-text-properties 0 (length text) `(line-prefix ,prefix wrap-prefix ,prefix) text)
    (insert text)))

(defun zotero-browser--item-pp (key)
  "Pretty print KEY."
  (let* ((table (zotero-cache-get :type zotero-browser-type :id zotero-browser-id :resource "items"))
         (entry (zotero-cache-get :type zotero-browser-type :id zotero-browser-id :resource "item" :key key))
         (itemtype (zotero-lib-plist-get* entry :object :data :itemType))
         (level (zotero-cache-level key table))
         (indentation (+ zotero-browser-padding level))
         (prefix (make-string indentation ?\s)))
    (let ((beg (point)))
      (pcase itemtype
        ("attachment"
         (let* ((linkmode (zotero-lib-plist-get* entry :object :data :linkMode))
                (file (zotero-browser--attachment-icon linkmode)))
           (when (file-readable-p file)
             (let ((image (create-image file 'png)))
               (insert-image image itemtype)
               (insert " "))))
         (when-let ((title (zotero-lib-plist-get* entry :object :data :title)))
           (insert title)))
        ("note"
         (let* ((itemtype (zotero-lib-plist-get* entry :object :data :itemType))
                (file (zotero-browser--itemtype-icon itemtype)))
           (when file
             (let ((image (create-image file 'png)))
               (insert-image image itemtype)
               (insert " "))))
         (when-let ((note (zotero-lib-plist-get* entry :object :data :note))
                    (text (replace-regexp-in-string "<[^>]+>" "" note)) ; Remove all HTML tags
                    (match (string-match "^.+$" text)) ; Match first non-empty line
                    (first-line (match-string-no-properties 0 text)))
           (insert first-line)))
        (_
         (dolist (key zotero-browser-item-keys)
           (pcase key
             ('key
              (when-let ((key (zotero-lib-plist-get* entry :object :data :key)))
                (insert key)
                (insert " ")))
             ('version
              (when-let ((version (number-to-string (zotero-lib-plist-get* entry :object :data :version))))
                (insert version)
                (insert " ")))
             ('itemtype
              (let* ((itemtype (zotero-lib-plist-get* entry :object :data :itemType))
                     (file (zotero-browser--itemtype-icon itemtype)))
                (when file
                  (let ((image (create-image file 'png)))
                    (insert-image image itemtype)
                    (insert " ")))))
             ('title
              (when-let ((title (zotero-lib-plist-get* entry :object :data :title)))
                (insert title)
                (insert " ")))
             ('creators
              (when-let ((creators (zotero-lib-plist-get* entry :object :data :creators))
                         (names (cond
                                 ((seq-some (lambda (elt) (plist-get elt :lastName)) creators)
                                  (seq-map (lambda (elt) (plist-get elt :lastName)) creators))
                                 ((seq-some (lambda (elt) (plist-get elt :name)) creators)
                                  (seq-map (lambda (elt) (plist-get elt :name)) creators)))))
                (pcase (length names)
                  (1 (insert (seq-elt names 0)))
                  (2 (insert (concat (seq-elt names 0)
                                     " and "
                                     (seq-elt names 1))))
                  ((pred (< 2))
                   (let* ((selection (seq-take names 1)))
                     (insert (concat (string-join selection ", ")
                                     " et al.")))))
                (insert " ")))
             ('date
              (when-let ((date (zotero-lib-plist-get* entry :object :data :date)))
                (insert date)
                (insert " ")))
             ('year
              (when-let ((date (zotero-lib-plist-get* entry :object :data :date))
                         (match (string-match "[[:digit:]]\\{4\\}" date))
                         (year (match-string 0 date)))
                (insert year)
                (insert " ")))
             ('publisher
              (when-let ((publisher (zotero-lib-plist-get* entry :object :data :publisher)))
                (insert publisher)
                (insert " ")))
             ('publication-title
              (when-let ((publication-title (zotero-lib-plist-get* entry :object :data :publicationTitle)))
                (insert publication-title)
                (insert " ")))
             ('date-added
              (when-let ((date-added (zotero-lib-plist-get* entry :object :data :dateAdded)))
                (insert date-added)
                (insert " ")))
             ('date-modified
              (when-let ((date-modified (zotero-lib-plist-get* entry :object :data :dateModified)))
                (insert date-modified)
                (insert " ")))
             ('extra
              (when-let ((extra (zotero-lib-plist-get* entry :object :data :extra)))
                (insert extra)
                (insert " ")))
             ('attachments
              (when (zotero-cache-has-attachments-p entry table)
                (let* ((icon "attach.png")
                       (dir (file-name-as-directory "img"))
                       (file (expand-file-name (concat dir icon) zotero-browser-base)))
                  (when (file-readable-p file)
                    (let ((image (create-image file 'png)))
                      (insert-image image "attachments")
                      (insert " "))))))
             ('notes)))))
      (add-text-properties beg (point) `(line-prefix ,prefix wrap-prefix ,prefix)))))

(defun zotero-browser--expand (ewoc node)
  "Expand the children of NODE in EWOC."
  (let* ((key (ewoc-data node))
         (table (zotero-browser--children key)))
    (zotero-browser--prefix "▾" (ewoc-location node))
    (zotero-browser--add ewoc node table)))

(defun zotero-browser--collapse (ewoc node)
  "Collapse the children of NODE in EWOC."
  (let* ((type zotero-browser-type)
         (id zotero-browser-id)
         (table (pcase major-mode
                  ('zotero-browser-collections-mode
                   (zotero-cache-get :type type :id id :resource "collections"))
                  ('zotero-browser-items-mode
                   (zotero-cache-get :type type :id id :resource "items"))))
         (key (ewoc-data node))
         parents)
    (zotero-browser--prefix "▸" (ewoc-location node))
    (while
        (let* ((next-node (ewoc-next ewoc node))
               (next-key (when next-node (ewoc-data next-node)))
               (parent (pcase major-mode
                         ('zotero-browser-collections-mode
                          (zotero-cache-parentcollection next-key table))
                         ('zotero-browser-items-mode
                          (zotero-cache-parentitem next-key table)))))
          ;; add to parents if the next entry is a child of the current entry
          (when (equal parent key)
            (push key parents))
          ;; delete the next entry if it is a child of one of the parents
          (when (member parent parents)
            (ewoc-delete ewoc next-node)
            (delete next-key zotero-browser-keys)
            (setq key next-key))))))

(defun zotero-browser--add (ewoc node table)
  "Add items of TABLE after NODE in ewoc"
  (let* ((key (ewoc-data node))
         (idx (seq-position zotero-browser-keys key))
         (head (seq-subseq zotero-browser-keys 0 (1+ idx)))
         (tail (seq-subseq zotero-browser-keys (1+ idx)))
         (keys (pcase major-mode
                 ('zotero-browser-collections-mode
                  (zotero-cache-sort-by :name 'asc table))
                 ('zotero-browser-items-mode
                  (zotero-cache-sort-by :title 'asc table)))))
    (setq zotero-browser-keys (seq-concatenate 'list head keys tail))
    (while keys
      (setq node (ewoc-enter-after ewoc node (pop keys))))))

(defun zotero-browser--prefix (string pos)
  (let* ((prefix (get-text-property (point) 'line-prefix))
         (spacing (substring prefix 0 -1)))
    (put-text-property (point) (line-end-position) 'line-prefix (concat spacing string))))

(defun zotero-browser--has-children-p (node)
  "Return non-nil if KEY has children."
  (let ((type zotero-browser-type)
        (id zotero-browser-id)
        (key (ewoc-data node)))
    (pcase major-mode
      ('zotero-browser-collections-mode
       (let ((table (zotero-cache-get :type type :id id :resource "collections")))
         (zotero-cache-has-subcollections-p key table)))
      ('zotero-browser-items-mode
       (let ((table (zotero-cache-get :type type :id id :resource "items")))
         (zotero-cache-has-subitems-p key table))))))

(defun zotero-browser--expanded-p (ewoc node)
  "Return non-nil if NODE in EWOC is expanded."
  (let* ((type zotero-browser-type)
         (id zotero-browser-id)
         (table (pcase major-mode
                  ('zotero-browser-collections-mode
                   (zotero-cache-get :type type :id id :resource "collections"))
                  ('zotero-browser-items-mode
                   (zotero-cache-get :type type :id id :resource "items"))))
         (key (ewoc-data node))
         (next-node (ewoc-next ewoc node))
         (next-key (when next-node (ewoc-data next-node)))
         (parent (pcase major-mode
                   ('zotero-browser-collections-mode
                    (zotero-cache-parentcollection next-key table))
                   ('zotero-browser-items-mode
                    (zotero-cache-parentitem next-key table)))))
    (equal parent key)))

(defun zotero-browser--children (key)
  "Return the children of KEY."
  (let* ((type zotero-browser-type)
         (id zotero-browser-id)
         (table (pcase major-mode
                  ('zotero-browser-collections-mode
                   (zotero-cache-get :type type :id id :resource "collections"))
                  ('zotero-browser-items-mode
                   (zotero-cache-get :type type :id id :resource "items")))))
    (pcase major-mode
      ('zotero-browser-collections-mode
       (zotero-cache-subcollections key table))
      ('zotero-browser-items-mode
       (zotero-cache-subitems key table)))))

(defun zotero-browser--parent (key)
  "Return the parent of KEY."
  (let* ((type zotero-browser-type)
         (id zotero-browser-id)
         (table (pcase major-mode
                  ('zotero-browser-collections-mode
                   (zotero-cache-get :type type :id id :resource "collections"))
                  ('zotero-browser-items-mode
                   (zotero-cache-get :type type :id id :resource "items")))))
    (pcase major-mode
      ('zotero-browser-collections-mode
       (zotero-cache-parentcollection key table))
      ('zotero-browser-items-mode
       (zotero-cache-parentitem key table)))))

(defun zotero-browser--itemtype-icon (itemtype)
  "Return the icon file for ITEMTYPE, or nil if none exists."
  (let* ((icon (concat  "treeitem-" itemtype ".png"))
         (dir (file-name-as-directory "img"))
         (filename (expand-file-name (concat dir icon) zotero-browser-base))
         (fallback (expand-file-name (concat dir "treeitem.png") zotero-browser-base)))
    (cond
     ((file-readable-p filename) filename)
     ((file-readable-p fallback) fallback)
     (t
      nil))))

(defun zotero-browser--attachment-icon (linkmode)
  (let* ((icon (pcase linkmode
                 ("imported_url" "treeitem-attachment-snapshot.png")
                 ("imported_file" "treeitem-attachment-pdf.png")
                 ("linked_file" "treeitem-attachment-pdf-link.png")
                 ("linked_url" "treeitem-attachment-web-link.png")))
         (dir (file-name-as-directory "img"))
         (filename (expand-file-name (concat dir icon) zotero-browser-base)))
    (if (file-readable-p filename) filename nil)))

(provide 'zotero-browser)

;;; zotero-browser.el ends here
