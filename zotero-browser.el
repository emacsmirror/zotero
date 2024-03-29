;;; zotero-browser.el --- Interface to Zotero  -*- lexical-binding: t; -*-

;; Author: Folkert van der Beek <folkertvanderbeek@gmail.com>
;; URL: https://gitlab.com/fvdbeek/emacs-zotero

;; This file is part of Emacs-zotero.

;; Emacs-zotero is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; Emacs-zotero is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; Emacs-zotero. If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'ewoc)
(require 'hl-line)
(require 'ht)
(require 'mailcap)
(require 'seq)
(require 'subr-x)
(require 'url)
(require 'zotero)
(require 'zotero-lib)
(require 'zotero-cache)
(require 'zotero-sync)
(require 'zotero-edit)
(require 'zotero-fulltext)
(require 'zotero-recognize)
(require 'zotero-arxiv)
(require 'zotero-crossref)
(require 'zotero-openlibrary)
(require 'zotero-pmid)

;;;; Variables

(defvar zotero-browser-libraries-buffer-action
  '((display-buffer-reuse-window display-buffer-in-side-window) . ((side . left)
                                                                   (slot . -1)
                                                                   (window-width . 0.2)
                                                                   (window-height . fit-window-to-buffer)
                                                                   (preserve-size . (t . nil))
                                                                   (reusable-frames . nil))))

(defvar zotero-browser-collections-buffer-action
  '((display-buffer-reuse-window display-buffer-in-side-window) . ((side . left)
                                                                   (slot . 0)
                                                                   (window-width . 0.2)
                                                                   (preserve-size . (t . nil))
                                                                   (reusable-frames . nil))))

(defvar zotero-browser-items-buffer-action
  '((display-buffer-same-window) . ((window-width . 0.5)
                                    (preserve-size . (t . nil)))))

(defvar zotero-browser-edit-buffer-action
  '((display-buffer-reuse-window display-buffer-in-side-window) . ((side . right)
                                                                   (slot . 0)
                                                                   (window-width . 0.3)
                                                                   (preserve-size . (t . nil))
                                                                   (reusable-frames . nil))))

(defvar zotero-browser-note-buffer-action '((display-buffer-reuse-window display-buffer-in-side-window)
                                            (side . bottom)
                                            (window-height . 0.5)
                                            (preserve-size . (nil . t))))

(defvar zotero-browser-after-change-functions nil
  "List of functions to be called when items have changed.
Each function is called with the item key as argument.")

(defvar zotero-browser-padding 1
  "Set the number of characters preceding each entry.")

(defvar zotero-browser-default-itemtypes nil
  "Default itemtypes when creating a new item.")

(defvar zotero-browser-default-linkmodes nil
  "Default linkmodes when creating a new item.")

(defvar-local zotero-browser-ewoc nil
  "Ewoc of the current buffer.")

(defvar-local zotero-browser-type nil
  "Type of the current buffer.")

(defvar-local zotero-browser-id nil
  "ID of the current buffer.")

(defvar-local zotero-browser-key nil
  "Key of the current buffer.")

(defvar-local zotero-browser-resource nil
  "Resource of the current buffer.")

(defvar-local zotero-browser-collection nil
  "Collection of the current buffer.")

(defvar-local zotero-browser-status nil
  "Visibility status of the current buffer.")

;;;; Keymap

(defvar zotero-browser-libraries-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'zotero-browser-display)
    (define-key map (kbd "g") #'zotero-browser-revert)
    (define-key map (kbd "n") #'zotero-browser-next)
    (define-key map (kbd "p") #'zotero-browser-prev)
    (define-key map (kbd "C-c C-u") #'zotero-browser-up-collection)
    (define-key map (kbd "C-c C-n") #'zotero-browser-next-collection)
    (define-key map (kbd "C-c C-p") #'zotero-browser-prev-collection)
    (define-key map (kbd "+") #'zotero-browser-create)
    (define-key map (kbd "e") #'zotero-browser-edit)
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
    (define-key map (kbd "u") #'zotero-browser-up)
    (define-key map (kbd "n") #'zotero-browser-next)
    (define-key map (kbd "p") #'zotero-browser-prev)
    (define-key map (kbd "C-c C-f") #'zotero-browser-forward-same-level)
    (define-key map (kbd "C-c C-b") #'zotero-browser-backward-same-level)
    (define-key map (kbd "C-c C-u") #'zotero-browser-up-collection)
    (define-key map (kbd "C-c C-n") #'zotero-browser-next-collection)
    (define-key map (kbd "C-c C-p") #'zotero-browser-prev-collection)
    (define-key map (kbd "g") #'zotero-browser-revert)
    (define-key map (kbd "D") #'zotero-browser-delete)
    (define-key map (kbd "+") #'zotero-browser-create)
    (define-key map (kbd "e") #'zotero-browser-edit)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Local keymap for `zotero-browser-collections-mode'.")

(defvar zotero-browser-items-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'zotero-browser-open)
    (define-key map (kbd "TAB") #'zotero-browser-toggle)
    (define-key map (kbd "<backtab>") #'zotero-browser-cycle)
    (define-key map (kbd "$") #'zotero-browser-expand-all)
    (define-key map (kbd "M-$") #'zotero-browser-collapse-all)
    (define-key map (kbd "u") #'zotero-browser-up)
    (define-key map (kbd "n") #'zotero-browser-next)
    (define-key map (kbd "p") #'zotero-browser-prev)
    (define-key map (kbd "C-c C-f") #'zotero-browser-forward-same-level)
    (define-key map (kbd "C-c C-b") #'zotero-browser-backward-same-level)
    (define-key map (kbd "C-c C-u") #'zotero-browser-up-collection)
    (define-key map (kbd "C-c C-n") #'zotero-browser-next-collection)
    (define-key map (kbd "C-c C-p") #'zotero-browser-prev-collection)
    (define-key map (kbd "g") #'zotero-browser-revert)
    (define-key map (kbd "D") #'zotero-browser-delete)
    (define-key map (kbd "R") #'zotero-browser-remove-from-collection)
    (define-key map (kbd "M") #'zotero-browser-move-to-parent)
    (define-key map (kbd "C") #'zotero-browser-copy-to-collection)
    (define-key map (kbd "+") #'zotero-browser-create)
    (define-key map (kbd "e") #'zotero-browser-edit)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Local keymap for `zotero-items-mode'.")

(defvar zotero-browser-note-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'zotero-browser-note-exit)
    (define-key map (kbd "C-x C-s") #'zotero-browser-note-save)
    (define-key map (kbd "C-c C-k") #'zotero-browser-note-abort)
    map)
  "Local keymap for `zotero-browser-note-mode'.")

(defvar zotero-browser-header-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'zotero-browser-sort)
    map)
  "Keymap for mouse events on the header.")

(defvar zotero-browser-toggle-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'zotero-browser-toggle)
    map)
  "Keymap for mouse events on expand or collaps symbols.")

(defvar zotero-browser-library-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'zotero-browser-display)
    map)
  "Keymap for mouse events on libraries.")

(defvar zotero-browser-collection-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'zotero-browser-display)
    (define-key map [mouse-3] #'zotero-browser-collection-popup-menu)
    map)
  "Keymap for mouse events on collections.")

(defvar zotero-browser-item-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'zotero-browser-edit)
    (define-key map [mouse-3] #'zotero-browser-item-popup-menu)
    (define-key map [drag-mouse-1] #'zotero-browser-drag)
    map)
  "Keymap for mouse events on items.")

(defvar zotero-browser-trash-item-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'zotero-browser-edit)
    (define-key map [mouse-3] #'zotero-browser-trash-item-popup-menu)
    map)
  "Keymap for mouse events on items.")

(defvar zotero-browser-top-attachment-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'zotero-browser-edit)
    (define-key map [mouse-3] #'zotero-browser-top-attachment-popup-menu)
    map)
  "Keymap for mouse events on top level attachments.")

(defvar zotero-browser-child-attachment-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'zotero-browser-edit)
    (define-key map [mouse-3] #'zotero-browser-child-attachment-popup-menu)
    map)
  "Keymap for mouse events on child attachments.")

(defvar zotero-browser-note-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'zotero-browser-edit)
    (define-key map [mouse-3] #'zotero-browser-note-popup-menu)
    map)
  "Keymap for mouse events on notes.")

(defvar zotero-browser-expand-symbol (propertize "▸"
                                                 'mouse-face 'highlight
                                                 'help-echo "mouse-1: expand"
                                                 'keymap zotero-browser-toggle-keymap)
  "Expand symbol.
The symbol appears next to items that contains children and means
that the item is collapsed and the children are hidden in the
item tree.")

(defvar zotero-browser-collapse-symbol (propertize "▾"
                                                   'mouse-face 'highlight
                                                   'help-echo "mouse-1: collapse"
                                                   'keymap zotero-browser-toggle-keymap)
  "Collapse symbol.
The symbol appears next to items that contains children and means
that the item is expanded and the children appear in the item
tree.")

(defconst zotero-browser-note-usage-message
  "Type \\[zotero-browser-note-exit] to finish, \
\\[zotero-browser-note-save] to save, or \
\\[zotero-browser-note-abort] to abort.")

;;;; Menu

(easy-menu-define zotero-browser-libraries-mode-menu zotero-browser-libraries-mode-map
  "Menu for `zotero-browser-libraries-mode'."
  `("Zotero-Browser"
    ["Edit group" zotero-browser-edit :help "Change the group settings"]
    ["Create group" zotero-create-group :help "Create a new group"]
    "--"
    ("Navigate"
     ["Next" zotero-browser-next]
     ["Previous" zotero-browser-prev]
     ["Up collection" zotero-browser-up-collection]
     ["Next collection" zotero-browser-next-collection]
     ["Previous collection" zotero-browser-prev-collection])
    "--"
    ["Revert" zotero-browser-revert]
    ["Quit" quit-window]
    ["Customize" (customize-group 'zotero-browser)]))

(easy-menu-define zotero-browser-collections-mode-menu zotero-browser-collections-mode-map
  "Menu for `zotero-browser-collections-mode'."
  `("Zotero-Browser"
    ("Show/Hide"
     ["Toggle" zotero-browser-toggle :help "Expand or collapse the children of the current item"]
     ["Cycle" zotero-browser-cycle :help "Cycle the visibility of children"]
     ["Expand all" zotero-browser-expand-all :help "Expand all children"]
     ["Collapse all" zotero-browser-collapse-all :help "Collapse all children"])
    "--"
    ("Navigate"
     ["Up" zotero-browser-up]
     ["Next" zotero-browser-next]
     ["Previous" zotero-browser-prev]
     ["Next same level" zotero-browser-forward-same-level]
     ["Previous same level" zotero-browser-backward-same-level])
    "--"
    ["Edit collection" zotero-browser-edit :help "Edit current collection"]
    ["Create collection" zotero-browser-create :help "Create a new collection"]
    "--"
    ["Delete" zotero-browser-delete :help "Delete current collection or collections in active region"]
    "--"
    ["Revert" zotero-browser-revert]
    ["Quit" quit-window]
    ["Customize" (customize-group 'zotero-browser)]))

(easy-menu-define zotero-browser-items-mode-menu zotero-browser-items-mode-map
  "Menu for `zotero-browser-items-mode'."
  `("Zotero-Browser"
    ("Show/Hide"
     ["Toggle" zotero-browser-toggle :help "Expand or collapse the children of the current item"]
     ["Cycle" zotero-browser-cycle :help "Cycle the visibility of children"]
     ["Expand all" zotero-browser-expand-all :help "Expand all children"]
     ["Collapse all" zotero-browser-collapse-all :help "Collapse all children"])
    "--"
    ("Navigate"
     ["Up" zotero-browser-up]
     ["Next" zotero-browser-next]
     ["Previous" zotero-browser-prev]
     ["Next same level" zotero-browser-forward-same-level]
     ["Previous same level" zotero-browser-backward-same-level]
     ["Up collection" zotero-browser-up-collection]
     ["Next collection" zotero-browser-next-collection]
     ["Previous collection" zotero-browser-prev-collection])
    "--"
    ["Edit item" zotero-browser-edit :help "Edit current entry"]
    ("Create item"    ["Create" zotero-browser-create :help "Create a new item"]
     ["Create note" zotero-browser-create-note :help "Create a new note"]
     ["Create attachment" zotero-browser-create-attachment :help "Create a new attachment"]
     ["Update attachment" zotero-browser-update-attachment :help "Update current attachment"]
     ["Import attachment" zotero-browser-import-attachment :help "Import a PDF file and create a new attachment."]
     ["Link attachment" zotero-browser-link-attachment :help "Link to a PDF file and create a new attachment."]
     ["Add by identifier" zotero-browser-add-by-identifier :help "Add a new item by ISBN, DOI, PMID, or arXiv ID"])
    "--"
    ["Move to trash" zotero-browser-move-to-trash :help "Move current entry or entries in active region to trash"]
    ["Restore from trash" zotero-browser-restore :help "Restore current entry or entries in active region"]
    ["Delete" zotero-browser-delete :help "Delete current entry or entries in active region"]
    "--"
    ("File item"
     ["Remove from collection" zotero-browser-remove-from-collection :help "Remove current entry from a collection"]
     ["Copy to collection" zotero-browser-copy-to-collection :help "Copy current entry to a collection"]
     ["Move to parent" zotero-browser-move-to-parent :help "Move current entry to a parent item"])
    "--"
    ["Open attachment" zotero-browser-open]
    ["Open attachment directory" zotero-browser-open-directory]
    "--"
    ["Revert" zotero-browser-revert]
    ["Quit" quit-window]
    ["Customize" (customize-group 'zotero-browser)]))

(easy-menu-define zotero-browser-note-mode-menu zotero-browser-note-mode-map
  "Menu for `zotero-browser-note-mode'."
  `("Zotero-Browser"
    "--"
    ["Exit" zotero-browser-note-exit :help "Save note and kill buffer"]
    ["Save" zotero-browser-note-save :help "Save note"]
    ["Abort" zotero-browser-note-abort :help "Abort editing note and kill buffer"]))

(easy-menu-define zotero-browser-collection-menu zotero-browser-collection-keymap
  "Popup menu for collections."
  `("Zotero collection"
    "--"
    ["Create collection" zotero-browser-create :help "Create a new collection"]
    ["Edit collection" zotero-browser-edit :help "Edit current collection"]
    "--"
    ["Delete" zotero-browser-delete :help "Delete current collection"]))

(easy-menu-define zotero-browser-item-menu zotero-browser-item-keymap
  "Popup menu for items."
  `("Zotero item"
    "--"
    ["Open attachment" zotero-browser-open]
    ["Open attachment directory" zotero-browser-open-directory]
    "--"
    ["Add note" zotero-browser-create-note :help "Add a new note"]
    ["Add attachment" zotero-browser-create-attachment :help "Add a new attachment"]
    ["Update attachment" zotero-browser-update-attachment :help "Update current attachment"]
    "--"
    ["Move to trash" zotero-browser-move-to-trash :help "Move current entry to trash"]
    ["Remove from collection" zotero-browser-remove-from-collection :help "Remove current entry from a collection"]
    ["Copy to collection" zotero-browser-copy-to-collection :help "Copy current entry to a collection"]))

(easy-menu-define zotero-browser-trash-item-menu zotero-browser-trash-item-keymap
  "Popup menu for trashed items."
  `("Zotero item"
    "--"
    ["Open attachment" zotero-browser-open]
    ["Open attachment directory" zotero-browser-open-directory]
    "--"
    ["Restore from trash" zotero-browser-restore-from-trash :help "Restore current entry from trash"]
    ["Delete" zotero-browser-delete :help "Delete current entry"]))

(easy-menu-define zotero-browser-top-attachment-menu zotero-browser-top-attachment-keymap
  "Popup menu for top-level attachments."
  `("Zotero item"
    "--"
    ["Open attachment" zotero-browser-open]
    ["Open attachment directory" zotero-browser-open-directory]
    "--"
    ["Recognize content" zotero-browser-recognize-attachment]
    "--"
    ["Move to trash" zotero-browser-move-to-trash :help "Move current entry to trash"]
    ["Delete" zotero-browser-delete :help "Delete current entry"]
    ["Remove from collection" zotero-browser-remove-from-collection :help "Remove current entry from a collection"]
    ["Copy to collection" zotero-browser-copy-to-collection :help "Copy current entry to a collection"]))

(easy-menu-define zotero-browser-child-attachment-menu zotero-browser-child-attachment-keymap
  "Popup menu for child attachments."
  `("Zotero item"
    "--"
    ["Open attachment" zotero-browser-open]
    ["Open attachment directory" zotero-browser-open-directory]
    "--"
    ["Move to trash" zotero-browser-move-to-trash :help "Move current entry to trash"]
    ["Remove from collection" zotero-browser-remove-from-collection :help "Remove current entry from a collection"]
    ["Copy to collection" zotero-browser-copy-to-collection :help "Copy current entry to a collection"]))

(easy-menu-define zotero-browser-note-menu zotero-browser-note-keymap
  "Popup menu for notes."
  `("Zotero item"
    "--"
    ["Move to trash" zotero-browser-move-to-trash :help "Move current entry to trash"]
    ["Remove from collection" zotero-browser-remove-from-collection :help "Remove current entry from a collection"]
    ["Copy to collection" zotero-browser-copy-to-collection :help "Copy current entry to a collection"]))

;;;; Customization

(defgroup zotero-browser nil
  "Interface to Zotero Browser."
  :group 'zotero)

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

(defcustom zotero-browser-note-buffer-name "*Zotero Note*"
  "The default name of the note buffer."
  :group 'zotero-browser
  :type 'string)

(defcustom zotero-browser-dnd-protocol-alist
  '(("^file:" . zotero-browser-dnd-protocol))
  "The functions to call when a drop in `zotero-items-mode' is made.
See `dnd-protocol-alist' for more information.  When nil, behave
as in other buffers."
  :group 'zotero-browser
  :type '(choice (repeat (cons (regexp) (function)))
		 (const :tag "Behave as in other buffers" nil)))

(defcustom zotero-browser-default-collection-level 1
  "The default expansion level for collections."
  :group 'zotero-browser
  :type 'integer)

(defcustom zotero-browser-default-item-level 1
  "The default expansion level for items."
  :group 'zotero-browser
  :type 'integer)

(defcustom zotero-browser-icons t
  "When t show browser icons.
Icons are enabled by default."
  :group 'zotero-browser
  :type 'boolean)

(defcustom zotero-browser-libraries-sort-field '(:name . nil)
  "Sort field for the collections buffer.
If nil, no sorting is performed. Otherwise, this should be a cons
cell (FIELD . FLIP). FIELD is the prop of the object plist to be
sorted. FLIP, if non-nil, means to invert the resulting sort."
  :group 'zotero-browser
  :type '(choice (const :tag "No sorting" nil)
                 (cons (choice
                        (const :tag "ID" :id)
                        (const :tag "Version" :version)
                        (const :tag "Read permission" :library)
                        (const :tag "Write permission" :write)
                        (const :tag "Last Synced" :last-sync)
                        (const :tag "Name" :name)
                        (const :tag "Group Type" :group-type)
                        (const :tag "Description" :description)
                        (const :tag "URL" :url)
                        (const :tag "Library Reading" :libraryEditing)
                        (const :tag "Library Editing" :libraryReading)
                        (const :tag "File Editing" :fileEditing))
                       (choice (const :tag "Ascending" nil)
                               (const :tag "Descending" t)))))

(defcustom zotero-browser-collections-sort-field '(:name . nil)
  "Sort field for the collections buffer.
If nil, no sorting is performed. Otherwise, this should be a cons
cell (FIELD . FLIP). FIELD is the prop of the object plist to be
sorted. FLIP, if non-nil, means to invert the resulting sort."
  :group 'zotero-browser
  :type '(choice (const :tag "No sorting" nil)
                 (cons (choice
                        (const :tag "Key" :key)
                        (const :tag "Version" :version)
                        (const :tag "Name" :name))
                       (choice (const :tag "Ascending" nil)
                               (const :tag "Descending" t)))))

(defcustom zotero-browser-items-sort-field '(:title . nil)
  "Sort field for the items buffer.
If nil, no sorting is performed. Otherwise, this should be a cons
cell (FIELD . FLIP). FIELD is the prop of the object plist to be
sorted. FLIP, if non-nil, means to invert the resulting sort."
  :group 'zotero-browser
  :type '(choice (const :tag "No sorting" nil)
                 (cons (choice
                        (const :tag "Key" :key)
                        (const :tag "Version" :version)
                        (const :tag "Item Type" :itemType)
                        (const :tag "Title" :title)
                        (const :tag "Creators" :creators)
                        (const :tag "Date" :date)
                        (const :tag "Year" :year)
                        (const :tag "Publisher" :publisher)
                        (const :tag "Publication Title" :publicationTitle)
                        (const :tag "Date Added" :dateAdded)
                        (const :tag "Date Modified" :dateModified)
                        (const :tag "Extra" :extra)
                        (const :tag "Note" :note))
                       (choice (const :tag "Ascending" nil)
                               (const :tag "Descending" t)))))

(defcustom zotero-browser-library-columns '((:name . 20))
  "Fields to show in the libraries buffer.
This should be a list of cons cells (FIELD . WIDTH),
where:
 - FIELD is the prop of the object plist to be sorted.
 - WIDTH is the width to reserve for the column."
  :group 'zotero-browser
  :type '(repeat (cons (choice
                        (const :tag "ID" :id)
                        (const :tag "Version" :version)
                        (const :tag "Read permission" :library)
                        (const :tag "Write permission" :write)
                        (const :tag "Last Synced" :last-sync)
                        (const :tag "Name" :name)
                        (const :tag "Group Type" :group-type)
                        (const :tag "Description" :description)
                        (const :tag "URL" :url)
                        (const :tag "Library Reading" :libraryEditing)
                        (const :tag "Library Editing" :libraryReading)
                        (const :tag "File Editing" :fileEditing))
                       (integer :tag "Width of column"))))

(defcustom zotero-browser-collection-columns '((:name . 25))
  "Fields to show in the collections buffer.
This should be a list of cons cells (FIELD . WIDTH),
where:
 - FIELD is the prop of the object plist to be sorted.
 - WIDTH is the width to reserve for the column."
  :group 'zotero-browser
  :type '(repeat (cons (choice
                        (const :tag "Key" :key)
                        (const :tag "Version" :version)
                        (const :tag "Name" :name))
                       (integer :tag "Width of column"))))

(defcustom zotero-browser-item-columns '((:title . 50)(:creators . 25)(:year . 4))
  "Fields to show in the items buffer.
This should be a list cons cells (FIELD . WIDTH),
where:
 - FIELD is the prop of the object plist to be sorted.
 - WIDTH is the width to reserve for the column."
  :group 'zotero-browser
  :type '(repeat (cons (choice
                        (const :tag "Key" :key)
                        (const :tag "Version" :version)
                        (const :tag "Item Type" :itemType)
                        (const :tag "Title" :title)
                        (const :tag "Creators" :creators)
                        (const :tag "Date" :date)
                        (const :tag "Year" :year)
                        (const :tag "Publisher" :publisher)
                        (const :tag "Publication Title" :publicationTitle)
                        (const :tag "Date Added" :dateAdded)
                        (const :tag "Date Modified" :dateModified)
                        (const :tag "Extra" :extra)
                        (const :tag "Note" :note))
                       (integer :tag "Width of column"))))

(defcustom zotero-browser-filename-keys '(" - " :creators :title :year)
  "Fields to show in the attachment filename.
Join all the key values with the separator in between."
  :group 'zotero-browser
  :type
  '(cons (string :tag "Separator")
         (repeat (choice
                  (const :tag "Key" :key)
                  (const :tag "Version" :version)
                  (const :tag "Item Type" :itemType)
                  (const :tag "Title" :title)
                  (const :tag "Creators" :creators)
                  (const :tag "Date" :date)
                  (const :tag "Year" :year)
                  (const :tag "Publisher" :publisher)
                  (const :tag "Publication Title" :publicationTitle)
                  (const :tag "Date Added" :dateAdded)
                  (const :tag "Date Modified" :dateModified)
                  (const :tag "Extra" :extra)
                  (const :tag "Note" :note)))))

(defcustom zotero-browser-filename-max-length 50
  "Maximum length of fields in attachment filenames.
Fields exceeding the maximum length are truncated."
  :group 'zotero-browser
  :type 'integer)

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
  (visual-line-mode 1)
  ;; Turn on highlighting of the current node
  (hl-line-mode 1)
  (setq-local hl-line-range-function #'zotero-browser-hl-range)
  (setq-local dnd-protocol-alist
              (append zotero-browser-dnd-protocol-alist dnd-protocol-alist)))

;;;###autoload
(define-minor-mode zotero-browser-note-mode
  "Minor mode for Zotero note buffers.

All currently available key bindings:

\\{zotero-browser-note-mode-map}"
  nil "ZotNote" nil)

;;;; Functions

(defun zotero-browser-collection-popup-menu (event)
  "Pop up a menu with mouse EVENT."
  (interactive "@e")
  (popup-menu zotero-browser-collection-menu event))

(defun zotero-browser-item-popup-menu (event)
  "Pop up a menu with mouse EVENT."
  (interactive "@e")
  (popup-menu zotero-browser-item-menu event))

(defun zotero-browser-trash-item-popup-menu (event)
  "Pop up a menu with mouse EVENT."
  (interactive "@e")
  (popup-menu zotero-browser-trash-item-menu event))

(defun zotero-browser-top-attachment-popup-menu (event)
  "Pop up a menu with mouse EVENT."
  (interactive "@e")
  (popup-menu zotero-browser-top-attachment-menu event))

(defun zotero-browser-child-attachment-popup-menu (event)
  "Pop up a menu with mouse EVENT."
  (interactive "@e")
  (popup-menu zotero-browser-child-attachment-menu event))

(defun zotero-browser-note-popup-menu (event)
  "Pop up a menu with mouse EVENT."
  (interactive "@e")
  (popup-menu zotero-browser-note-menu event))

(defun zotero-browser-sort (event)
  "Sort entries by the column of mouse EVENT."
  (interactive "@e")
  (let* ((pos (event-start event))
         (point (posn-point pos))
         (string (posn-string pos))
         (window (posn-window pos))
         (buffer (window-buffer window)))
    (with-current-buffer buffer
      (let ((field (get-text-property (if string (cdr string) point) 'zotero-browser-field (car string))))
        (zotero-browser--sort-by field)))))

(defun zotero-browser--sort-by (field)
  "Sort entries by FIELD in the items buffer."
  (zotero-browser-ensure-items-mode)
  (let ((prev-field (car zotero-browser-items-sort-field))
        (prev-flip (cdr zotero-browser-items-sort-field)))
    ;; Flip the sort order on a second click.
    (if (eq field prev-field)
        (setcdr zotero-browser-items-sort-field
	        (not prev-flip))
      (setq zotero-browser-items-sort-field (cons field nil)))
    (zotero-browser-revert)))

(defun zotero-browser--nodes (ewoc)
  "Return a list with the EWOC node at point.
If region is active, return a list of the nodes in the active
region instead."
  (if (use-region-p)
      (save-excursion
        (let* ((first-node (ewoc-locate ewoc (region-beginning)))
               (last-node (ewoc-locate ewoc (region-end)))
               (node first-node)
               nodes)
          (while
              (progn
                (ewoc-goto-node ewoc node)
                (push node nodes)
                (setq node (ewoc-next ewoc node))
                (not (eq node (ewoc-next ewoc last-node)))))
          nodes))
    (list (ewoc-locate ewoc))))

(defun zotero-browser--keys (ewoc)
  "Return a list with the key of the EWOC node at point.
If region is active, return a list of the keys in the active
region instead."
  (if (use-region-p)
      (let ((nodes (zotero-browser--nodes ewoc)))
        (seq-map (lambda (node) (ewoc-data node)) nodes))
    (let ((node (ewoc-locate ewoc)))
      (list (ewoc-data node)))))

(defun zotero-browser--level (key)
  "Return the level of KEY."
  (zotero-browser-ensure-browser-buffer)
  (let* ((type zotero-browser-type)
         (id zotero-browser-id)
         (table (pcase major-mode
                  ('zotero-browser-collections-mode
                   (zotero-cache-synccache "collections" nil type id))
                  ('zotero-browser-items-mode
                   (zotero-cache-synccache "items" nil type id))))
         (level 0))
    (while
        (let ((parent (pcase major-mode
                        ('zotero-browser-collections-mode
                         (zotero-cache-parentcollection key table))
                        ('zotero-browser-items-mode
                         (zotero-cache-parentitem key table)))))
          (when parent
            (setq level (1+ level))
            (setq key parent))))
    level))

(defun zotero-browser--prefix (position string)
  "Set the prefix of POSITION to STRING.
STRING should contain only one character."
  (let* ((prefix (get-text-property position 'line-prefix))
         (spacing (substring prefix 0 -1)))
    (put-text-property position (line-end-position) 'line-prefix (concat spacing string))))

(defun zotero-browser--find (ewoc key)
  "Return the first node of item KEY in EWOC."
  (when-let ((node (ewoc-nth ewoc 0)))
    (catch 'break
      (while
          (progn
            (ewoc-goto-node ewoc node)
            (when (equal (ewoc-data node) key)
              (throw 'break node))
            (setq node (ewoc-next ewoc node)))))))

(defun zotero-browser--add (ewoc node table)
  "Add items of TABLE after NODE in EWOC."
  (let ((keys (pcase major-mode
                ('zotero-browser-collections-mode
                 (let ((field (car zotero-browser-collections-sort-field))
                       (flip (cdr zotero-browser-collections-sort-field)))
                   (zotero-cache-sort-by field flip table)))
                ('zotero-browser-items-mode
                 (let ((field (car zotero-browser-items-sort-field))
                       (flip (cdr zotero-browser-items-sort-field)))
                   (zotero-cache-sort-by field flip table))))))
    (while keys
      (setq node (ewoc-enter-after ewoc node (pop keys))))))

(defun zotero-browser--has-children-p (node)
  "Return non-nil if NODE has children."
  (let ((type zotero-browser-type)
        (id zotero-browser-id)
        (key (ewoc-data node)))
    (pcase major-mode
      ('zotero-browser-collections-mode
       (let ((table (zotero-cache-synccache "collections" nil type id)))
         (zotero-cache-has-subcollections-p key table)))
      ('zotero-browser-items-mode
       (let ((table (zotero-cache-synccache "items" nil type id)))
         (zotero-cache-has-subitems-p key table))))))

(defun zotero-browser--expanded-p (ewoc node)
  "Return non-nil if NODE in EWOC is expanded."
  (let* ((type zotero-browser-type)
         (id zotero-browser-id)
         (table (pcase major-mode
                  ('zotero-browser-collections-mode
                   (zotero-cache-synccache "collections" nil type id))
                  ('zotero-browser-items-mode
                   (zotero-cache-synccache "items" nil type id))))
         (key (ewoc-data node))
         (next-node (ewoc-next ewoc node))
         (next-key (when next-node (ewoc-data next-node)))
         (parent (pcase major-mode
                   ('zotero-browser-collections-mode
                    (zotero-cache-parentcollection next-key table))
                   ('zotero-browser-items-mode
                    (zotero-cache-parentitem next-key table)))))
    (equal parent key)))

(defun zotero-browser--expand (ewoc node)
  "Expand the children of NODE in EWOC."
  (let* ((key (ewoc-data node))
         (table (zotero-browser--children key)))
    (zotero-browser--prefix (ewoc-location node) zotero-browser-collapse-symbol)
    (zotero-browser--add ewoc node table)))

(defun zotero-browser--collapse (ewoc node)
  "Collapse the children of NODE in EWOC."
  (let* ((type zotero-browser-type)
         (id zotero-browser-id)
         (table (pcase major-mode
                  ('zotero-browser-collections-mode
                   (zotero-cache-synccache "collections" nil type id))
                  ('zotero-browser-items-mode
                   (zotero-cache-synccache "items" nil type id))))
         (key (ewoc-data node))
         parents)
    (zotero-browser--prefix (ewoc-location node) zotero-browser-expand-symbol)
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
            (setq key next-key))))))

(defun zotero-browser--children (key)
  "Return the children of KEY."
  (let* ((type zotero-browser-type)
         (id zotero-browser-id)
         (table (pcase major-mode
                  ('zotero-browser-collections-mode
                   (zotero-cache-synccache "collections" nil type id))
                  ('zotero-browser-items-mode
                   (zotero-cache-synccache "items" nil type id)))))
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
                   (zotero-cache-synccache "collections" nil type id))
                  ('zotero-browser-items-mode
                   (zotero-cache-synccache "items" nil type id)))))
    (pcase major-mode
      ('zotero-browser-collections-mode
       (zotero-cache-parentcollection key table))
      ('zotero-browser-items-mode
       (zotero-cache-parentitem key table)))))

(defun zotero-browser--update-collection (key)
  "Update collection KEY in the collections buffer.
Called if collection KEY is added, deleted, or changed."
  (when-let ((buffer (get-buffer zotero-browser-collections-buffer-name)))
    (with-current-buffer buffer
      (zotero-browser--update key))))

(defun zotero-browser--update-item (key)
  "Update item KEY in the items buffer.
Called if item KEY is added, deleted, or changed."
  (when-let ((buffer (get-buffer zotero-browser-items-buffer-name)))
    (with-current-buffer buffer
      (zotero-browser--update key))))

(defun zotero-browser--update (key)
  "Update KEY in a browser buffers.
Called if KEY is added, deleted, or changed.

This function tries to find the correct location to display the
item, without affecting the display of other items like
`zotero-browser-revert' would."
  (zotero-browser-ensure-browser-buffer)
  (let* ((inhibit-read-only t)
         (ewoc zotero-browser-ewoc)
         (table (zotero-cache-synccache zotero-browser-resource
                                        zotero-browser-collection
                                        zotero-browser-type
                                        zotero-browser-id))
         (parent (zotero-browser--parent key)))
    ;; If node is visible, delete it
    (when-let ((node (zotero-browser--find ewoc key)))
      (ewoc-delete ewoc node))
    ;; If key is in table, place it in the buffer
    (when (or (ht-contains-p table key) (ht-contains-p table parent))
      (let ((sort-fn (pcase major-mode
                       ('zotero-browser-collections-mode
                        (let ((field (car zotero-browser-collections-sort-field))
                              (flip (cdr zotero-browser-collections-sort-field)))
                          (apply-partially #'zotero-cache-sort-by field flip)))
                       ('zotero-browser-items-mode
                        (let ((field (car zotero-browser-items-sort-field))
                              (flip (cdr zotero-browser-items-sort-field)))
                          (apply-partially #'zotero-cache-sort-by field flip))))))
        (if parent
            ;; Item is a child
            (let ((parent-node (zotero-browser--find ewoc parent)))
              (if (zotero-browser--expanded-p ewoc parent-node)
                  ;; If the parent is expanded, place the item in the children
                  (progn
                    (let* ((siblings (zotero-browser--children parent))
                           (keys (funcall sort-fn siblings)))
                      (if (equal (car keys) key)
                          ;; If the key is the first child, place it after the
                          ;; parent
                          (ewoc-enter-after ewoc parent-node key)
                        (let* ((idx (seq-position keys key))
                               (prev-key (seq-elt keys (1- idx)))
                               (prev-node (zotero-browser--find ewoc prev-key)))
                          (ewoc-enter-after ewoc prev-node key)))))
                ;; If the parent is collapsed, expand it
                (zotero-browser--expand ewoc parent-node)))
          ;; Top level item
          (let ((children (zotero-browser--children key)))
            (if (ht-empty-p children)
                ;; Item is not a parent
                (let ((keys (funcall sort-fn table)))
                  (if (equal (car (last keys)) key)
                      ;; If the item is the last item, place at the end. But
                      ;; place it before the 'unfiled and 'trash collection
                      (if-let ((last-node (ewoc-nth ewoc -1)))
                          (progn
                            (while (symbolp (ewoc-data last-node))
                              (setq last-node (ewoc-prev ewoc last-node)))
                            (ewoc-enter-after ewoc last-node key))
                        (ewoc-enter-last ewoc key))
                    ;; Else place it before the next item (we don't place it after the
                    ;; previous item, because it could be expanded)
                    (let* ((idx (seq-position keys key))
                           (next-key (seq-elt keys (1+ idx)))
                           (next-node (zotero-browser--find ewoc next-key)))
                      (ewoc-enter-before ewoc next-node key))))
              ;; Item is a parent
              (let* ((keys (funcall sort-fn children))
                     (child (zotero-browser--find ewoc (car keys))))
                (if child
                    ;; If the item is expanded, place before the first child
                    (progn
                      (let ((node (ewoc-enter-before ewoc child key)))
                        ;; And prefix it with the collapse symbol
                        (zotero-browser--prefix (ewoc-location node) zotero-browser-collapse-symbol)))
                  ;; If the item is collapsed
                  (let ((keys (funcall sort-fn table)))
                    (if (equal (car (last keys)) key)
                        ;; If the item is the last item, place at the end. But
                        ;; place it before the 'unfiled and 'trash collection
                        (if-let ((last-node (ewoc-nth ewoc -1)))
                            (progn
                              (while (symbolp (ewoc-data last-node))
                                (setq last-node (ewoc-prev ewoc last-node)))
                              (let ((node (ewoc-enter-after ewoc last-node key)))
                                ;; And prefix it with the expand symbol
                                (zotero-browser--prefix (ewoc-location node) zotero-browser-expand-symbol)))
                          (let ((node (ewoc-enter-last ewoc key)))
                            (zotero-browser--prefix (ewoc-location node) zotero-browser-expand-symbol)))
                      ;; Else place it before the next item (we don't place it after the
                      ;; previous item, because it could be expanded)
                      (let* ((idx (seq-position keys key))
                             (next-key (seq-elt keys (1+ idx)))
                             (next-node (zotero-browser--find ewoc next-key))
                             (node (ewoc-enter-before ewoc next-node key)))
                        ;; And prefix it with the expand symbol
                        (zotero-browser--prefix (ewoc-location node) zotero-browser-expand-symbol)))))))))))))

(defun zotero-browser--itemtype-icon (itemtype)
  "Return the icon file for ITEMTYPE, or nil if none exists."
  (let* ((icon (concat  "treeitem-" itemtype ".png"))
         (dir (file-name-as-directory "img"))
         (filename (expand-file-name (concat dir icon) zotero-directory))
         (fallback (expand-file-name (concat dir "treeitem.png") zotero-directory)))
    (cond
     ((file-readable-p filename) filename)
     ((file-readable-p fallback) fallback)
     (t
      nil))))

(defun zotero-browser--attachment-icon (linkmode)
  "Return the icon file for LINKMODE, or nil if none exists."
  (let* ((icon (pcase linkmode
                 ("imported_url" "treeitem-attachment-snapshot.png")
                 ("imported_file" "treeitem-attachment-pdf.png")
                 ("linked_file" "treeitem-attachment-pdf-link.png")
                 ("linked_url" "treeitem-attachment-web-link.png")))
         (dir (file-name-as-directory "img"))
         (filename (expand-file-name (concat dir icon) zotero-directory)))
    (if (file-readable-p filename) filename nil)))

(defun zotero-browser--find-attachment (entry)
  "Return the path of the attachment of the current ENTRY."
  (when-let ((key (zotero-lib-plist-get* entry :data :key))
             (filename (zotero-lib-plist-get* entry :data :filename))
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

(defun zotero-browser--open-externally (path)
  "Open PATH in an external program.

This function is intented for graphical desktop environments on
GNU/Linux, macOS, or Microsoft Windows."
  (pcase system-type
    ('cygwin
     (start-process-shell-command "zotero-browser-open-externally" nil (concat "cygstart" " " (shell-quote-argument path))))
    ('darwin
     (start-process-shell-command "zotero-browser-open-externally" nil (concat "open" " " (shell-quote-argument path))))
    ('gnu/linux
     (start-process-shell-command "zotero-browser-open-externally" nil (concat "xdg-open" " " (shell-quote-argument path))))
    ('windows-nt
     (when (fboundp 'w32-shell-execute)
       (w32-shell-execute "open" path)))
    (_
     (error "Unable to determine default application on operating system %S" system-type))))

(defun zotero-browser--open-file (path)
  "Open the file at PATH.

The preferred method of opening is customizable by setting the
variable `zotero-browser-preferred-application'. If no
application is found, Emacs simply visits the file."
  (let ((file (expand-file-name path)))
    (pcase zotero-browser-preferred-application
      ('emacs
       (find-file-other-frame file))
      ('external
       (zotero-browser--open-externally file))
      ('mailcap
       (mailcap-parse-mailcaps)
       (let* ((mime-type (mailcap-extension-to-mime (file-name-extension file)))
	      (command (mailcap-mime-info mime-type))
              cmd)
         (if (stringp command)
	     (setq cmd command)
	   (setq cmd 'emacs))
         (cond
          ((and (stringp cmd) (not (string-match-p "^[[:space:]]*$" cmd)))
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

(defun zotero-browser--open-imported-file (entry)
  "Open attachment ENTRY."
  (let ((path (expand-file-name (zotero-browser--find-attachment entry))))
    (zotero-browser--open-file path)))

(defun zotero-browser--open-imported-url (entry)
  "Open attachment ENTRY."
  (let* ((path (expand-file-name (zotero-browser--find-attachment entry)))
         (contenttype (zotero-lib-plist-get* entry :data :contentType)))
    (if (equal contenttype "text/html")
        (browse-url-of-file path)
      (zotero-browser--open-file path))))

(defun zotero-browser--open-linked-file (entry)
  "Open attachment ENTRY."
  (let ((path (zotero-lib-plist-get* entry :data :path)))
    (zotero-browser--open-file path)))

(defun zotero-browser--open-linked-url (entry)
  "Open attachment ENTRY."
  (let ((url (zotero-lib-plist-get* entry :data :url)))
    (browse-url url)))

(defun zotero-browser--open-attachment (entry)
  "Open attachment ENTRY."
  (when-let ((linkmode (zotero-lib-plist-get* entry :data :linkMode)))
    (pcase linkmode
      ("imported_file" (zotero-browser--open-imported-file entry))
      ("imported_url" (zotero-browser--open-imported-url entry))
      ("linked_file" (zotero-browser--open-linked-file entry))
      ("linked_url" (zotero-browser--open-linked-url entry)))))

(defun zotero-browser--open-note (entry)
  "Edit note ENTRY.

The text is copied to a separate buffer. When done, exit with
`\\[zotero-edit-text-exit]'. This will remove the original text
in the source buffer, and replace it with the edited version."
  (when-let ((type zotero-browser-type)
             (id zotero-browser-id)
             (key (zotero-lib-plist-get* entry :data :key))
             (note (zotero-lib-plist-get* entry :data :note)))
    (let ((buffer (generate-new-buffer zotero-browser-note-buffer-name)))
      (with-current-buffer buffer
        (setq zotero-browser-type type
              zotero-browser-id id
              zotero-browser-key key)
        (insert note)
        (set-buffer-modified-p nil))
      ;; Switch to note buffer.
      (pop-to-buffer buffer zotero-browser-note-buffer-action)
      (zotero-browser-note-mode 1)
      (visual-line-mode 1)
      (message (substitute-command-keys zotero-browser-note-usage-message)))))

(defun zotero-browser--filename-base (data)
  "Return a base filename to match DATA.

The format can be changed by customizing
`zotero-browser-filename-keys' and
`zotero-browser-filename-max-length'."
  (let ((separator (car zotero-browser-filename-keys))
        (keys (cdr zotero-browser-filename-keys))
        result)
    (dolist (key keys)
      (pcase key
        (:creators
         (when-let ((creators (plist-get data :creators))
                    (names (when (seq-some (lambda (elt) (or (plist-get elt :lastName) (plist-get elt :name))) creators)
                             (seq-map (lambda (elt) (or (plist-get elt :lastName) (plist-get elt :name))) creators))))
           (push (seq-elt names 0) result)))
        (:year
         (when-let ((date (plist-get data :date))
                    (match (string-match "[[:digit:]]\\{4\\}" date))
                    (year (match-string 0 date)))
           (push year result)))
        (:title
         (when-let ((title (plist-get data :title)))
           (push title result)))
        ((pred keywordp)
         (when-let ((value (plist-get data key)))
           (push value result)))))
    (mapconcat (lambda (elt) (s-truncate zotero-browser-filename-max-length elt)) (nreverse result) separator)))

(defun zotero-browser--creators (entry)
  "Return creators in ENTRY."
  (when-let ((creators (zotero-lib-plist-get* entry :data :creators))
             (names (seq-map (lambda (elt) (or (plist-get elt :lastName) (plist-get elt :name))) creators)))
    (pcase (length names)
      (1 (seq-elt names 0))
      (2 (concat (seq-elt names 0)
                 " and "
                 (seq-elt names 1)))
      ((pred (< 2))
       (let* ((selection (seq-take names 1)))
         (concat (string-join selection ", ") " et al."))))))

(defun zotero-browser--version (entry)
  "Return the version in ENTRY."
  (when-let ((value (zotero-lib-plist-get* entry :data :version)))
    (number-to-string value)))

(defun zotero-browser--year (entry)
  "Return the year in ENTRY."
  (when-let ((date (zotero-lib-plist-get* entry :data :date))
             (match (string-match "[[:digit:]]\\{4\\}" date))
             (year (match-string 0 date)))
    year))

(defun zotero-browser--date-added (entry)
  "Return the added date in ENTRY."
  (when-let ((date (zotero-lib-plist-get* entry :data :dateAdded))
             (time (iso8601-parse date))
             (timestamp (encode-time time)))
    (format-time-string "%x %X" timestamp)))

(defun zotero-browser--date-modified (entry)
  "Return the modified date in ENTRY."
  (when-let ((date (zotero-lib-plist-get* entry :data :dateModified))
             (time (iso8601-parse date))
             (timestamp (encode-time time)))
    (format-time-string "%x %X" timestamp)))

(defun zotero-browser--note (entry)
  "Return the first line of note in ENTRY."
  (when-let ((note (zotero-lib-plist-get* entry :data :note))
             (text (replace-regexp-in-string "<[^>]+>" "" note)) ; Remove all HTML tags
             (match (string-match "^.+$" text)) ; Match first non-empty line
             (first-line (match-string-no-properties 0 text)))
    first-line))

(defun zotero-browser--header ()
  "Return the header of the items buffer."
  (let* ((sort-field (car zotero-browser-items-sort-field))
         (flip (cdr zotero-browser-items-sort-field))
         (pos (max zotero-browser-padding 0))
         (prefix (make-string pos ?\s))
         (padding-right 1)
         (props `(line-prefix ,prefix
                              wrap-prefix ,prefix))
         (button-props `(help-echo "Click to sort by column"
			           mouse-face header-line-highlight
			           keymap ,zotero-browser-header-keymap
                                   line-prefix ,prefix
                                   wrap-prefix ,prefix
                                   rear-nonsticky t))
         (count (length zotero-browser-item-columns))
         columns)
    (when (and (display-graphic-p) zotero-browser-icons)
      (let ((field :itemType))
        (setq pos (+ pos 2))
        (push
         (cond
          ((eq field sort-field)
           ;; The selected sort column
           (apply #'propertize (if flip "▴" "▾")
                  'face '(header-line bold)
                  'zotero-browser-field field
                  button-props))
          ;; Unselected sortable column.
	  (t
           (apply #'propertize " "
                  'face 'header-line
                  'zotero-browser-field field
                  button-props)))
         columns))
      (setq pos (+ pos padding-right))
      (push (apply #'propertize " "
                   'display `(space :align-to ,pos)
                   'face 'header-line
                   props)
            columns))
    (dotimes (i count)
      (let* ((column (nth i zotero-browser-item-columns))
             (field (car column))
             (width (cdr column))
             (label (s-capitalized-words (zotero-lib-keyword->string field)))
             (label (truncate-string-to-width label width nil nil t t)))
        (push
         (cond
          ((eq field sort-field)
           ;; The selected sort column
           (apply #'propertize (concat label
			               (cond
			                ((> (+ 2 (length label)) width) "")
			                (flip " ▴")
			                (t " ▾")))
                  'face '(header-line bold)
                  'zotero-browser-field field
                  button-props))
          ;; Unselected sortable column.
	  (t
           (apply #'propertize label
                  'face 'header-line
                  'zotero-browser-field field
                  button-props)))
         columns)
        (setq pos (+ pos width))
        (push (apply #'propertize " "
                     'display `(space :align-to ,pos)
                     'face 'header-line
                     'zotero-browser-field field
                     button-props)
              columns)
        (unless (eq i (1- count))
          (setq pos (+ pos padding-right))
          (push (apply #'propertize " "
                       'display `(space :align-to ,pos)
                       'face 'header-line
                       props)
                columns))))
    (apply #'concat (nreverse columns))))

(defun zotero-browser--print-library (key)
  "Insert library KEY at point."
  (let* ((library (zotero-cache-library nil key))
         (type (plist-get library :type))
         (id (plist-get library :id))
         (pos (max zotero-browser-padding 0))
         (padding-right 1)
         (prefix (make-string pos ?\s))
         (props `(line-prefix ,prefix
                              wrap-prefix ,prefix
                              mouse-face highlight
                              help-echo "mouse-1: open library"
                              keymap ,zotero-browser-library-keymap))
         (count (length zotero-browser-collection-columns)))
    (when (and (display-graphic-p) zotero-browser-icons)
      (let* ((icon (pcase type
                     ("user" "treesource-library.png")
                     ("group" "treesource-groups.png")))
             (dir (file-name-as-directory "img"))
             (file (expand-file-name (concat dir icon) zotero-directory))
             (image (create-image file 'png nil :height (window-font-height))))
        (insert (apply #'propertize type
                       'display image
                       'rear-nonsticky t
                       props))
        (setq pos (+ pos 2 padding-right))
        (insert (apply #'propertize " "
                       'display `(space :align-to ,pos)
                       props))))
    (dotimes (i count)
      (let* ((column (nth i zotero-browser-collection-columns))
             (field (car column))
             (width (cdr column))
             (value (pcase field
                      (:name
                       (let ((value (pcase type
                                      ("user" "User library")
                                      ("group" (let ((group (zotero-cache-group id)))
                                                 (zotero-lib-plist-get* group :data field))))))
                         value))
                      (:version
                       (when-let ((value (plist-get library field)))
                         (number-to-string value)))
                      ((or :library :write))
                      (:last-sync
                       (when-let ((value (plist-get library field)))
                         (format-time-string "%c" value)))
                      ((or :group-type :description :url :libraryEditing :libraryReading :fileEditing)
                       (when (equal type "group")
                         (let* ((group (zotero-cache-group id))
                                (value (zotero-lib-plist-get* group :data field)))
                           value)))
                      ((pred keywordp)
                       (when-let ((value (plist-get library field)))
                         value))))
             (value-width (length value))
             (string (cond
                      ((null value)
                       "")
                      ((> value-width width)
                       (propertize (truncate-string-to-width value width nil nil t t)
                                   'help-echo value))
                      (t
                       value))))
        (insert (apply #'propertize string props))
        (unless (eq i (1- count))
          (setq pos (+ pos width padding-right))
          (insert (apply #'propertize " "
                         'display `(space :align-to ,pos)
                         props)))))))

(defun zotero-browser--print-collection (key)
  "Insert collection KEY at point."
  (let ((padding (max zotero-browser-padding 0))
        (padding-right 1))
    (pcase key
      ('unfiled
       (let* ((level 0)
              (pos (+ padding level))
              (prefix (make-string pos ?\s))
              (props `(line-prefix ,prefix
                                   wrap-prefix ,prefix
                                   mouse-face highlight
                                   help-echo "mouse-1: open collection; mouse-3: popup menu"
                                   keymap ,zotero-browser-collection-keymap)))
         (when (and (display-graphic-p) zotero-browser-icons)
           (let* ((icon "treesource-unfiled.png")
                  (dir (file-name-as-directory "img"))
                  (file (expand-file-name (concat dir icon) zotero-directory))
                  (image (create-image file 'png nil :height (window-font-height))))
             (insert (apply #'propertize "unfiled"
                            'display image
                            'rear-nonsticky t
                            props))
             (setq pos (+ pos 2 padding-right))
             (insert (apply #'propertize " "
                            'display `(space :align-to ,pos)
                            props))))
         (insert (apply #'propertize "Unfiled" props))))
      ('trash
       (let* ((level 0)
              (pos (+ padding level))
              (prefix (make-string pos ?\s))
              (props `(line-prefix ,prefix
                                   wrap-prefix ,prefix
                                   mouse-face highlight
                                   keymap ,zotero-browser-collection-keymap)))
         (when (and (display-graphic-p) zotero-browser-icons)
           (let* ((icon "treesource-trash.png")
                  (dir (file-name-as-directory "img"))
                  (file (expand-file-name (concat dir icon) zotero-directory))
                  (image (create-image file 'png nil :height (window-font-height))))
             (insert (apply #'propertize "trash"
                            'display image
                            'rear-nonsticky t
                            props))
             (setq pos (+ pos 2 padding-right))
             (insert (apply #'propertize " "
                            'display `(space :align-to ,pos)
                            props))))
         (insert (apply #'propertize "Trash" props))))
      (_
       (let* ((entry (zotero-cache-synccache "collection" key zotero-browser-type zotero-browser-id))
              (level (zotero-browser--level key))
              (pos (+ padding level))
              (prefix (make-string pos ?\s))
              (props `(line-prefix ,prefix
                                   wrap-prefix ,prefix
                                   mouse-face highlight
                                   keymap ,zotero-browser-collection-keymap))
              (count (length zotero-browser-collection-columns)))
         (when (and (display-graphic-p) zotero-browser-icons)
           (let* ((icon "treesource-collection.png")
                  (dir (file-name-as-directory "img"))
                  (file (expand-file-name (concat dir icon) zotero-directory))
                  (image (create-image file 'png nil :height (window-font-height))))
             (insert (apply #'propertize "collection"
                            'display image
                            'rear-nonsticky t
                            props))
             (setq pos (+ pos 2 padding-right))
             (insert (apply #'propertize " "
                            'display `(space :align-to ,pos)
                            props))))
         (dotimes (i count)
           (let* ((column (nth i zotero-browser-collection-columns))
                  (field (car column))
                  ;; The width of the first column is shorter for higher level
                  ;; items to allow for indentation
                  (width (if (eq i 0)
                             (- (cdr column) level)
                           (cdr column)))
                  (value (pcase field
                           (:version
                            (zotero-browser--version entry))
                           ((pred keywordp)
                            (zotero-lib-plist-get* entry :data field))))
                  (value-width (length value))
                  (string (cond
                           ((null value)
                            "")
                           ((> value-width width)
                            (propertize (truncate-string-to-width value (- width level) nil nil t t)
                                        'help-echo value))
                           (t
                            value))))
             (insert (apply #'propertize string props))
             (unless (eq i (1- count))
               (setq pos (+ pos width padding-right))
               (insert (apply #'propertize " "
                              'display `(space :align-to ,pos)
                              props))))))))))

(defun zotero-browser--print-item (key)
  "Insert entry KEY at point."
  (let* ((entry (zotero-cache-synccache "item" key zotero-browser-type zotero-browser-id))
         (itemtype (zotero-lib-plist-get* entry :data :itemType))
         (deleted-p (zotero-lib-plist-get* entry :data :deleted))
         (level (zotero-browser--level key))
         (padding (max zotero-browser-padding 0))
         (padding-right 1)
         (pos (+ padding level))
         (prefix (make-string pos ?\s))
         (keymap (cond
                  (deleted-p
                   zotero-browser-trash-item-keymap)
                  ((equal itemtype "attachment")
                   (pcase level
                     ;; Top-level
                     (0 zotero-browser-top-attachment-keymap)
                     ;; Child
                     (1 zotero-browser-child-attachment-keymap)))
                  ((equal itemtype "note")
                   zotero-browser-note-keymap)
                  (t
                   zotero-browser-item-keymap)))
         (props `(line-prefix ,prefix
                              wrap-prefix ,prefix
                              mouse-face highlight
                              keymap ,keymap))
         (count (length zotero-browser-item-columns)))
    (when (and (display-graphic-p) zotero-browser-icons)
      (let* ((file (if (equal itemtype "attachment")
                       (let ((linkmode (zotero-lib-plist-get* entry :data :linkMode)))
                         (zotero-browser--attachment-icon linkmode))
                     (zotero-browser--itemtype-icon itemtype)))
             (image (create-image file 'png nil :height (window-font-height))))
        (insert (apply #'propertize itemtype
                       'display image
                       'rear-nonsticky t
                       props))
        (setq pos (+ pos 2 padding-right))
        (insert (apply #'propertize " "
                       'display `(space :align-to ,pos)
                       props))))
    (dotimes (i count)
      (let* ((column (nth i zotero-browser-item-columns))
             (field (car column))
             ;; The width of the first column is shorter for higher level
             ;; items to allow for indentation
             (width (if (eq i 0)
                        (- (cdr column) level)
                      (cdr column)))
             (value (pcase field
                      ;; Insert the first line of the note
                      ((and :title (guard (equal itemtype "note")))
                       (zotero-browser--note entry))
                      (:creators
                       (zotero-browser--creators entry))
                      (:version
                       (zotero-browser--version entry))
                      (:year
                       (zotero-browser--year entry))
                      (:dateAdded
                       (zotero-browser--date-added entry))
                      (:dateModified
                       (zotero-browser--date-modified entry))
                      (:note
                       (zotero-browser--note entry))
                      ((pred keywordp)
                       (zotero-lib-plist-get* entry :data field))))
             (value-width (length value))
             (string (cond
                      ((null value)
                       "")
                      ((> value-width width)
                       (propertize (truncate-string-to-width value (- width level) nil nil t t)
                                   'help-echo value))
                      (t
                       value))))
        (insert (apply #'propertize string props))
        (unless (eq i (1- count))
          (setq pos (+ pos width padding-right))
          (insert (apply #'propertize " "
                         'display `(space :align-to ,pos)
                         props)))))))

;;;; Commands

(defun zotero-browser-open ()
  "Open attachment at point."
  (interactive)
  (zotero-browser-ensure-browser-buffer)
  (let* ((ewoc zotero-browser-ewoc)
         (type zotero-browser-type)
         (id zotero-browser-id)
         (key (ewoc-data (ewoc-locate ewoc)))
         (entry (zotero-cache-synccache "item" key type id))
         (itemtype (zotero-lib-plist-get* entry :data :itemType)))
    (cond
     ((equal itemtype "note")
      (zotero-browser--open-note entry))
     ((equal itemtype "attachment")
      (zotero-browser--open-attachment entry))
     (t
      (ht-each (lambda (_key value)
                 (zotero-browser--open-attachment value))
               (zotero-browser--children key))))))

(defun zotero-browser-open-directory ()
  "Open directory of the attachment of the current entry."
  (interactive)
  (zotero-browser-ensure-items-mode)
  (if-let ((ewoc zotero-browser-ewoc)
           (type zotero-browser-type)
           (id zotero-browser-id)
           (key (ewoc-data (ewoc-locate ewoc)))
           (entry (zotero-cache-synccache "item" key type id))
           (itemtype (zotero-lib-plist-get* entry :data :itemType)))
      (cond
       ((equal itemtype "attachment")
        (when-let ((file (zotero-browser--find-attachment entry))
                   (dir (file-name-directory file)))
          (if (file-exists-p dir)
              (dired dir)
            (user-error "Directory doesn't exist"))))
       (t
        (when-let ((children (zotero-browser--children key))
                   ;; Find the first child that is an attachment
                   (child (ht-find (lambda (_key value)
                                     (let ((itemtype (zotero-lib-plist-get* value :data :itemType)))
                                       (equal itemtype "attachment")))
                                   children))
                   (file (zotero-browser--find-attachment (cadr child)))
                   (dir (file-name-directory file)))
          (if (file-exists-p dir)
              (dired dir)
            (user-error "Directory doesn't exist")))))))

(defun zotero-browser-ensure-browser-buffer ()
  "Check if the current buffer is a Zotero browser buffer."
  (unless (or (eq major-mode 'zotero-browser-libraries-mode)
              (eq major-mode 'zotero-browser-collections-mode)
              (eq major-mode 'zotero-browser-items-mode))
    (user-error "Current buffer is not a Zotero browser buffer")))

(defun zotero-browser-ensure-items-mode ()
  "Check if the current buffer is a Zotero items buffer."
  (unless (eq major-mode 'zotero-browser-items-mode)
    (user-error "Current buffer is not a Zotero items buffer")))

(defun zotero-browser-ensure-note-buffer ()
  "Check if the current buffer is a Zotero note buffer."
  (let ((mode 'zotero-browser-note-mode))
    (unless (and (fboundp mode)
                 (symbol-value mode))
      (user-error "Current buffer is not a Zotero note buffer"))))

(defun zotero-browser-ensure-write-access ()
  "Check if the library in the current buffer has write access."
  (let* ((type zotero-browser-type)
         (id zotero-browser-id)
         (library (zotero-cache-library type id)))
    (unless (zotero-cache-write-access-p library)
      (user-error "Library %s had no write access" id))))

(defun zotero-browser-ensure-item-at-point ()
  "Check if there is an item at point."
  (unless (ewoc-locate zotero-browser-ewoc)
    (user-error "No item at point")))

(defun zotero-browser-hl-range ()
  "Return highlight range."
  (when-let ((ewoc zotero-browser-ewoc)
             (node (ewoc-locate ewoc)))
    (let* ((beg (ewoc-location node))
           (next-node (ewoc-next ewoc node))
           (end (if next-node
                    (save-excursion
                      (goto-char (ewoc-location next-node))
                      (line-end-position 0))
                  (save-excursion
                    (while (not (looking-at "[ \t]*$"))
                      (forward-line)))
                  (line-end-position))))
      (cons beg end))))

(defun zotero-browser-revert ()
  "Reload the current buffer."
  (interactive)
  (zotero-browser-ensure-browser-buffer)
  (let ((pos (point)))
    (pcase major-mode
      ('zotero-browser-libraries-mode
       (zotero-browser-libraries))
      ('zotero-browser-collections-mode
       (zotero-browser-collections zotero-browser-resource zotero-browser-type zotero-browser-id))
      ('zotero-browser-items-mode
       (zotero-browser-items zotero-browser-resource zotero-browser-collection zotero-browser-type zotero-browser-id)))
    (goto-char pos)))

(defun zotero-browser-next (arg)
  "Move to the next item.
With ARG, repeats or can move backward if negative."
  (interactive "p")
  (zotero-browser-ensure-browser-buffer)
  (let* ((ewoc zotero-browser-ewoc)
         (count (if arg (abs arg) 1))
         (backward-p (and arg (< arg 0)))
         (move (if backward-p #'ewoc-goto-prev #'ewoc-goto-next)))
    (when (ewoc-nth ewoc 0)
      (funcall move ewoc count))))

(defun zotero-browser-prev (arg)
  "Move to the previous item.
With ARG, repeats or can move forward if negative."
  (interactive "p")
  (zotero-browser-next (if arg (- arg) -1)))

(defun zotero-browser-forward-same-level (arg)
  "Move to the next item at same level.
Stop at the first and last child of a parent. With ARG, repeats
or can move backward if negative."
  (interactive "p")
  (zotero-browser-ensure-browser-buffer)
  (let* ((ewoc zotero-browser-ewoc)
         (node (ewoc-locate ewoc))
         (level (zotero-browser--level (ewoc-data node)))
         (count (if arg (abs arg) 1))
         (backward-p (and arg (< arg 0)))
         (move (if backward-p #'ewoc-prev #'ewoc-next)))
    (dotimes (_ count)
      (let ((next-node (funcall move ewoc node))
            next-level)
        (while (not (or
                     ;; Same level
                     (eq next-level level)
                     ;; Lower level
                     (and next-level (< next-level level))
                     ;; End of ewoc
                     (not next-node)))
          (setq next-level (zotero-browser--level (ewoc-data next-node)))
          (if (eq next-level level)
              (setq node next-node)
            (setq next-node (funcall move ewoc next-node))))))
    (ewoc-goto-node ewoc node)))

(defun zotero-browser-backward-same-level (arg)
  "Move to the previous item at same level.
Stop at the  first and last child of a  parent. With ARG, repeats
or can move forward if negative."
  (interactive "p")
  (zotero-browser-forward-same-level (if arg (- arg) -1)))

(defun zotero-browser-up ()
  "Move point to the parent item."
  (interactive)
  (zotero-browser-ensure-browser-buffer)
  (zotero-browser-ensure-item-at-point)
  (let ((ewoc zotero-browser-ewoc))
    (when (ewoc-nth ewoc 0)
      (let* ((node (ewoc-locate ewoc))
             (key (ewoc-data node))
             (parent (zotero-browser--parent key))
             (n 0))
        (when parent
          (while
              (let* ((node (ewoc-nth ewoc n))
                     (key (ewoc-data node)))
                (ewoc-goto-node ewoc node)
                (prog1
                    (not (equal key parent))
                  (setq n (1+ n))))))))))

(defun zotero-browser-next-collection (arg)
  "Move point to the next collection.
With ARG, repeats or can move backward if negative."
  (interactive "p")
  (with-current-buffer zotero-browser-collections-buffer-name
    (zotero-browser-next arg)))

(defun zotero-browser-prev-collection (arg)
  "Move point to the previous collection.
With ARG, repeats or can move forward if negative."
  (interactive "p")
  (with-current-buffer zotero-browser-collections-buffer-name
    (zotero-browser-prev arg)))

(defun zotero-browser-up-collection ()
  "Move point to the parent collection."
  (interactive)
  (with-current-buffer zotero-browser-collections-buffer-name
    (zotero-browser-up)))

(defun zotero-browser-all-items ()
  "Show all items."
  (interactive)
  (zotero-browser-ensure-browser-buffer)
  (zotero-browser-items "items" nil zotero-browser-type zotero-browser-id))

(defun zotero-browser-unfiled-items ()
  "Show unfiled items."
  (interactive)
  (zotero-browser-ensure-browser-buffer)
  (zotero-browser-items "items-top" nil zotero-browser-type zotero-browser-id))

(defun zotero-browser-trash-items ()
  "Show trashed items."
  (interactive)
  (zotero-browser-ensure-browser-buffer)
  (zotero-browser-items "trash-items" nil zotero-browser-type zotero-browser-id))

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
  "Expand children till level NUM.

If NUM is omitted or nil, expand till level 1."
  (interactive "P")
  (zotero-browser-ensure-browser-buffer)
  (let* ((ewoc zotero-browser-ewoc)
         (num (or num 1))
         (inhibit-read-only t))
    (save-excursion
      (when-let ((node (ewoc-nth ewoc 0)))
        (ewoc-goto-node ewoc node)
        (while
            (let* ((node (ewoc-locate ewoc))
                   (key (ewoc-data node))
                   (level (zotero-browser--level key)))
              (when (zotero-browser--has-children-p node)
                (cond
                 ((and (zotero-browser--expanded-p ewoc node)
                       (>= level num))
                  (zotero-browser--collapse ewoc node))
                 ((and (not (zotero-browser--expanded-p ewoc node))
                       (or (< level num) (eq num 0)))
                  (zotero-browser--expand ewoc node))
                 (t
                  (zotero-browser--prefix (ewoc-location node) zotero-browser-collapse-symbol))))
              (prog1
                  ;; End-test of while loop
                  (ewoc-next ewoc node)
                (ewoc-goto-next ewoc 1))))))))

(defun zotero-browser-drag (event)
  "Drag an item with the mouse.
EVENT is the starting mouse event of the drag action."
  (interactive "e")
  (let* ((start-pos (event-start event))
         (start-point (posn-point start-pos))
         (start-window (posn-window start-pos))
         (start-buffer (window-buffer start-window))
         (start-mode (with-current-buffer start-buffer major-mode))
         (end-pos (event-end event))
         (end-point (posn-point end-pos))
         (end-window (posn-window end-pos))
         (end-buffer (window-buffer end-window))
         (end-mode (with-current-buffer end-buffer major-mode)))
    (cond
     ;; Drag within items buffer
     ((and (eq start-mode 'zotero-browser-items-mode)
           (eq end-mode 'zotero-browser-items-mode))
      (let* ((start-ewoc (with-current-buffer start-buffer zotero-browser-ewoc))
             (start-node (ewoc-locate start-ewoc start-point))
             (start-key (ewoc-data start-node))
             (end-ewoc (with-current-buffer end-buffer zotero-browser-ewoc))
             (end-node (ewoc-locate end-ewoc end-point))
             (end-key (ewoc-data end-node)))
        (zotero-cache-move-to-parent start-key
                                     end-key
                                     zotero-browser-type
                                     zotero-browser-id)))
     ;; Drag from items buffer to collections buffer
     ((and (eq start-mode 'zotero-browser-items-mode)
           (eq end-mode 'zotero-browser-collections-mode))
      (let* ((start-ewoc (with-current-buffer start-buffer zotero-browser-ewoc))
             (start-node (ewoc-locate start-ewoc start-point))
             (start-key (ewoc-data start-node))
             (end-ewoc (with-current-buffer end-buffer zotero-browser-ewoc))
             (end-node (ewoc-locate end-ewoc end-point))
             (end-key (ewoc-data end-node)))
        (cond
         ;; Drag item from one collection to another
         ((and (stringp zotero-browser-collection)
               (stringp start-key)
               (stringp end-key))
          (zotero-cache-substitute-collection start-key
                                              end-key
                                              zotero-browser-collection
                                              zotero-browser-type
                                              zotero-browser-id)
          (run-hook-with-args 'zotero-browser-after-change-functions start-key)
          (ht-each (lambda (key _value)
                     (run-hook-with-args 'zotero-browser-after-change-functions key))
                   (zotero-browser--children start-key)))
         ;; Drag unfiled item to collection
         ((and (eq zotero-browser-collection 'unfiled)
               (stringp start-key)
               (stringp end-key))
          (zotero-cache-add-to-collection start-key
                                          end-key
                                          zotero-browser-type
                                          zotero-browser-id)
          (run-hook-with-args 'zotero-browser-after-change-functions start-key)
          (ht-each (lambda (key _value)
                     (run-hook-with-args 'zotero-browser-after-change-functions key))
                   (zotero-browser--children start-key)))
         ;; Drag trashed item to collection
         ((and (eq zotero-browser-collection 'trash)
               (stringp start-key)
               (stringp end-key))
          (zotero-cache-restore start-key
                                zotero-browser-type
                                zotero-browser-id)
          (zotero-cache-add-to-collection start-key
                                          end-key
                                          zotero-browser-type
                                          zotero-browser-id)
          (run-hook-with-args 'zotero-browser-after-change-functions start-key)
          ;; Restore all children from trash as well
          (ht-each (lambda (key _value)
                     (zotero-cache-restore key zotero-browser-type zotero-browser-id)
                     (run-hook-with-args 'zotero-browser-after-change-functions key))
                   (zotero-browser--children start-key)))
         ;; Drag item to unfiled
         ((and (stringp start-key)
               (eq end-key 'unfiled))
          (zotero-cache-remove-item-from-collection start-key
                                                    zotero-browser-collection
                                                    zotero-browser-type
                                                    zotero-browser-id)
          (run-hook-with-args 'zotero-browser-after-change-functions start-key)
          (ht-each (lambda (key _value)
                     (run-hook-with-args 'zotero-browser-after-change-functions key))
                   (zotero-browser--children start-key)))
         ;; Drag item to trash
         ((and (stringp start-key)
               (eq end-key 'trash))
          (zotero-cache-trash start-key
                              zotero-browser-type
                              zotero-browser-id)
          (run-hook-with-args 'zotero-browser-after-change-functions start-key)
          ;; Move all children to trash as well
          (ht-each (lambda (key _value)
                     (zotero-cache-trash key zotero-browser-type zotero-browser-id)
                     (run-hook-with-args 'zotero-browser-after-change-functions key))
                   (zotero-browser--children start-key)))))))))

(defun zotero-browser-edit ()
  "Edit current entry."
  (interactive)
  (zotero-browser-ensure-browser-buffer)
  (zotero-browser-ensure-write-access)
  ;; (zotero-browser-ensure-item-at-point)
  (let* ((type zotero-browser-type)
         (id zotero-browser-id)
         (ewoc zotero-browser-ewoc)
         (node (ewoc-locate ewoc))
         (key (ewoc-data node)))
    (pcase major-mode
      ('zotero-browser-libraries-mode
       (if (zotero-cache-group key)
           (zotero-group-settings key)
         (user-error "Library %s is not a group" key)))
      ('zotero-browser-collections-mode
       (let* ((entry (zotero-cache-synccache "collection" key type id))
              (data (plist-get entry :data)))
         (pop-to-buffer (zotero-edit-collection data type id) zotero-browser-edit-buffer-action)))
      ('zotero-browser-items-mode
       (let* ((entry (zotero-cache-synccache "item" key type id))
              (data (plist-get entry :data)))
         (pop-to-buffer (zotero-edit-item data type id) zotero-browser-edit-buffer-action))))))

(defun zotero-browser-move-to-parent (&optional arg)
  "Move current entry to a parent item.
With a `C-u' prefix, move to top level.
Optional argument ARG is the prefix argument."
  (interactive "P")
  (zotero-browser-ensure-items-mode)
  (zotero-browser-ensure-write-access)
  (zotero-browser-ensure-item-at-point)
  (let* ((inhibit-read-only t)
         (type zotero-browser-type)
         (id zotero-browser-id)
         (ewoc zotero-browser-ewoc)
         (node (ewoc-locate ewoc))
         (key (ewoc-data node)))
    (if (equal arg '(4))
        (zotero-cache-move-to-parent key nil type id)
      (let* ((table (zotero-cache-synccache "items" nil type id))
             (choices (zotero-cache-parentitems table))
             (name (completing-read "Select parent item:" choices nil t))
             (parent (cdr (assoc name choices))))
        (zotero-cache-move-to-parent key parent type id)))
    (run-hook-with-args 'zotero-browser-after-change-functions key)))

(defun zotero-browser-move-to-collection ()
  "Move current entry to a collection."
  (interactive)
  (zotero-browser-ensure-items-mode)
  (zotero-browser-ensure-write-access)
  (zotero-browser-ensure-item-at-point)
  (let* ((inhibit-read-only t)
         (ewoc zotero-browser-ewoc)
         (type zotero-browser-type)
         (id zotero-browser-id)
         (node (ewoc-locate ewoc))
         (key (ewoc-data node))
         (table (zotero-cache-synccache "collections" nil type id))
         (choices (zotero-cache-parentcollections table))
         (name (completing-read "Select collection:" choices nil t))
         (collection (cdr (assoc name choices))))
    (pcase zotero-browser-collection
      ((pred stringp)
       (zotero-cache-substitute-collection key collection zotero-browser-collection type id))
      ('unfiled
       (zotero-cache-add-to-collection key
                                       collection
                                       zotero-browser-type
                                       zotero-browser-id))
      ('trash
       (zotero-cache-restore key
                             zotero-browser-type
                             zotero-browser-id)
       (zotero-cache-add-to-collection key
                                       collection
                                       zotero-browser-type
                                       zotero-browser-id)
       ;; Restore all children from trash as well
       (ht-each (lambda (key _value)
                  (zotero-cache-restore key zotero-browser-type zotero-browser-id)
                  (run-hook-with-args 'zotero-browser-after-change-functions key))
                (zotero-browser--children key))))
    (run-hook-with-args 'zotero-browser-after-change-functions key)
    (ht-each (lambda (key _value)
               (run-hook-with-args 'zotero-browser-after-change-functions key))
             (zotero-browser--children key))))

(defun zotero-browser-copy-to-collection ()
  "Copy current entry to another collection."
  (interactive)
  (zotero-browser-ensure-items-mode)
  (zotero-browser-ensure-write-access)
  (zotero-browser-ensure-item-at-point)
  (let* ((inhibit-read-only t)
         (ewoc zotero-browser-ewoc)
         (type zotero-browser-type)
         (id zotero-browser-id)
         (node (ewoc-locate ewoc))
         (key (ewoc-data node))
         (table (zotero-cache-synccache "collections" nil type id))
         (choices (zotero-cache-parentcollections table))
         (name (completing-read "Select collection:" choices nil t))
         (collection (cdr (assoc name choices))))
    (zotero-cache-add-to-collection key collection type id)
    (run-hook-with-args 'zotero-browser-after-change-functions key)
    (ht-each (lambda (key _value)
               (run-hook-with-args 'zotero-browser-after-change-functions key))
             (zotero-browser--children key))))

(defun zotero-browser-remove-from-collection ()
  "Remove current entry from the collection."
  (interactive)
  (zotero-browser-ensure-items-mode)
  (zotero-browser-ensure-write-access)
  (zotero-browser-ensure-item-at-point)
  (let* ((inhibit-read-only t)
         (type zotero-browser-type)
         (id zotero-browser-id)
         (ewoc zotero-browser-ewoc)
         (node (ewoc-locate ewoc))
         (key (ewoc-data node))
         (collection zotero-browser-collection))
    (zotero-cache-remove-item-from-collection key collection type id)
    (run-hook-with-args 'zotero-browser-after-change-functions key)
    (ht-each (lambda (key _value)
               (run-hook-with-args 'zotero-browser-after-change-functions key))
             (zotero-browser--children key))))

(defun zotero-browser-move-to-trash ()
  "Move current entry to trash.
If region is active, trash entries in active region instead."
  (interactive)
  (zotero-browser-ensure-browser-buffer)
  (zotero-browser-ensure-write-access)
  (zotero-browser-ensure-item-at-point)
  (when (equal zotero-browser-resource "trash-items")
    (user-error "Current buffer is a Zotero trash buffer"))
  (when (eq major-mode 'zotero-browser-libraries-mode)
    (user-error "Trashing libraries is not supported"))
  (when (eq major-mode 'zotero-browser-collections-mode)
    (user-error "Trashing collections is not supported"))
  (let* ((inhibit-read-only t)
         (type zotero-browser-type)
         (id zotero-browser-id)
         (ewoc zotero-browser-ewoc)
         (keys (zotero-browser--keys ewoc)))
    (dolist (key keys)
      (zotero-cache-trash key type id)
      (run-hook-with-args 'zotero-browser-after-change-functions key)
      ;; Move all children to trash as well
      (ht-each (lambda (key _value) (zotero-cache-trash key type id)
                 (run-hook-with-args 'zotero-browser-after-change-functions key))
               (zotero-browser--children key)))))

(defun zotero-browser-restore-from-trash ()
  "Restore current entry from trash.
If region is active, restore entries in active region instead."
  (interactive)
  (zotero-browser-ensure-items-mode)
  (zotero-browser-ensure-write-access)
  (zotero-browser-ensure-item-at-point)
  (unless (equal zotero-browser-resource "trash-items")
    (user-error "Current buffer is not a Zotero trash buffer"))
  (let* ((inhibit-read-only t)
         (type zotero-browser-type)
         (id zotero-browser-id)
         (ewoc zotero-browser-ewoc)
         (keys (zotero-browser--keys ewoc)))
    (dolist (key keys)
      (zotero-cache-restore key type id)
      (run-hook-with-args 'zotero-browser-after-change-functions key)
      ;; Restore all children from trash as well
      (ht-each (lambda (key _value) (zotero-cache-restore key type id)
                 (run-hook-with-args 'zotero-browser-after-change-functions key))
               (zotero-browser--children key)))))

(defun zotero-browser-delete ()
  "Delete current entry.
If region is active, delete entries in active region instead."
  (interactive)
  (zotero-browser-ensure-browser-buffer)
  (zotero-browser-ensure-write-access)
  (zotero-browser-ensure-item-at-point)
  (let* ((inhibit-read-only t)
         (type zotero-browser-type)
         (id zotero-browser-id)
         (resource (pcase major-mode
                     ('zotero-browser-libraries-mode (user-error "Deleting libraries is not supported"))
                     ('zotero-browser-collections-mode "collections")
                     ('zotero-browser-items-mode "items")))
         (ewoc zotero-browser-ewoc)
         (keys (zotero-browser--keys ewoc)))
    (dolist (key keys)
      (zotero-cache-delete resource key type id)
      (run-hook-with-args 'zotero-browser-after-change-functions key)
      (ht-each (lambda (key _value)
                 (run-hook-with-args 'zotero-browser-after-change-functions key))
               (zotero-browser--children key)))))

(defun zotero-browser-create ()
  "Create a new collection or item."
  (interactive)
  (zotero-browser-ensure-browser-buffer)
  (zotero-browser-ensure-write-access)
  (let* ((type zotero-browser-type)
         (id zotero-browser-id))
    (pcase major-mode
      ('zotero-browser-libraries-mode
       (zotero-create-group))
      ('zotero-browser-collections-mode
       (let ((template (copy-tree (zotero-collection-template))))
         (pop-to-buffer (zotero-edit-collection template type id) zotero-browser-edit-buffer-action)))
      ('zotero-browser-items-mode
       (let* ((itemtype (completing-read "Select an item type: " (zotero-cache-itemtypes) nil t nil nil zotero-browser-default-itemtypes ))
              (template (copy-tree (zotero-cache-item-template itemtype)))
              (collection zotero-browser-collection)
              (data (if (stringp collection) (plist-put template :collections (vector collection)) template)))
         (cl-pushnew itemtype zotero-browser-default-itemtypes :test #'equal)
         (pop-to-buffer (zotero-edit-item data type id) zotero-browser-edit-buffer-action))))))

(defun zotero-browser-create-note (&optional arg)
  "Create a new note.
With a `C-u' prefix, create a new top level note.
Optional argument ARG is the prefix argument."
  (interactive "P")
  (zotero-browser-ensure-items-mode)
  (zotero-browser-ensure-write-access)
  (let* ((type zotero-browser-type)
         (id zotero-browser-id)
         (collection zotero-browser-collection)
         (ewoc zotero-browser-ewoc)
         (node (ewoc-locate ewoc))
         ;; top level notes can be created by excluding the parentItem property
         ;; or setting it to false.
         (parent (cond
                  ((equal arg '(4)) nil)
                  ((null node) nil)
                  (t (ewoc-data node))))
         (template (copy-tree (zotero-cache-item-template "note")))
         (data (cond
                (parent (plist-put template :parentItem parent))
                ((stringp collection) (plist-put template :collections (vector collection)))
                (t template))))
    (pop-to-buffer (zotero-edit-item data type id) zotero-browser-edit-buffer-action)))

(defun zotero-browser-create-attachment (&optional arg)
  "Create a new attachment with the current entry as parent.
With a `C-u' prefix, create a new top level attachment.
Optional argument ARG is the prefix argument.

Only file attachments (imported_file/linked_file) and PDF
imported web attachments (imported_url with content type
application/pdf) are allowed as top level items, as in the Zotero
client."
  (interactive "P")
  (zotero-browser-ensure-items-mode)
  (zotero-browser-ensure-write-access)
  (let* ((inhibit-read-only t)
         (type zotero-browser-type)
         (id zotero-browser-id)
         (collection zotero-browser-collection)
         (ewoc zotero-browser-ewoc)
         (node (ewoc-locate ewoc))
         ;; top level attachments can be created by excluding the parentItem
         ;; property or setting it to false.
         (parent (cond
                  ((equal arg '(4)) nil)
                  ((null node) nil)
                  (t (ewoc-data node))))
         (linkmode (completing-read "Select a linkmode: " (zotero-attachment-linkmodes) nil t nil nil zotero-browser-default-linkmodes)))
    (cl-pushnew linkmode zotero-browser-default-linkmodes :test #'equal)
    (pcase linkmode
      ("imported_file"
       (let* ((file (expand-file-name (read-file-name "Select file: " nil nil t)))
              (attributes (zotero-file-attributes file))
              (filename (file-name-nondirectory file))
              (content-type (plist-get attributes :content-type))
              (accessdate (plist-get attributes :accessdate))
              (template (copy-tree (zotero-cache-attachment-template "imported_file")))
              (data (thread-first template
                      (plist-put :title filename)
                      (plist-put :accessDate accessdate)
                      (plist-put :contentType content-type)
                      (plist-put :filename filename)
                      ;; md5 and mtime can be edited directly in
                      ;; personal libraries for WebDAV-based file
                      ;; syncing. They should not be edited directly
                      ;; when using Zotero File Storage, which provides
                      ;; an atomic method for setting the properties
                      ;; along with the corresponding file.
                      (plist-put :md5 nil)
                      (plist-put :mtime nil)))
              (data (cond
                     (parent (plist-put data :parentItem parent))
                     ((stringp collection) (plist-put data :collections (vector collection)))
                     (t data))))
         (when-let ((data (zotero-cache-save data "items" type id))
                    (key (plist-get data :key)))
           (run-hook-with-args 'zotero-browser-after-change-functions key)
           (if-let ((response (zotero-upload-attachment key file nil :type type :id id))
                    (object (zotero-response-data response))
                    (data (plist-get object :data)))
               (progn
                 (run-hook-with-args 'zotero-browser-after-change-functions key)
                 (zotero-cache-save data "items" type id)
                 (display-buffer (zotero-edit-item data type id) zotero-browser-edit-buffer-action))
             (error "Failed to associate attachment with item %s" key)))))
      ("imported_url"
       (user-error "Creating a snapshot is not supported"))
      ("linked_file"
       (unless (equal zotero-browser-type "user")
         (user-error "Linked files can only be added to user library"))
       (let* ((file (expand-file-name (read-file-name "Select file: " nil nil t)))
              (attributes (zotero-file-attributes file))
              (filename (file-name-nondirectory file))
              (content-type (plist-get attributes :content-type))
              (accessdate (plist-get attributes :accessdate))
              (template (copy-tree (zotero-cache-attachment-template "linked_file")))
              (data (thread-first template
                      (plist-put :title filename)
                      (plist-put :accessDate accessdate)
                      (plist-put :contentType content-type)
                      ;; (plist-put :charset charset) ; charset cannot be determined without external tools
                      (plist-put :path file)))
              (data (cond
                     (parent (plist-put data :parentItem parent))
                     ((stringp collection) (plist-put data :collections (vector collection)))
                     (t data))))
         (when-let ((data (zotero-cache-save data "items" type id))
                    (key (plist-get data :key)))
           (run-hook-with-args 'zotero-browser-after-change-functions key)
           (display-buffer (zotero-edit-item data type id) zotero-browser-edit-buffer-action))))
      ("linked_url"
       (if parent
           (let* ((template (copy-tree (zotero-cache-attachment-template "linked_url")))
                  (data (cond
                         (parent (plist-put template :parentItem parent))
                         ((stringp collection) (plist-put template :collections (vector collection)))
                         (t template))))
             (pop-to-buffer (zotero-edit-item data type id) zotero-browser-edit-buffer-action))
         (user-error "Links to URLs are not allowed as top level items"))))))

(defun zotero-browser-update-attachment ()
  "Update the attachment of the current entry."
  (interactive)
  (zotero-browser-ensure-items-mode)
  (zotero-browser-ensure-write-access)
  (zotero-browser-ensure-item-at-point)
  (let* ((type zotero-browser-type)
         (id zotero-browser-id)
         (ewoc zotero-browser-ewoc)
         (node (ewoc-locate ewoc))
         (key (ewoc-data node))
         (entry (zotero-cache-synccache "item" key type id))
         (data (plist-get entry :data))
         (itemtype (zotero-lib-plist-get* entry :data :itemType))
         (linkmode (zotero-lib-plist-get* entry :data :linkMode))
         (filename (zotero-lib-plist-get* entry :data :filename))
         (hash (zotero-lib-plist-get* entry :data :md5))
         (dir (concat (file-name-as-directory zotero-cache-storage-dir) key))
         (path (concat (file-name-as-directory dir) filename)))
    (when (and (equal itemtype "attachment")
               (equal linkmode "imported_file"))
      (let* ((file (expand-file-name (read-file-name "Select file: " dir nil t path)))
             (attributes (zotero-file-attributes file))
             (filename (file-name-nondirectory file))
             (content-type (plist-get attributes :content-type))
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
        (unless (zotero-upload-attachment key file hash :type type :id id)
          (error "Failed to associate attachment with item %s" key))
        (display-buffer (zotero-edit-item data type id) zotero-browser-edit-buffer-action)))))

(defun zotero-browser-rename-attachment ()
  "Rename the attachment of the current entry to match the metadata."
  (interactive)
  (zotero-browser-ensure-items-mode)
  (zotero-browser-ensure-write-access)
  (zotero-browser-ensure-item-at-point)
  (let* ((type zotero-browser-type)
         (id zotero-browser-id)
         (ewoc zotero-browser-ewoc)
         (node (ewoc-locate ewoc))
         (key (ewoc-data node))
         (entry (zotero-cache-synccache "item" key type id))
         (itemtype (zotero-lib-plist-get* entry :data :itemType))
         (linkmode (zotero-lib-plist-get* entry :data :linkMode))
         parent attachment)
    (if (and (equal itemtype "attachment")
             (equal linkmode "imported_file"))
        (let ((parent-key (zotero-browser--parent key)))
          (setq parent (zotero-cache-synccache "item" parent-key type id))
          (setq attachment entry))
      (let* ((children (zotero-browser--children key))
             (attachments (ht-find
                           (lambda (_key value) (and (equal (zotero-lib-plist-get* value :data :itemType) "attachment")
                                                     (equal (zotero-lib-plist-get* value :data :linkMode) "imported_file")))
                           children)))
        (setq parent entry)
        (if attachments
            (setq attachment (cadr attachments))
          (user-error "Item has no attachments"))))
    ;; Rename the attachment to match the metadata
    (let* ((parent-data (plist-get parent :data))
           (parent-key (plist-get parent-data :key))
           (attachment-data (plist-get attachment :data))
           (attachment-key (plist-get attachment-data :key))
           (version (plist-get attachment-data :version))
           (base (zotero-browser--filename-base parent-data))
           (file (zotero-browser--find-attachment attachment))
           (attributes (zotero-file-attributes file))
           (dir (file-name-directory file))
           (ext (file-name-extension file t))
           (newname (concat dir base ext)))
      (rename-file file newname t)
      ;; Update the attachment item
      (let* ((data (thread-first attachment-data
                     (plist-put :parentItem parent-key)
                     ;; Children don't have collections
                     (plist-put :collections [])
                     (plist-put :title base)
                     (plist-put :filename (concat base ext))
                     ;; md5 and mtime can be edited directly in
                     ;; personal libraries for WebDAV-based file
                     ;; syncing. They should not be edited directly
                     ;; when using Zotero File Storage, which provides
                     ;; an atomic method for setting the properties
                     ;; along with the corresponding file.
                     (plist-put :md5 nil)
                     (plist-put :mtime nil)))
             (response (zotero-update-item attachment-key data version :type type :id id))
             (status-code (zotero-response-status-code response)))
        (when (eq status-code 204)
          (zotero-cache-save data "items" type id)
          ;; Get upload authorization for the renamed file
          (if-let ((md5 (plist-get attributes :md5))
                   (response (zotero-upload-attachment attachment-key newname md5 :type type :id id))
                   (object (zotero-response-data response))
                   (data (plist-get object :data)))
              (progn
                (zotero-cache-save data "items" type id)
                (run-hook-with-args 'zotero-browser-after-change-functions attachment-key))
            (user-error "Failed to associate attachment with item %s" attachment-key)))))))

(defun zotero-browser--recognize (file)
  "Return the bibliographic metadata recognized from PDF FILE."
  (let* ((response (zotero-recognize file))
         (metadata (zotero-response-data response))
         (data (cond
                ((not (plist-member metadata :title))
                 (user-error "The attachment was not recognized"))
                ((plist-member metadata :arxiv)
                 (zotero-arxiv (plist-get metadata :arxiv)))
                ((plist-member metadata :doi)
                 (zotero-crossref (plist-get metadata :doi)))
                ((plist-member metadata :isbn)
                 (zotero-openlibrary (plist-get metadata :isbn)))
                (t
                 (zotero-recognize-parse-metadata metadata)))))
    (when (and (string-empty-p (plist-get data :abstractNote))
               (plist-member metadata :abstract))
      (setq data (plist-put data :abstractNote (plist-get metadata :abstract))))
    (when (and (string-empty-p (plist-get data :language))
               (plist-member metadata :language))
      (setq data (plist-put data :language (plist-get metadata :language))))
    data))

(defun zotero-browser-import (file)
  "Import PDF FILE and create a new attachment.
Retrieve the metadata automatically, create an appropriate parent
item, and rename the associated file based on the metadata."
  (let* ((inhibit-read-only t)
         (type zotero-browser-type)
         (id zotero-browser-id)
         (collection zotero-browser-collection)
         (parent-data (zotero-browser--recognize file))
         (parent-data (if (stringp collection)
                          (plist-put parent-data :collections (vector collection))
                        parent-data)))
    ;; Create the parent item
    (when-let ((data (zotero-cache-save parent-data "items" type id))
               (parent-key (plist-get data :key)))
      (run-hook-with-args 'zotero-browser-after-change-functions parent-key)
      ;; Create the attachment item
      (let* ((attributes (zotero-file-attributes file))
             (content-type (plist-get attributes :content-type))
             (accessdate (plist-get attributes :accessdate))
             (base (zotero-browser--filename-base data))
             (ext (file-name-extension file t))
             (newname (concat base ext))
             (tempfile (concat temporary-file-directory newname))
             (template (copy-tree (zotero-cache-attachment-template "imported_file")))
             (attachment-data (thread-first template
                                (plist-put :parentItem parent-key)
                                (plist-put :title newname)
                                (plist-put :accessDate accessdate)
                                (plist-put :contentType content-type)
                                (plist-put :filename newname)
                                ;; md5 and mtime can be edited directly in
                                ;; personal libraries for WebDAV-based file
                                ;; syncing. They should not be edited directly
                                ;; when using Zotero File Storage, which provides
                                ;; an atomic method for setting the properties
                                ;; along with the corresponding file.
                                (plist-put :md5 nil)
                                (plist-put :mtime nil)))
             (data (zotero-cache-save attachment-data "items" type id))
             (attachment-key (plist-get data :key)))
        (condition-case nil
            (copy-file file tempfile 1 t)
          (file-already-exists (user-error "File already exists: %s" tempfile)))
        ;; Upload the attachment
        (if-let ((response (zotero-upload-attachment attachment-key tempfile nil :type type :id id))
                 (object (zotero-response-data response))
                 (data (plist-get object :data)))
            (progn
              (zotero-cache-save data "items" type id)
              (run-hook-with-args 'zotero-browser-after-change-functions attachment-key)
              (let* ((dir (concat (file-name-as-directory zotero-cache-storage-dir) attachment-key))
                     (newfile (concat (file-name-as-directory dir) newname)))
                (unless (file-exists-p dir)
                  (make-directory dir))
                (rename-file tempfile newfile)))
          (delete-file tempfile)
          (user-error "Failed to associate attachment with item %s" attachment-key))))))

(defun zotero-browser-link (file)
  "Link to PDF FILE and create a new attachment.
Retrieve the metadata automatically and create an appropriate
parent item."
  (let* ((inhibit-read-only t)
         (type zotero-browser-type)
         (id zotero-browser-id)
         (collection zotero-browser-collection)
         (parent-data (zotero-browser--recognize file))
         (parent-data (if (stringp collection)
                          (plist-put parent-data :collections (vector collection))
                        parent-data)))
    ;; Create the parent item
    (when-let ((data (zotero-cache-save parent-data "items" type id))
               (parent-key (plist-get data :key)))
      (run-hook-with-args 'zotero-browser-after-change-functions parent-key)
      ;; Create the attachment item
      (let* ((attributes (zotero-file-attributes file))
             (filename (file-name-nondirectory file))
             (content-type (plist-get attributes :content-type))
             (accessdate (plist-get attributes :accessdate))
             (template (copy-tree (zotero-cache-attachment-template "linked_file")))
             (attachment-data (thread-first template
                                (plist-put :parentItem parent-key)
                                (plist-put :title filename)
                                (plist-put :accessDate accessdate)
                                (plist-put :contentType content-type)
                                (plist-put :path file)))
             (data (zotero-cache-save attachment-data "items" type id))
             (attachment-key (plist-get data :key)))
        (run-hook-with-args 'zotero-browser-after-change-functions attachment-key)))))

(defun zotero-browser-import-attachment (file)
  "Import PDF FILE and create a new attachment.
Retrieve the metadata automatically, create an appropriate parent
item, and rename the associated file based on the metadata."
  (interactive "f")
  (zotero-browser-ensure-items-mode)
  (zotero-browser-ensure-write-access)
  (zotero-browser-import file))

(defun zotero-browser-link-attachment (file)
  "Link to PDF FILE and create a new attachment.
Retrieve the metadata automatically and create an appropriate
parent item."
  (interactive "f")
  (zotero-browser-ensure-items-mode)
  (zotero-browser-ensure-write-access)
  (unless (equal zotero-browser-type "user")
    (user-error "Linked files can only be added to user library"))
  (zotero-browser-link file))

(defun zotero-browser-add-by-identifier (string)
  "Create a new item by providing an identifier.

Argument STRING is a ISBN, DOI, PMID, or arXiv ID."
  (interactive "sEnter a ISBN, DOI, PMID, or arXiv ID: ")
  (zotero-browser-ensure-items-mode)
  (zotero-browser-ensure-write-access)
  (let* ((type zotero-browser-type)
         (id zotero-browser-id)
         (collection zotero-browser-collection)
         (string (s-trim string))
         identifier
         data)
    (cond
     ((setq identifier (zotero-lib-validate-isbn string))
      (setq data (zotero-openlibrary identifier)))
     ((setq identifier (zotero-lib-validate-doi string))
      (setq data (zotero-crossref identifier)))
     ((setq identifier (zotero-lib-validate-pmid string) )
      (setq data (zotero-pmid identifier)))
     ((setq identifier (zotero-lib-validate-arxiv string) )
      (setq data (zotero-arxiv identifier)))
     (t (user-error "Identifier \"%s\" is not a valid arXiv id, DOI, or ISBN" string)))
    (when (and data (stringp collection))
      (setq data (plist-put data :collections (vector collection))))
    (if data
        (pop-to-buffer (zotero-edit-item data type id) zotero-browser-edit-buffer-action)
      (user-error "No metadata found for identifier \"%s\"" identifier))))

(defun zotero-browser-recognize-attachment ()
  "Recognize content of the current entry."
  (interactive)
  (zotero-browser-ensure-items-mode)
  (zotero-browser-ensure-write-access)
  (zotero-browser-ensure-item-at-point)
  (let* ((inhibit-read-only t)
         (type zotero-browser-type)
         (id zotero-browser-id)
         (collection zotero-browser-collection)
         (ewoc zotero-browser-ewoc)
         (node (ewoc-locate ewoc))
         (key (ewoc-data node))
         (entry (zotero-cache-synccache "item" key type id))
         (table (zotero-cache-synccache "items" nil type id))
         (attachment-data (plist-get entry :data))
         (itemtype (plist-get attachment-data :itemType))
         (linkmode (plist-get attachment-data :linkMode))
         (version (plist-get attachment-data :version)))
    (when (and (equal itemtype "attachment")
               (equal linkmode "imported_file"))
      (let* ((file (zotero-browser--find-attachment entry))
             (attributes (zotero-file-attributes file))
             (content-type (plist-get attributes :content-type)))
	;; Check whether a given PDF could theoretically be recognized
        (cond
         ((zotero-cache-parentitem key table)
          (user-error "Attachment already has a parent"))
         ((not (equal content-type "application/pdf"))
          (user-error "Attachment is not a PDF"))
         (t
          (let* ((parent-data (zotero-browser--recognize file))
                 (parent-data (if (stringp collection)
	                          ;; Put the parent item in the same collections as the attachment
                                  (plist-put parent-data :collections (vector collection))
                                parent-data)))
            ;; Create the parent item
            (when-let ((data (zotero-cache-save parent-data "items" type id))
                       (parent-key (plist-get data :key)))
              (run-hook-with-args 'zotero-browser-after-change-functions parent-key)
              ;; Rename the attachment to match new metadata and make it a child
              (let* ((base (zotero-browser--filename-base data))
                     (dir (file-name-directory file))
                     (ext (file-name-extension file t))
                     (newname (concat dir base ext)))
                (rename-file file newname t)
                ;; Update the attachment item
                (let* ((data (thread-first attachment-data
                               (plist-put :parentItem parent-key)
                               ;; Children don't have collections
                               (plist-put :collections [])
                               (plist-put :title base)
                               (plist-put :filename (concat base ext))
                               ;; md5 and mtime can be edited directly in
                               ;; personal libraries for WebDAV-based file
                               ;; syncing. They should not be edited directly
                               ;; when using Zotero File Storage, which provides
                               ;; an atomic method for setting the properties
                               ;; along with the corresponding file.
                               (plist-put :md5 nil)
                               (plist-put :mtime nil)))
                       (response (zotero-update-item key data version :type type :id id))
                       (status-code (zotero-response-status-code response)))
                  (when (eq status-code 204)
                    (zotero-cache-save data "items" type id)
                    ;; Get upload authorization for the renamed file
                    (if-let ((md5 (plist-get attributes :md5))
                             (response (zotero-upload-attachment key newname md5 :type type :id id))
                             (object (zotero-response-data response))
                             (data (plist-get object :data)))
                        (progn
                          (zotero-cache-save data "items" type id)
                          (run-hook-with-args 'zotero-browser-after-change-functions key))
                      (user-error "Failed to associate attachment with item %s" key)))))))))))))

(defun zotero-browser-index-attachment ()
  "Index the full-text content of the current entry."
  (interactive)
  (zotero-browser-ensure-items-mode)
  (zotero-browser-ensure-write-access)
  (zotero-browser-ensure-item-at-point)
  (let* ((type zotero-browser-type)
         (id zotero-browser-id)
         (ewoc zotero-browser-ewoc)
         (node (ewoc-locate ewoc))
         (key (ewoc-data node))
         (entry (zotero-cache-synccache "item" key type id))
         (itemtype (zotero-lib-plist-get* entry :data :itemType))
         (linkmode (zotero-lib-plist-get* entry :data :linkMode)))
    (when (and (equal itemtype "attachment")
               (or (equal linkmode "imported_file")
                   (equal linkmode "linked_file")))
      (let* ((file (zotero-browser--find-attachment entry))
             (attributes (zotero-file-attributes file))
             (content-type (plist-get attributes :content-type)))
        (zotero-fulltext-index-item key file content-type :type type :id id)))))

(defun zotero-browser-download-attachment (&optional dir)
  "Download the attachment of the current entry.

Optional argument DIR is the directory. If DIR is omitted or nil,
the attachment is downloaded to the default storage directory
`zotero-cache-storage-dir' and a subdirectory named as the item
key."
  (zotero-browser-ensure-items-mode)
  (zotero-browser-ensure-item-at-point)
  (let* ((type zotero-browser-type)
         (id zotero-browser-id)
         (ewoc zotero-browser-ewoc)
         (node (ewoc-locate ewoc))
         (key (ewoc-data node))
         (entry (zotero-cache-synccache "item" key type id))
         (filename (zotero-lib-plist-get* entry :data :filename))
         (dir (or dir (concat (file-name-as-directory zotero-cache-storage-dir) key))))
    (unless (file-exists-p dir)
      (make-directory dir t))
    (zotero-download-file key filename dir t :type type :id id)))

(defun zotero-browser-dnd-protocol (uri _action)
  "Drag-n-drop protocol for files.
For PDF files URI will be a string like \"file:path\"."
  (let* ((url (url-generic-parse-url uri))
         (filename (url-filename url)))
    (zotero-browser-import filename)))

(defun zotero-browser-libraries ()
  "Create a libraries browser buffer."
  (let ((buffer (get-buffer-create zotero-browser-libraries-buffer-name)))
    (with-current-buffer buffer
      (zotero-browser-libraries-mode)
      (let* ((user (zotero-cache-library "user"))
             (groups (zotero-cache-group))
             (ewoc (ewoc-create #'zotero-browser--print-library))
             (inhibit-read-only t))
        (erase-buffer)
        (setq zotero-browser-ewoc ewoc)
        ;; There should be only one user library, so no sorting
        (let ((keys (ht-keys user)))
          (dolist (key keys)
            (ewoc-enter-last ewoc key)))
        (let* ((field (car zotero-browser-libraries-sort-field))
               (flip (cdr zotero-browser-libraries-sort-field))
               (keys (zotero-cache-sort-by field flip groups)))
          (dolist (key keys)
            (ewoc-enter-last ewoc key)))))
    buffer))

(defun zotero-browser-collections (&optional resource type id)
  "Create a collections browser buffer.

Optional argument RESOURCE is one of:
  - \"collections\": collections in the library
  - \"items\": all items in the library, excluding trashed items
  - \"searches\": all saved searches in the library

Argument TYPE is \"user\" for your personal library, and
\"group\" for the group libraries. ID is the ID of the personal
or group library you want to access, that is the user ID or group
ID."
  (let ((buffer (get-buffer-create zotero-browser-collections-buffer-name)))
    (with-current-buffer buffer
      (zotero-browser-collections-mode)
      (let ((ewoc (ewoc-create #'zotero-browser--print-collection))
            (inhibit-read-only t))
        ;; Remove previous entries
        (erase-buffer)
        (when (and type id resource)
          (let* ((table (zotero-cache-synccache resource nil type id))
                 (field (car zotero-browser-collections-sort-field))
                 (flip (cdr zotero-browser-collections-sort-field))
                 (keys (zotero-cache-sort-by field flip table)))
            (setq zotero-browser-ewoc ewoc
                  zotero-browser-type type
                  zotero-browser-id id
                  zotero-browser-resource resource)
            (dolist (key keys)
              ;; Create a new node if key is not a child
              (unless (zotero-cache-parentitem key table)
                (ewoc-enter-last ewoc key))
              ;; Then create nodes for the children of key
              (when-let ((children (ht-keys (zotero-cache-subcollections key table))))
                (dolist (child children)
                  (ewoc-enter-last ewoc child))))
            (ewoc-enter-last ewoc 'unfiled)
            (ewoc-enter-last ewoc 'trash)
            (zotero-browser-expand-level zotero-browser-default-collection-level)
            (zotero-browser-items "items-top" nil type id)))))
    buffer))

(defun zotero-browser-items (&optional resource collection type id)
  "Create an items buffer.

Optional argument RESOURCE is one of:
  - \"collections\": collections in the library
  - \"items\": all items in the library, excluding trashed items
  - \"searches\": all saved searches in the library

Argument COLLECTION is the collection. TYPE is \"user\" for your
personal library, and \"group\" for the group libraries. ID is
the ID of the personal or group library you want to access, that
is the user ID or group ID."
  (let ((buffer (get-buffer-create zotero-browser-items-buffer-name)))
    (with-current-buffer buffer
      (zotero-browser-items-mode)
      (add-hook 'zotero-browser-after-change-functions #'zotero-browser--update-collection)
      (add-hook 'zotero-browser-after-change-functions #'zotero-browser--update-item)
      (let ((ewoc (ewoc-create #'zotero-browser--print-item))
            (inhibit-read-only t))
        ;; Remove previous entries
        (erase-buffer)
        (when (and type id resource)
          (let* ((table (zotero-cache-synccache resource collection type id))
                 (field (car zotero-browser-items-sort-field))
                 (flip (cdr zotero-browser-items-sort-field))
                 (keys (zotero-cache-sort-by field flip table)))
            (setq zotero-browser-ewoc ewoc
                  zotero-browser-type type
                  zotero-browser-id id
                  zotero-browser-resource resource
                  zotero-browser-collection collection)
            (ewoc-set-hf ewoc (zotero-browser--header) "")
            (dolist (key keys)
              (let ((parent (zotero-cache-parentitem key table))
                    (children (ht-keys (zotero-cache-subitems key table))))
                ;; Create a new node if key is not a child of an existing parent
                (unless (member parent keys)
                  (ewoc-enter-last ewoc key)
                  ;; Orphans shouldn't happen
                  (when parent
                    (message "Item %s is an orphan" key)))
                ;; Then create nodes for the children of key
                (dolist (child children)
                  (ewoc-enter-last ewoc child))))
            (zotero-browser-expand-level zotero-browser-default-item-level)))))
    buffer))

(defun zotero-browser-note-exit ()
  "Save note and kill buffer."
  (interactive)
  (zotero-browser-ensure-note-buffer)
  (when-let ((entry (zotero-cache-synccache "item" zotero-browser-key zotero-browser-type zotero-browser-id t))
             (data (plist-get entry :data))
             (contents (save-excursion (widen) (buffer-string))))
    (zotero-cache-save (plist-put data :note contents) "items" zotero-browser-type zotero-browser-id)
    (run-hook-with-args 'zotero-browser-after-change-functions zotero-browser-key)
    (set-buffer-modified-p nil)
    (kill-buffer-and-window)))

(defun zotero-browser-note-save ()
  "Save note."
  (interactive)
  (zotero-browser-ensure-note-buffer)
  (when-let ((entry (zotero-cache-synccache "item" zotero-browser-key zotero-browser-type zotero-browser-id t))
             (data (plist-get entry :data))
             (contents (save-excursion (widen) (buffer-string))))
    (zotero-cache-save (plist-put data :note contents) "items" zotero-browser-type zotero-browser-id)
    (run-hook-with-args 'zotero-browser-after-change-functions zotero-browser-key)
    (set-buffer-modified-p nil)))

(defun zotero-browser-note-abort ()
  "Abort editing note and kill buffer."
  (interactive)
  (zotero-browser-ensure-note-buffer)
  (set-buffer-modified-p nil)
  (kill-buffer-and-window))

(defun zotero-browser-display ()
  "Display current library or collection."
  (interactive)
  (zotero-browser-ensure-browser-buffer)
  (when-let ((ewoc zotero-browser-ewoc)
             (node (ewoc-locate ewoc))
             (key (ewoc-data node)))
    (pcase major-mode
      ('zotero-browser-libraries-mode
       (let* ((table (zotero-cache-library))
              (type (plist-get (ht-get table key) :type))
              (id (plist-get (ht-get table key) :id)))
         (setq zotero-browser-type type
               zotero-browser-id id)
         (display-buffer (zotero-browser-collections "collections-top" type id))
         (display-buffer (zotero-browser-items "items-top" nil type id))))
      ('zotero-browser-collections-mode
       (let* ((type zotero-browser-type)
              (id zotero-browser-id))
         (pcase key
           ('unfiled
            (display-buffer (zotero-browser-items "items-top" key type id)))
           ('trash
            (display-buffer (zotero-browser-items "trash-items" key type id)))
           ((pred stringp)
            (display-buffer (zotero-browser-items "collection-items" key type id))))))
      ('zotero-browser-items-mode
       (let* ((type zotero-browser-type)
              (id zotero-browser-id)
              (entry (zotero-cache-synccache "item" key type id))
              (data (plist-get entry :data)))
         (display-buffer (zotero-edit-item data type id) zotero-browser-edit-buffer-action))))))

(defun zotero-browser ()
  "Create a new Zotero browser buffer.
Switch if one already exists."
  (interactive)
  (zotero-cache-maybe-initialize-cache)
  (let ((items-buffer (or (get-buffer zotero-browser-items-buffer-name)
                          (zotero-browser-items)))
        (libraries-buffer (or (get-buffer zotero-browser-libraries-buffer-name)
                              (zotero-browser-libraries)))
        (collections-buffer (or (get-buffer zotero-browser-collections-buffer-name)
                                (zotero-browser-collections))))
    (pop-to-buffer items-buffer zotero-browser-items-buffer-action)
    (display-buffer collections-buffer zotero-browser-collections-buffer-action)
    (display-buffer libraries-buffer zotero-browser-libraries-buffer-action)
    ;; Display the currently selected library
    (with-current-buffer libraries-buffer
      (zotero-browser-display))))

(defun zotero-browser-sync (&optional full-sync)
  "Synchronize the Zotero library.
When optional argument FULL-SYNC is non-nil, or with a ‘C-u’
prefix, force a full sync."
  (interactive "P")
  (zotero-sync full-sync)
  (when-let ((libraries-buffer (get-buffer zotero-browser-libraries-buffer-name)))
    (with-current-buffer libraries-buffer (zotero-browser-revert)))
  (when-let ((collections-buffer (get-buffer zotero-browser-collections-buffer-name)))
    (with-current-buffer collections-buffer (zotero-browser-revert)))
  (when-let ((items-buffer (get-buffer zotero-browser-items-buffer-name)))
    (with-current-buffer items-buffer (zotero-browser-revert))))

(provide 'zotero-browser)

;;; zotero-browser.el ends here
