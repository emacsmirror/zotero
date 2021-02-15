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

(require 'zotero-arxiv)
(require 'zotero-cache)
(require 'zotero-crossref)
(require 'zotero-edit)
(require 'zotero-fulltext)
(require 'zotero-lib)
(require 'zotero-openlibrary)
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

(defvar zotero-browser-padding 1
  "Set the number of characters preceding each entry")

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

(defvar-local zotero-browser-resource nil
  "Resource of the current buffer.")

(defvar-local zotero-browser-keys nil
  "Keys of the current buffer.")

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
    (define-key map (kbd "n") #'zotero-browser-next)
    (define-key map (kbd "p") #'zotero-browser-prev)
    (define-key map (kbd "u") #'zotero-browser-up)
    (define-key map (kbd "C-c C-n") #'zotero-browser-next-collection)
    (define-key map (kbd "C-c C-p") #'zotero-browser-prev-collection)
    (define-key map (kbd "C-c C-u") #'zotero-browser-up-collection)
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
    (define-key map (kbd "n") #'zotero-browser-next)
    (define-key map (kbd "p") #'zotero-browser-prev)
    (define-key map (kbd "u") #'zotero-browser-up)
    (define-key map (kbd "C-c C-n") #'zotero-browser-next-collection)
    (define-key map (kbd "C-c C-p") #'zotero-browser-prev-collection)
    (define-key map (kbd "C-c C-u") #'zotero-browser-up-collection)
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
    ["Create note" zotero-browser-create-note :help "Create a new note"]
    ["Create attachment" zotero-browser-create-attachment :help "Create a new attachment"]
    ["Update attachment" zotero-browser-update-attachment :help "Update current attachment"]
    ["Add by identifier" zotero-browser-add-by-identifier :help "Add a new item by ISBN, DOI, PMID, or arXiv ID"]
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

(defcustom zotero-browser-default-collection-level 1
  "The default expansion level for collections."
  :group 'zotero-browser
  :type 'integer)

(defcustom zotero-browser-default-item-level 1
  "The default expansion level for items."
  :group 'zotero-browser
  :type 'integer)

(defcustom zotero-browser-library-keys '(icon " " :name)
  "Keys to show in the items browser"
  :group 'zotero-browser
  :type
  '(repeat (choice
            (const :tag "Icon" icon)
            (string :tag "Spacing character(s)")
            (const :tag "Newline" "\n")
            (const :tag "Tab" "\t")
            (const :tag "Space" " ")
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
            (const :tag "File Editing" :fileEditing))))

(defcustom zotero-browser-collection-keys '(:name)
  "Keys to show in the collections browser"
  :group 'zotero-browser
  :type '(repeat (choice
                  (const :tag "Icon" icon)
                  (string :tag "Spacing character(s)")
                  (const :tag "Newline" "\n")
                  (const :tag "Tab" "\t")
                  (const :tag "Space" " ")
                  (const :tag "Key" :key)
                  (const :tag "Version" :version)
                  (const :tag "Name" :name))))

(defcustom zotero-browser-attachment-keys '(icon " " :title)
  "Keys to show in the items browser"
  :group 'zotero-browser
  :type
  '(repeat (choice
            (const :tag "Icon" icon)
            (string :tag "Spacing character(s)")
            (const :tag "Period" ".")
            (const :tag "Newline" "\n")
            (const :tag "Tab" "\t")
            (const :tag "Space" " ")
            (const :tag "Key" :key)
            (const :tag "Version" :version)
            (const :tag "Item Type" :itemtype)
            (const :tag "Link Mode" :linkMode)
            (const :tag "Title" :title)
            (const :tag "Access Date" :accessDate)
            (const :tag "Content Type" :contentType)
            (const :tag "Charset" :charset))))

(defcustom zotero-browser-note-keys '(icon " " :note)
  "Keys to show in the items browser"
  :group 'zotero-browser
  :type
  '(repeat (choice
            (const :tag "Icon" icon)
            (string :tag "Spacing character(s)")
            (const :tag "Period" ".")
            (const :tag "Newline" "\n")
            (const :tag "Tab" "\t")
            (const :tag "Space" " ")
            (const :tag "Key" :key)
            (const :tag "Version" :version)
            (const :tag "Item Type" :itemtype)
            (const :tag "Note" :note))))

(defcustom zotero-browser-item-keys '(icon " " :creators "." " " :title "." " " :year)
  "Keys to show in the items browser"
  :group 'zotero-browser
  :type
  '(repeat (choice
            (const :tag "Icon" icon)
            (string :tag "Spacing character(s)")
            (const :tag "Period" ".")
            (const :tag "Space" " ")
            (const :tag "Newline" "\n")
            (const :tag "Tab" "\t")
            (const :tag "Key" :key)
            (const :tag "Version" :version)
            (const :tag "Item Type" :itemtype)
            (const :tag "Title" :title)
            (const :tag "Creators" :creators)
            (const :tag "Date" :date)
            (const :tag "Year" :year)
            (const :tag "Publisher" :publisher)
            (const :tag "Publication Title" :publicationTitle)
            (const :tag "Date Added" :dateAdded)
            (const :tag "Date Modified" :dateModified)
            (const :tag "Extra" :extra)
            (const :tag "Note" :note))))

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

;;;; Functions

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
          (setq level (1+ level))
          (unless (or (eq parent :json-false) (null parent))
            (setq key parent))))
    level))

(defun zotero-browser--prefix (position string)
  "Set the prefix of POSITION to STRING.
STRING should contain only one character."
  (let* ((prefix (get-text-property position 'line-prefix))
         (spacing (substring prefix 0 -1)))
    (put-text-property position (line-end-position) 'line-prefix (concat spacing string))))

(defun zotero-browser--add (ewoc node table)
  "Add items of TABLE after NODE in EWOC"
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

(defun zotero-browser--expand (ewoc node)
  "Expand the children of NODE in EWOC."
  (let* ((key (ewoc-data node))
         (table (zotero-browser--children key)))
    (zotero-browser--prefix (ewoc-location node) "▾")
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
    (zotero-browser--prefix (ewoc-location node) "▸")
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
     (w32-shell-execute "open" path))
    (system
     (error "Unable to determine default application on operating system %S."))))

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

(defun zotero-browser--open-imported-file (entry)
  (let ((path (expand-file-name (zotero-browser-find-attachment))))
    (zotero-browser--open-file path)))

(defun zotero-browser--open-imported-url (entry)
  (let ((path (expand-file-name (zotero-browser-find-attachment)))
        (contenttype (zotero-lib-plist-get* entry :object :data :contentType))
        (key (zotero-lib-plist-get* entry :object :data :key))
        (filename (zotero-lib-plist-get* entry :object :data :filename))
        (dir (concat temporary-file-directory key)))
    (if (equal contenttype "application/pdf")
        (zotero-browser--open-file path)
      (let* ((unzip (or (executable-find "unzip")
                        (error "Unable to find executable \"unzip\"")))
             (exit-status (call-process unzip nil nil nil "-o" "-d" dir path)))
        (if (eq exit-status 0)
            (let ((path (concat (file-name-as-directory dir) filename)))
              (browse-url-file-url path))
          (error "Error extracting snapshot"))))))

(defun zotero-browser--open-linked-file (entry)
  (let ((path (zotero-lib-plist-get* entry :object :data :path)))
    (zotero-browser--open-file path)))

(defun zotero-browser--open-linked-url (entry)
  (let ((url (zotero-lib-plist-get* entry :object :data :url)))
    (browse-url url)))

(defun zotero-browser--library-pp (key)
  "Pretty print library KEY."
  (let* ((library (zotero-cache-library nil key))
         (type (plist-get library :type))
         (id (plist-get library :id))
         ;; Set icon height (in pixels) to 1 characters
         (height (window-font-height)))
    (dolist (key zotero-browser-library-keys)
      (pcase key
        ((and (pred stringp) str) (insert str))
        ('icon
         (let* ((icon (pcase type
                        ("user" "treesource-library.png")
                        ("group" "treesource-groups.png")))
                (dir (file-name-as-directory "img"))
                (file (expand-file-name (concat dir icon) zotero-directory)))
           (when (and file (file-readable-p file))
             (let ((image (create-image file 'png nil :height (window-font-height))))
               (insert-image image type)))))
        (:name
         (let ((value (pcase type
                        ("user" "User library")
                        ("group" (let ((group (zotero-cache-group id)))
                                   (zotero-lib-plist-get* group :object :data key))))))
           (insert value)))
        (:version
         (when-let ((value (plist-get library key)))
           (insert (number-to-string value))))
        ((or :library :write))
        (:last-sync
         (when-let ((value (plist-get library key)))
           (insert (format-time-string "%c" value))))
        ((or :group-type :description :url :libraryEditing :libraryReading :fileEditing)
         (when (equal type "group")
           (let* ((group (zotero-cache-group id))
                  (value (zotero-lib-plist-get* group :object :data key)))
             (insert value))))
        ((pred keywordp)
         (when-let ((value (plist-get library key)))
           (insert value)))))))

(defun zotero-browser--collection-pp (key)
  "Pretty print collection KEY."
  (let* ((entry (zotero-cache-synccache "collection" key zotero-browser-type zotero-browser-id))
         (level (zotero-browser--level key))
         (indentation (+ zotero-browser-padding level))
         (prefix (make-string indentation ?\s))
         (beg (point)))
    (dolist (key zotero-browser-collection-keys)
      (pcase key
        ((and (pred stringp) str) (insert str))
        ('icon
         (let* ((icon "treesource-collection.png")
                (dir (file-name-as-directory "img"))
                (file (expand-file-name (concat dir icon) zotero-directory)))
           (when (and file (file-readable-p file))
             (let ((image (create-image file 'png nil :height (window-font-height))))
               (insert-image image "collection")))))
        ((pred keywordp)
         (when-let ((value (zotero-lib-plist-get* entry :object :data key)))
           (insert value)))))
    (add-text-properties beg (point) `(line-prefix ,prefix wrap-prefix ,prefix))))

(defun zotero-browser--item-pp (key)
  "Pretty print item KEY."
  (let* ((entry (zotero-cache-synccache "item" key zotero-browser-type zotero-browser-id))
         (itemtype (zotero-lib-plist-get* entry :object :data :itemType))
         (level (zotero-browser--level key))
         (indentation (+ zotero-browser-padding level))
         (prefix (make-string indentation ?\s))
         (beg (point)))
    (pcase itemtype
      ("attachment"
       (dolist (key zotero-browser-attachment-keys)
         (pcase key
           ((pred stringp) (unless (save-excursion (backward-char) (looking-at-p (regexp-quote key))) (insert key)))
           ('icon
            (let* ((linkmode (zotero-lib-plist-get* entry :object :data :linkMode))
                   (file (zotero-browser--attachment-icon linkmode)))
              (when (and file (file-readable-p file))
                (let ((image (create-image file 'png nil :height (window-font-height))))
                  (insert-image image itemtype)))))
           (:version
            (when-let ((value (plist-get library key)))
              (insert (number-to-string value))))
           ((pred keywordp)
            (when-let ((value (zotero-lib-plist-get* entry :object :data key)))
              (insert value))))))
      ("note"
       (dolist (key zotero-browser-note-keys)
         (pcase key
           ((pred stringp) (unless (save-excursion (backward-char) (looking-at-p (regexp-quote key))) (insert key)))
           ('icon
            (let* ((itemtype (zotero-lib-plist-get* entry :object :data :itemType))
                   (file (zotero-browser--itemtype-icon itemtype)))
              (when file
                (let ((image (create-image file 'png nil :height (window-font-height))))
                  (insert-image image itemtype)))))
           (:note
            (when-let ((note (zotero-lib-plist-get* entry :object :data :note))
                       (text (replace-regexp-in-string "<[^>]+>" "" note)) ; Remove all HTML tags
                       (match (string-match "^.+$" text)) ; Match first non-empty line
                       (first-line (match-string-no-properties 0 text)))
              (insert first-line)))
           (:version
            (when-let ((value (plist-get library key)))
              (insert (number-to-string value))))
           ((pred keywordp)
            (when-let ((value (zotero-lib-plist-get* entry :object :data key)))
              (insert value))))))
      (_
       (dolist (key zotero-browser-item-keys)
         (pcase key
           ((pred stringp) (unless (save-excursion (backward-char) (looking-at-p (regexp-quote key))) (insert key)))
           ('icon
            (let* ((itemtype (zotero-lib-plist-get* entry :object :data :itemType))
                   (file (zotero-browser--itemtype-icon itemtype)))
              (when file
                (let ((image (create-image file 'png nil :height (window-font-height))))
                  (insert-image image itemtype)))))
           (:creators
            (when-let ((creators (zotero-lib-plist-get* entry :object :data :creators))
                       (names (when (seq-some (lambda (elt) (or (plist-get elt :lastName) (plist-get elt :name))) creators)
                                (seq-map (lambda (elt) (or (plist-get elt :lastName) (plist-get elt :name))) creators))))
              (pcase (length names)
                (1 (insert (seq-elt names 0)))
                (2 (insert (concat (seq-elt names 0)
                                   " and "
                                   (seq-elt names 1))))
                ((pred (< 2))
                 (let* ((selection (seq-take names 1)))
                   (insert (concat (string-join selection ", ")
                                   " et al.")))))))
           (:version
            (when-let ((value (plist-get library key)))
              (insert (number-to-string value))))
           (:year
            (when-let ((date (zotero-lib-plist-get* entry :object :data :date))
                       (match (string-match "[[:digit:]]\\{4\\}" date))
                       (year (match-string 0 date)))
              (insert year)))
           (:note
            (when-let ((note (zotero-lib-plist-get* entry :object :data :note))
                       (text (replace-regexp-in-string "<[^>]+>" "" note)) ; Remove all HTML tags
                       (match (string-match "^.+$" text)) ; Match first non-empty line
                       (first-line (match-string-no-properties 0 text)))
              (insert first-line)))
           ((pred keywordp)
            (when-let ((value (zotero-lib-plist-get* entry :object :data key)))
              (insert value)))))))
    (add-text-properties beg (point) `(line-prefix ,prefix wrap-prefix ,prefix))))

;;;; Commands

(defun zotero-browser-open-attachment ()
  "Open attachment at point."
  (interactive)
  (zotero-browser-ensure-browser-buffer)
  (let* ((ewoc zotero-browser-ewoc)
         (type zotero-browser-type)
         (id zotero-browser-id)
         (key (ewoc-data (ewoc-locate ewoc)))
         (entry (zotero-cache-synccache "item" key type id))
         (itemtype (zotero-lib-plist-get* entry :object :data :itemType)))
    (when (equal itemtype "attachment")
      (let ((linkmode (zotero-lib-plist-get* entry :object :data :linkMode)))
        (pcase linkmode
          ("imported_file" (zotero-browser--open-imported-file entry))
          ("imported_url" (zotero-browser--open-imported-url entry))
          ("linked_file" (zotero-browser--open-linked-file entry))
          ("linked_url" (zotero-browser--open-linked-url entry)))))))

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

(defun zotero-browser-revert ()
  "Reload the current buffer."
  (interactive)
  (zotero-browser-ensure-browser-buffer)
  (let ((pos (point)))
    (pcase major-mode
      ('zotero-browser-collections-mode
       (display-buffer (zotero-browser-collections zotero-browser-resource zotero-browser-type zotero-browser-id)))
      ('zotero-browser-items-mode
       (display-buffer (zotero-browser-items zotero-browser-resource zotero-browser-collection zotero-browser-type zotero-browser-id))))
    (goto-char pos)))

(defun zotero-browser-next ()
  "Move point to the next item."
  (interactive)
  (zotero-browser-ensure-browser-buffer)
  (let ((ewoc zotero-browser-ewoc))
    (when (ewoc-nth ewoc 0)
      (ewoc-goto-next ewoc 1)
      (zotero-browser-display))))

(defun zotero-browser-prev ()
  "Move point to the previous item."
  (interactive)
  (zotero-browser-ensure-browser-buffer)
  (let ((ewoc zotero-browser-ewoc))
    (when (ewoc-nth ewoc 0)
      (ewoc-goto-prev ewoc 1)
      (zotero-browser-display))))

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
    (zotero-browser-next)
    (pop-to-buffer buffer)))

(defun zotero-browser-prev-collection ()
  "Move point to the previous collection."
  (interactive)
  (zotero-browser-ensure-browser-buffer)
  (let ((buffer (current-buffer)))
    (pop-to-buffer zotero-browser-collections-buffer-name)
    (zotero-browser-prev)
    (pop-to-buffer buffer)))

(defun zotero-browser-up-collection ()
  "Move point to the parent collection."
  (interactive)
  (zotero-browser-ensure-browser-buffer)
  (let ((buffer (current-buffer)))
    (pop-to-buffer zotero-browser-collections-buffer-name)
    (zotero-browser-up)
    (pop-to-buffer buffer)))

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
  "Expand children till LEVEL."
  (interactive "P")
  (zotero-browser-ensure-browser-buffer)
  (let* ((ewoc zotero-browser-ewoc)
         (type zotero-browser-type)
         (id zotero-browser-id)
         (table (pcase major-mode
                  ('zotero-browser-collections-mode
                   (zotero-cache-synccache "collections" nil type id))
                  ('zotero-browser-items-mode
                   (zotero-cache-synccache "items" nil type id))))
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
                  (zotero-browser--prefix (ewoc-location node) "▸"))))
              (prog1
                  ;; End-test of while loop
                  (ewoc-next ewoc node)
                (ewoc-goto-next ewoc 1))))))))

(defun zotero-browser-edit ()
  "Edit current entry."
  (interactive)
  (zotero-browser-ensure-browser-buffer)
  (zotero-browser-ensure-write-access)
  (zotero-browser-ensure-item-at-point)
  (let* ((type zotero-browser-type)
         (id zotero-browser-id))
    (pcase major-mode
      ('zotero-browser-collections-mode
       (let* ((ewoc zotero-browser-ewoc)
              (node (ewoc-locate ewoc))
              (key (ewoc-data node))
              (entry (zotero-cache-synccache "collection" key type id))
              (data (zotero-lib-plist-get* entry :object :data)))
         (pop-to-buffer (zotero-edit-collection data type id) zotero-browser-edit-buffer-action)))
      ('zotero-browser-items-mode
       (let* ((ewoc zotero-browser-ewoc)
              (node (ewoc-locate ewoc))
              (key (ewoc-data node))
              (entry (zotero-cache-synccache "item" key type id))
              (data (zotero-lib-plist-get* entry :object :data)))
         (pop-to-buffer (zotero-edit-item data type id) zotero-browser-edit-buffer-action))))))

(defun zotero-browser-move-to-parent (&optional arg)
  "Move current entry to a parent item.
With a `C-u' prefix, move to top level."
  (interactive "P")
  (zotero-browser-ensure-items-mode)
  (zotero-browser-ensure-write-access)
  (zotero-browser-ensure-item-at-point)
  (let* ((inhibit-read-only t)
         (type zotero-browser-type)
         (id zotero-browser-id)
         (ewoc zotero-browser-ewoc)
         (node (ewoc-locate ewoc))
         (key (ewoc-data node))
         (entry (zotero-cache-synccache "item" key type id))
         (data (zotero-lib-plist-get* entry :object :data))
         (itemtype (zotero-lib-plist-get* entry :object :data :itemType))
         updated-data)
    (unless (or (equal itemtype "attachment") (equal itemtype "note"))
      (user-error "Item type %s cannot be moved to a parent"))
    (if (equal arg '(4))
        (setq updated-data (zotero-lib-plist-delete data :parentItem))
      (let* ((table (zotero-cache-synccache "items" nil type id))
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
    (zotero-cache-save updated-data "items" type id)))

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
         (choices (zotero-cache-field :name table))
         (name (completing-read "Select collection:" choices nil t))
         (new (cdr (assoc name choices)))
         (old zotero-browser-collection))
    (delete key zotero-browser-keys)
    (ewoc-delete ewoc node)
    (zotero-cache-substitute-collection key new old type id)))

(defun zotero-browser-copy-to-collection ()
  "Copy current entry to a collection."
  (interactive)
  (zotero-browser-ensure-items-mode)
  (zotero-browser-ensure-write-access)
  (zotero-browser-ensure-item-at-point)
  (let* ((inhibit-read-only t)
         (ewoc zotero-browser-ewoc)
         (type zotero-browser-type)
         (id zotero-browser-id)
         (resource zotero-browser-resource)
         (node (ewoc-locate ewoc))
         (key (ewoc-data node))
         (table (zotero-cache-synccache "collections" nil type id))
         (choices (zotero-cache-field :name table))
         (name (completing-read "Select collection:" choices nil t))
         (collection (cdr (assoc name choices))))
    (when (equal resource "items-top")
      (delete key zotero-browser-keys)
      (ewoc-delete ewoc node))
    (zotero-cache-add-to-collection key collection type id)))

(defun zotero-browser-remove-from-collection ()
  "Remove current entry from a collection."
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
    (delete key zotero-browser-keys)
    (ewoc-delete ewoc node)
    (zotero-cache-remove-from-collection key collection type id)))

(defun zotero-browser-move-to-trash ()
  "Move current entry to trash.
If region is active, trash entries in active region instead."
  (interactive)
  (zotero-browser-ensure-browser-buffer)
  (zotero-browser-ensure-write-access)
  (zotero-browser-ensure-item-at-point)
  (let* ((inhibit-read-only t)
         (type zotero-browser-type)
         (id zotero-browser-id)
         (resource (pcase major-mode
                     ('zotero-browser-libraries-mode (user-error "Trashing libraries is not supported"))
                     ('zotero-browser-collections-mode "collections")
                     ('zotero-browser-items-mode "items")))
         (ewoc zotero-browser-ewoc)
         (nodes (zotero-browser--nodes ewoc))
         (keys (zotero-browser--keys ewoc)))
    (dolist (key keys)
      (delete key zotero-browser-keys)
      (zotero-cache-trash key type id))
    (apply #'ewoc-delete ewoc nodes)))

(defun zotero-browser-restore-from-trash ()
  "Restore current entry from trash.
If region is active, restore entries in active region instead."
  (interactive)
  (zotero-browser-ensure-items-mode)
  (zotero-browser-ensure-write-access)
  (zotero-browser-ensure-item-at-point)
  (let* ((inhibit-read-only t)
         (type zotero-browser-type)
         (id zotero-browser-id)
         (resource (pcase major-mode
                     ('zotero-browser-collections-mode "collections")
                     ('zotero-browser-items-mode "items")))
         (ewoc zotero-browser-ewoc)
         (nodes (zotero-browser--nodes ewoc))
         (keys (zotero-browser--keys ewoc)))
    (dolist (key keys)
      (delete key zotero-browser-keys)
      (zotero-cache-restore key type id))
    (apply #'ewoc-delete ewoc nodes)))

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
         (nodes (zotero-browser--nodes ewoc))
         (keys (zotero-browser--keys ewoc)))
    (dolist (key keys)
      (delete key zotero-browser-keys)
      (zotero-cache-delete resource key type id))
    (apply #'ewoc-delete ewoc nodes)))

(defun zotero-browser-create ()
  "Create a new collection or item."
  (interactive)
  (zotero-browser-ensure-browser-buffer)
  (zotero-browser-ensure-write-access)
  (let* ((type zotero-browser-type)
         (id zotero-browser-id))
    (pcase major-mode
      ('zotero-browser-libraries-mode
       (user-error "Creating new groups or libraries is not supported"))
      ('zotero-browser-collections-mode
       (display-buffer (zotero-edit-create-collection type id) zotero-browser-edit-buffer-action))
      ('zotero-browser-items-mode
       (let ((itemtype (completing-read "Select an item type: " (zotero-cache-itemtypes) nil t nil nil zotero-browser-default-itemtypes )))
         (cl-pushnew itemtype zotero-browser-default-itemtypes :test #'equal)
         (pop-to-buffer (zotero-edit-create-item itemtype type id zotero-browser-collection) zotero-browser-edit-buffer-action))))))

(defun zotero-browser-create-note (&optional arg)
  "Create a new note.
With a `C-u' prefix, create a new top level note."
  (interactive "P")
  (zotero-browser-ensure-items-mode)
  (zotero-browser-ensure-write-access)
  (let* ((type zotero-browser-type)
         (id zotero-browser-id)
         (ewoc zotero-browser-ewoc)
         (node (ewoc-locate ewoc))
         ;; Top-level notes can be created by excluding the parentItem property
         ;; or setting it to false.
         (parent (cond
                  ((equal arg '(4)) nil)
                  ((null node) nil)
                  (t (ewoc-data node)))))
    (pop-to-buffer (zotero-edit-create-note type id parent) zotero-browser-edit-buffer-action)))

(defun zotero-browser-add-by-identifier (string)
  (interactive "sEnter a ISBN, DOI, PMID, or arXiv ID: ")
  (let* ((type zotero-browser-type)
         (id zotero-browser-id)
         (string (s-trim string))
         (data (cond
                ((setq identifier (zotero-lib-validate-isbn string))
                 (zotero-openlibrary identifier))
                ((setq identifier (zotero-lib-validate-doi string))
                 (zotero-crossref identifier))
                ((setq identifier (zotero-lib-validate-pmid string) )
                 (zotero-pmid identifier))
                ((setq identifier (zotero-lib-validate-arxiv string) )
                 (zotero-arxiv identifier))
                (t (user-error "Identifier \"%s\" is not a valid arXiv id, DOI, or ISBN." string)))))
    (if data
        (pop-to-buffer (zotero-edit-item data type id) zotero-browser-edit-buffer-action)
      (user-error "No metadata found for identifier \"%s\"." identifier))))

(defun zotero-browser-create-attachment (&optional arg)
  "Create a new attachment with the current entry as parent.
With a `C-u' prefix, create a new top level attachment.

Only file attachments (imported_file/linked_file) and PDF
imported web attachments (imported_url with content type
application/pdf) are allowed as top-level items, as in the Zotero
client."
  (interactive "P")
  (zotero-browser-ensure-items-mode)
  (zotero-browser-ensure-write-access)
  (let* ((type zotero-browser-type)
         (id zotero-browser-id)
         (ewoc zotero-browser-ewoc)
         (node (ewoc-locate ewoc))
         ;; Top-level attachments can be created by excluding the parentItem
         ;; property or setting it to false.
         (parent (cond
                  ((equal arg '(4)) :json-false)
                  ((null node) :json-false)
                  (t (ewoc-data node))))
         (linkmode (completing-read "Select a linkmode: " (zotero-attachment-linkmodes) nil t nil nil zotero-browser-default-linkmodes))
         (template (copy-tree (zotero-cache-attachment-template linkmode))))
    (cl-pushnew linkmode zotero-browser-default-linkmodes :test #'equal)
    (pcase linkmode
      ("imported_file"
       (let* ((file (expand-file-name (read-file-name "Select file: " nil nil t)))
              (attributes (zotero-file-attributes file))
              (filename (file-name-nondirectory file))
              (filesize (plist-get attributes :filesize))
              (content-type (plist-get attributes :content-type))
              (md5 (plist-get attributes :md5))
              (mtime (plist-get attributes :mtime))
              (accessdate (plist-get attributes :accessdate))
              ;; REVIEW: charset cannot be determined without external tools
              (data (thread-first template
                      (plist-put :parentItem parent)
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
                      (plist-put :mtime nil))))
         (when-let ((object (zotero-cache-save data "items" type id))
                    (key (plist-get object :key)))
           (unless (zotero-upload-attachment key file nil :type type :id id)
             (error "Failed to associate attachment with item %s" key))
           (display-buffer (zotero-edit-item data type id) zotero-browser-edit-buffer-action))))
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
              (data (thread-first template
                      (plist-put :parentItem parent)
                      (plist-put :title filename)
                      (plist-put :accessDate accessdate)
                      (plist-put :contentType content-type)
                      ;; (plist-put :charset charset) ; charset cannot be determined without external tools
                      (plist-put :path file))))
         (when-let ((object (zotero-cache-save data "items" type id))
                    (key (plist-get object :key)))
           (display-buffer (zotero-edit-item (plist-get object :data) type id) zotero-browser-edit-buffer-action))))
      ("linked_url"
       (if (eq parent :json-false)
           (user-error "Links to URLs are not allowed as top-level items")
         (pop-to-buffer (zotero-edit-item template type id) zotero-browser-edit-buffer-action))))))

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
         (data (zotero-lib-plist-get* entry :object :data))
         (itemtype (zotero-lib-plist-get* entry :object :data :itemType))
         (linkmode (zotero-lib-plist-get* entry :object :data :linkMode))
         (filename (zotero-lib-plist-get* entry :object :data :filename))
         (hash (zotero-lib-plist-get* entry :object :data :md5))
         (dir (concat (file-name-as-directory zotero-cache-storage-dir) key))
         (path (concat (file-name-as-directory dir) filename)))
    (when (and (equal itemtype "attachment")
               (equal linkmode "imported_file"))
      (let* ((file (expand-file-name (read-file-name "Select file: " dir nil t path)))
             (attributes (zotero-file-attributes file))
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
        (unless (zotero-upload-attachment key file hash :type type :id id)
          (error "Failed to associate attachment with item %s" key))
        (display-buffer (zotero-edit-item data type id) zotero-browser-edit-buffer-action)))))

(defun zotero-browser-find-attachment ()
  "Return the path of the attachment of the current entry."
  (zotero-browser-ensure-items-mode)
  (zotero-browser-ensure-item-at-point)
  (when-let ((ewoc zotero-browser-ewoc)
             (type zotero-browser-type)
             (id zotero-browser-id)
             (node (ewoc-locate ewoc))
             (key (ewoc-data node))
             (entry (zotero-cache-synccache "item" key type id))
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
  "Download the attachment of the current entry."
  (zotero-browser-ensure-items-mode)
  (zotero-browser-ensure-item-at-point)
  (let* ((type zotero-browser-type)
         (id zotero-browser-id)
         (ewoc zotero-browser-ewoc)
         (node (ewoc-locate ewoc))
         (key (ewoc-data node))
         (entry (zotero-cache-synccache "item" key type id))
         (filename (zotero-lib-plist-get* entry :object :data :filename))
         (dir (or dir (concat (file-name-as-directory zotero-cache-storage-dir) key))))
    (unless (file-exists-p dir)
      (make-directory dir t))
    (zotero-download-file key filename dir t :type type :id id)))

;; TODO: finish this function
(defun zotero-browser-import-attachment ()
  "Create a new attachment with the current entry as parent.
With a `C-u' prefix, create a new top level attachment."
  (interactive)
  (zotero-browser-ensure-items-mode)
  (zotero-browser-ensure-write-access)
  (let* ((type zotero-browser-type)
         (id zotero-browser-id)
         (ewoc zotero-browser-ewoc)
         (template (copy-tree (zotero-cache-attachment-template "imported_file")))
         (file (expand-file-name (read-file-name "Select file: " nil nil t)))
         (attributes (zotero-file-attributes file))
         (filename (file-name-nondirectory file))
         (filesize (plist-get attributes :filesize))
         (content-type (plist-get attributes :content-type))
         (md5 (plist-get attributes :md5))
         (mtime (plist-get attributes :mtime))
         (accessdate (plist-get attributes :accessdate))
         ;; REVIEW: charset cannot be determined without external tools
         (data (thread-first template
                 (plist-put :parentItem parent)
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
                 (plist-put :mtime nil))))
    (when-let ((object (zotero-cache-save data "items" type id))
               (key (plist-get object :key)))
      (unless (zotero-upload-attachment key file nil :type type :id id)
        (error "Failed to associate attachment with item %s" key))
      (display-buffer (zotero-edit-item data type id) zotero-browser-edit-buffer-action))))

;; TODO: finish this function
(defun zotero-browser-recognize-attachment ()
  "Recognize content of the current entry."
  (interactive)
  (zotero-browser-ensure-items-mode)
  (zotero-browser-ensure-write-access)
  (zotero-browser-ensure-item-at-point)
  (let* ((type zotero-browser-type)
         (id zotero-browser-id)
         (collection zotero-browser-collection)
         (ewoc zotero-browser-ewoc)
         (node (ewoc-locate ewoc))
         (key (ewoc-data node))
         (entry (zotero-cache-synccache "item" key type id))
         (attachment-data (zotero-lib-plist-get* entry :object :data))
         (itemtype (plist-get attachment-data :itemType))
         (linkmode (plist-get attachment-data :linkMode))
         (filename (plist-get attachment-data :filename)))
    (when (and (equal itemtype "attachment")
               (equal linkmode "imported_file"))
      (let* ((file (zotero-browser-find-attachment))
             (attributes (zotero-file-attributes file))
             (content-type (plist-get attributes :content-type)))
	;; Check whether a given PDF could theoretically be recognized
        (when (and (eq (zotero-browser--level key) 1)
                   (equal content-type "application/pdf"))
          (let* ((response (zotero-recognize file))
                 (metadata (zotero-response-data response))
                 (result (cond
                          ((plist-member metadata :arxiv)
                           (zotero-arxiv (plist-get metadata :arxiv)))
                          ((plist-member metadata :doi)
                           (zotero-crossref (plist-get metadata :doi)))
                          ((plist-member metadata :isbn)
                           (zotero-openlibrary (plist-get metadata :isbn)))
                          (t
                           (zotero-recognize-parse-metadata metadata)))))
            (when (and (string-empty-p (plist-get result :abstractNote))
                       (plist-member metadata :abstract))
              (setq result (plist-put result :abstractNote (plist-get metadata :abstract))))
            (when (and (string-empty-p (plist-get result :language))
                       (plist-member metadata :language))
              (setq result (plist-put result :language (plist-get metadata :language))))
	    ;; Put new item in same collections as the attachment
            (when collection
              (setq result (plist-put result :collections (vector collection))))
            ;; Save the item and place it as a child of the new item
            (when-let ((object (zotero-cache-save result "items" type id))
                       (key (plist-get object :key)))
              (zotero-cache-save (plist-put attachment-data :parentItem key) "items" type id))))))))

(defun zotero-browser-set-fulltext ()
  "Set the full-text content of the current entry."
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
         (itemtype (zotero-lib-plist-get* entry :object :data :itemType))
         (linkmode (zotero-lib-plist-get* entry :object :data :linkMode))
         (filename (zotero-lib-plist-get* entry :object :data :filename)))
    (when (and (equal itemtype "attachment")
               (equal linkmode "imported_file"))
      (let* ((file (zotero-browser-find-attachment))
             (attributes (zotero-file-attributes file))
             (content-type (plist-get attributes :content-type)))
        ;; TODO: add full-text content to cache
        (zotero-fulltext-index-item key file content-type :type type :id id)))))

(defun zotero-browser-libraries ()
  "Create a libraries browser buffer."
  (let ((buffer (get-buffer-create "*Zotero Libraries*")))
    (with-current-buffer buffer
      (zotero-browser-libraries-mode)
      (let* ((user (zotero-cache-library "user"))
             (groups (zotero-cache-group))
             (ewoc (ewoc-create #'zotero-browser--library-pp))
             (inhibit-read-only t))
        (erase-buffer)
        (setq zotero-browser-ewoc ewoc)
        (thread-last user
          (ht-keys)
          (seq-do (lambda (key) (ewoc-enter-last ewoc key))))
        (thread-last groups
          (zotero-cache-sort-by :name 'asc)
          (seq-do (lambda (key) (ewoc-enter-last ewoc key))))))
    buffer))

(defun zotero-browser-collections (&optional resource type id)
  "Create a collections browser buffer."
  (let ((buffer (get-buffer-create zotero-browser-collections-buffer-name)))
    (with-current-buffer buffer
      (zotero-browser-collections-mode)
      (let ((ewoc (ewoc-create #'zotero-browser--collection-pp))
            (inhibit-read-only t))
        ;; Remove previous entries
        (erase-buffer)
        (when (and type id resource)
          (let* ((table (zotero-cache-synccache resource nil type id))
                 (keys (zotero-cache-sort-by :name 'asc table)))
            (setq zotero-browser-ewoc ewoc
                  zotero-browser-type type
                  zotero-browser-resource resource
                  zotero-browser-id id
                  zotero-browser-keys keys)
            (seq-do (lambda (key) (ewoc-enter-last ewoc key)) keys)
            (zotero-browser-expand-level zotero-browser-default-collection-level)
            (zotero-browser-items "items-top" nil type id)))))
    buffer))

(defun zotero-browser-items (&optional resource collection type id)
  "Create an items buffer."
  (let ((buffer (get-buffer-create zotero-browser-items-buffer-name)))
    (with-current-buffer buffer
      (zotero-browser-items-mode)
      (let ((ewoc (ewoc-create #'zotero-browser--item-pp))
            (inhibit-read-only t))
        ;; Remove previous entries
        (erase-buffer)
        (when (and type id resource)
          (let* ((table (zotero-cache-synccache resource collection type id))
                 (keys (zotero-cache-sort-by :date 'asc table)))
            (setq zotero-browser-ewoc ewoc
                  zotero-browser-type type
                  zotero-browser-id id
                  zotero-browser-resource resource
                  zotero-browser-collection collection
                  zotero-browser-keys keys)
            (dolist (key keys)
              ;; Create a new node if key is not a child
              (unless (zotero-cache-parentitem key table)
                (ewoc-enter-last ewoc key))
              ;; Then create nodes for the children of key
              (when-let ((children (ht-keys (zotero-cache-subitems key table))))
                (dolist (child children)
                  (ewoc-enter-last ewoc child))))
            (zotero-browser-expand-level zotero-browser-default-item-level)))))
    buffer))

(defun zotero-browser-display ()
  "Display current library or collection."
  (interactive)
  (zotero-browser-ensure-browser-buffer)
  (let* ((ewoc zotero-browser-ewoc)
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
              (id zotero-browser-id)
              (table (zotero-cache-synccache "collections" nil type id)))
         (display-buffer (zotero-browser-items "collection-items" key type id))))
      ('zotero-browser-items-mode
       (let* ((type zotero-browser-type)
              (id zotero-browser-id)
              (entry (zotero-cache-synccache "item" key type id))
              (data (zotero-lib-plist-get* entry :object :data)))
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

(provide 'zotero-browser)

;;; zotero-browser.el ends here
