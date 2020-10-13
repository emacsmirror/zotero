;;; zotero-browser.el --- Interface to Zotero  -*- lexical-binding: t; -*-

;; Author: Folkert van der Beek <folkertvanderbeek@gmail.com>
;; Created: 2020-03-27
;; Version: 0.1
;; Keywords: zotero, hypermedia
;; Package-Requires: ((emacs "25.1") (oauth "1.0.4") (request "0.3.2"))
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

;; (require 'zotero-lib)
(require 'zotero-lib)
(require 'zotero-cache)
(require 'zotero-edit)
(require 'cl-lib)
(require 'ewoc)
(require 'seq)
(require 'subr-x)
(require 'ht)

;;;; Variables

;; (defcustom zotero-browser-collections-buffer-window-width 0.3
;;   "- `nil' means to leave the width of the chosen window alone.

;;   - An integer specifies the desired total width of the chosen
;;   window in columns.

;;   - A floating-point number specifies the fraction of the chosen
;;   window’s desired total width with respect to the total width
;;   of the frame’s root window."
;;   :group 'zotero-browser
;;   :type '(choice (const :tag "Leave width alone" nil)
;;                  (float :tag "Fraction of total width")
;;                  (integer :tag "Width in columns")))

;; (defcustom zotero-browser-show-buffer-window-width 0.3
;;   "- `nil' means to leave the width of the chosen window alone.

;;   - An integer specifies the desired total width of the chosen
;;   window in columns.

;;   - A floating-point number specifies the fraction of the chosen
;;   window’s desired total width with respect to the total width
;;   of the frame’s root window."
;;   :group 'zotero-browser
;;   :type '(choice (const :tag "Leave width alone" nil)
;;                  (float :tag "Fraction of total width")
;;                  (integer :tag "Width in columns")))

(defvar zotero-browser-default-itemtypes nil
  "Default itemtypes when creating a new item.")

(defvar zotero-browser-default-linkmodes nil
  "Default linkmodes when creating a new item.")

(defvar zotero-browser-libraries-update-hook nil
  "Hook run when the libraries list changes.")

(defvar zotero-browser-collections-update-hook nil
  "Hook run when the collections list changes.")

(defvar zotero-browser-items-update-hook nil
  "Hook run when the items list changes.")

(defvar zotero-browser-items-buffer-action '((display-buffer-same-window) . ((window-width . 0.4)(preserve-size . (t . nil)))))

(defvar zotero-browser-libraries-buffer-action '((display-buffer-reuse-window display-buffer-in-side-window) . ((side . left)(slot . -1)(window-width . 0.3)(window-height . fit-window-to-buffer)(preserve-size . (t . nil))(reusable-frames . nil))))

(defvar zotero-browser-collections-buffer-action '((display-buffer-reuse-window display-buffer-in-side-window) . ((side . left)(slot . 0)(window-width . 0.3)(preserve-size . (t . nil))(reusable-frames . nil))))

(defvar zotero-browser-show-buffer-action '((display-buffer-reuse-window display-buffer-in-side-window) . ((side . right)(slot . 0)(window-width . 0.3)(preserve-size . (t . nil))(reusable-frames . nil))))

(defconst zotero-browser-base (file-name-directory load-file-name))

(defvar zotero-browser-padding 1
  "Set the number of characters preceding each entry")

(defvar-local zotero-browser-ewoc nil)
(defvar-local zotero-browser-type nil)
(defvar-local zotero-browser-id nil)
(defvar-local zotero-browser-collections nil)
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
    (define-key map (kbd "g") #'zotero-browser-revert)
    (define-key map (kbd "n") #'zotero-browser-goto-next)
    (define-key map (kbd "p") #'zotero-browser-goto-prev)
    (define-key map (kbd "C-c C-n") #'zotero-browser-next-collection)
    (define-key map (kbd "C-c C-p") #'zotero-browser-prev-collection)
    (define-key map (kbd "e") #'zotero-browser-edit)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Local keymap for `zotero-browser-collections-mode'.")

(defvar zotero-browser-items-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'zotero-browser-open-attachment)
    (define-key map (kbd "TAB") #'zotero-browser-toggle)
    (define-key map (kbd "<backtab>") #'zotero-browser-cycle)
    (define-key map (kbd "g") #'zotero-browser-revert)
    (define-key map (kbd "n") #'zotero-browser-goto-next)
    (define-key map (kbd "p") #'zotero-browser-goto-prev)
    (define-key map (kbd "C-c C-n") #'zotero-browser-next-collection)
    (define-key map (kbd "C-c C-p") #'zotero-browser-prev-collection)
    (define-key map (kbd "e") #'zotero-browser-edit)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Local keymap for `zotero-items-mode'.")

;;;; Menu

(easy-menu-define zotero-browser-collections-mode-menu zotero-browser-collections-mode-map
  "Menu for `zotero-browser-collections-mode'."
  `("Zotero-Browser"
    "--"
    ["Quit" quit-window :help "Quit Zotero-Browser"]
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
    (display-buffer collections-buffer zotero-browser-collections-buffer-action)))

(defun zotero-browser-libraries ()
  "Create a libraries browser buffer."
  (let ((buffer (get-buffer-create "*Zotero Libraries*")))
    (with-current-buffer buffer
      (zotero-browser-libraries-mode)
      (setq zotero-browser-table nil
            zotero-browser-ewoc nil
            zotero-browser-type nil
            zotero-browser-id nil)
      (let* ((table (ht-get zotero-cache "libraries"))
             (user (ht-select (lambda (key value) (equal (plist-get value :type) "user")) table))
             (groups (ht-select (lambda (key value) (equal (plist-get value :type) "group")) table))
             (ewoc (ewoc-create #'zotero-browser--library-pp nil nil))
             (inhibit-read-only t))
        (erase-buffer)
        (setq zotero-browser-table table)
        (thread-last user
          (ht-keys)
          (seq-do (lambda (key) (ewoc-enter-last ewoc key))))
        (thread-last groups
          (ht-keys)
          (seq-do (lambda (key) (ewoc-enter-last ewoc key))))
        (setq zotero-browser-ewoc ewoc)
        (when-let ((node (ewoc-nth ewoc 0))
                   (key (ewoc-data node)))
          (ewoc-goto-node ewoc node)
          (setq zotero-browser-type (plist-get (ht-get table key) :type)
                zotero-browser-id (plist-get (ht-get table key) :id)))))
    buffer))

(cl-defun zotero-browser-collections (&key type id)
  "Create a collections browser buffer."
  (let ((buffer (get-buffer-create zotero-browser-collections-buffer-name)))
    (with-current-buffer buffer
      (zotero-browser-collections-mode)
      (setq zotero-browser-table nil
            zotero-browser-ewoc nil
            zotero-browser-type nil
            zotero-browser-id nil)
      (let ((ewoc (ewoc-create #'zotero-browser--collection-pp nil nil nil))
            (inhibit-read-only t))
        ;; Remove previous entries
        (erase-buffer)
        (when (and type id)
          (let* ((libraries (ht-get zotero-cache "libraries"))
                 (library (ht-select (lambda (key value) (equal (plist-get value :type) type)) libraries))
                 (table (ht-get* zotero-cache "synccache" id "collections"))
                 (selection (zotero-browser--filter (lambda (elt) (eq (plist-get elt :parentCollection) :json-false)) table)))
            (setq zotero-browser-table table
                  zotero-browser-ewoc ewoc
                  zotero-browser-type type
                  zotero-browser-id id)
            (thread-last selection
              (zotero-browser--sort-by (lambda (elt) (plist-get elt :name)) #'string-lessp)
              (seq-do (lambda (key) (ewoc-enter-last ewoc key))))
            (zotero-browser-expand-level zotero-browser-default-collection-level)
            (zotero-browser-items :type type :id id :resource 'items-top)))))
    buffer))

(cl-defun zotero-browser-items (&key type id resource key)
  "Create an items buffer."
  (let ((buffer (get-buffer-create zotero-browser-items-buffer-name)))
    (with-current-buffer buffer
      (zotero-browser-items-mode)
      (setq zotero-browser-table nil
            zotero-browser-ewoc nil
            zotero-browser-type nil
            zotero-browser-id nil
            zotero-browser-collections nil)
      (let ((ewoc (ewoc-create #'zotero-browser--item-pp nil nil nil))
            (inhibit-read-only t))
        ;; Remove previous entries
        (erase-buffer)
        (when (and type id)
          (let* ((table (if (eq resource 'deletions) (ht-get* zotero-cache "deletions" id "items")
                          (ht-get* zotero-cache "synccache" id "items")))
                 (selection (pcase resource
                              ('collection
                               (zotero-browser--filter (lambda (elt) (seq-contains-p (plist-get elt :collections) key)) table))
                              ('items
                               table)
                              ('items-top
                               (zotero-browser--filter (lambda (elt) (eq (plist-get elt :collections) [])) table))
                              ('deletions
                               table))))
            (setq zotero-browser-table table
                  zotero-browser-ewoc ewoc
                  zotero-browser-type type
                  zotero-browser-id id
                  zotero-browser-collection key)
            (thread-last selection
              (zotero-browser--sort-by (lambda (elt) (plist-get elt :title)) #'string-lessp)
              (seq-do (lambda (key) (ewoc-enter-last ewoc key))))
            (zotero-browser-expand-level zotero-browser-default-item-level)))))
    buffer))

(defun zotero-browser--add (ewoc node table)
  "Add items of TABLE after NODE in ewoc"
  (let ((keys (pcase major-mode
                ('zotero-browser-collections-mode
                 (zotero-browser--sort-by (lambda (elt) (plist-get elt :name)) #'string-lessp table))
                ('zotero-browser-items-mode
                 (zotero-browser--sort-by (lambda (elt) (plist-get elt :title)) #'string-lessp table)))))
    (while keys
      (setq node (ewoc-enter-after ewoc node (pop keys))))))

(defun zotero-browser-revert ()
  "Revert the buffer."
  (interactive)
  (ewoc-refresh zotero-browser-ewoc))

(defun zotero-browser-display ()
  "Display current library or collection."
  (interactive)
  (pcase major-mode
    ('zotero-browser-libraries-mode
     (let* ((node (ewoc-locate zotero-browser-ewoc))
            (key (ewoc-data node))
            (table zotero-browser-table)
            (type (plist-get (ht-get table key) :type))
            (id (plist-get (ht-get table key) :id)))
       (setq zotero-browser-type type
             zotero-browser-id id)
       (display-buffer (zotero-browser-collections :type type :id id))
       (display-buffer (zotero-browser-items :type type :id id :resource 'items-top))))
    ('zotero-browser-collections-mode
     (let* ((ewoc zotero-browser-ewoc)
            (node (ewoc-locate ewoc))
            (key (ewoc-data node))
            (table zotero-browser-table)
            (type zotero-browser-type)
            (id zotero-browser-id))
       (display-buffer (zotero-browser-items :type type :id id :resource 'collection :key key))))
    ('zotero-browser-items-mode
     (let* ((ewoc zotero-browser-ewoc)
            (node (ewoc-locate ewoc))
            (key (ewoc-data node))
            (table zotero-browser-table)
            (type zotero-browser-type)
            (id zotero-browser-id)
            (value (ht-get table key))
            (data (zotero-lib-plist-get* value :object :data)))
       (display-buffer (zotero-edit-item :type type :id id :data data :locale zotero-lib-locale) zotero-browser-show-buffer-action)))))

(defun zotero-browser-goto-next ()
  "Move point to the next item."
  (interactive)
  (let ((ewoc zotero-browser-ewoc))
    (when (ewoc-nth ewoc 0)
      (ewoc-goto-next ewoc 1)
      (zotero-browser-display))))

(defun zotero-browser-goto-prev ()
  "Move point to the previous item."
  (interactive)
  (let ((ewoc zotero-browser-ewoc))
    (when (ewoc-nth ewoc 0)
      (ewoc-goto-prev ewoc 1)
      (zotero-browser-display))))

(defun zotero-browser-next-collection ()
  "Move point to the next collection."
  (interactive)
  (let ((buffer (current-buffer)))
    (pop-to-buffer zotero-browser-collections-buffer-name)
    (zotero-browser-goto-next)
    (pop-to-buffer buffer)))

(defun zotero-browser-prev-collection ()
  "Move point to the previous collection."
  (interactive)
  (let ((buffer (current-buffer)))
    (pop-to-buffer zotero-browser-collections-buffer-name)
    (zotero-browser-goto-prev)
    (pop-to-buffer buffer)))

(defun zotero-browser-toggle ()
  "Expand or collapse the children of the current item."
  (interactive)
  (let* ((inhibit-read-only t)
         (ewoc zotero-browser-ewoc)
         (node (ewoc-locate ewoc))
         (key (ewoc-data node)))
    (when (zotero-browser--has-children-p key)
      (if (zotero-browser--expanded-p ewoc node)
          (progn
            (zotero-browser--prefix "▸" (ewoc-location node))
            (zotero-browser-collapse ewoc node))
        (zotero-browser--prefix "▾" (ewoc-location node))
        (zotero-browser-expand ewoc node)))))

(defun zotero-browser-cycle ()
  "Cycle the visibility of children."
  (interactive)
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
  (let ((ewoc zotero-browser-ewoc)
        (inhibit-read-only t))
    (save-excursion
      (when-let ((node (ewoc-nth ewoc 0)))
        (ewoc-goto-node ewoc node)
        (while
            (let ((node (ewoc-locate ewoc)))
              (unless (zotero-browser--expanded-p ewoc node)
                (zotero-browser-expand ewoc node))
              (prog1
                  ;; End-test of while loop
                  (ewoc-next ewoc node)
                (ewoc-goto-next ewoc 1))))))))

(defun zotero-browser-collapse-all ()
  "Collapse all children."
  (interactive)
  (let ((ewoc zotero-browser-ewoc)
        (inhibit-read-only t))
    (save-excursion
      (when-let ((node (ewoc-nth ewoc 0)))
        (ewoc-goto-node ewoc node)
        (while
            (let ((node (ewoc-locate ewoc)))
              (when (zotero-browser--expanded-p ewoc node)
                (zotero-browser-collapse ewoc node))
              (prog1
                  (ewoc-next ewoc node)
                (ewoc-goto-next ewoc 1))))))))

(defun zotero-browser-ensure-browser-buffer ()
  (unless (eq major-mode 'zotero-browser-collections-mode)
    (error "Current buffer is not an zotero-browser buffer")))

(defun zotero-browser-expand (ewoc node)
  "Expand the children of NODE in EWOC."
  (let* ((key (ewoc-data node))
         (table (zotero-browser--get-children key)))
    (zotero-browser--add ewoc node table)))

(defun zotero-browser-collapse (ewoc node)
  "Collapse the children of NODE in EWOC."
  (let (parents nodes)
    (while
        (let* ((key (ewoc-data node))
               (next-node (ewoc-next ewoc node))
               (next-key (when next-node (ewoc-data next-node)))
               (parent (zotero-browser--get-parent next-key)))
          (when (equal parent key)
            (push key parents))
          (when (member parent parents)
            (setq node next-node)
            (push node nodes))))
    (apply #'ewoc-delete ewoc nodes)))

(defun zotero-browser-expand-level (num)
  "Expand children till LEVEL."
  (interactive)
  (let ((ewoc zotero-browser-ewoc)
        (inhibit-read-only t))
    (save-excursion
      (when-let ((node (ewoc-nth ewoc 0)))
        (ewoc-goto-node ewoc node)
        (while
            (let* ((node (ewoc-locate ewoc))
                   (key (ewoc-data node))
                   (level (zotero-browser--get-level key)))
              (when (zotero-browser--has-children-p key)
                (cond
                 ((and (zotero-browser--expanded-p ewoc node)
                       (>= level num))
                  (zotero-browser--prefix "▸" (ewoc-location node))
                  (zotero-browser-collapse ewoc node))
                 ((and (not (zotero-browser--expanded-p ewoc node))
                       (or (< level num) (eq num 0)))
                  (zotero-browser--prefix "▾" (ewoc-location node))
                  (zotero-browser-expand ewoc node))
                 (t
                  (zotero-browser--prefix "▸" (ewoc-location node)))))
              (prog1
                  ;; End-test of while loop
                  (ewoc-next ewoc node)
                (ewoc-goto-next ewoc 1))))))))

(defun zotero-browser-open (title path contenttype encoding)
  (let* ((buffer (get-buffer-create title))
         (handle (mm-make-handle buffer (list contenttype) encoding)))
    (with-current-buffer buffer (insert-file-contents-literally path))
    (mm-display-part handle)
    (kill-buffer buffer)))

(defun zotero-browser-open-externally (path)
  "Open PATH in an external program."
  (pcase system-type
    ((and 'windows-nt)
     (w32-shell-execute "open" path))
    ('darwin
     (call-process-shell-command "open" (shell-quote-argument path) nil 0))
    ('cygwin
     (call-process-shell-command "cygstart" (shell-quote-argument path) nil 0))
    ('gnu/linux
     (call-process-shell-command "xdg-open" (shell-quote-argument path) nil 0))))

(defun zotero-browser-open-imported-file (object)
  (let* ((title (zotero-lib-plist-get* object :object :data :title))
         (filename (zotero-lib-plist-get* object :object :data :filename))
         (path (expand-file-name (zotero-browser-find-attachment)))
         (contenttype (zotero-lib-plist-get* object :object :data :contentType))
         (charset (zotero-lib-plist-get* object :object :data :charset)))
    (zotero-browser-open title path contenttype charset)))

(defun zotero-browser-open-imported-url (object)
  (let* ((title (zotero-lib-plist-get* object :object :data :title))
         (key (zotero-lib-plist-get* object :object :data :key))
         (filename (zotero-lib-plist-get* object :object :data :filename))
         (path (expand-file-name (zotero-browser-find-attachment)))
         (contenttype (zotero-lib-plist-get* object :object :data :contentType))
         (dir (concat temporary-file-directory key)))
    (if (equal contenttype "application/pdf")
        (zotero-browser-open title path contenttype charset)
      (let* ((unzip (or (executable-find "unzip")
                        (error "Unable to find executable \"unzip\"")))
             (exit-status (call-process unzip nil nil nil "-o" "-d" dir path)))
        (if (eq exit-status 0)
            (let ((path (concat (file-name-as-directory dir) filename)))
              (browse-url-file-url path))
          (error "Error extracting snapshot"))))))

(defun zotero-browser-open-linked-file (object)
  (let* ((title (zotero-lib-plist-get* object :object :data :title))
         (path (zotero-lib-plist-get* object :object :data :path))
         (contenttype (zotero-lib-plist-get* object :object :data :contentType))
         (charset (zotero-lib-plist-get* object :object :data :charset)))
    (zotero-browser-open title path contenttype charset)))

(defun zotero-browser-open-linked-url (object)
  (let ((url (zotero-lib-plist-get* object :object :data :url)))
    (browse-url url)))

(defun zotero-browser-open-attachment ()
  "Open attachment at point."
  (interactive)
  (let* ((ewoc zotero-browser-ewoc)
         (key (ewoc-data (ewoc-locate ewoc)))
         (table zotero-browser-table)
         (object (ht-get table key))
         (itemtype (zotero-lib-plist-get* object :object :data :itemType)))
    (when (equal itemtype "attachment")
      (let ((linkmode (zotero-lib-plist-get* object :object :data :linkMode)))
        (pcase linkmode
          ("imported_file" (zotero-browser-open-imported-file object))
          ("imported_url" (zotero-browser-open-imported-url object))
          ("linked_file" (zotero-browser-open-linked-file object))
          ("linked_url" (zotero-browser-open-linked-url object)))))))

(defun zotero-browser-find-attachment ()
  "Return the path of attachment at point."
  (interactive)
  (when-let ((ewoc zotero-browser-ewoc)
             (key (ewoc-data (ewoc-locate ewoc)))
             (table zotero-browser-table)
             (value (ht-get table key))
             (filename (zotero-lib-plist-get* value :object :data :filename))
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
  (let* ((ewoc zotero-browser-ewoc)
         (key (ewoc-data (ewoc-locate ewoc)))
         (table zotero-browser-table)
         (value (ht-get table key))
         (filename (zotero-lib-plist-get* value :object :data :filename))
         (dir (or dir (concat (file-name-as-directory zotero-cache-storage-dir) key)))
         (type zotero-browser-type)
         (id zotero-browser-id)
         (token (zotero-auth-token))
         (api-key (zotero-auth-api-key token)))
    (unless (file-exists-p dir)
      (make-directory dir t))
    (zotero-lib-download-file :file filename :dir dir :type type :id id :key key :api-key api-key)))

(defun zotero-browser-edit ()
  "Edit current entry."
  (interactive)
  (pcase major-mode
    ('zotero-browser-libraries-mode
     ;; TODO
     )
    ('zotero-browser-collections-mode
     (let* ((ewoc zotero-browser-ewoc)
            (node (ewoc-locate ewoc))
            (key (ewoc-data node))
            (table zotero-browser-table)
            (type zotero-browser-type)
            (id zotero-browser-id)
            (value (ht-get table key))
            (data (zotero-lib-plist-get* value :object :data)))
       (display-buffer (zotero-edit-collection :type type :id id :data data) zotero-browser-show-buffer-action)))
    ('zotero-browser-items-mode
     (let* ((ewoc zotero-browser-ewoc)
            (node (ewoc-locate ewoc))
            (key (ewoc-data node))
            (table zotero-browser-table)
            (type zotero-browser-type)
            (id zotero-browser-id)
            (value (ht-get table key))
            (data (zotero-lib-plist-get* value :object :data)))
       (display-buffer (zotero-edit-item :type type :id id :data data :locale zotero-lib-locale) zotero-browser-show-buffer-action)))))

(defun zotero-browser-create ()
  "Create a new collection or item."
  (interactive)
  (pcase major-mode
    ('zotero-browser-libraries-mode
     ;; TODO
     )
    ('zotero-browser-collections-mode
     (let* ((type zotero-browser-type)
            (id zotero-browser-id))
       (display-buffer (zotero-edit-create-collection :type type :id id) zotero-browser-show-buffer-action)))
    ('zotero-browser-items-mode
     (let* ((type zotero-browser-type)
            (id zotero-browser-id)
            (itemtype (completing-read "Select an item type: " (zotero-cache-itemtypes) nil t nil nil zotero-browser-default-itemtypes)))
       (cl-pushnew itemtype zotero-browser-default-itemtypes :test #'equal)
       (display-buffer (zotero-edit-create-item :type type :id id :itemtype itemtype :locale zotero-lib-locale) zotero-browser-show-buffer-action)))))

(defun zotero-browser-create-note (&optional arg)
  "Create a new note.
With a `C-u' prefix, create a new top level note."
  (interactive "P")
  (when (eq major-mode 'zotero-browser-items-mode)
    (let* ((type zotero-browser-type)
           (id zotero-browser-id)
           (ewoc zotero-browser-ewoc)
           (node (ewoc-locate ewoc))
           (key (ewoc-data node))
           (parent (if (equal arg '(4)) nil key)))
      (display-buffer (zotero-edit-create-note :type type :id id :parent parent :itemtype "note" :locale zotero-lib-locale) zotero-browser-show-buffer-action))))

(defun zotero-browser-move-to-parent (&optional arg)
  "Move current entry to a parent item.
With a `C-u' prefix, move to top level."
  (interactive "P")
  (when (eq major-mode 'zotero-browser-items-mode)
    (let* ((node (ewoc-locate zotero-browser-ewoc))
           (key (ewoc-data node))
           (table zotero-browser-table)
           (entry (ht-get table key))
           (type zotero-browser-type)
           (id zotero-browser-id)
           (data (zotero-lib-plist-get* entry :object :data))
           (itemtype (zotero-lib-plist-get* entry :object :data :itemType))
           updated-data)
      (unless (or (equal itemtype "attachment") (equal itemtype "note"))
        (user-error "Item type %s cannot be moved to a parent"))
      (if (equal arg '(4))
          (setq updated-data (zotero-lib-plist-delete data :parentItem))
        (let* ((choices (ht-map (lambda (key value) (cons (zotero-lib-plist-get* value :object :data :title) key)) (ht-get* zotero-cache "synccache" id "items")))
               (name (completing-read "Select parent item:" choices nil t))
               (parent (cdr (assoc name choices)))
               (entry (ht-get table parent))
               (itemtype (zotero-lib-plist-get* entry :object :data :itemType)))
          (when (or (equal itemtype "attachment") (equal itemtype "note"))
            (user-error "Parent item cannot be a note or attachment"))
          (setq updated-data (plist-put data :parentItem parent))))
      (zotero-cache-save-item :type type :id id :data updated-data))))

(defun zotero-browser-copy-to-collection ()
  "Copy current entry to a collection."
  (interactive)
  (when (eq major-mode 'zotero-browser-items-mode)
    (let* ((node (ewoc-locate zotero-browser-ewoc))
           (key (ewoc-data node))
           (table zotero-browser-table)
           (entry (ht-get table key))
           (type zotero-browser-type)
           (id zotero-browser-id)
           (choices (ht-map (lambda (key value) (cons (zotero-lib-plist-get* value :object :data :name) key)) (ht-get* zotero-cache "synccache" id "collections")))
           (name (completing-read "Select collection:" choices nil t))
           (collection (cdr (assoc name choices)))
           (collections (zotero-lib-plist-get* entry :object :data :collections))
           (updated-collections (if (seq-contains-p collections collection)
                                    collections
                                  (vconcat (vector collection) collections)))
           (data (zotero-lib-plist-get* entry :object :data))
           (updated-data (plist-put data :collections updated-collections)))
      (zotero-cache-save-item :type type :id id :data updated-data))))

(defun zotero-browser-remove-from-collection ()
  "Remove current entry from a collection."
  (interactive)
  (when (eq major-mode 'zotero-browser-items-mode)
    (let* ((node (ewoc-locate zotero-browser-ewoc))
           (key (ewoc-data node))
           (table zotero-browser-table)
           (entry (ht-get table key))
           (type zotero-browser-type)
           (id zotero-browser-id)
           (collection zotero-browser-collection)
           (collections (zotero-lib-plist-get* entry :object :data :collections))
           (updated-collections (seq-remove (lambda (elt) (equal elt collection)) collections))
           (data (zotero-lib-plist-get* entry :object :data))
           (updated-data (plist-put data :collections updated-collections)))
      (zotero-cache-save-item :type type :id id :data updated-data)
      (display-buffer (zotero-browser-items :type type :id id :key collection)))))

(defun zotero-browser-delete ()
  "Delete current entry.
If region is active, delete entries in active region instead."
  (interactive)
  (pcase major-mode
    ('zotero-browser-collections-mode)
    ('zotero-browser-items-mode
     (let ((ewoc zotero-browser-ewoc)
           (type zotero-browser-type)
           (id zotero-browser-id))
       (if (use-region-p)
           (save-excursion
             (let* ((first-node (ewoc-locate ewoc (region-beginning)))
                    (last-node (ewoc-locate ewoc (region-end)))
                    (node first-node))
               (while
                   (progn
                     (ewoc-goto-node ewoc node)
                     (zotero-cache-delete :type type :id id :key (ewoc-data node))
                     (setq node (ewoc-next ewoc node))
                     (not (eq node (ewoc-next ewoc last-node)))))))
         (let ((node (ewoc-locate ewoc)))
           (zotero-cache-delete :type type :id id :key (ewoc-data node))))
       (display-buffer (zotero-browser-items :type zotero-browser-type :id zotero-browser-id :resource 'collection :key zotero-browser-collection))))))

(defun zotero-browser-restore ()
  "Restore current entry.
If region is active, delete entries in active region instead."
  (interactive)
  (pcase major-mode
    ('zotero-browser-collections-mode)
    ('zotero-browser-items-mode
     (let ((ewoc zotero-browser-ewoc)
           (type zotero-browser-type)
           (id zotero-browser-id))
       (if (use-region-p)
           (save-excursion
             (let* ((first-node (ewoc-locate ewoc (region-beginning)))
                    (last-node (ewoc-locate ewoc (region-end)))
                    (node first-node))
               (while
                   (progn
                     (ewoc-goto-node ewoc node)
                     (zotero-cache-restore :type type :id id :key (ewoc-data node))
                     (setq node (ewoc-next ewoc node))
                     (not (eq node (ewoc-next ewoc last-node)))))))
         (let ((node (ewoc-locate ewoc)))
           (zotero-cache-restore :type type :id id :key (ewoc-data node))))
       (display-buffer (zotero-browser-items :type zotero-browser-type :id zotero-browser-id :resource 'collection :key zotero-browser-collection))))))

(defun zotero-browser--prefix (string pos)
  (let* ((prefix (get-text-property (point) 'line-prefix))
         (spacing (substring prefix 0 -1)))
    (put-text-property (point) (line-end-position) 'line-prefix (concat spacing string))))

(defun zotero-browser--get-level (key)
  "Return the level of KEY."
  (let* ((table zotero-browser-table)
         (level 0))
    (while
        (let ((parent (zotero-browser--get-parent key)))
          (setq level (1+ level))
          (unless (or (eq parent :json-false) (null parent))
            (setq key parent))))
    level))

(defun zotero-browser--get-parent (key)
  "Return the parent of KEY, or nil."
  (let* ((table zotero-browser-table)
         (value (ht-get table key)))
    (pcase major-mode
      ('zotero-browser-collections-mode
       (zotero-lib-plist-get* value :object :data :parentCollection))
      ('zotero-browser-items-mode
       (zotero-lib-plist-get* value :object :data :parentItem)))))

(defun zotero-browser--get-children (key)
  "Return the children of KEY."
  (pcase major-mode
    ('zotero-browser-collections-mode
     (zotero-browser--get-subcollections key))
    ('zotero-browser-items-mode
     (zotero-browser--get-subitems key))))

(defun zotero-browser--get-subitems (key)
  "Return the subcollections of KEY."
  (zotero-browser--filter (lambda (elt) (equal (plist-get elt :parentItem) key)) zotero-browser-table))

(defun zotero-browser--get-subcollections (key)
  "Return the subcollections of KEY."
  (zotero-browser--filter (lambda (elt) (equal (plist-get elt :parentCollection) key)) zotero-browser-table))

(defun zotero-browser--has-children-p (key)
  "Return non-nil if KEY has children."
  (pcase major-mode
    ('zotero-browser-collections-mode
     (zotero-browser--has-subcollections-p key))
    ('zotero-browser-items-mode
     (zotero-browser--has-subitems-p key))))

(defun zotero-browser--has-subitems-p (key)
  "Return non-nil if KEY has subitems"
  (zotero-browser--some (lambda (elt) (equal (plist-get elt :parentItem) key))
                        zotero-browser-table))

(defun zotero-browser--has-subcollections-p (key)
  "Return non-nil if KEY has subcollections."
  (zotero-browser--some (lambda (elt) (equal (plist-get elt :parentCollection) key))
                        zotero-browser-table))

(defun zotero-browser--has-attachments-p (key)
  "Return non-nil if KEY has attachments."
  (zotero-browser--some (lambda (elt)
                          (and (equal (plist-get value :parentItem) key)
                               (equal (plist-get value :itemType) "attachment")))
                        zotero-browser-table))

(defun zotero-browser--has-notes-p (key)
  "Return non-nil if KEY has attachments."
  (zotero-browser--some (lambda (elt)
                          (and (equal (plist-get elt :parentItem) key)
                               (equal (plist-get elt :itemType) "note")))
                        zotero-browser-table))

(defun zotero-browser--expanded-p (ewoc node)
  "Return non-nil if NODE in EWOC is expanded."
  (let* ((key (ewoc-data node))
         (next-node (ewoc-next ewoc node))
         (next-key (when next-node (ewoc-data next-node)))
         (parent (zotero-browser--get-parent next-key)))
    (equal parent key)))

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

(defun zotero-browser--filter (pred table)
  "Select elements from TABLE for which PRED returns non-`nil'.
Return a list of the keys in TABLE. PRED is a function that takes
a data element as its first argument."
  (thread-last
      table
    (ht-select (lambda (key value)
                 ;; keep the predicate nil-safe
                 (ignore-error wrong-type-argument
                   (funcall pred (zotero-lib-plist-get* value :object :data)))))))

(defun zotero-browser--sort-by (function pred table)
  "Sort TABLE using PRED as a comparison function.
Return a list of the keys in TABLE. PRED is a function that takes
two data elements as its arguments. Elements of TABLE are
transformed by FUNCTION before being sorted. FUNCTION must be a
function of one argument."
  (thread-last
      (ht->alist table)
    (seq-sort-by (lambda (elt)
                   (funcall function (zotero-lib-plist-get* (cdr elt) :object :data)))
                 (lambda (a b)
                   ;; keep the predicate nil-safe
                   (ignore-error wrong-type-argument
                     (funcall pred a b))))
    (seq-map #'car)))

(defun zotero-browser--some (pred table)
  "Return non-nil if PRED is satisfied for at least one element of TABLE.
PRED is a function that takes a data element as its first
argument."
  (if (ht-find (lambda (key value) (funcall pred (zotero-lib-plist-get* value :object :data))) table) t nil))

(defun zotero-browser--library-pp (key)
  "Pretty print KEY."
  (let* ((table zotero-browser-table)
         (value (ht-get table key))
         (id (zotero-lib-plist-get* value :id))
         (type (zotero-lib-plist-get* value :type))
         (groups (ht-get zotero-cache "groups"))
         (name (pcase type
                 ("group" (zotero-lib-plist-get* (ht-get groups id) :data :name))
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
  (let* ((table zotero-browser-table)
         (value (ht-get table key))
         (level (zotero-browser--get-level key))
         (indentation (+ zotero-browser-padding level))
         (prefix (make-string indentation ?\s))
         (values (seq-map (lambda (key) (zotero-lib-plist-get* value :object :data key)) zotero-browser-collection-keys))
         (text (string-join values " ")))
    (add-text-properties 0 (length text) `(line-prefix ,prefix wrap-prefix ,prefix) text)
    (insert text)))

(defun zotero-browser--item-pp (key)
  "Pretty print KEY."
  (let* ((table zotero-browser-table)
         (value (ht-get table key))
         (itemtype (zotero-lib-plist-get* value :object :data :itemType))
         (level (zotero-browser--get-level key))
         (indentation (+ zotero-browser-padding level))
         (prefix (make-string indentation ?\s)))
    (let ((beg (point)))
      (pcase itemtype
        ("attachment"
         (let* ((linkmode (zotero-lib-plist-get* value :object :data :linkMode))
                (file (zotero-browser--attachment-icon linkmode)))
           (when (file-readable-p file)
             (let ((image (create-image file 'png)))
               (insert-image image itemtype)
               (insert " "))))
         (when-let ((title (zotero-lib-plist-get* value :object :data :title)))
           (insert title)))
        ("note"
         (let* ((itemtype (zotero-lib-plist-get* value :object :data :itemType))
                (file (zotero-browser--itemtype-icon itemtype)))
           (when file
             (let ((image (create-image file 'png)))
               (insert-image image itemtype)
               (insert " "))))
         (when-let ((note (zotero-lib-plist-get* value :object :data :note))
                    (text (replace-regexp-in-string "<[^>]+>" "" note)) ; Remove all HTML tags
                    (match (string-match "^.+$" text)) ; Match first non-empty line
                    (first-line (match-string-no-properties 0 text)))
           (insert first-line)))
        (_
         (dolist (key zotero-browser-item-keys)
           (pcase key
             ('key
              (when-let ((key (zotero-lib-plist-get* value :object :data :key)))
                (insert key)
                (insert " ")))
             ('version
              (when-let ((version (number-to-string (zotero-lib-plist-get* value :object :data :version))))
                (insert version)
                (insert " ")))
             ('itemtype
              (let* ((itemtype (zotero-lib-plist-get* value :object :data :itemType))
                     (file (zotero-browser--itemtype-icon itemtype)))
                (when file
                  (let ((image (create-image file 'png)))
                    (insert-image image itemtype)
                    (insert " ")))))
             ('title
              (when-let ((title (zotero-lib-plist-get* value :object :data :title)))
                (insert title)
                (insert " ")))
             ('creators
              (when-let ((creators (zotero-lib-plist-get* value :object :data :creators))
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
              (when-let ((date (zotero-lib-plist-get* value :object :data :date)))
                (insert date)
                (insert " ")))
             ('year
              (when-let ((date (zotero-lib-plist-get* value :object :data :date))
                         (match (string-match "[[:digit:]]\\{4\\}" date))
                         (year (match-string 0 date)))
                (insert year)
                (insert " ")))
             ('publisher
              (when-let ((publisher (zotero-lib-plist-get* value :object :data :publisher)))
                (insert publisher)
                (insert " ")))
             ('publication-title
              (when-let ((publication-title (zotero-lib-plist-get* value :object :data :publicationTitle)))
                (insert publication-title)
                (insert " ")))
             ('date-added
              (when-let ((date-added (zotero-lib-plist-get* value :object :data :dateAdded)))
                (insert date-added)
                (insert " ")))
             ('date-modified
              (when-let ((date-modified (zotero-lib-plist-get* value :object :data :dateModified)))
                (insert date-modified)
                (insert " ")))
             ('extra
              (when-let ((extra (zotero-lib-plist-get* value :object :data :extra)))
                (insert extra)
                (insert " ")))
             ('attachments
              (when (zotero-browser--has-attachments-p value)
                (let* ((icon "attach.png")
                       (dir (file-name-as-directory "img"))
                       (file (expand-file-name (concat dir icon) zotero-browser-base)))
                  (when (file-readable-p file)
                    (let ((image (create-image file 'png)))
                      (insert-image image "attachments")
                      (insert " "))))))
             ('notes)))))
      (add-text-properties beg (point) `(line-prefix ,prefix wrap-prefix ,prefix)))))

(defun zotero-browser-add-imported-file (file)
  "Add a new attachment with an imported file."
  (interactive "f")
  (let* ((type zotero-browser-type)
         (id zotero-browser-id)
         (ewoc zotero-browser-ewoc)
         (node (ewoc-locate ewoc))
         (key (ewoc-data node))
         (parent key)
         (attributes (zotero-lib-file-attributes file)))
    (display-buffer (zotero-edit-create-attachment :type type :id id :parent parent :linkmode "imported_file" :content-type (plist-get attributes :contenttype) :filename (plist-get attributes :filename) :md5 (plist-get attributes :md5) :mtime (plist-get attributes :mtime) :accessdate (plist-get attributes :accessdate) :locale zotero-lib-locale) zotero-browser-show-buffer-action)))

(defun zotero-browser-create-attachment (&optional arg)
  "Create a new attachment with the current entry as parent.
With a `C-u' prefix, create a new top level attachment."
  (interactive "P")
  (when (eq major-mode 'zotero-browser-items-mode)
    (let* ((type zotero-browser-type)
           (id zotero-browser-id)
           (ewoc zotero-browser-ewoc)
           (node (ewoc-locate ewoc))
           (key (ewoc-data node))
           (parent (if (equal arg '(4)) nil key))
           (linkmode (completing-read "Select a linkmode: " (zotero-lib-attachment-linkmodes) nil t nil nil zotero-browser-default-itemtypes)))
      (cl-pushnew linkmode zotero-browser-default-linkmodes :test #'equal)
      (when (equal linkmode "imported_file")
        (let* ((file (expand-file-name (read-file-name "Select file:" nil nil t)))
               (attributes (zotero-lib-file-attributes file))
               (filename (file-name-nondirectory file))
               (filesize (plist-get attributes :filesize))
               (content-type (plist-get attributes :content-type))
               (md5 (plist-get attributes :md5))
               (mtime (plist-get attributes :mtime))
               (accessdate (plist-get attributes :accessdate))
               (data (zotero-browser-attachment :type type :id id :parent parent :linkmode linkmode :content-type content-type :filename filename :accessdate accessdate))
               (saved-data (zotero-edit-save-item :type type :id id :data data))
               (key (plist-get saved-data :key))
               (token (zotero-auth-token))
               (api-key (zotero-auth-api-key token))
               (authorisation (zotero-lib-authorize-upload :type type :id id :key key :filename filename :filesize filesize :md5 md5 :mtime mtime :api-key api-key))
               (exists (plist-get authorisation :exists)))
          (if exists
              (message "File already exists on the server and was successfully associated with the specified item")
            (let ((response (zotero-lib-upload-file file (plist-get authorisation :url) (plist-get authorisation :contentType) (plist-get authorisation :prefix) (plist-get authorisation :suffix))))
              ;; A status-code 201 means the file was successfully uploaded.
              (if (eq (request-response-status-code response) 201)
                  (progn
                    (message "The file was successfully uploaded.")
                    (zotero-lib-register-upload :type type :id id :key key :uploadkey (plist-get authorisation :uploadKey) :api-key api-key))))))))))

(cl-defun zotero-browser-attachment (&key type id parent linkmode content-type charset filename accessdate)
  "Return an attachment object."
  ;; md5 and mtime can be edited directly in personal libraries for WebDAV-based
  ;; file syncing. They should not be edited directly when using Zotero File
  ;; Storage, which provides an atomic method (detailed below) for setting the
  ;; properties along with the corresponding file.
  (let* ((template (zotero-cache-attachment-template linkmode))
         (new-template (copy-tree template)))
    (when parent (plist-put new-template :parentItem parent))
    (when content-type (plist-put new-template :contentType content-type))
    (when charset (plist-put new-template :charset charset))
    (when filename
      (plist-put new-template :title filename)
      (plist-put new-template :filename filename))
    ;; (when md5
    ;;   (plist-put new-template :md5 md5))
    ;; (when mtime
    ;;   (plist-put new-template :mtime mtime))
    (when accessdate (plist-put new-template :accessDate accessdate))
    new-template))

(defun zotero-browser-upload-file (file)
  "Upload FILE."
  (let* ((ewoc zotero-browser-ewoc)
         ;; (key (ewoc-data (ewoc-locate ewoc)))
         ;; (table zotero-browser-table)
         ;; (value (ht-get table key))
         ;; (filename (zotero-lib-plist-get* value :object :data :filename))
         ;; (dir (or dir (concat (file-name-as-directory zotero-cache-storage-dir) key)))
         (type zotero-browser-type)
         (id zotero-browser-id)
         (token (zotero-auth-token))
         (api-key (zotero-auth-api-key token)))
    (unless (file-exists-p dir)
      (make-directory dir t))
    (zotero-lib-download-file :file filename :dir dir :type type :id id :key key :api-key api-key)))

(provide 'zotero-browser)

;;; zotero-browser.el ends here
