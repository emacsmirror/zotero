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
(require 'zotero-cache)
(require 'ewoc)
(require 'ht)

;;;; Variables
(defvar zotero-browser-padding 1
  "Set the number of characters preceding each entry")

(defvar-local zotero-browser-status nil)

(setq zotero-browser-collections-top (lambda (elt _arg) (eq (plist-get elt :parentCollection) :json-false)))

(setq zotero-browser-subcollections (lambda (elt arg) (equal (plist-get elt :parentCollection) arg)))

;;;; Keymap

(defvar zotero-browser-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'zotero-browser-display-items)
    (define-key map (kbd "SPC") #'zotero-browser-toggle-subcollections)
    (define-key map (kbd "TAB") #'zotero-browser-cycle-subcollections)
    (define-key map (kbd "n") #'zotero-browser-goto-next)
    (define-key map (kbd "p") #'zotero-browser-goto-prev)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Local keymap for `zotero-browser-mode'.")

(defvar zotero-browser-items-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'zotero-browser-show
      )
    (define-key map (kbd "SPC") #'zotero-browser-toggle-subitems)
    (define-key map (kbd "TAB") #'zotero-browser-cycle-subitems)
    (define-key map (kbd "g") #'zotero-browser-revert)
    (define-key map (kbd "n") #'zotero-browser-goto-next)
    (define-key map (kbd "p") #'zotero-browser-goto-prev)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Local keymap for `zotero-items-mode'.")

;;;; Menu

(easy-menu-define zotero-browser-mode-menu zotero-browser-mode-map
  "Menu for `zotero-browser-mode'."
  `("Zotero-Browser"
    "--"
    ["Quit" quit-window :help "Quit Zotero-Browser"]
    ["Customize" (customize-group 'zotero-browser)]))

;;;; Customization

(defgroup zotero-browser nil
  "Interface to Zotero-Browser."
  :group 'external)

(defcustom zotero-browser-buffer-name "*Zotero Browser*"
  "The default buffer name."
  :group 'zotero-browser
  :type 'string)

(defcustom zotero-browser-items-buffer-name "*Zotero Items*"
  "The default name of the items buffer."
  :group 'zotero-browser
  :type 'string)

(defcustom zotero-browser-collection-keys '(:name)
  "Keys to show in the collections browser"
  :group 'zotero-browser
  :type '(repeat (choice
                  (const :tag "Key" :key)
                  (const :tag "Version" :version)
                  (const :tag "Name" :name))))

(defcustom zotero-browser-item-keys '(:itemType :title :creators)
  "Keys to show in the items browser"
  :group 'zotero-browser
  :type
  '(repeat (choice
            (const :tag "Key" :key)
            (const :tag "Version" :version)
            (const :tag "Item Type" :itemType)
            (const :tag "Title" :title)
            (const :tag "Creators" :creators)
            (const :tag "Date" :date)
            (const :tag "Year" 'year)
            (const :tag "Publisher" :publisher)
            (const :tag "Publication Title" :publicationTitle)
            (const :tag "Date Added" :dateAdded)
            (const :tag "Date Modified" :dateModified)
            (const :tag "Extra" :extra)
            (const :tag "Attachments" 'attachments)
            (const :tag "Notes" 'notes))))

;;;; Mode

;;;###autoload
(define-derived-mode zotero-browser-mode special-mode "Zotero browser"
  "Major mode for the Zotero browser.

All currently available key bindings:

\\{zotero-browser-mode-map}"
  ;; Turn on highlighting
  (font-lock-mode 1)
  ;; Turn on word wrap
  (visual-line-mode 1))

(define-derived-mode zotero-browser-items-mode special-mode "Zotero browser"
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
  (if-let ((buffer (get-buffer zotero-browser-buffer-name)))
      ;; if the buffer is displayed, switch the window instead
      (progn
        (if-let ((window (get-buffer-window buffer)))
            (select-window window)
          (switch-to-buffer buffer))
        ;; TODO: make this a hook
        (zotero-browser-items-create))
    ;; if there's no buffer, create a new window
    (zotero-browser-create)))

(defun zotero-browser-create ()
  "Create a new zotero-browser buffer and start zotero-browser-mode."
  (switch-to-buffer (generate-new-buffer
                     zotero-browser-buffer-name))
  (zotero-browser-mode)
  (zotero-cache--maybe-initialize-cache)
  (let* ((libraries (ht-get zotero-cache "libraries"))
         (library (ht-find (lambda (key value) (equal (plist-get value :type) "user")) libraries))
         (userid (car library))
         (table (ht-get* zotero-cache "synccache" userid "collections")))
    ;; Remove previous entries
    (erase-buffer)
    (setq-local zotero-browser-library-type "user")
    (setq-local zotero-browser-library-id userid)
    (setq-local zotero-browser-table table)
    (setq-local zotero-browser-ewoc (ewoc-create #'zotero-browser--collection-pp nil nil nil))
    (zotero-browser-display-collections table zotero-browser-collections-top)
    (zotero-browser-indent)
    ;; TODO: make this a hook
    (zotero-browser-items-create)))

(defun zotero-browser-items-create ()
  "A hook to show the items when the browser is displayed.
Returns the items window."
  (interactive)
  (let ((buffer (zotero-browser--get-linked-buffer))
        (window (zotero-browser--get-linked-window)))
    (unless (window-live-p window)
      (save-selected-window
        (split-window-horizontally)
        (other-window 1)
        (if buffer
            (switch-to-buffer buffer)
          (setq buffer (zotero-browser--create-items-buffer :type zotero-browser-library-type :id zotero-browser-library-id))
          (switch-to-buffer buffer))
        (setq window (get-buffer-window buffer))))
    window))

(defun zotero-browser-display-collections (table func &rest args)
  "Populate the zotero buffer."
  (interactive)
  (let ((ewoc zotero-browser-ewoc)
        (items (zotero-browser--filter table func args)))
    (ht-each (lambda (key value) (ewoc-enter-last ewoc value)) items)
    (when-let ((node (ewoc-nth ewoc 0)))
      (ewoc-goto-node ewoc node)
      (zotero-browser-display-items))))

(defun zotero-browser-display-items ()
  "Populate the zotero items buffer."
  (interactive)
  (let* ((ewoc zotero-browser-ewoc)
         (node (ewoc-locate ewoc))
         (data (ewoc-data node))
         (collection (zotero-lib-plist-get* data :object :data :key))
         ;; (library-type (zotero-lib-plist-get* :data :library :type))
         (library-id (zotero-lib-plist-get* data :object :library :id))
         (items-window (zotero-browser-items-create))
         (items-buffer (window-buffer items-window)))
    (with-current-buffer items-buffer
      (let* ((inhibit-read-only t)
             (table (ht-get* zotero-cache "synccache" (number-to-string library-id) "items"))
             (ewoc zotero-browser-ewoc)
             (items (ht-select (lambda (key value)
                                 (let ((collections (zotero-lib-plist-get* value :object :data :collections)))
                                   (seq-contains-p collections collection)))
                               table)))
        ;; Remove previous entries
        (erase-buffer)
        (setq-local zotero-browser-table table)
        ;; (setq-local zotero-browser-ewoc (ewoc-create #'zotero-browser--pp nil nil nil))
        (ht-each (lambda (key value) (ewoc-enter-last zotero-browser-ewoc value)) items)
        (zotero-browser-indent)))))

(defun zotero-browser-indent ()
  "Indent all nodes according to the level.
Add symbols before nodes with children."
  (interactive)
  (let ((ewoc zotero-browser-ewoc)
        (inhibit-read-only t))
    (save-excursion
      (ewoc-goto-node ewoc (ewoc-nth ewoc 0))
      (while
          (let* ((node (ewoc-locate ewoc))
                 (data (ewoc-data node))
                 (level (zotero-browser--get-level data))
                 (indentation (+ zotero-browser-padding level))
                 (has-children (zotero-browser--has-children-p data))
                 (expanded (zotero-browser--expanded-p ewoc node))
                 (prefix (get-text-property (point) 'line-prefix))
                 (spacing (make-string (1- indentation) ?\s)))
            (when has-children
              (let ((prefix (if expanded (concat spacing "▾") (concat spacing "▸"))))
                (put-text-property (point) (line-end-position) 'line-prefix prefix)))
            (prog1
                ;; End-test of while loop
                (ewoc-next ewoc node)
              (ewoc-goto-next ewoc 1)))))))

(defun zotero-browser--get-linked-buffer ()
  "Return linked buffer (eg browser if items is selected."
  (cond
   ((eq major-mode 'zotero-browser-mode)
    (car (zotero-browser--get-items-buffers)))
   ((eq major-mode 'zotero-browser-items-mode)
    (get-buffer zotero-browser-buffer-name))))

(defun zotero-browser--get-items-buffers ()
  "Return a list of all live items buffers."
  (cl-loop for buffer in (buffer-list)
           when (eq (buffer-local-value 'major-mode (get-buffer buffer)) 'zotero-browser-items-mode)
           collect buffer))

(defun zotero-browser--get-linked-window ()
  "Return linked window (eg browser if items is selected."
  (let ((buffer (zotero-browser--get-linked-buffer)))
    (when buffer
      (get-buffer-window buffer))))

(cl-defun zotero-browser--create-items-buffer (&key type id)
  "Create a new items buffer."
  (let ((buffer (generate-new-buffer zotero-browser-items-buffer-name)))
    (with-current-buffer buffer
      (zotero-browser-items-mode)
      ;; Remove previous entries
      (erase-buffer)
      (setq-local zotero-browser-library-type type)
      (setq-local zotero-browser-library-id id)
      (setq-local zotero-browser-ewoc (ewoc-create #'zotero-browser--item-pp nil nil nil)))
    buffer))

(defun zotero-browser-expand-subcollections (ewoc node)
  "Expand the subcollections of NODE in EWOC."
  (let* ((data (ewoc-data node))
         (items (zotero-browser--get-subcollections data)))
    (ht-each (lambda (key value)
               (ewoc-enter-after ewoc node value))
             items)))

(defun zotero-browser-expand-subitems (ewoc node)
  "Expand the subitems of NODE in EWOC."
  (let* ((data (ewoc-data node))
         (items (zotero-browser--get-subitems data)))
    (ht-each (lambda (key value)
               (ewoc-enter-after ewoc node value))
             items)))

(defun zotero-browser-collapse-subcollections (ewoc node)
  "Collapse the subcollections of NODE in EWOC."
  (let ((inhibit-read-only t)
        parents nodes)
    (while
        (let* ((key (zotero-lib-plist-get* (ewoc-data node) :object :data :key))
               (next-node (ewoc-next ewoc node))
               (parent (zotero-lib-plist-get* (ewoc-data next-node) :object :data :parentCollection)))
          (when (equal parent key)
            (push key parents))
          (when (member parent parents)
            (setq node next-node)
            (push node nodes))))
    (apply #'ewoc-delete ewoc nodes)))

(defun zotero-browser-collapse-subitems (ewoc node)
  "Collapse the subitems of NODE in EWOC."
  (let ((inhibit-read-only t)
        parents nodes)
    (while
        (let* ((key (zotero-browser--node-data node :object :data :key))
               (next-node (ewoc-next ewoc node))
               (parent (zotero-browser--node-data next-node :object :data :parentItem)))
          (when (equal parent key)
            (push key parents))
          (when (member parent parents)
            (setq node next-node)
            (push node nodes))))
    (apply #'ewoc-delete ewoc nodes)))

(defun zotero-browser--expanded-p (ewoc node)
  "Return non-nil if NODE in EWOC is expanded."
  (let* ((key (zotero-browser--node-data node :object :data :key))
         (next-node (ewoc-next ewoc node))
         (parent (or (zotero-browser--node-data next-node :object :data :parentCollection)
                     (zotero-browser--node-data next-node :object :data :parentItem))))
    (equal parent key)))

(defun zotero-browser--get-subcollections (entry)
  "Return the subcollections of ENTRY."
  (let ((key (zotero-lib-plist-get* entry :object :data :key)))
    (zotero-browser--filter zotero-browser-table zotero-browser-subcollections key)))

(defun zotero-browser--get-subitems (entry)
  "Return the subcollections of ENTRY."
  (let ((key (zotero-lib-plist-get* entry :object :data :key)))
    (zotero-browser--filter zotero-browser-table (lambda (elt arg) (equal (plist-get elt :parentItem) arg)) key)))

(defun zotero-browser--has-children-p (entry)
  "Return non-nil if ENTRY has subcollections."
  (let ((parent-key (zotero-lib-plist-get* entry :object :data :key)))
    (if (ht-find
         (lambda (key value)
           (let ((parent-collection (or (zotero-lib-plist-get* value :object :data :parentCollection)
                                        (zotero-lib-plist-get* value :object :data :parentItem))))
             (equal parent-collection parent-key))) zotero-browser-table)
        t nil)))

(defun zotero-browser--has-subitems-p (entry)
  "Return non-nil if ENTRY has subitems."
  (let ((parent-key (zotero-lib-plist-get* entry :data :key)))
    (if (ht-find
         (lambda (key value)
           (let ((parent-collection (zotero-lib-plist-get* value :object :data :parentItem)))
             (equal parent-collection parent-key))) zotero-browser-table)
        t nil)))

(defun zotero-browser--has-attachments-p (entry)
  "Return non-nil if ENTRY has attachments."
  (let ((parent-key (zotero-lib-plist-get* entry :object :data :key)))
    (if (ht-find
         (lambda (key value)
           (let ((parent-collection ))
             (and
              (equal (zotero-lib-plist-get* value :object :data :parentItem) parent-key)
              (equal (zotero-lib-plist-get* value :object :data :itemType) "attachment"))))
         zotero-browser-table)
        t nil)))

(defun zotero-browser--get-level (entry)
  "Return the level of ENTRY."
  (let ((level 1))
    (while
        (let ((parent (or (zotero-lib-plist-get* entry :object :data :parentCollection)
                          (zotero-lib-plist-get* entry :object :data :parentItem))))
          (when (or (not (eq parent :json-false))
                    parent)
            (setq level (1+ level))
            (setq entry (ht-get zotero-browser-table parent)))))
    level))

(defun zotero-browser-revert ()
  "Revert the buffer."
  (interactive)
  (ewoc-refresh zotero-browser-ewoc)
  (zotero-browser-indent))

(defun zotero-browser-show ()
  "Show current entry."
  (interactive)
  (let* ((ewoc zotero-browser-ewoc)
         (node (ewoc-locate ewoc))
         (data (zotero-browser--node-data node :object :data)))
    (zotero-show-item :type zotero-browser-library-type :id zotero-browser-library-id :data data :cache zotero-cache :locale zotero-lib-locale)))

(defun zotero-browser-goto-next ()
  "Move point to the next item."
  (interactive)
  (ewoc-goto-next zotero-browser-ewoc 1)
  (when (eq major-mode 'zotero-browser-mode)
    (zotero-browser-display-items)))

(defun zotero-browser-goto-prev ()
  "Move point to the previous item."
  (interactive)
  (ewoc-goto-prev zotero-browser-ewoc 1)
  (when (eq major-mode 'zotero-browser-mode)
    (zotero-browser-display-items)))

(defun zotero-browser-toggle-subcollections ()
  "Expand or collapse the subcollections of the current item."
  (interactive)
  (let* ((ewoc zotero-browser-ewoc)
         (node (ewoc-locate ewoc))
         (data (ewoc-data node)))
    (when (zotero-browser--has-children-p data)
      (if (zotero-browser--expanded-p ewoc node)
          (zotero-browser-collapse-subcollections ewoc node)
        (zotero-browser-expand-subcollections ewoc node))
      (zotero-browser-indent))))

(defun zotero-browser-toggle-subitems ()
  "Expand or collapse the subitems of the current item."
  (interactive)
  (let* ((ewoc zotero-browser-ewoc)
         (node (ewoc-locate ewoc))
         (data (ewoc-data node)))
    (when (zotero-browser--has-children-p data)
      (if (zotero-browser--expanded-p ewoc node)
          (zotero-browser-collapse-subitems ewoc node)
        (zotero-browser-expand-subitems ewoc node))
      (zotero-browser-indent))))

(defun zotero-browser-cycle-subcollections ()
  "Cycle the visibility of subcollections."
  (interactive)
  ;; (let ((inhibit-read-only t)))
  (cond
   ((and (eq last-command this-command)
	 (eq zotero-browser-status 'collections-top))
    (zotero-browser-expand-all)
    (setq zotero-browser-status 'collections))
   ((and (eq last-command this-command)
	 (eq zotero-browser-status 'collections))
    (zotero-browser-collapse-all)
    (setq zotero-browser-status 'collections-top))
   (t
    (zotero-browser-expand-all)
    (setq zotero-browser-status 'collections))))

(defun zotero-browser-cycle-subitems ()
  "Cycle the visibility of subitems."
  (interactive)
  ;; (let ((inhibit-read-only t)))
  (cond
   ((and (eq last-command this-command)
	 (eq zotero-browser-status 'items-top))
    (zotero-browser-expand-all-subitems)
    (setq zotero-browser-status 'items))
   ((and (eq last-command this-command)
	 (eq zotero-browser-status 'items))
    (zotero-browser-collapse-all-subitems)
    (setq zotero-browser-status 'items-top))
   (t
    (zotero-browser-expand-all-subitems)
    (setq zotero-browser-status 'items))))

(defun zotero-browser-expand-all ()
  "Expand all subcollections."
  (interactive)
  (let ((ewoc zotero-browser-ewoc)
        (inhibit-read-only t))
    (save-excursion
      (ewoc-goto-node ewoc (ewoc-nth ewoc 0))
      (while
          (let ((node (ewoc-locate ewoc)))
            (unless (zotero-browser--expanded-p ewoc node)
              (zotero-browser-expand-subcollections ewoc node))
            (prog1
                ;; End-test of while loop
                (ewoc-next ewoc node)
              (ewoc-goto-next ewoc 1)))))
    (zotero-browser-indent)))

(defun zotero-browser-expand-all-subitems ()
  "Expand all subitems."
  (interactive)
  (let ((ewoc zotero-browser-ewoc)
        (inhibit-read-only t))
    (save-excursion
      (ewoc-goto-node ewoc (ewoc-nth ewoc 0))
      (while
          (let ((node (ewoc-locate ewoc)))
            (unless (zotero-browser--expanded-p ewoc node)
              (zotero-browser-expand-subitems ewoc node))
            (prog1
                ;; End-test of while loop
                (ewoc-next ewoc node)
              (ewoc-goto-next ewoc 1)))))
    (zotero-browser-indent)))

(defun zotero-browser-collapse-all ()
  "Collapse all subcollections."
  (interactive)
  (let ((ewoc zotero-browser-ewoc)
        (inhibit-read-only t))
    (save-excursion
      (ewoc-goto-node ewoc (ewoc-nth ewoc 0))
      (while
          (let ((node (ewoc-locate ewoc)))
            (when (zotero-browser--expanded-p ewoc node)
              (zotero-browser-collapse-subcollections ewoc node))
            (prog1
                (ewoc-next ewoc node)
              (ewoc-goto-next ewoc 1)))))
    (zotero-browser-indent)))

(defun zotero-browser-collapse-all-subitems ()
  "Collapse all subitems."
  (interactive)
  (let ((ewoc zotero-browser-ewoc)
        (inhibit-read-only t))
    (save-excursion
      (ewoc-goto-node ewoc (ewoc-nth ewoc 0))
      (while
          (let ((node (ewoc-locate ewoc)))
            (when (zotero-browser--expanded-p ewoc node)
              (zotero-browser-collapse-subitems ewoc node))
            (prog1
                (ewoc-next ewoc node)
              (ewoc-goto-next ewoc 1)))))
    (zotero-browser-indent)))

(defun zotero-browser--node-data (node &rest props)
  "Return a value from the data of NODE.

This function returns the value corresponding to the given PROPS
in a nested plist. The lookup for each prop should return another
plist, except for the final prop, which may return any value."
  (when node
    (let ((data (ewoc-data node)))
      (apply #'zotero-lib-plist-get* data props))))

;; (if (zotero-auth--token-valid-p token)
;;     (let ((api-key (zotero-auth--api-key token))
;;           (userid (zotero-auth--userid token))
;;           (username (zotero-auth--username token)))))

(defun zotero-browser--ensure-browser-buffer ()
  (unless (eq major-mode 'zotero-browser-mode)
    (error "Current buffer is not an zotero-browser buffer")))

(defun zotero-browser--filter (table pred &rest args)
  "Select elements from TABLE for which PRED returns non-`nil'.
  Return a hash table of all selected data elements. PRED is a
  function that takes a data element as its first argument. If
  more than two arguments are given the remaining arguments will
  be passed to PRED."
  (ht-select (lambda (key value)
               (let ((elt (zotero-lib-plist-get* value :object :data)))
                 ;; keep the predicate nil-safe
                 (ignore-error wrong-type-argument
                   (apply pred elt args))))
             table))

(defun zotero-browser--collection-pp (entry)
  "Pretty print ENTRY."
  (let* ((data (zotero-lib-plist-get* entry :object :data))
         (level (zotero-browser--get-level entry))
         (indentation (+ zotero-browser-padding level))
         (prefix (make-string indentation ?\s))
         (values (seq-map (lambda (key) (plist-get data key)) zotero-browser-collection-keys))
         (text (string-join values " ")))
    (add-text-properties 0 (length text) `(line-prefix ,prefix wrap-prefix ,prefix) text)
    (insert text)))

(defun zotero-browser--item-pp (entry)
  "Pretty print ENTRY."
  (let* ((data (zotero-lib-plist-get* entry :object :data))
         (level (zotero-browser--get-level entry))
         (indentation (+ zotero-browser-padding level))
         (prefix (make-string indentation ?\s))
         (values (seq-map (lambda (key) (plist-get data key)) zotero-browser-item-keys))
         (text (string-join values " ")))
    (add-text-properties 0 (length text) `(line-prefix ,prefix wrap-prefix ,prefix) text)
    (insert text)))

(provide 'zotero-browser)

;;; zotero-browser.el ends here
