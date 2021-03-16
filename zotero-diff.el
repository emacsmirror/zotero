;;; zotero-diff.el --- Interface to Zotero  -*- lexical-binding: t; -*-

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
(require 'iso8601)
(require 's)
(require 'seq)
(require 'widget)
(require 'zotero-lib)

(declare-function zotero-cache-item-template "zotero-cache")
(declare-function zotero-cache-attachment-template "zotero-cache")
(declare-function zotero-cache-itemfield-locale "zotero-cache")

;;;; Mode

;;;###autoload
(define-derived-mode zotero-diff-mode special-mode "Zotero edit"
  "Major mode for Zotero Edit buffers.

All currently available key bindings:

\\{zotero-diff-mode-map}"
  ;; Turn on highlighting
  (font-lock-mode 1)
  ;; Turn on word wrap
  (visual-line-mode 1))

;;;; Functions

(defun zotero-diff (local-data remote-data)
  "Show LOCAL-DATA and REMOTE-DATA side-by-side.
Ask which copy should be kept to resolve a sync conflict."
  (let ((local-buffer (zotero-diff-item :data local-data :buffer-name "Local" :locale "en-US"))
        (remote-buffer (zotero-diff-item :data remote-data :buffer-name "Remote" :locale "en-US")))
    (pop-to-buffer local-buffer '((display-buffer-pop-up-frame)))
    (display-buffer remote-buffer '((display-buffer-in-direction) . ((direction . right))))
    (let ((choice (read-multiple-choice
                   "Conflict between local and remote object cannot be automatically resolved. How should this be resolved? "
                   '((?l "keep the local copy")
                     (?r "keep the remote copy")
                     (?q "quit")))))
      (prog1
          choice
        (delete-frame)))))

(cl-defun zotero-diff-item (&key data buffer-name locale)
  "Create a new item buffer with DATA.
BUFFER-NAME is the name of the buffer. LOCALE is the locale used
in translations."
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (zotero-diff-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (save-excursion
          (let* ((itemtype (plist-get data :itemType))
                 (linkmode (plist-get data :linkMode))
                 (template (pcase itemtype
                             ("attachment" (zotero-cache-attachment-template linkmode))
                             (_ (zotero-cache-item-template itemtype)))))
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
                       (:itemType
                        (let* ((fieldname "Item Type")
                               (value (plist-get data key)))
                          (widget-insert (concat fieldname ": "))
                          (widget-insert (concat value "\n"))))
                       ;; Title
                       (:title
                        (let ((fieldname (zotero-cache-itemfield-locale key locale))
                              (value (or (plist-get data key) "")))
                          (widget-insert (concat fieldname ": "))
                          (widget-insert (concat value "\n"))))
                       ;; Creators
                       (:creators
                        (let* ((fieldname "Creators")
                               (value (plist-get data key))
                               (creators (seq-map (lambda (elt)
                                                    (list (plist-get elt :creatorType)
                                                          (plist-get elt :firstName)
                                                          (plist-get elt :lastName)))
                                                  value)))
                          (widget-insert (concat fieldname "\n"))
                          (dolist (creator creators)
                            (widget-insert (concat (nth 0 creator) ": " (nth 1 creator) " " (nth 2 creator) "\n")))))
                       ;; Abstract
                       (:abstractNote
                        (let* ((fieldname (zotero-cache-itemfield-locale key locale))
                               (value (or (plist-get data key) "")))
                          (widget-insert (concat fieldname ":\n"))
                          (widget-insert (concat value "\n"))))
                       ;; Note
                       (:note
                        (let* ((fieldname "Note")
                               (value (or (plist-get data key) "")))
                          (widget-insert (concat fieldname ":\n"))
                          (widget-insert (concat value "\n"))))
                       (:md5
                        (when-let ((fieldname "MD5")
                                   (value (plist-get data key)))
                          (unless (or (null value) (eq value :json-false))
                            (widget-insert (concat fieldname ": "))
                            (widget-insert (concat value "\n")))))
                       (:mtime
                        (when-let ((fieldname "Mtime")
                                   (value (plist-get data key)))
                          (unless (or (null value) (eq value :json-false))
                            (widget-insert (concat fieldname ": "))
                            (widget-insert (concat (format "%d\n" value))))))
                       (:tags
                        (let* ((fieldname "Tags")
                               (value (plist-get data key))
                               (values (unless (eq value :json-empty) (seq-into value 'list))))
                          (widget-insert (format "%d %s:\n" (length values) fieldname))
                          (widget-insert (s-join "\n" values))))
                       ;; Collections
                       (:collections
                        (let* ((key :collections)
                               (fieldname "Collections")
                               (value (plist-get data key))
                               (values (unless (eq value :json-empty) (seq-into value 'list))))
                          (widget-insert (format "%d %s:\n" (length values) fieldname))
                          (widget-insert (s-join "\n" values))))
                       ;; Relations
                       (:relations
                        (let* ((fieldname "Related")
                               (value (plist-get data key))
                               (values (unless (eq value :json-empty) (seq-into value 'list))))
                          (widget-insert (format "%d %s:\n" (length values) fieldname))
                          (widget-insert (s-join "\n" values))))
                       ;; Rest
                       (_
                        (let* ((fieldname (or (zotero-cache-itemfield-locale key locale) (capitalize (zotero-lib-keyword->string key))))
                               (value (or (plist-get data key) "")))
                          (widget-insert (concat fieldname ": "))
                          (widget-insert (concat value "\n"))))))
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
            (use-local-map widget-keymap)
            (widget-setup)))))
    buffer))

(provide 'zotero-diff)

;;; zotero-diff.el ends here
