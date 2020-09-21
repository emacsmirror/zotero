;;; zotero-show.el --- Interface to Zotero  -*- lexical-binding: t; -*-

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

(require 'seq)
(require 'widget)

;;;; Variables

(defvar zotero-show-doi-regexp "10\.[[:digit:]]\\{4,9\\}/[0-9A-Za-z()./:;_-]+"
  "A regular expression probably matching a modern Crossref DOI.")

;;;; Keymap

;;;; Menu

;;;; Customization

(defgroup zotero-show nil
  "Interface to Zotero-Show."
  :group 'external)

(defcustom zotero-show-buffer-name "*Zotero Show*"
  "The default buffer name."
  :group 'zotero-show
  :type 'string)

;;;; Mode
;;;###autoload
(define-derived-mode zotero-show-mode text-mode "Zotero show"
  "Major mode for the Zotero Show.

All currently available key bindings:

\\{zotero-show-mode-map}"
  ;; Turn on highlighting
  (font-lock-mode 1)
  ;; Turn on word wrap
  (visual-line-mode 1)
  ;; Turn on buttonizing of URLs and e-mail addresses
  (goto-address-mode 1))

;;;; Commands

(eval-when-compile
  (require 'wid-edit))

;; (defun zotero-show-fontify-doi (&optional start end)
;;   "Fontify the DOIS in the current buffer.
;; This function implements `goto-address-highlight-p'
;; and `goto-address-fontify-p'."
;;   (interactive)
;;   (save-excursion
;;     (goto-char (or start (point-min)))
;;     (when (or (eq t goto-address-fontify-maximum-size)
;; 	      (< (- (or end (point-max)) (point))
;;                  goto-address-fontify-maximum-size))
;;       (while (re-search-forward zotero-show-doi-regexp end t)
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

;; (defun zotero-show-find-doi-at-point ()
;;   "Find DOI around or before point.
;; Then search backwards to beginning of line for the start of DOI.
;; If no DOI found, return nil."
;;   (re-search-backward zotero-show-doi-regexp (line-beginning-position) 'lim)
;;   (if (or (looking-at zotero-show-doi-regexp)
;; 	  (and (re-search-forward zotero-show-doi-regexp
;; 		                  (line-end-position) 'lim)
;; 	       (goto-char (match-beginning 0))))
;;       (match-string-no-properties 0)))

;; (defun zotero-show-goto-doi-at-point (&optional event)
;;   "Load the DOI at point."
;;   (interactive (list last-input-event))
;;   (save-excursion
;;     (if event (posn-set-point (event-end event)))
;;     (if-let ((doi (save-excursion (zotero-show-find-doi-at-point))))
;;         (browse-url (concat "https://doi.org/" doi))
;;       (user-error "No DOI found"))))

(cl-defun zotero-show-create-item (&key cache type id itemtype locale)
  "Create a new item of ITEMTYPE."
  (let ((template (zotero-cache-item-template itemtype)))
    (zotero-show-item :cache cache :type type :id id :data template :locale locale)))

(cl-defun zotero-show-item (&key cache type id data locale)
  "Create a new zotero-show buffer and start zotero-show-mode."
  (switch-to-buffer (generate-new-buffer
                     zotero-show-buffer-name))
  (zotero-show-mode)
  (let* ((schema (zotero-cache-schema))
         (itemtype (plist-get data :itemType))
         (itemtypes (zotero-cache-itemtypes))
         (itemfields (zotero-cache-itemtypefields itemtype))
         (creatortypes (zotero-cache-itemtypecreatortypes itemtype)))
    (setq-local zotero-show-data (copy-tree data))
    (remove-overlays)
    ;; Key
    (when-let ((key :key)
               (fieldname "Key")
               (value (plist-get data key)))
      (widget-insert (concat fieldname ": "))
      (widget-insert (concat value "\n")))
    ;; Version
    (when-let ((key :version)
               (fieldname "Version")
               (value (plist-get data key)))
      (widget-insert (concat fieldname ": "))
      (widget-insert (format "%d\n" value)))
    ;; Itemtype
    (let* ((key :itemType)
           (fieldname "Item Type")
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
                                              (data (plist-put zotero-show-data key new-itemtype))
                                              (data (apply #'zotero-lib-plist-delete data props-to-delete))
                                              (template (zotero-cache-item-template new-itemtype))
                                              (merged (zotero-cache-merge-plist template data)))
                                         (zotero-show-item :type type :id id :data merged :cache cache :locale locale))
                                     (setq zotero-show-data (plist-put zotero-show-data key current-itemtype))
                                     (widget-value-set widget current-itemtype)
                                     (widget-setup)))))
                     :button-prefix "▾"
                     :value value
                     :args choices))
    ;; Creators
    (let* ((key :creators)
           (fieldname "Creators")
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
                                 (setq zotero-show-data (plist-put zotero-show-data key creators-vector))))
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
                        :format " %v "))))
    (dolist (field itemfields)
      (let* ((key (zotero-lib-string->keyword field))
             (fieldname (zotero-cache-itemfield-locale field locale))
             (value (or (plist-get data key) "")))
        (widget-insert (concat fieldname ": "))
        (widget-create 'text
                       :size 10
                       :format "%v\n"
                       :notify (lambda (widget &rest ignore)
			         (setq zotero-show-data (plist-put zotero-show-data key (widget-value widget))))
                       :value value)))
    ;; Collections
    (let* ((key :collections)
           (fieldname "Collections")
           (value (plist-get data key))
           (values (seq-map (lambda (elt) elt) value)))
      (widget-insert (format "%d %s:\n" (length values) fieldname))
      (widget-create 'editable-list
                     :entry-format "%d %v"
                     :notify (lambda (widget &rest ignore)
                               (let* ((value (seq-into (values (widget-value widget)) 'vector)))
                                 (setq zotero-show-data (plist-put zotero-show-data key value))))
                     :value values
                     '(editable-field "")))
    ;; Relations
    (let* ((key :relations)
           (fieldname "Related")
           (value (plist-get data key))
           (values (unless value :json-empty (seq-map (lambda (elt) elt) value))))
      (widget-insert (format "%d %s:\n" (length values) fieldname))
      (widget-create 'editable-list
                     :entry-format "%d %v"
                     :notify (lambda (widget &rest ignore)
                               (let* ((value (if-let ((values (widget-value widget)))
                                                 (seq-into values'vector)
                                               :json-empty)))
                                 (setq zotero-show-data (plist-put zotero-show-data key value))))
                     :value values
                     '(editable-field "")))
    ;; Date Added
    (when-let ((key :dateAdded)
               (fieldname "Date Added")
               (value (plist-get data key))
               (time (iso8601-parse value))
               (timestamp (encode-time time))
               (time-string (format-time-string "%c" timestamp)))
      (widget-insert (concat fieldname ": " time-string "\n")))
    ;; Date Modified
    (when-let ((key :dateModified)
               (fieldname "Date Modified")
               (value (plist-get data key))
               (time (iso8601-parse value))
               (timestamp (encode-time time))
               (time-string (format-time-string "%c" timestamp)))
      (widget-insert (concat fieldname ": " time-string "\n")))
    ;; Save button
    (widget-insert "\n")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (let* ((now (current-time))
                                    (date (format-time-string "%F" now))
                                    (time (format-time-string "%T" now))
                                    (tz (format-time-string "%z" now))
                                    (ISO-8601-date (concat date "T" time tz))
                                    (data (progn
                                            (unless (plist-member zotero-show-data :dateAdded)
                                              (plist-put zotero-show-data :dateAdded ISO-8601-date))
                                            (plist-put zotero-show-data :dateModified ISO-8601-date))))
                               (zotero-cache-save-item :cache cache :type type :id id :data data)
                               (zotero-show-item :type type :id id :data data :cache cache :locale locale)))
                   "Save")
    (widget-insert " ")
    ;; Reset button
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
                             (zotero-show-item :type type :id id :data data :cache cache :locale locale))
		   "Reset")
    (use-local-map widget-keymap)
    (widget-setup)))

(provide 'zotero-show)

;;; zotero-show.el ends here
