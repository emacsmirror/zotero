;;; zotero-lib.el --- Library for the Zotero API  -*- lexical-binding: t; -*-

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
;; Helper functions for Zotero.

;;; Code:

;;;; Helper functions

(defun zotero-lib-keyword->string (keyword)
  "Convert KEYWORD to a string.

Strip the leading \":\" from the keyword."
  (substring (symbol-name keyword) 1))

(defun zotero-lib-string->keyword (string)
  "Convert STRING to a keyword.

Add a leading \":\" to the string."
  (intern (concat ":" string)))

(defun zotero-lib-plist-get* (plist &rest props)
  "Recursively extract a value from a property list.

This function returns the value corresponding to the given PROPS
in a nested PLIST. The lookup for each prop should return another
plist, except for the final prop, which may return any value."
  (while props
    (setf plist (plist-get plist (pop props))))
  plist)

(defun zotero-lib-plist-delete (plist &rest props)
  "Delete PROPS from PLIST."
  (if props
      (let (result)
        (while plist
          (unless (eq (car props) (car plist))
	    (setq result (plist-put result (car plist) (cadr plist))))
          (setq plist (cddr plist)))
        (apply #'zotero-lib-plist-delete result (cdr props)))
    plist))

(provide 'zotero-lib)

;;; zotero-lib.el ends here
