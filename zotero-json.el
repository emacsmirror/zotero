;;; zotero-json.el --- Authorization for the Zotero API  -*- lexical-binding: t; -*-

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
;; Functions used for JSON parsing. Because the `json' library parses both
;; \"null\" and \"{}\" as nil, there's no convenient way to differentiate
;; between an empty value or an empty object. However, a \"null\" instead of
;; \"{}\" for an empty object will return a \"400 Bad Request\" error.
;; Therefore, `json-read-object' and `json-encode-keyword' are advised to use the value
;; `:json-empty' when reading or writing a JSON empty object.

;;; Code:

;;;; Requirements

(require 'json)
(require 'seq)

;;;; Functions

(defun zotero-json--read (orig-fun)
  "Advice around `json-read-object'.

This function reads `:json-empty' as JSON empty object (\"{}\").
Both \"null\" and \"{}\" are parsed as nil, so there's no
convenient way to differentiate between an empty value or an
empty object. However, a \"null\" instead of \"{}\" for an empty
object will return a \"400 Bad Request\" error. The advices
around `json-read-object' and `json-encode-keyword' use the value
`:json-empty' when reading or writing a JSON empty object.

Argument ORIG-FUN is the original function `zotero-read-object'."
  (let ((beg (point)))
    ;; Skip over the "{"
    (json-advance)
    (json-skip-whitespace)
    (if (= (json-peek) ?})
        (progn
          ;; Skip over the "}"
          (json-advance)
          :json-empty)
      (goto-char beg)
      (funcall orig-fun))))

(defun zotero-json-encode-keyword (orig-fun arg)
  "Advice around `json-encode-keyword'.

This function writes `:json-empty' as JSON empty object (\"{}\").
Both \"null\" and \"{}\" are parsed as nil, so there's no
convenient way to differentiate between an empty value or an
empty object. However, a \"null\" instead of \"{}\" for an empty
object will return a \"400 Bad Request\" error. The advices
around `json-read-object' and `json-encode-keyword' use the value
`:json-empty' when reading or writing a JSON empty object.

Argument ORIG-FUN is the original function `json-encode-keyword'. ARG is
the keyword passed to `json-encode-keyword'."
  (declare (side-effect-free t))
  (cond ((eq arg t) "true")
        ((eq arg json-false) "false")
        ((eq arg json-null) "null")
        ((eq arg :json-empty) "{}")))

(defun zotero-json--before-read-function ()
  "Function to be run before a JSON read."
  (advice-add #'json-read-object :around #'zotero-json--read))

(defun zotero-json--after-read-function ()
  "Function to be run after a JSON read."
  (advice-remove #'json-read-object #'zotero-json--read))

(defun zotero-json--before-write-function ()
  "Function to be run before a JSON write."
  (advice-add #'json-encode-keyword :around #'zotero-json-encode-keyword))

(defun zotero-json--after-write-function ()
  "Function to be run after a JSON write."
  (advice-remove #'json-encode-keyword #'zotero-json-encode-keyword))

(defun zotero-json-read (object)
  "Convert the JSON OBJECT to Lisp data, else return nil.

A JSON object will be converted to a plist. A JSON array of
objects wil be converted to a vector of plists.

OBJECT may be:
- a buffer (read one Lisp expression from the beginning)
- a function (call it with no arguments)
- a file (read one Lisp expression from the beginning)
- a string (take text from string, starting at the beginning)."
  (zotero-json--before-read-function)
  (let ((json-object-type 'plist))
    (prog1
        (pcase object
          ((pred functionp)
           (json-read-from-string (funcall object)))
          ((pred bufferp)
           (with-current-buffer object
             (save-excursion
               (goto-char (point-min))
               (json-read))))
          ((pred file-readable-p)
           (json-read-file object))
          ((pred stringp)
           (json-read-from-string object))
          (_ (error "Object %S doesn't return a JSON array or object" object)))
      (zotero-json--after-read-function))))

(defun zotero-json-encode (object)
  "Return a JSON representation of OBJECT as a string.

OBJECT may be:
- a cons cell
- a buffer (read one Lisp expression from the beginning)
- a function (call it with no arguments)
- a file (read one Lisp expression from the beginning)
- a string (takes text from string, starting at the beginning)."
  (zotero-json--before-write-function)
  (let ((plist (pcase object
                 ((pred functionp)
                  (funcall object))
                 ((pred consp)
                  object)
                 ((pred bufferp)
                  (with-current-buffer object
                    (save-excursion
                      (goto-char (point-min))
                      (read (current-buffer)))))
                 ((pred file-readable-p)
                  (with-temp-buffer
                    (insert-file-contents object)
                    (goto-char (point-min))
                    (read (current-buffer))))
                 ((pred stringp)
                  (read object)))))
    (prog1
        (if (json-plist-p plist)
            (json-encode plist)
          (error "Object %S doesn't return a property list" object))
      (zotero-json--after-write-function))))

(defun zotero-json-encode-to-array (&rest objects)
  "Return a JSON array with OBJECTS.

Each of the OBJECTS may be:
- a cons cell
- a buffer (read one Lisp expression from the beginning)
- a function (call it with no arguments)
- a file (read one Lisp expression from the beginning)
- a string (takes text from string, starting at the beginning)."
  (concat "[" (mapconcat #'zotero-json-encode objects ",") "]"))

(provide 'zotero-json)

;;; zotero-json.el ends here
