;;; zotero-lib.el --- Library for the Zotero API  -*- lexical-binding: t; -*-

;; Author: Folkert van der Beek <folkertvanderbeek@gmail.com>
;; Created: 2020-03-27
;; Version: 0.1
;; Keywords: zotero, hypermedia
;; Package-Requires: ((emacs "26.1") (ht "2.2") (oauth "1.0.4") (request "0.3.2") (s "1.12.0"))
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

;; This is a GNU Emacs library to interact with the Zotero API (see
;; https://www.zotero.org/support/dev/web_api/v3/start)

;; Note

;; REVIEW: FILES is implemented only for curl backend for now.
;; REVIEW: Emacs 26 is required because of the incompatible struct changes, accessors for the value returned by `file-attributes', the use of `read-multiple-choice'

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'url)
;; (require 'aio)
(require 'ht)
(require 'request)
;; (require 'request-deferred)
(require 's)
(require 'zotero-auth)

;;;; Variables

;; TODO: Specify a User-Agent header that properly identifies emacs-zotero and that provides a means of contacting the developer via email using "mailto:". For example: emacs-zotero/0.1 (https://gitlab.com/fvdbeek/emacs-zotero/; mailto:folkertvanderbeek@gmail.com).

(defconst zotero-lib-base-url "https://api.zotero.org")

(defconst zotero-lib-api-version 3
  "API version. Version 3 is currently the default and recommended version.")

;; (defvar zotero-lib-status-codes '(;; The request completed. See the response JSON for status of individual writes.
;;                                   (200 . (lambda (&rest _) (message "OK")))
;;  	                          ;; The item was successfully updated.
;;                                   ;; The item was deleted.
;;                                   (204 . (lambda (&rest _) (message "No Content")))
;;                                   ;; Invalid type/field; unparseable JSON
;;                                   (400 . (lambda (&rest _) (message "Bad Request")))
;;                                   ;; The target library is locked.
;;                                   (409 . (lambda (&rest _) (message "Conflict")))
;;                                   ;; The provided Zotero-Write-Token has already been submitted.
;;                                   ;; The item has changed since retrieval (i.e., the provided item version no longer matches).
;;                                   (412 . (lambda (&rest _) (message "Precondition Failed")))
;;                                   ;; Too many items submitted
;;                                   (413 . (lambda (&rest _) (message "Request Entity Too Large")))
;;                                   ;; If-Unmodified-Since-Version was not provided.
;;                                   (428 . (lambda (&rest _) (message "Precondition Required")))))

(defvar zotero-lib-headers
  `(:api-key "Zotero-API-Key"
             :api-version "Zotero-API-Version"
             :last-modified-version "Last-Modified"
             :if-modified-since-version "If-Modified-Since"
             :if-unmodified-since-version "If-Unmodified-Since-Version"
             :if-match "If-Match"
             :if-none-match "If-None-Match"
             :content-type "Content-Type"
             :expect "Expect"
             :write-token "Zotero-Write-Token"))

(defvar zotero-lib-params
  `(:format "format"
            :since "since"
            :include "include"
            :content "content"
            :style "style"
            :linkwrap "linkwrap"
            :locale "locale"
            :sort "sort"
            :direction "direction"
            :limit "limit"
            :itemtype "itemType"
            :linkmode "linkMode"
            :itemkey "itemKey"
            :collectionkey "collectionKey"
            :searchkey "searchKey"
            :tag "tag"))

;; (defvar zotero-lib-status-line-regexp "\\([^ ]+\\) \\([[:digit:]]\\{3\\}\\) \\(.*\\)")
;; (defvar zotero-lib-fieldname-regexp (regexp-opt-charset (nconc (number-sequence 33 57) (number-sequence 59 126))))
;; (defvar zotero-lib-fieldbody-regexp (regexp-opt-charset (nconc '(9) (number-sequence 32 126))))
;; (defvar zotero-lib-header-regexp (concat "^\\(" zotero-lib-fieldname-regexp "+\\)"
;;                                          ": \\(" zotero-lib-fieldbody-regexp "*\\)$"))

(defvar zotero-lib-link-regexp "<\\([^>]*\\)>; rel=\"\\([[:word:]]+\\)\"")

;;;; Customization

(defgroup zotero-lib nil
  "Library for the Zotero API"
  :group 'external)

(defconst zotero-lib-directory
  (or (and load-file-name
           (file-name-directory load-file-name))
      default-directory)
  "The directory from where this library was first loaded.")

(defcustom zotero-lib-timeout 5
  "Default timeout in seconds.
`nil' means no timeout."
  :group 'zotero-lib
  :type '(choice (integer :tag "Timeout seconds")
                 (boolean :tag "No timeout" nil)))

(defcustom zotero-lib-sort 'dateModified
  "The name of the field by which entries are sorted."
  :group 'zotero-lib
  :type '(choice (const dateAdded)
                 (const dateModified)
                 (const title)
                 (const creator)
                 (const publisher)
                 (const title)
                 (const publicationTitle)
                 (const journalAbbreviation)
                 (const language)
                 (const accessDate)
                 (const libraryCatalog)
                 (const callNumber)
                 (const rights)
                 (const addedBy)
                 (const :tag "numItems (tags)" numItems)))

;; This variable is not used, because the default sorting direction varies by sort method.
;; (defcustom zotero-lib-direction 'asc
;;   "The sorting direction of the field specified in the sort parameter."
;;   :group 'zotero-lib
;;   :type '(choice (const asc)
;;                  (const desc)))

(defcustom zotero-lib-limit 100
  "The maximum number of results to return with a single request.
The number should be an integer between 1 and 100."
  :group 'zotero-lib
  :type '(integer :validate
		  (lambda (widget)
                    (let ((int (widget-value widget)))
		      (unless (and (< int 1)
                                   (> int 100))
		        (widget-put widget :error
				    "Invalid value: must be an integer between 1 and 100.")
		        widget)))))

;; (defcustom zotero-lib-export-format 'bibtex
;;   "Export Formats.
;; The following bibliographic data formats can be used as format, include, and content parameters."
;;   :group 'zotero-lib
;;   :type '(choice ((const :tag "BibTeX" bibtex)
;;                   (const :tag "BibLaTeX" biblatex)
;;                   (const :tag "Netscape Bookmark File Format" bookmarks)
;;                   (const :tag "COinS" coins)
;;                   (const :tag "Citation Style Language data format" csljson)
;;                   (const :tag "MODS" mods)
;;                   (const :tag "Refer/BibIX" refer)
;;                   (const :tag "Bibliographic Ontology RDF" rdf)
;;                   (const :tag "Unqualified Dublin Core RDF" rdf)
;;                   (const :tag "Zotero RDF" rdf)
;;                   (const :tag "RIS" ris)
;;                   (const :tag "Text Encoding Initiative (TEI)" tei)
;;                   (const :tag "Wikipedia Citation Templates" wikipedia))))

;; (defcustom zotero-lib-style "chicago-note-bibliography"
;;   "Citation style to use for formatted references. Can be either
;; the file name (without the .csl extension) of one of the styles
;; in the Zotero Style Repository (e.g., apa) or the URL of a remote
;; CSL file."
;;   :group 'zotero-lib
;;   :type 'string
;;   :link '(url-link "https://www.zotero.org/styles/"))

(defcustom zotero-lib-linkwrap nil
  "Non-nil means to return URLs and DOIs as links."
  :group 'zotero-lib
  :type 'boolean)

(defcustom zotero-lib-locale "en-US"
  "Bibliography locale.
See the available CSL locales. Note that some styles use a fixed
locale and cannot be localized."
  :group 'zotero-lib
  :type 'string
  :link '(url-link "https://github.com/citation-style-language/locales"))

;;;; Helper functions

(defun zotero-lib--keyword->string (keyword)
  "Convert a keyword to a string.
Strip the leading \":\" from the keyword."
  (substring (symbol-name keyword) 1))

(defun zotero-lib--string->keyword (string)
  "Convert a string to a keyword.
Add a leading \":\" to the string."
  (intern (concat ":" string)))

;;;; Parser functions

(defun zotero-lib--get-statusline (buffer)
  "Return the status-line from a response BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (buffer-substring (point-min) (line-end-position))))

(defun zotero-lib--get-header (buffer)
  "Return the headers from a response BUFFER."
  (with-current-buffer buffer
    ;; Move beyond status line
    (goto-char (point-min))
    (forward-line 1)
    (let ((pos (point)))
      ;; Move to blank line at end of headers.
      (while (progn
               (forward-line 1)
               (not (looking-at "^\r?\n"))))
      (buffer-substring pos (point)))))

(defun zotero-lib--get-body (buffer)
  "Return the body from a response BUFFER."
  (with-current-buffer buffer
    ;; Move beyond blank line at end of headers.
    (goto-char (point-min))
    (while (progn
             (forward-line 1)
             (not (looking-at "^\r?\n"))))
    (forward-line 1)
    (buffer-substring (point) (point-max))))

;; (defun zotero-lib-parse-statusline (string)
;;   "Return a plist with the parsed status line from STRING.
;; The status line is the first line of a response message, consisting of the protocol version followed by a 3-digit status code and its associated reason phrase, with each element separated by space characters."
;;   (string-match zotero-lib-status-line-regexp string)
;;   `(:http-version ,(match-string 1 string) :status-code ,(match-string 2 string) :reason-phrase ,(match-string 3 string)))

(defun zotero-lib--parse-headers (string)
  "Return an alist with the parsed response headers from STRING.
Header fields are lines beginning with a field name, followed by
a colon, followed by a field body. A field name is composed of
printable US-ASCII characters (i.e., characters that have values
between 33 and 126), except colon. A field body may be
composed of printable US-ASCII characters as well as the
space (ASCII value 32) and horizontal tab (ASCII value
9) characters."
  (let ((pos 0)        ; string marker
        (matches ()))  ; return list
    (while (string-match zotero-lib-header-regexp string pos)
      (push `(,(match-string 1 string) . ,(match-string 2 string)) matches)
      (setq pos (match-end 0)))
    (nreverse matches)))

(defun zotero-lib--parse-links (string)
  "Return links header as a plist.
STRING is the HTTP \"Link\" header. Return a plist containing
the `:alternate', `:last', `:next', `:prev', and `:first' links."
  (when string
    (let ((pos 0)
          (matches ()))
      (while (string-match zotero-lib-link-regexp string pos)
        (push (intern (concat ":" (match-string 2 string))) matches)
        (push (match-string 1 string) matches)
        (setq pos (match-end 0)))
      (nreverse matches))))

;;;; JSON parsing

(defun json-read-object--empty-object (orig-fun)
  "Advice around `json-read' to read `:json-empty' as JSON empty object (\"{}\").

Both \"null\" and \"{}\" are parsed as `nil', so there's no
convenient way to differentiate between an empty value or an
empty object. However, a \"null\" instead of \"{}\" for an empty
object will return a \"400 Bad Request\" error. The advices
around `json-read' and `json-encode' use the value `:json-empty'
when reading or writing a JSON empty object."
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

(defun json-encode--empty-object (orig-fun arg)
  "Advice around `json-encode' to write `:json-empty' as JSON empty object (\"{}\").

Both \"null\" and \"{}\" are parsed as `nil', so there's no
convenient way to differentiate between an empty value or an
empty object. However, a \"null\" instead of \"{}\" for an empty
object will return a \"400 Bad Request\" error. The advices
around `json-read' and `json-encode' use the value `:json-empty'
when reading or writing a JSON empty object."
  (if (eq arg :json-empty)
      "{}"
    (funcall orig-fun arg)))

(defun zotero-lib--before-read-function ()
  "Function to be run before a JSON read."
  (advice-add #'json-read-object :around #'json-read-object--empty-object))

(defun zotero-lib--after-read-function ()
  "Function to be run after a JSON read."
  (advice-remove #'json-read-object #'json-read-object--empty-object))

(defun zotero-lib--before-write-function ()
  "Function to be run before a JSON write."
  (advice-add #'json-encode :around #'json-encode--empty-object))

(defun zotero-lib--after-write-function ()
  "Function to be run after a JSON write."
  (advice-remove #'json-encode #'json-encode--empty-object))

(defun zotero-lib--read-json (object)
  "Convert the JSON OBJECT to Lisp data, else return nil.
  A JSON Object will be converted to a plist. A JSON array of
  objects wil be converted to a vector of plists.

  OBJECT may be: a buffer (read one Lisp expression from the
                                beginning) a function (call it with no arguments) a file (read
                                one Lisp expression from the beginning) a string (takes text
                                from string, starting at the beginning)."
  (zotero-lib--before-read-function)
  (let* ((json-object-type 'plist)
         (data (cond ((bufferp object)
                      (with-current-buffer object
                        (save-excursion
                          (goto-char (point-min))
                          (json-read))))
                     ((functionp object) (json-read-from-string (funcall object)))
                     ((stringp object)
                      (if (file-readable-p object)
                          (json-read-file object)
                        (json-read-from-string object))))))
    (zotero-lib--after-read-function)
    (when (or (json-plist-p data) (vectorp data)) data)))

(defun zotero-lib--read (object)
  "Read Lisp data from OBJECT, else return nil.
  The OBJECT should return a plist or a vector of plists.

  OBJECT may be:
  a plist
  a buffer (read one Lisp expression from the beginning)
  a function (call it with no arguments)
  a file (read one Lisp expression from the beginning)
  a string (takes text from string, starting at the beginning)."
  (zotero-lib--before-read-function)
  (let ((data (cond ((consp object) object)
                    ((bufferp object)
                     (with-current-buffer object
                       (save-excursion
                         (goto-char (point-min))
                         (read (current-buffer)))))
                    ((functionp object) (read object))
                    ((stringp object)
                     (if (file-readable-p object)
                         (with-temp-buffer
                           (insert-file-contents object)
                           (goto-char (point-min))
                           (read (current-buffer))))
                     (read object)))))
    (prog1
        data
      (zotero-lib--after-read-function))))

(defun zotero-lib--encode-object (&rest objects)
  "Return a JSON array with OBJECTS.
  OBJECTS should be a list of objects, each of which may be:
  a cons cell
  a buffer (read one Lisp expression from the beginning)
  a function (call it with no arguments)
  a file (read one Lisp expression from the beginning)
  a string (takes text from string, starting at the beginning)."
  (zotero-lib--before-write-function)
  (let (result)
    (dolist (object objects result)
      (let ((plist (zotero-lib--read object)))
        (if plist
            (push plist result)
          (user-error "Object %S doesn't return a property list" object))))
    (let ((json (json-encode-array (vconcat (nreverse result)))))
      (zotero-lib--after-write-function)
      json)))

;;;; Resources

;; TODO: is this function necessary?
(defun zotero-lib--error-p (response)
  "Return t if the request gave an error, else return nil."
  (when (eq (request-response-symbol-status response) 'error) t))

;; TODO: is this function necessary?
(defun zotero-lib--succes-p (response)
  "Return t if the request was succesfull, else return nil."
  (when (eq (request-response-symbol-status response) 'succes) t))

;; TODO: is this function necessary?
(defun zotero-lib--not-modified-p (response)
  "Return t if the data was not modified, else return nil.
If the \"If-Modified-Since-Version\" header is passed with a multi-object read request and data has not changed in the library since the specified version, the API will return 304 Not Modified. Single-object conditional requests are not currently supported, but will be supported in the future."
  (let ((status-code (request-response-status-code response)))
    (if (eq status-code 304) t nil)))

;; TODO: is this function necessary?
(defun zotero-lib--modified-p (response)
  "Return t if the data has changed since retrieval (i.e., the provided item version no longer matches), else return nil.

The \"If-Unmodified-Since-Version\" request header is used to
ensure that existing data won't be overwritten by a client with
out-of-date data. All write requests that modify existing objects
must include either the \"If-Unmodified-Since-Version:
<version>\" header or a `:version' property for each object. If
both are omitted, the API will return a 428 Precondition
Required.

For write requests to multi-object endpoints, the API will return
412 Precondition Failed if the library has been modified since
the passed version. For write requests to single-object
endpoints, the API will return a 412 if the object has been
modified since the passed version."
  (let ((status-code (request-response-status-code response)))
    (if (eq status-code 412) t nil)))

(defun zotero-lib--rate-limit (response)
  "Return the number of seconds to wait if the rate is limited, else return nil.

If the API servers are overloaded, the API may include a
\"Backoff: <seconds>\" HTTP header in responses, indicating that
the client should perform the minimum number of requests
necessary to maintain data consistency and then refrain from
making further requests for the number of seconds indicated.
Backoff can be included in any response, including successful
ones.

If a client has made too many requests within a given time
period, the API may return 429 Too Many Requests with a
\"Retry-After: <seconds>\" header. Clients receiving a 429 should
wait the number of seconds indicated in the header before
retrying the request."
  (when-let ((seconds (or (request-response-header response "Backoff")
                          (request-response-header response "Retry-After"))))
    (string-to-number seconds)))

(defun zotero-lib--add-to-headers (handle &rest keys)
  "Return an alist with the headers to send with the request."
  (let ((headers))
    (dolist (key keys headers)
      (when-let ((fieldname (plist-get zotero-lib-headers key))
                 (fieldbody (plist-get handle key)))
        (setq headers (cons `(,fieldname . ,fieldbody) headers))))))

(defun zotero-lib--add-to-params (handle &rest keys)
  "Return an alist with the params to send with the request."
  (let ((params))
    (dolist (key keys params)
      (when-let ((param (plist-get zotero-lib-params key))
                 ;; (param (substring (symbol-name key) 1))
                 (value (plist-get handle key)))
        (setq params (cons `(,param . ,value) params))))))

(defun zotero-lib--create-params (&rest params)
  (let ((result))
    (dolist (param params result)
      (when (consp param)
        (setq result (push param result))))))

;; Due to limitation of url-retrieve-synchronously, response slot
;; request-response-error-thrown is unknown (always nil) when using
;; synchronous request with url-retrieve backend.
;; (cl-defun zotero-lib--error-function (&key error-thrown &allow-other-keys)
;;   (user-error "%s: %s" (car error-thrown) (cdr error-thrown)))

;; (cl-defun zotero-lib--complete-function (&key symbol-status &allow-other-keys)
;;   (cond
;;    ((eq symbol-status 'success)
;;     (message "Searching...succes"))
;;    ((eq symbol-status 'error)
;;     (message "Searching...failed"))
;;    ((eq symbol-status 'timeout)
;;     (message "Searching...timeout"))
;;    ((eq symbol-status 'abort)
;;     (message "Searching...cancelled"))
;;    ((eq symbol-status 'parse-error)
;;     (message "Searching...failed"))))

(cl-defun zotero-lib--endpoint (&key resource key user group)
  "Return the url from which the Zotero can access RESOURCE.
RESOURCE is one of ... KEY is the item key, collection key, or
search key. Which key is needed varies by resource. LIBRARY is
'user for your personal library, and 'group for the group
libraries. ID is the ID of the personal or group library you want
to access, e.g. the user ID or group ID. Your personal library ID
is available at <https://www.zotero.org/settings/keys/>. For
group libraries, the ID can be found by opening the group's page
at <https://www.zotero.org/groups/>."
  ;; Requests for data in a specific library begin with
  ;; /users/<userID> or /groups/<groupID>. User IDs are
  ;; different from usernames and can be found on the
  ;; API Keys page and in OAuth responses. Group IDs
  ;; are different from group names and can be
  ;; retrieved from /users/<userID>/groups.
  (let* ((prefix (cond (user (concat "/users/" (pcase user
                                                 ((pred numberp) (number-to-string user))
                                                 ((pred symbolp) (symbol-name user))
                                                 ((pred stringp) user))))
                       (group (concat "/groups/" (pcase group
                                                   ((pred numberp) (number-to-string group))
                                                   ((pred symbolp) (symbol-name group))
                                                   ((pred stringp) group))))))
         (suffix (pcase resource
                   ('collections "/collections")
                   ('collections-top "/collections/top")
                   ('collection (concat "/collections/" key))
                   ('subcollections (concat "/collections/" key "/collections"))
                   ('items "/items")
                   ('items-top "/items/top")
                   ('trash-items "/items/trash")
                   ('item (concat "/items/" key))
                   ('item-children (concat "/items/" key "/children"))
                   ('publication-items "/publications/items/")
                   ('collection-items (concat "/collections/" key "/items"))
                   ('collection-items-top (concat "/collections/" key "/items/top"))
                   ('searches "/searches")
                   ('search (concat "/searches/" key))
                   ('tags "/tags")
                   ('tags (concat "/tags/" (url-hexify-string key)))
                   ('item-tags (concat "/items/" key "/items/tags"))
                   ('collection-tags (concat "/collection/" key "/tags"))
                   ('items-tags "/items/tags")
                   ('items-top-tags "/items/top/tags")
                   ('trash-items-tags "/items/trash/tags")
                   ('collection-items-tags (concat "/items/" key "/items/tags"))
                   ('collection-items-top-tags (concat "/items/" key "/items/top/tags"))
                   ('publication-items-tags "/publications/tags")
                   ('keys (concat "/keys/" key))
                   ('groups "/groups")
                   ('all-fulltext "/fulltext")
                   ('item-fulltext (concat "/items/" key "/fulltext"))
                   ('file (concat "/items/" key "/file"))
                   ('deleted (concat "/deleted" ))
                   ;; Default
                   (_ nil))))
    (concat zotero-lib-base-url prefix suffix)))

;; (cl-defun zotero-lib--endpoint (&key resource key library id)
;;   "Return the url from which the Zotero can access RESOURCE.
;; RESOURCE is one of ... KEY is the item key, collection key, or
;; search key. Which key is needed varies by resource. LIBRARY is
;; 'user for your personal library, and 'group for the group
;; libraries. ID is the ID of the personal or group library you want
;; to access, e.g. the user ID or group ID. Your personal library ID
;; is available at <https://www.zotero.org/settings/keys/>. For
;; group libraries, the ID can be found by opening the group's page
;; at <https://www.zotero.org/groups/>."
;;   (let* ((pcase library
;;            ;; Requests for data in a specific library begin with
;;            ;; /users/<userID> or /groups/<groupID>. User IDs are
;;            ;; different from usernames and can be found on the
;;            ;; API Keys page and in OAuth responses. Group IDs
;;            ;; are different from group names and can be
;;            ;; retrieved from /users/<userID>/groups.
;;            ('user (concat "/users/" id))
;;            ('group (concat "/groups/" id))
;;            ;; Default
;;            (library nil))
;;          (pcase resource
;;            ('collections "/collections")
;;            ('collections-top "/collections/top")
;;            ('collection (concat "/collections/" key))
;;            ('subcollections (concat "/collections/" key "/collections"))
;;            ('items "/items")
;;            ('items-top "/items/top")
;;            ('trash-items "/items/trash")
;;            ('item (concat "/items/" key))
;;            ('item-children (concat "/items/" key "/children"))
;;            ('publication-items "/publications/items/")
;;            ('collection-items (concat "/collections/" key "/items"))
;;            ('collection-items-top (concat "/collections/" key "/items/top"))
;;            ('searches "/searches")
;;            ('search (concat "/searches/" key))
;;            ('tags "/tags")
;;            ('tags (concat "/tags/" (url-hexify-string key)))
;;            ('item-tags (concat "/items/" key "/items/tags"))
;;            ('collection-tags (concat "/collection/" key "/tags"))
;;            ('items-tags "/items/tags")
;;            ('items-top-tags "/items/top/tags")
;;            ('trash-items-tags "/items/trash/tags")
;;            ('collection-items-tags (concat "/items/" key "/items/tags"))
;;            ('collection-items-top-tags (concat "/items/" key "/items/top/tags"))
;;            ('publication-items-tags "/publications/tags")
;;            ('keys (concat "/keys/" key))
;;            ('groups "/groups")
;;            ('all-fulltext "/fulltext")
;;            ('item-fulltext (concat "/items/" key "/fulltext"))
;;            ('file (concat "/items/" key "/file"))
;;            ('deleted (concat "/deleted" ))
;;            ;; Default
;;            (resource nil)))
;;     (concat zotero-lib-base-url prefix suffix)))

;; FIXME: is this function necessary?
;; (cl-defun zotero-lib--request (&key url type extra-headers extra-params data files api-key modified-version unmodified-version)
;;   "Return response struct for an API request to Zotero.
;; The response body is automatically parsed with `json-read'."
;;   (request url
;;     :type type
;;     :headers (zotero-lib--header-function :extra-headers extra-headers :api-key api-key :modified-version modified-version :unmodified-version unmodified-version)
;;     :data data
;;     ;; :params (zotero-lib--param-function :extra-params extra-params)
;;     :params extra-params
;;     :files files
;;     ;; The response body is first returned as a string, so it can be
;;     ;; parsed later according to the content type
;;     :parser #'buffer-string
;;     :error #'zotero-lib--error-function
;;     :complete #'zotero-lib--complete-function
;;     :timeout zotero-lib-timeout
;;     :sync t))

;; FIXME: cleaner handling of headers and params
(defun zotero-lib--request (handle)
  "Return response for an API request to Zotero."
  (let* ((response (request (plist-get handle :url)
                     :type (plist-get handle :method)
                     :headers (zotero-lib--add-to-headers handle :api-key :api-version :last-modified-version :if-modified-since-version :if-unmodified-since-version :content-type :expect :write-token :if-match :if-none-match)
                     :data (plist-get handle :data)
                     :params (zotero-lib--add-to-params handle :format :since :include :content :style :linkwrap :locale :sort :direction :limit :itemtype :linkmode :itemkey :collectionkey :searchkey :tag)
                     ;; The response body is first returned as a string, so it can be
                     ;; parsed later according to the content type
                     :parser #'buffer-string
                     :timeout zotero-lib-timeout
                     :sync t))
         (error-thrown (request-response-error-thrown response))
         (symbol-status (request-response-symbol-status response))
         (status-code (request-response-status-code response))
         (content-type (request-response-header response "Content-Type"))
         (etag (when-let ((etag (request-response-header response "Etag"))) (substring etag 1 -1)))
         (last-modified-version (when-let ((version (request-response-header response "Last-Modified-Version"))) (string-to-number version)))
         (links (zotero-lib--parse-links (request-response-header response "Link")))
         (raw-header (request-response--raw-header response))
         (raw-data (request-response-data response))
         (total-results (when-let ((results (request-response-header response "Total-Results"))) (string-to-number results)))
         (data (if (or (equal content-type "application/json") (equal content-type "application/json; charset=utf-8"))
                   (zotero-lib--read-json raw-data)
                 raw-data)))
    `(:symbol-status ,symbol-status
                     :status-code ,status-code
                     :content-type ,content-type
                     :data ,data
                     :version ,last-modified-version
                     :etag ,etag
                     :total-results ,total-results
                     :next-url ,(plist-get links :next)
                     :prev-url ,(plist-get links :prev)
                     :first-url ,(plist-get links :first)
                     :last-url ,(plist-get links :last)
                     :alternate-url ,(plist-get links :alternate)
                     :raw-header ,raw-header
                     :raw-data ,raw-data)))

(defun zotero-lib--authorize (handle)
  "Reauthorize Zotero and return a new request handle."
  (if (y-or-n-p (format "Invalid API key. Authorize Zotero and retry? "))
      (when-let ((token (zotero-auth-authorize))
                 (api-key (zotero-auth--api-key token)))
        (plist-put handle :api-key api-key))
    (user-error "Invalid API key")))

(defun zotero-lib--privileges (handle)
  "Forward to Zotero key settings an return a new request handle."
  (let* ((api-key (plist-get handle :api-key))
         (url (concat "https://www.zotero.org/settings/keys/edit/" api-key)))
    (kill-new url)
    (message "Added URL of Zotero API key settings to the kill-ring")
    (if (y-or-n-p (format "Insufficient privileges. Ask a WWW browser to visit the Zotero settings and retry? " url))
        (progn
          (browse-url url)
          (read-string "Press enter when you have changed the privileges: ")
          handle)
      (user-error "Insufficient privileges"))))

;; (defun zotero-lib--request-deferred (handle)
;;   "Return a deferred response for an API request to Zotero."
;;   (deferred:$
;;     (request-deferred (plist-get handle :url)
;;                       :type (plist-get handle :method)
;;                       :headers (plist-get handle :headers)
;;                       :data (plist-get handle :data)
;;                       :params (plist-get handle :params)
;;                       ;; The response body is first returned as a string, so it can be
;;                       ;; parsed later according to the content type
;;                       :parser #'buffer-string
;;                       :timeout zotero-lib-timeout)
;;     (deferred:nextc it
;;       (lambda (response)
;;         (let* ((status-code (request-response-status-code response))
;;                (error-thrown (request-response-error-thrown response))
;;                (raw-header (request-response--raw-header response))
;;                (raw-data (request-response-data response))
;;                (content-type (request-response-header response "Content-Type"))
;;                (if-modified-since-version (request-response-header response "If-Modified-Since-Version"))
;;                (last-modified-version (request-response-header response "Last-Modified-Version"))
;;                (links (request-response-header response "Link"))
;;                (total-results (request-response-header response "Total-Results"))
;;                (data (if (equal content-type "application/json")
;;                          (let ((json-object-type 'plist))
;;                            (json-read-from-string raw-data))
;;                        raw-data)))
;;           `(:status-code ,status-code :content-type ,content-type :data ,data :if-modified-since-version ,if-modified-since-version :last-modified-version ,last-modified-version :total-results ,total-results :links ,links  :raw-header ,raw-header :raw-data ,raw-data))))))

;; (aio-defun zotero-lib--request-aio (handle)
;;   "Return a promise that resolves to the Zotero request."
;;   (let* ((promise (let ((url-request-method (plist-get handle :method))
;;                         (url-request-extra-headers (plist-get handle :headers)))
;;                     (aio-url-retrieve (plist-get handle :url))))
;;          (result (aio-await promise))
;;          (status (car result))
;;          (raw-data (with-current-buffer (cdr result)
;;                      (buffer-string)))
;;          (raw-statusline (zotero-lib--get-statusline (cdr result)))
;;          (raw-header (zotero-lib--get-header (cdr result)))
;;          (body (zotero-lib--get-body (cdr result)))
;;          (statusline (zotero-lib-parse-statusline raw-statusline))
;;          (status-code (plist-get statusline :status-code))
;;          (reason-phrase (plist-get statusline :reason-phrase))
;;          (header (zotero-lib--parse-headers raw-header))
;;          (if-modified-since-version (cdr (assoc "If-Modified-Since-Version" header)))
;;          (last-modified-version (cdr (assoc "Last-Modified-Version" header)))
;;          (links (cdr (assoc "Link" header)))
;;          (total-results (cdr (assoc "Total-Results" header)))
;;          (content-type (cdr (assoc "Content-Type" header)))
;;          (data (if (equal content-type "application/json")
;;                    (let ((json-object-type 'plist))
;;                      (json-read-from-string body))
;;                  body)))
;;     ;; The buffer created by `aio-url-retrieve' should be killed
;;     (with-current-buffer (cdr result)
;;       (kill-buffer))
;;     `(:status ,status :status-code ,(string-to-number status-code) :total-results ,(string-to-number total-results) :links ,links :last-modified-version ,(string-to-number last-modified-version) :content-type ,content-type :data ,data :raw-header ,raw-header :raw-data ,raw-data)))

;; TODO: timeout error
;; (defun zotero-lib--decider (handle response)
;;   "Handle the RESPONSE and return a plist with the data and a new handle.
;; This function is called by `zotero-lib--dispatch' and provides the
;; logic to decide what should be done based on the RESPONSE."
;;   ;; (pcase (plist-get response :symbol-status)
;;   ;;   ('success)
;;   ;;   ('error)
;;   ;;   ('timeout)
;;   ;;   ('abort)
;;   ;;   ('parse-error))
;;   (pcase (plist-get response :status-code)
;;     ;; OK
;;     (200
;;      (pcase (plist-get response :content-type)
;;        ("application/pdf"
;;         (let ((data (plist-get response :data)))
;;           `(:data ,data)))
;;        ((or "application/json" "application/json; charset=utf-8")
;;         ;; When the total number of results matched by a read
;;         ;; request is greater than the current limit, the API will
;;         ;; include pagination links in the HTTP Link header.
;;         (if-let ((data (plist-get response :data))
;;                  (next-url (plist-get response :next-url)))
;;             ;; Return data and a new handle for the next pagination link
;;             `(:data ,data :handle ,(plist-put handle :url next-url))
;;           ;; Return data, but no new handle
;;           `(:data ,data)))))
;;     ;; No Content
;;     (204
;;      (message "Success."))
;;     ;; Not Modified
;;     (304
;;      (zotero-lib--cache-get))
;;     (400
;;      (pcase (plist-get response :data)
;;        ("Item is not an attachment"
;;         (user-error "Item is not an attachment"))))
;;     ;; Forbidden
;;     (403
;;      (pcase (plist-get response :data)
;;        ("Invalid key"
;;         ;; Authorize and return a new handle, but no data
;;         (let ((handle (zotero-lib--authorize handle)))
;;           `(:handle ,handle)))
;;        ("Forbidden"
;;         ;; Offer to review the privileges and return a new handle, but no data
;;         (let ((handle (zotero-lib--privileges handle)))
;;           `(:handle ,handle)))
;;        (message
;;         (user-error "Unknown authentication error: %s" message))))
;;     ;; Not found
;;     (404
;;      (user-error "Not found"))
;;     ;; Precondition Failed
;;     (412)
;;     ;; Precondition Required
;;     (428
;;      (user-error "If-Match or If-None-Match was not provided."))
;;     ;; Too Many Requests
;;     (429)
;;     ;; Service Unavailable
;;     (503)))

(defun zotero-lib--decider (handle response)
  "Return a new handle and response.
This function is called by `zotero-lib--dispatch' and provides
the logic what should be done based on the HANDLE and RESPONSE."
  (pcase (plist-get response :status-code)
    ;; OK
    (200
     (pcase (plist-get response :content-type)
       ("application/pdf"
        `(:response ,response))
       ((rx "application/json" (zero-or-more anything))
        ;; When the total number of results matched by a read
        ;; request is greater than the current limit, the API will
        ;; include pagination links in the HTTP Link header.
        (if-let ((next-url (plist-get response :next-url)))
            ;; Return response and a new handle for the next pagination link
            `(:response ,response :handle ,(plist-put handle :url next-url))
          ;; Return response, but no new handle
          `(:response ,response)))))
    ;; No Content
    (204
     (message "204 Success."))
    ;; TODO
    (304
     (message "304 Not Modified"))
    (400
     (pcase (plist-get response :data)
       ("Item is not an attachment"
        (user-error "Item is not an attachment"))))
    ;; Forbidden
    (403
     (pcase (plist-get response :data)
       ("Invalid key"
        ;; Authorize and return a new handle, but no data
        (let ((handle (zotero-lib--authorize handle)))
          `(:handle ,handle)))
       ("Forbidden"
        ;; Offer to review the privileges and return a new handle, but no data
        (let ((handle (zotero-lib--privileges handle)))
          `(:handle ,handle)))
       (message
        (user-error "Unknown authentication error: %s" message))))
    (404
     (user-error "404 Not found"))
    (412
     (user-error "412 Precondition Failed. The library has been modified since the passed version."))
    (428
     (user-error "428 Precondition Required. The \"If-Match\" or \"If-None-Match\" header was not provided."))
    ;; TODO
    (429
     (user-error "Too Many Requests"))
    ;; TODO
    (503
     (user-error "Service Unavailable"))))

(defun zotero-lib--recurse (handle &optional total)
  "Return a new handle and response.
This function is called by `zotero-lib--dispatch' and provides
the logic what should be done based on the HANDLE and RESPONSE."
  (let ((response (zotero-lib--request handle)))
    (pcase (plist-get response :status-code)
      ;; OK
      (200
       (pcase (plist-get response :content-type)
         ("application/pdf"
          response)
         ((rx "application/json" (zero-or-more anything))
          ;; When the total number of results matched by a read
          ;; request is greater than the current limit, the API will
          ;; include pagination links in the HTTP Link header.
          (let ((data (thread-first
                          (plist-get result :response)
                        (plist-get :data)))
                (total (seq-concatenate 'vector total data)))
            (if-let ((next-url (plist-get response :next-url))
                     (handle (plist-put handle :url next-url)))
                (zotero-lib--recurse handle total)
              (plist-put response :data total))))))
      ;; No Content
      (204
       (message "204 Success.")
       response)
      (304
       (message "304 Not Modified")
       response)
      (400
       (pcase (plist-get response :data)
         ("Item is not an attachment"
          (user-error "Item is not an attachment"))))
      ;; Forbidden
      (403
       (pcase (plist-get response :data)
         ("Invalid key"
          ;; Authorize and return a new handle, but no data
          (let ((handle (zotero-lib--authorize handle)))
            (zotero-lib--recurse handle)))
         ("Forbidden"
          ;; Offer to review the privileges and return a new handle, but no data
          (let ((handle (zotero-lib--privileges handle)))
            (zotero-lib--recurse handle)))
         (message
          (user-error "Unknown authentication error: %s" message))))
      (404
       (user-error "404 Not found"))
      (412
       (user-error "412 Precondition Failed. The library has been modified since the passed version."))
      (428
       (user-error "428 Precondition Required. The \"If-Match\" or \"If-None-Match\" header was not provided."))
      ;; TODO
      (429
       (user-error "Too Many Requests"))
      ;; TODO
      (503
       (user-error "Service Unavailable")))))

(defun zotero-lib--dispatch (handle)
  "Dispatch HANDLE and return the last-modified-version and data.
The result is a cons of (version . data)."
  ;; The main flow of execution is a loop between the \"worker\" (`zotero-lib--request'), that sends a request to the Zotero API, and the \"decider\" (`zotero-lib--decider'), that decides what to do next:
  ;; - Dispatch the handle to the worker and bind the response
  ;; - Dispatch the handle and response to the decider and bind the response and a new handle, if applicable
  ;; - Repeat while new handles are returned.
  (let (version
        all-data)
    (while
        (let* ((response (zotero-lib--request handle))
               (result (zotero-lib--decider handle response)))
          (setq version (thread-first
                            (plist-get result :response)
                          (plist-get :version)))
          (pcase (thread-first
                     (plist-get result :response)
                   (plist-get :data))
            ((and (pred vectorp) data)
             (setq all-data (seq-concatenate 'vector all-data data)))
            ((and (pred consp) data)
             (setq all-data (seq-concatenate 'list all-data data)))
            ((and (pred stringp) data)
             (setq all-data data)))
          (setq handle (plist-get result :handle))))
    (cons version all-data)))

(cl-defun zotero-lib--retrieve (&key url resource key user group api-key version last-modified-version locale itemtype linkmode format since itemkey collectionkey searchkey)
  "Return the last-modified-version and the data returned by the Zotero request.
The result is a cons of (version . data).

KEY is the item key, collection key, or search key. Which key is
needed varies by resource. LIBRARY is 'user (default) for your
personal library, and 'group for the group libraries. ID is the
ID of the personal or group library you want to access, e.g. the
user ID (default) or group ID. Your personal library ID is
available at <https://www.zotero.org/settings/keys/>. For group
libraries, the ID can be found by opening the group's page at
<https://www.zotero.org/groups/>."
  ;; Request the specified URL or construct the endpoint of the resource
  (let* ((url (or url
                  (zotero-lib--endpoint :resource resource :key key :user user :group group)))
         (handle `(:url ,url :method "GET" :api-version ,zotero-lib-api-version :api-key ,api-key :if-modified-since-version ,version :last-modified-version ,last-modified-version :locale ,locale :itemtype ,itemtype :linkmode ,linkmode :format ,format :since ,since :itemkey ,itemkey :collectionkey ,collectionkey :searchkey ,searchkey)))
    (zotero-lib--dispatch handle)))

(cl-defun zotero-lib--submit (&key method url resource key user group data api-key version content-type expect if-match if-none-match write-token)
  "Return a plist with the response of the Zotero request.

KEY is the item key, collection key, or search key. Which key is
needed varies by resource. LIBRARY is 'user (default) for your
personal library, and 'group for the group libraries. ID is the
ID of the personal or group library you want to access, e.g. the
user ID (default) or group ID. Your personal library ID is
available at <https://www.zotero.org/settings/keys/>. For group
libraries, the ID can be found by opening the group's page at
<https://www.zotero.org/groups/>."
  ;; Request the specified URL or construct the endpoint of the resource
  (let* ((url (or url
                  (zotero-lib--endpoint :resource resource :key key :user user :group group)))
         (handle `(:url ,url :method ,method :data ,data :api-version ,zotero-lib-api-version :api-key ,api-key :if-modified-since-version ,version :content-type ,(or content-type "application/json") :expect "" :if-match ,if-match :if-none-match ,if-none-match :write-token ,write-token)))
    (zotero-lib--dispatch handle)))

(defun zotero-lib--write-token ()
  "Return a unique 32-char write-token.

Zotero-Write-Token is an optional HTTP header, containing a
client-generated random 32-character identifier string, that can
be included with unversioned write requests to prevent them from
being processed more than once. The Zotero server caches write
tokens for successful requests for 12 hours, and subsequent
requests from the same API key using the same write token will be
rejected with a \"412 Precondition Failed\" status code. If a
request fails, the write token will not be stored.

If using versioned write requests (i.e., those that include an
If-Unmodified-Since-Version HTTP header or individual object
version properties), Zotero-Write-Token is redundant and should
be omitted."
  (let ((characters "0123456789abcdefghijklmnopqrstuvwxyz")
        (token ""))
    (dotimes (i 32 token)
      (let ((char (elt characters (random 36))))
        (setq token (concat token (string char)))))))

(defun zotero-lib--file-attributes (file)
  "Get the file attributes."
  (when (file-readable-p file)
    (let* ((md5 (with-temp-buffer
                  (insert-file-contents file)
                  (secure-hash 'md5 (current-buffer))))
           (attributes (file-attributes file))
           ;; (approximate) time of last modification in milliseconds
           (mtime (thread-last
                      ;; mtime as a Lisp timestamp
                      (file-attribute-modification-time attributes)
                    ;; convert to seconds since the epoch
                    (format-time-string "%s")
                    ;; convert to number
                    (string-to-number)
                    ;; and multiply by 1000
                    (* 1000)))
           (accessdate (thread-last
                           (file-attribute-access-time attributes)
                         ;; convert to ISO 8601 date format
                         (format-time-string "%F")))
           ;; filename without its directory
           (filename (file-name-nondirectory file))
           ;; filesize in bytes
           (filesize (file-attribute-size attributes))
           (contenttype (or (mailcap-file-name-to-mime-type filename) "application/octet-stream")))
      `(:filename ,filename :filesize ,filesize :contenttype ,contenttype :md5 ,md5 :mtime ,mtime :accessdate ,accessdate))))

;;;; Methods

(cl-defun zotero-lib-get-collections (&key user group api-key)
  "Collections in the library.
  Optional argument LIBRARY is 'user for your personal library, and
  'group for the group libraries. Optional argument ID is the ID of
  the personal or group library you want to access, e.g. the \"user
  ID\" or \"group ID\"."
  (zotero-lib--retrieve :resource 'collections :user user :group group :api-key api-key))

(cl-defun zotero-lib-get-collections-top (&key user group api-key)
  "Top-level collections in the library.
  Optional argument LIBRARY is 'user for your personal library, and
  'group for the group libraries. Optional argument ID is the ID of
  the personal or group library you want to access, e.g. the \"user
  ID\" or \"group ID\"."
  (zotero-lib--retrieve :resource 'collections-top :user user :group group :api-key api-key))

(cl-defun zotero-lib-get-collection (&key user group key api-key)
  "A specific collection in the library.
  KEY is the collection key. Optional argument LIBRARY is 'user for
  your personal library, and 'group for the group libraries.
  Optional argument ID is the ID of the personal or group library
  you want to access, e.g. the \"user ID\" or \"group ID\"."
  (zotero-lib--retrieve :resource 'collection :user user :group group :key key :api-key api-key))

(cl-defun zotero-lib-get-subcollections (&key user group key api-key)
  "Subcollections within a specific collection in the library.
  KEY is the collection key. Optional argument LIBRARY is 'user for
  your personal library, and 'group for the group libraries.
  Optional argument ID is the ID of the personal or group library
  you want to access, e.g. the \"user ID\" or \"group ID\"."
  (zotero-lib--retrieve :resource 'subcollections :user user :group group :key key :api-key api-key))

;; FIXME: too slow
(cl-defun zotero-lib-get-items (&key user group api-key)
  "All items in the library, excluding trashed items.
  Optional argument LIBRARY is 'user for your personal library, and
  'group for the group libraries. Optional argument ID is the ID of
  the personal or group library you want to access, e.g. the \"user
  ID\" or \"group ID\"."
  (zotero-lib--retrieve :resource 'items :user user :group group :api-key api-key))

;; FIXME: too slow
(cl-defun zotero-lib-get-items-top (&key user group api-key)
  "Top-level items in the library, excluding trashed items.
  Optional argument LIBRARY is 'user for your personal library, and
  'group for the group libraries. Optional argument ID is the ID of
  the personal or group library you want to access, e.g. the \"user
  ID\" or \"group ID\"."
  (zotero-lib--retrieve :resource 'items-top :user user :group group :api-key api-key))

(cl-defun zotero-lib-get-trash-items (&key user group api-key)
  "Items in the trash.
  Optional argument LIBRARY is 'user for your personal library, and
  'group for the group libraries. Optional argument ID is the ID of
  the personal or group library you want to access, e.g. the \"user
  ID\" or \"group ID\"."
  (zotero-lib--retrieve :resource 'trash-items :user user :group group :api-key api-key))

(cl-defun zotero-lib-get-item (&key user group key api-key)
  "A specific item in the library.
  KEY is the item key. Optional argument LIBRARY is 'user for
  your personal library, and 'group for the group libraries.
  Optional argument ID is the ID of the personal or group library
  you want to access, e.g. the \"user ID\" or \"group ID\"."
  (zotero-lib--retrieve :resource 'item :user user :group group :key key :api-key api-key))

(cl-defun zotero-lib-get-item-children (&key user group key api-key)
  "Child items under a specific item.
  KEY is the item key. Optional argument LIBRARY is 'user for
  your personal library, and 'group for the group libraries.
  Optional argument ID is the ID of the personal or group library
  you want to access, e.g. the \"user ID\" or \"group ID\"."
  (zotero-lib--retrieve :resource 'item-children :user user :group group :key key :api-key api-key))

(cl-defun zotero-lib-get-publication-items (&key user group api-key)
  "Items in My Publications.
  Optional argument LIBRARY is 'user for your personal library, and
  'group for the group libraries. Optional argument ID is the ID of
  the personal or group library you want to access, e.g. the \"user
  ID\" or \"group ID\"."
  (zotero-lib--retrieve :resource 'publication-items :user user :group group :api-key api-key))

(cl-defun zotero-lib-get-collection-items (&key user group key api-key)
  "Items within a specific collection in the library.
  KEY is the collection key. Optional argument LIBRARY is 'user for
  your personal library, and 'group for the group libraries.
  Optional argument ID is the ID of the personal or group library
  you want to access, e.g. the \"user ID\" or \"group ID\"."
  (zotero-lib--retrieve :resource 'collection-items :user user :group group :key key :api-key api-key))

(cl-defun zotero-lib-get-collection-items-top (&key user group key api-key)
  "Top-level items within a specific collection in the library.
  KEY is the collection key. Optional argument LIBRARY is 'user for
  your personal library, and 'group for the group libraries.
  Optional argument ID is the ID of the personal or group library
  you want to access, e.g. the \"user ID\" or \"group ID\"."
  (zotero-lib--retrieve :resource 'collection-items-top :user user :group group :key key :api-key api-key))

(cl-defun zotero-lib-get-searches (&key user group api-key)
  "All saved searches in the library.
  Optional argument LIBRARY is 'user for your personal library, and
  'group for the group libraries. Optional argument ID is the ID of
  the personal or group library you want to access, e.g. the \"user
  ID\" or \"group ID\".

  Note: Only search metadata is currently available, not search results."
  (zotero-lib--retrieve :resource 'searches :user user :group group :api-key api-key))

(cl-defun zotero-lib-get-search (&key user group key api-key)
  "A specific saved search in the library.

  KEY is the search key. Optional argument LIBRARY is 'user for
  your personal library, and 'group for the group libraries.
  Optional argument ID is the ID of the personal or group library
  you want to access, e.g. the \"user ID\" or \"group ID\".

  Note: Only search metadata is currently available, not search results."
  (zotero-lib--retrieve :resource 'search :user user :group group :key key :api-key api-key))

;; FIXME: too slow
(cl-defun zotero-lib-get-all-tags (&key user group api-key)
  "All tags in the library.
  Optional argument LIBRARY is 'user for your personal library, and
  'group for the group libraries. Optional argument ID is the ID of
  the personal or group library you want to access, e.g. the \"user
  ID\" or \"group ID\"."
  (zotero-lib--retrieve :resource 'tags :user user :group group :api-key api-key))

(cl-defun zotero-lib-get-tags (&key user group key api-key)
  "Tags of all types matching a specific name.
  KEY is the search key. Optional argument LIBRARY is 'user for
  your personal library, and 'group for the group libraries.
  Optional argument ID is the ID of the personal or group library
  you want to access, e.g. the \"user ID\" or \"group ID\"."
  (zotero-lib--retrieve :resource 'tags :user user :group group :key key :api-key api-key))

(cl-defun zotero-lib-get-item-tags (&key user group key api-key)
  "Tags associated with a specific item.
  KEY is the item key. Optional argument LIBRARY is 'user for your
  personal library, and 'group for the group libraries. Optional
  argument ID is the ID of the personal or group library you want
  to access, e.g. the \"user ID\" or \"group ID\"."
  (zotero-lib--retrieve :resource 'item-tags :user user :group group :key key :api-key api-key))

(cl-defun zotero-lib-get-collection-tags (&key user group key api-key)
  "Tags within a specific collection in the library.
  KEY is the collection key. Optional argument LIBRARY is 'user for
  your personal library, and 'group for the group libraries.
  Optional argument ID is the ID of the personal or group library
  you want to access, e.g. the \"user ID\" or \"group ID\"."
  (zotero-lib--retrieve :resource 'collection-tags :user user :group group :key key :api-key api-key))

(cl-defun zotero-lib-get-items-tags (&key user group api-key)
  "All tags in the library, with the ability to filter based on the items.
  Optional argument LIBRARY is 'user for your personal library, and
  'group for the group libraries. Optional argument ID is the ID of
  the personal or group library you want to access, e.g. the \"user
  ID\" or \"group ID\"."
  (zotero-lib--retrieve :resource 'items-tags :user user :group group :api-key api-key))

(cl-defun zotero-lib-get-items-top-tags (&key user group api-key)
  "Tags assigned to top-level items.
  Optional argument LIBRARY is 'user for your personal library, and
  'group for the group libraries. Optional argument ID is the ID of
  the personal or group library you want to access, e.g. the \"user
  ID\" or \"group ID\"."
  (zotero-lib--retrieve :resource 'items-top-tags :user user :group group :api-key api-key))

(cl-defun zotero-lib-get-trash-items-tags (&key user group api-key)
  "Tags assigned to items in the trash.
  Optional argument LIBRARY is 'user for your personal library, and
  'group for the group libraries. Optional argument ID is the ID of
  the personal or group library you want to access, e.g. the \"user
  ID\" or \"group ID\"."
  (zotero-lib--retrieve :resource 'trash-items-tags :user user :group group :api-key api-key))

(cl-defun zotero-lib-get-collection-items-tags (&key user group key api-key)
  "Tags assigned to items in a given collection.
  KEY is the collection key. Optional argument LIBRARY is 'user for
  your personal library, and 'group for the group libraries.
  Optional argument ID is the ID of the personal or group library
  you want to access, e.g. the \"user ID\" or \"group ID\"."
  (zotero-lib--retrieve :resource 'collection-items-tags :user user :group group :key key :api-key api-key))

(cl-defun zotero-lib-get-collection-items-top-tags (&key user group key api-key)
  "Tags assigned to top-level items in a given collection.
  KEY is the collection key. Optional argument LIBRARY is 'user for
  your personal library, and 'group for the group libraries.
  Optional argument ID is the ID of the personal or group library
  you want to access, e.g. the \"user ID\" or \"group ID\"."
  (zotero-lib--retrieve :resource 'collection-items-top-tags :user user :group group :key key :api-key api-key))

(cl-defun zotero-lib-get-publication-items-tags (&key user group api-key)
  "Tags assigned to items in My Publications.
  Optional argument LIBRARY is 'user for your personal library, and
  'group for the group libraries. Optional argument ID is the ID of
  the personal or group library you want to access, e.g. the \"user
  ID\" or \"group ID\"."
  (zotero-lib--retrieve :resource 'publication-items-tags :user user :group group :api-key api-key))

(cl-defun zotero-lib-get-key (&key api-key)
  "The user id and privileges of the API key."
  (zotero-lib--retrieve :resource 'keys :key api-key))

(cl-defun zotero-lib-get-groups (&key user api-key)
  "The set of groups the current API key has access to, including
  public groups the key owner belongs to even if the key doesn't
  have explicit permissions for them."
  (zotero-lib--retrieve :resource 'groups :user user :api-key api-key))

(cl-defun zotero-lib-get-group (&key group api-key)
  "Retrieve the group metadata."
  (zotero-lib--retrieve :group group :api-key api-key))

(cl-defun zotero-lib-get-all-fulltext (&key user group api-key)
  "For each item with a full-text content VERSION greater than stored locally, get the item's full-text content.
  Optional argument LIBRARY is 'user for your personal library, and
  'group for the group libraries. Optional argument ID is the ID of
  the personal or group library you want to access, e.g. the \"user
  ID\" or \"group ID\"."
  (zotero-lib--retrieve :resource 'all-fulltext :user user :group group :api-key api-key))

(cl-defun zotero-lib-get-item-fulltext (&key user group key api-key)
  "Getting an item's full-text content.
  KEY is the item key. Optional argument LIBRARY is 'user for your
  personal library, and 'group for the group libraries. Optional
  argument ID is the ID of the personal or group library you want
  to access, e.g. the \"user ID\" or \"group ID\"."
  (zotero-lib--retrieve :resource 'item-fulltext :user user :group group :key key :api-key api-key))

(cl-defun zotero-lib-get-file (&key user group key api-key)
  "Return the raw content of a specific item in the library.
  KEY is the item key. Optional argument LIBRARY is 'user for your
  personal library, and 'group for the group libraries. Optional
  argument ID is the ID of the personal or group library you want
  to access, e.g. the \"user ID\" or \"group ID\"."
  (zotero-lib--retrieve :resource 'file :user user :group group :key key :api-key api-key))

(cl-defun zotero-lib-download-file (&key file dir user group key api-key)
  "A convenient wrapper around `zotero-lib-file'.
Write an attachment to disk using the optional DIR and FILE. DIR
is directory to start with if FILE is relative
(does not start with slash or tilde). If DIR is nil, the current
buffer’s value of ‘default-directory’ is used. If FILE is not
supplied, a `zotero-lib-item' call is made to determine the
attachment filename. If successful, the full path including the
file name is returned."
  (let* ((content (zotero-lib-file :user user :group group :key key :api-key api-key))
         (coding-system-for-write 'binary)
         (filename (or file
                       (let ((object (zotero-lib-item :user user :group group :key key :api-key api-key)))
                         (thread-first
                             (plist-get object :data)
                           (plist-get :filename)))))
         (full-filename (expand-file-name filename dir))
         (md5 (zotero-lib-attachment-md5 :user user :group group :key key :api-key api-key))
         (attributes (zotero-lib--file-attributes full-filename)))
    (write-region content nil full-filename nil nil nil t)
    ;; Check the ETag header of the response to make sure it matches
    ;; the attachment item's md5 value. If it doesn't, offer to
    ;; download the attachment item again.
    (if (equal md5 (plist-get attributes :md5))
        full-filename
      (if (y-or-n-p (format "MD5 value doesn't match the response header. Retry? "))
          (zotero-lib-download-file :file file :dir dir :user user :group group :key key :api-key api-key)
        (warn "Item has the wrong hash. The latest version of the
        file may be available only via WebDAV, not via Zotero
        File Storage.")))))

(cl-defun zotero-lib-attachment-attributes (&key user group key api-key)
  "Get the attachment attributes."
  (let* ((item (zotero-lib-item :user user :group group :key key :api-key api-key))
         (data (plist-get item :data)))
    `(:filename ,(plist-get data :filename) :contenttype ,(plist-get data :contentType) :md5 ,(plist-get data :md5) :mtime ,(plist-get data :mtime))))

;; TODO: should this be merged with one of the attachment functions above?
(cl-defun zotero-lib-attachment-md5 (&key user group key api-key)
  (let* ((url (zotero-lib--endpoint :resource 'file :key key :user user :group group))
         (handle `(:url ,url :method "HEAD" :api-version ,zotero-lib-api-version :api-key ,api-key))
         (response (zotero-lib--request handle))
         (etag (plist-get response :etag)))
    etag))

;;;; Item Type/Field Requests

(defun zotero-lib-itemtypes ()
  "Return all item types."
  (let ((url (concat zotero-lib-base-url "/itemTypes")))
    (zotero-lib--retrieve :url url :locale zotero-lib-locale)))

(defun zotero-lib-itemfields ()
  "Get all item fields."
  (let ((url (concat zotero-lib-base-url "/itemFields")))
    (zotero-lib--retrieve :url url :locale zotero-lib-locale)))

(defun zotero-lib-itemtypefields (itemtype)
  "Get all valid fields for an item type."
  (let ((url (concat zotero-lib-base-url "/itemTypeFields")))
    (zotero-lib--retrieve :url url :itemtype itemtype :locale zotero-lib-locale)))

(defun zotero-lib-itemtypecreatortypes (itemtype)
  "Get valid creator types for an item type."
  (let ((url (concat zotero-lib-base-url "/itemTypeFields")))
    (zotero-lib--retrieve :url url :itemtype itemtype :locale zotero-lib-locale)))

(defun zotero-lib-creatorfields ()
  "Get localized creator fields."
  (let ((url (concat zotero-lib-base-url "/creatorFields")))
    (zotero-lib--retrieve :url url :locale zotero-lib-locale)))

(defun zotero-lib-attachment-linkmodes ()
  "Get linkmode types."
  '("imported_file" "imported_url" "linked_file" "linked_url"))

;;;; Template Requests

(defun zotero-lib-item-template (itemtype)
  "Get a template for a new item."
  (let ((url (concat zotero-lib-base-url "/items/new")))
    (zotero-lib--retrieve :url url :itemtype itemtype :locale zotero-lib-locale)))

(defun zotero-lib-attachment-template (linkmode)
  "Get a template for a new attachment item.
  LINKMODE should be one of:
  \"imported_file\",
  \"imported_url\",
  \"linked_file\", or
  \"linked_url\"."
  (let ((url (concat zotero-lib-base-url "/items/new")))
    (zotero-lib--retrieve :url url :itemtype "attachment" :linkmode linkmode :locale zotero-lib-locale)))

;;;; Write Requests

(cl-defun zotero-lib-create-item (object &key user group api-key)
  "Create an item in the library.

  OBJECT is a plist of the new item. When creating a new item,
  first get an empty property list for the item type with
  `zotero-lib-template' (or use a cached version of the template).
  Then modify it and resubmit it to the server in an array. Note
  that when uploading the full property list, only the `:data'
  property is processed. All other properties are ignored.

  Optional argument LIBRARY is 'user for your personal library, and
  'group for the group libraries. Optional argument ID is the ID of
  the personal or group library you want to access, e.g. the \"user
  ID\" or \"group ID\"."
  (let ((write-token (zotero-lib--write-token))
        (json (zotero-lib--encode-object object)))
    (zotero-lib--submit :method "POST" :resource 'items :user user :group group :data json :write-token write-token :api-key api-key :content-type "application/json" :expect "")))

(cl-defun zotero-lib-create-items (&rest objects &key user group)
  "Create multiple items.
  Up to 50 items can be created in a single request.

  Each object should return a plist and may be:
  a cons cell (containing a propery list),
  a buffer (read one Lisp expression from the beginning),
  a function (call it with no arguments),
  a file (read one Lisp expression from the beginning),
  a string (takes text from string, starting at the beginning).

  Optional argument LIBRARY is 'user for your
  personal library, and 'group for the group libraries. Optional
  argument ID is the ID of the personal or group library you want
  to access, e.g. the \"user ID\" or \"group ID\"."
  ;; Apparently for data > 1024 bytes CURL automatically sets "Expect:
  ;; 100-continue". The client expects the server to only fetch the
  ;; header and then send a "100" return code to get the rest of the
  ;; data. However, passing an Expect header is unsupported and will
  ;; result in a 417 Expectation Failed response. Setting "Expect: "
  ;; explicitly disables this automatic behaviour.
  (let ((json (zotero-lib--encode-object objects)))
    (zotero-lib--submit :method "POST" :resource 'items :user user :group group :data json :content-type "application/json" :expect "")))

(cl-defun zotero-lib-update-item (object &key user group key)
  "Update an existing item in the library.

OBJECT is a plist of the modified item. To update an existing item,
first retrieve the current version of the item with
`zotero-lib-item'. The editable data will be found in the `:data'
property of the response. Modify it and resubmit it to the
server. When uploading the full property list, only the `:data'
property is processed.

Notes and attachments can be made child items by assigning the
parent item's key to the `:parentItem' property. If parent and
child items are created in the same POST request, the child items
must appear after the parent item in the array of items.

The item's current version number should be included in the
`:version' property or provided in the optional VERSION argument.
The `:version' property is included in responses from the API, so
clients that simply modify the editable data do not need to
bother with a VERSION argument.

Items can also include `:dateAdded' and `:dateModified'
properties containing timestamps in ISO 8601 format (e.g.,
“2014-06-10T13:52:43Z”). If `:dateAdded' is included with an
existing item, it must match the existing `:dateAdded' value or
else the API will return a \"400 Bad Request\" error. If a new
`:dateModified' time is not included with an update to existing
item, the item's `:dateModified' value will be set to the current
time. Editable data returned from the API includes `:dateAdded'
and `:dateModified' in the correct format, so clients that are
content with server-set modification times can simply ignore
these properties.

KEY is the item key. Optional argument LIBRARY is user' for your
personal library, and 'group for the group libraries. Optional
argument ID is the ID of the personal or group library you want
to access, e.g. the \"user ID\" or \"group ID\"."
  (let ((json (zotero-lib--encode-object object)))
    (zotero-lib--submit :method "PUT" :resource 'item :user user :group group :key key :data json :content-type "application/json" :expect "")))

(cl-defun zotero-lib-update-items (&rest data &key user group version)
  "Update existing items in the library.

Up to 50 items can be updated in a single request. Note that any
properties not specified will be left untouched on the server. To
erase an existing property, include it with an empty string or
false as the value.

Follow the instructions in `zotero-lib-create-item', but include
a key and version property in each object. If modifying editable
a property list returned from the API, these properties will
already exist and shouldn't be modified. As an alternative to
individual version properties, the last-known library version can
be passed via the VERSION argument.

Items can also include `:dateAdded' and `:dateModified'
properties containing timestamps in ISO 8601 format (e.g.,
“2014-06-10T13:52:43Z”). If `:dateAdded' is included with an
existing item, it must match the existing `:dateAdded' value or
else the API will return a \"400 Bad Request\" error. If a new
`:dateModified' time is not included with an update to existing
item, the item's `:dateModified' value will be set to the current
time. Editable data returned from the API includes `:dateAdded'
and `:dateModified' in the correct format, so clients that are
content with server-set modification times can simply ignore
these properties.

You can also submit just the properties that have actually
changed, for a potentially much more efficient operation.
Properties not included in the uploaded data are left untouched
on the server. To clear a property, pass an empty string or an
empty array as appropriate.

Optional argument LIBRARY is user' for your personal library, and
'group for the group libraries. Optional argument ID is the ID of
the personal or group library you want to access, e.g. the \"user
ID\" or \"group ID\"."
  (let ((json (zotero-lib--encode-object data)))
    (zotero-lib--submit :method "POST" :resource 'items :user user :group group :data json :content-type "application/json" :expect "")))

(cl-defun zotero-lib-delete-item (&key user group key version)
  "Delete an item.

KEY is the item key. VERSION is the last known item version.
Optional argument LIBRARY is user' for your personal library, and
'group for the group libraries. Optional argument ID is the ID of
the personal or group library you want to access, e.g. the \"user
ID\" or \"group ID\"."
  (zotero-lib--submit :method "DELETE" :resource 'item :user user :group group :key key :version version))

(cl-defun zotero-lib-delete-items (&rest keys &key user group version)
  "Delete multiple items.
Up to 50 items can be deleted in a single request.

KEYS are the item key. VERSION is the last known item version.
Optional argument LIBRARY is user' for your personal library, and
'group for the group libraries. Optional argument ID is the ID of
the personal or group library you want to access, e.g. the \"user
ID\" or \"group ID\"."
  (cond
   ((eq (length keys) 1)
    (let (key (car keys))
      (zotero-lib-delete-item :user user :group group :key key :version version)))
   ((> (length keys) 50)
    (user-error "Up to 50 items can be deleted in a single request."))
   (t
    (let* ((items (s-join "," keys)))
      (zotero-lib--submit :method "DELETE" :resource 'items :user user :group group :api-key api-key :version version :itemkey items)))))

(cl-defun zotero-lib-create-collection (object &key user group version)
  "Create a collection.

OBJECT is a plist of the new collection. VERSION is the
last-known library version. Optional argument LIBRARY is user'
for your personal library, and 'group for the group libraries.
Optional argument ID is the ID of the personal or group library
you want to access, e.g. the \"user ID\" or \"group ID\"."
  (let ((json (zotero-lib--encode-object object)))
    (zotero-lib--submit :method "POST" :resource 'collections :user user :group group :data json :content-type "application/json" :expect "")))

(cl-defun zotero-lib-update-collection (object &key user group key version)
  "Update an existing collection.

OBJECT is a plist of the modified collection. To update an existing
collection, first retrieve the current version of the item with
`zotero-lib-collection'. The editable data will be found in the
`:data' property of the response. Modify it and resubmit it to
the server. When uploading the full property list, only the
`:data' property is processed.

The collection's current version number should be included in the
`:version' property or provided in the optional VERSION argument.
The `:version' property is included in responses from the API, so
clients that simply modify the editable data do not need to
bother with a VERSION argument.

KEY is the collection key. Optional argument LIBRARY is user' for your
personal library, and 'group for the group libraries. Optional
argument ID is the ID of the personal or group library you want
to access, e.g. the \"user ID\" or \"group ID\"."
  (let ((json (zotero-lib--encode-object object)))
    (zotero-lib--submit :method "PUT" :resource 'collection :user user :group group :key key :data json :version version :content-type "application/json" :expect "")))

(cl-defun zotero-lib-update-collections (&rest data &key user group version)
  "Update existing collections in the library.
Up to 50 collections can be updated in a single request. Note
that any properties not specified will be left untouched on the
server. To erase an existing property, include it with an empty
string or false as the value.

Optional argument LIBRARY is user' for your personal library, and
'group for the group libraries. Optional argument ID is the ID of
the personal or group library you want to access, e.g. the \"user
ID\" or \"group ID\"."
  (let ((json (zotero-lib--encode-object data)))
    (zotero-lib--submit :method "POST" :resource 'collections :user user :group group :data json :content-type "application/json" :expect "")))

(cl-defun zotero-lib-delete-collection (&key user group key version)
  "Delete a collection.

KEY is the collection key. VERSION is the collection's current
version number. Optional argument LIBRARY is user' for your
personal library, and 'group for the group libraries. Optional
argument ID is the ID of the personal or group library you want
to access, e.g. the \"user ID\" or \"group ID\"."
  (zotero-lib--submit :method "DELETE" :resource 'collection :user user :group group :key key :version version))

(cl-defun zotero-lib-delete-collections (&rest keys &key user group version)
  "Delete multiple collections.
KEYS are the collection keys. Up to 50 collections can be deleted in a single request.

VERSION is the last-known library version. Optional argument
LIBRARY is user' for your personal library, and 'group for the
group libraries. Optional argument ID is the ID of the personal
or group library you want to access, e.g. the \"user ID\" or
\"group ID\"."
  (cond
   ((eq (length keys) 1)
    (let (key (car keys))
      (zotero-lib-delete-collection :user user :group group :key key :version version)))
   ((> (length keys) 50)
    (user-error "Up to 50 collections can be deleted in a single request."))
   (t
    (let* ((collections (s-join "," keys)))
      (zotero-lib--submit :method "DELETE" :resource 'collections :user user :group group :api-key api-key :version version :collectionkey collections)))))

(cl-defun zotero-lib-create-search (object &key user group)
  "Create a saved search.
OBJECT is a plist of the new search. Optional argument LIBRARY is
user' for your personal library, and 'group for the group
libraries. Optional argument ID is the ID of the personal or
group library you want to access, e.g. the \"user ID\" or \"group
ID\"."
  (let ((json (zotero-lib--encode-object object)))
    (zotero-lib--submit :method "POST" :resource 'searches :user user :group group :data json :content-type "application/json" :expect "")))

(cl-defun zotero-lib-update-searches (&rest data &key user group version)
  "Update existing searches in the library.
Up to 50 searches can be updated in a single request. Note
that any properties not specified will be left untouched on the
server. To erase an existing property, include it with an empty
string or false as the value.

Optional argument LIBRARY is user' for your personal library, and
'group for the group libraries. Optional argument ID is the ID of
the personal or group library you want to access, e.g. the \"user
ID\" or \"group ID\"."
  (let ((json (zotero-lib--encode-object data)))
    (zotero-lib--submit :method "POST" :resource 'searches :user user :group group :data json :content-type "application/json" :expect "")))

(cl-defun zotero-lib-delete-searches (&rest keys &key user group version)
  "Delete multiple searches.
Up to 50 searches can be deleted in a single request.

KEYS are the search keys. VERSION is the last-known library
version. Optional argument LIBRARY is user' for your personal
library, and 'group for the group libraries. Optional argument ID
is the ID of the personal or group library you want to access,
e.g. the \"user ID\" or \"group ID\"."
  (cond
   ((> (length keys) 50)
    (user-error "Up to 50 searches can be deleted in a single request."))
   (t
    (let ((searches (s-join "," keys)))
      (zotero-lib--submit :method "DELETE" :resource 'searches :user user :group group :api-key api-key :version version :searchkey searches)))))

(cl-defun zotero-lib-delete-tags (&rest tags &key user group version)
  "Delete multiple tags.
Up to 50 tags can be deleted in a single request.

TAGS are the tag strings. VERSION is the last-known library
version. Optional argument LIBRARY is user' for your personal
library, and 'group for the group libraries. Optional argument ID
is the ID of the personal or group library you want to access,
e.g. the \"user ID\" or \"group ID\"."
  (cond
   ((> (length keys) 50)
    (user-error "Up to 50 tags can be deleted in a single request."))
   (t
    (let* ((url-encoded-tags (mapcar 'url-hexify-string tags))
           (value (s-join "||" url-encoded-tags)))
      (zotero-lib--submit :method "DELETE" :resource 'tags :user user :group group :api-key api-key :version version :tag value)))))

;; TODO: test
(cl-defun zotero-lib-delete-key (&key api-key)
  "Delete the API key."
  (zotero-lib--submit :method "DELETE" :resource 'keys :key api-key))

;;;; File Uploads

;; The exact process depends on whether you are adding a new
;; attachment file or modifying an existing one and whether you are
;; performing a full upload or uploading a binary diff.

;;;;; 1a) Create a new attachment
;; https://www.zotero.org/support/dev/web_api/v3/file_upload#a_create_a_new_attachment

;; i. Get attachment item template with `zotero-lib-attachment-template'
;; https://www.zotero.org/support/dev/web_api/v3/file_upload#i_get_attachment_item_template

;; ii. Create child attachment item with `zotero-lib-create-attachment'
;; https://www.zotero.org/support/dev/web_api/v3/file_upload#ii_create_child_attachment_item

(cl-defun zotero-lib-create-attachment (object &key user group api-key)
  "Create a child attachment item in the library.

`:md5' and `:mtime' properties should not be edited directly when
using Zotero File Storage, which provides an atomic method for
setting the properties along with the corresponding file.

Top-level attachments can be created by excluding the
`:parentItem' property or setting it to false. Though the API
allows all attachments to be made top-level items for
backward-compatibility, it is recommended that only file
attachments (\"imported_file\"/\"linked_file\") and PDF imported web
attachments (\"imported_url\" with content type \"application/pdf\") be
allowed as top-level items, as in the Zotero client."
  ;; Curl automatically sets an "Expect:
  ;; 100-continue" header if the request is a
  ;; POST and the data size is larger than
  ;; 1024 bytes. However, the Zotero API
  ;; doesn't support the "Expect:" header,
  ;; resulting in a "417 Expectation Failed"
  ;; response. Setting the "Expect:" header to
  ;; an empty string explicitly disables this
  ;; automatic behaviour.
  (let ((write-token (zotero-lib--write-token))
        (json (zotero-lib--encode-object object)))
    (zotero-lib--submit :method "POST" :resource 'items :user user :group group :data json :content-type "application/json" :expect "" :write-token write-token :api-key api-key)))

;; TODO: test
(cl-defun zotero-lib-update-attachment (object &key md5 user group api-key)
  "Create a child attachment item in the library.

`:md5' and `:mtime' properties should not be edited directly when
using Zotero File Storage, which provides an atomic method for
setting the properties along with the corresponding file.

Argument `:md5' is the previous MD5 hash of the file
(as provided in the ETag header when downloading it)."
  (let ((write-token (zotero-lib--write-token))
        (json (zotero-lib--encode-object object)))
    (zotero-lib--submit :method "POST" :resource 'items :user user :group group :data json :content-type "application/json" :expect "" :write-token write-token :if-match md5 :api-key api-key)))

;;;;; 1b) Modify an existing attachment
;; https://www.zotero.org/support/dev/web_api/v3/file_upload#b_modify_an_existing_attachment

;; i. Retrieve the attachment information
;; https://www.zotero.org/support/dev/web_api/v3/file_upload#i_retrieve_the_attachment_information

;; TODO

;; For existing attachments, use If-Match: <hash> in place of
;; If-None-Match: *, where <hash> is the previous MD5 hash of the file
;; (as provided in the ETag header when downloading it).

;; Note that mtime must be provided in milliseconds, not seconds.

;; ii. Download the existing file with `zotero-lib-download-file'
;; https://www.zotero.org/support/dev/web_api/v3/file_upload#ii_download_the_existing_file

;; Check the ETag header of the response to make sure it matches the
;; attachment item's md5 value. If it doesn't, check the attachment
;; item again. If the attachment item still has a different hash, the
;; latest version of the file may be available only via WebDAV, not
;; via Zotero File Storage, and it is up to the client how to proceed.

;; Save the file as filename and set the modification time to the
;; mtime provided in the attachment item.

;; iii. Make changes to the file
;; https://www.zotero.org/support/dev/web_api/v3/file_upload#iii_make_changes_to_the_file

;; Note that to perform a faster partial upload using a binary diff,
;; you must save a copy of the file before changes are made.

;;;;; 2) Get upload authorization
;; https://www.zotero.org/support/dev/web_api/v3/file_upload#get_upload_authorization

(cl-defun zotero-lib-set-item-fulltext (object &key user group key)
  "Set an item's full-text content.

OBJECT should be a a plist containing three props:
- `:content': the full-text content, and either
- `:indexedChars' and `:totalChars' for text documents, or
- `:indexedPages' and `:totalPages' for PDFs.

KEY is the item key. Optional argument LIBRARY is 'user for your
personal library, and 'group for the group libraries. Optional
argument ID is the ID of the personal or group library you want
to access, e.g. the \"user ID\" or \"group ID\"."
  (let ((json (zotero-lib--encode-object object)))
    (zotero-lib--submit :method PUT :resource 'item-fulltext :user user :group group :key key :data json :api-key api-key)))

(cl-defun zotero-lib-authorize-upload (&key user group key api-key filename filesize md5 mtime etag)
  "Get upload authorisation for a file."
  (let ((data (url-build-query-string `(("filename" ,filename)
                                        ("filesize" ,filesize)
                                        ("md5" ,md5)
                                        ("mtime" ,mtime)))))
    ;; For existing attachments, use If-Match: <hash> in place of If-None-Match:
    ;; *, where <hash> is the previous MD5 hash of the file (as provided in the
    ;; ETag header when downloading it).
    (apply #'zotero-lib--submit :method "POST" :resource 'file :user user :group group :key key :data data :content-type "application/x-www-form-urlencoded" :api-key api-key (if hash '(:if-match hash) '(:if-none-match "*")))))

;; A successful 200 response returns one of two possible JSON objects:

;; {
;; "url": ...,
;; "contentType": ...,
;; "prefix": ...,
;; "suffix": ...,
;; "uploadKey": ...
;; }

;; or

;; { "exists": 1 }

;; In the latter case, the file already exists on the server and was successfully associated with the specified item. No further action is necessary.

;;;;; 3a) Full upload
;; https://www.zotero.org/support/dev/web_api/v3/file_upload#a_full_upload

;; i. POST file
;; https://www.zotero.org/support/dev/web_api/v3/file_upload#i_post_file

;; Concatenate prefix, the file contents, and suffix and POST to url
;; with the Content-Type header set to contentType.

;; prefix and suffix are strings containing multipart/form-data. In
;; some environments, it may be easier to work directly with the form
;; parameters. Add params=1 to the upload authorization request above
;; to retrieve the individual parameters in a params array, which will
;; replace contentType, prefix, and suffix.

(defun zotero-lib-upload-file (file url contenttype prefix suffix)
  "Upload file."
  (let ((content (with-temp-buffer
                   (insert-file-contents-literally file)
                   (buffer-string))))
    (request url
      :headers `(("Content-Type" . ,contenttype))
      :data (concat prefix content suffix)
      :parser 'buffer-string
      :sync t)))

;; ii. Register upload
;; https://www.zotero.org/support/dev/web_api/v3/file_upload#ii_register_upload

(cl-defun zotero-lib-register-upload (&key user group key api-key uploadkey hash)
  "Register upload."
  (let ((data (url-build-query-string `(("upload" ,uploadkey)))))
    ;; For existing attachments, use If-Match: <hash>, where <hash> is
    ;; the previous MD5 hash of the file, provided as the md5 property
    ;; in the attachment item.
    (if hash
        (zotero-lib--submit :method "POST" :resource 'file :user user :group group :key key :data data :content-type "application/x-www-form-urlencoded" :if-match hash :api-key api-key)
      (zotero-lib--submit :method "POST" :resource 'file :user user :group group :key key :data data :content-type "application/x-www-form-urlencoded" :if-none-match "*" :api-key api-key))))

;; After the upload has been registered, the attachment item will reflect the new metadata.
;; 3b) Partial upload
;; https://www.zotero.org/support/dev/web_api/v3/file_upload#b_partial_upload

;; TODO: is this one necessary?
(cl-defun zotero-lib-add-attachment (&key file linkmode parent user group)
  "A convenient wrapper."
  (let* ((attributes (zotero-lib--file-attributes file))
         (filename (plist-get attributes :filename))
         (contenttype (plist-get attributes :contenttype))
         (accessdate (plist-get attributes :accessdate))
         ;; create attachment item from template
         (object (thread-first
                     (zotero-lib-attachment-template linkmode)
                   (plist-put :parentItem parent)
                   (plist-put :contentType contenttype)
                   (plist-put :accessDate accessdate)
                   (plist-put :filename filename))))
    (zotero-lib-create-item object :user user :group group)))

;; TODO: error handling
(cl-defun zotero-lib-upload-attachment (&key file user group key api-key hash)
  "A convenient wrapper to authorize, upload and register an attachment."
  ;; Authorize upload
  (message "Authorize upload...")
  (let* ((attributes (zotero-lib--file-attributes file))
         (filename (plist-get attributes :filename))
         (filesize (plist-get attributes :filesize))
         (md5 (plist-get attributes :md5))
         (mtime (plist-get attributes :mtime))
         (data (zotero-lib-authorize-upload :user user :group group :key key :api-key api-key :filename filename :filesize filesize :md5 md5 :mtime mtime :hash hash)))
    (if (equal (plist-get data :exists) "1")
        (message "Authorize upload...already exists")
      (message "Authorize upload...done")
      ;; Upload file
      (message "Upload file...")
      (let ((url (plist-get data :url))
            (contenttype (plist-get data :contentType))
            (prefix (plist-get data :prefix))
            (suffix (plist-get data :suffix))
            (uploadkey (plist-get data :uploadKey)))
        (zotero-lib-upload-file file url contenttype prefix suffix)
        (message "Upload file...done")
        ;; Register upload
        (message "Register upload...")
        (zotero-lib-register-upload :user user :group group :key key :api-key api-key :uploadkey uploadkey :hash hash)
        (message "Register upload...done")))))

(provide 'zotero-lib)

;;; zotero-lib.el ends here
