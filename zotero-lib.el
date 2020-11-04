;;; zotero-lib.el --- Library for the Zotero API  -*- lexical-binding: t; -*-

;; Author: Folkert van der Beek <folkertvanderbeek@gmail.com>
;; Created: 2020-03-27
;; Version: 0.1
;; Keywords: zotero, hypermedia
;; Package-Requires: ((emacs "27.1") (ht "2.2") (oauth "1.0.4") (request "0.3.2") (s "1.12.0"))
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
;; Emacs 27 is required by zotero-cache.el because of ISO 8601 time parsing.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'url)
(require 'ht)
(require 'request)
(require 's)
(require 'zotero-auth)

;;;; Variables

;; TODO: Specify a User-Agent header that properly identifies emacs-zotero and that provides a means of contacting the developer via email using "mailto:". For example: emacs-zotero/0.1 (https://gitlab.com/fvdbeek/emacs-zotero/; mailto:folkertvanderbeek@gmail.com).

(defconst zotero-lib-base-url "https://api.zotero.org")

(defconst zotero-lib-api-version 3
  "API version. Version 3 is currently the default and recommended version.")

(defvar zotero-lib-headers
  '(:api-key "Zotero-API-Key"
             :api-version "Zotero-API-Version"
             :content-type "Content-Type"
             :expect "Expect"
             :last-modified-version "Last-Modified"
             :if-match "If-Match"
             :if-modified-since-version "If-Modified-Since-Version"
             :if-none-match "If-None-Match"
             :if-unmodified-since-version "If-Unmodified-Since-Version"
             :write-token "Zotero-Write-Token")
  "The header keys and their strings.")

(defvar zotero-lib-params
  '(:collectionkey "collectionKey"
                   :content "content"
                   :direction "direction"
                   :format "format"
                   :include-trashed "includeTrashed"
                   :itemkey "itemKey"
                   :itemtype "itemType"
                   :limit "limit"
                   :linkmode "linkMode"
                   :linkwrap "linkwrap"
                   :locale "locale"
                   :q "q"
                   :qmode "qmode"
                   :searchkey "searchKey"
                   :since "since"
                   :sort "sort"
                   :style "style"
                   :tag "tag")
  "The parameter keys and their strings.")

;; (defvar zotero-lib-status-line-regexp "\\([^ ]+\\) \\([[:digit:]]\\{3\\}\\) \\(.*\\)")
;; (defvar zotero-lib-fieldname-regexp (regexp-opt-charset (nconc (number-sequence 33 57) (number-sequence 59 126))))
;; (defvar zotero-lib-fieldbody-regexp (regexp-opt-charset (nconc '(9) (number-sequence 32 126))))
;; (defvar zotero-lib-header-regexp (concat "^\\(" zotero-lib-fieldname-regexp "+\\)"
;;                                          ": \\(" zotero-lib-fieldbody-regexp "*\\)$"))

(defvar zotero-lib-link-regexp "<\\([^>]*\\)>; rel=\"\\([[:word:]]+\\)\""
  "A regular expression matching the body of the link header.")

;;;; Customization

(defgroup zotero-lib nil
  "Library for the Zotero API"
  :group 'external)

(defconst zotero-lib-directory
  (or (and load-file-name
           (file-name-directory load-file-name))
      default-directory)
  "The directory from where this library was first loaded.")

(defcustom zotero-lib-timeout 30
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

(defcustom zotero-lib-linkwrap nil
  "Non-nil means to return URLs and DOIs as links."
  :group 'zotero-lib
  :type 'boolean)

(defcustom zotero-lib-locale "en-US"
  "Locale used in translations.
See the available CSL locales. Note that some styles use a fixed
locale and cannot be localized."
  :group 'zotero-lib
  :type 'string
  :link '(url-link "https://github.com/citation-style-language/locales"))

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
in a nested plist. The lookup for each prop should return another
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

(defun zotero-lib-error-p (response)
  "Return t if the request gave an error, else return nil."
  (plist-get response :error-thrown))

(defun zotero-lib-succes-p (response)
  "Return t if the request was succesfull, else return nil."
  (when (eq (plist-get response :symbol-status) 'succes) t))

(defun zotero-lib-not-modified-p (response)
  "Return t if the data was not modified, else return nil.
If the \"If-Modified-Since-Version\" header is passed with a multi-object read request and data has not changed in the library since the specified version, the API will return 304 Not Modified. Single-object conditional requests are not currently supported, but will be supported in the future."
  (if (eq (plist-get response :status-code) 304) t nil))

(defun zotero-lib-modified-p (response)
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
  (if (eq (plist-get response :status-code) 412) t nil))

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
between 33 and 126), except colon. A field body may be composed
of printable US-ASCII characters as well as the space (ASCII
value 32) and horizontal tab (ASCII value 9) characters."
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

A JSON object will be converted to a plist. A JSON array of
objects wil be converted to a vector of plists.

OBJECT may be: a buffer (read one Lisp expression from the
beginning) a function (call it with no arguments) a file (read
one Lisp expression from the beginning) a string (takes text from
string, starting at the beginning)."
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
- a plist
- a buffer (read one Lisp expression from the beginning)
- a function (call it with no arguments)
- a file (read one Lisp expression from the beginning)
- a string (takes text from string, starting at the beginning)."
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

(defun zotero-lib-encode-object (&rest objects)
  "Return a JSON array with OBJECTS.

Each of the OBJECTS may be:
- a cons cell
- a buffer (read one Lisp expression from the beginning)
- a function (call it with no arguments)
- a file (read one Lisp expression from the beginning)
- a string (takes text from string, starting at the beginning)."
  (zotero-lib--before-write-function)
  (let (result)
    (dolist (object objects result)
      (let ((plist (zotero-lib--read object)))
        (if plist
            (push plist result)
          (user-error "Object %S doesn't return a property list" object))))
    (prog1
        (json-encode-array (seq-into (nreverse result) 'vector))
      (zotero-lib--after-write-function))))

;;;; Resources

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
                 (value (plist-get handle key)))
        (setq params (cons `(,param . ,value) params))))))

(cl-defun zotero-lib--endpoint (&key type id resource key)
  "Return the url from which the Zotero can access RESOURCE.
RESOURCE is one of ... KEY is the item key, collection key, or
search key. Which key is needed varies by resource. TYPE is
\"user\" for your personal library, and \"group\" for the group
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

  (let* ((prefix (pcase type
                   ("user"
                    (concat "/users/" (pcase id
                                        ((pred numberp) (number-to-string id))
                                        ((pred stringp) id))))
                   ("group"
                    (concat "/groups/" (pcase id
                                         ((pred numberp) (number-to-string id))
                                         ((pred stringp) id))))))
         (suffix (pcase resource
                   ("collections" "/collections")
                   ("collections-top" "/collections/top")
                   ("collection" (concat "/collections/" key))
                   ("subcollections" (concat "/collections/" key "/collections"))
                   ("items" "/items")
                   ("items-top" "/items/top")
                   ("trash-items" "/items/trash")
                   ("item" (concat "/items/" key))
                   ("item-children" (concat "/items/" key "/children"))
                   ("publication-items" "/publications/items/")
                   ("collection-items" (concat "/collections/" key "/items"))
                   ("collection-items-top" (concat "/collections/" key "/items/top"))
                   ("searches" "/searches")
                   ("search" (concat "/searches/" key))
                   ("tags" "/tags")
                   ("tags" (concat "/tags/" (url-hexify-string key)))
                   ("item-tags" (concat "/items/" key "/items/tags"))
                   ("collection-tags" (concat "/collection/" key "/tags"))
                   ("items-tags" "/items/tags")
                   ("items-top-tags" "/items/top/tags")
                   ("trash-items-tags" "/items/trash/tags")
                   ("collection-items-tags" (concat "/items/" key "/items/tags"))
                   ("collection-items-top-tags" (concat "/items/" key "/items/top/tags"))
                   ("publication-items-tags" "/publications/tags")
                   ("keys" (concat "/keys/" key))
                   ("groups" "/groups")
                   ("all-fulltext" "/fulltext")
                   ("item-fulltext" (concat "/items/" key "/fulltext"))
                   ("file" (concat "/items/" key "/file"))
                   ("deleted" (concat "/deleted" ))
                   ;; Default
                   (_ nil))))
    (concat zotero-lib-base-url prefix suffix)))

;; FIXME: cleaner handling of headers and params
(defun zotero-lib--request (handle)
  "Return response for an API request to Zotero."
  (let* ((response (request (plist-get handle :url)
                     :type (plist-get handle :method)
                     :headers (apply #'zotero-lib--add-to-headers handle (cl-loop for key in zotero-lib-headers by #'cddr collect key))
                     :data (plist-get handle :data)
                     :params (apply #'zotero-lib--add-to-params handle (cl-loop for key in zotero-lib-params by #'cddr collect key))
                     ;; The response body is first returned as a string, so it can be
                     ;; parsed later according to the content type
                     :parser #'buffer-string
                     :timeout zotero-lib-timeout
                     :sync t))
         ;; Due to limitation of url-retrieve-synchronously, response slot
         ;; request-response-error-thrown is unknown (always nil) when using
         ;; synchronous request with url-retrieve backend.
         (error-thrown (request-response-error-thrown response))
         (symbol-status (request-response-symbol-status response))
         (status-code (request-response-status-code response))
         (backoff (when-let ((seconds (request-response-header response "Backoff"))) (string-to-number seconds)))
         (retry-after (when-let ((seconds (request-response-header response "Retry-After"))) (string-to-number seconds)))
         (content-type (request-response-header response "Content-Type"))
         (etag (when-let ((etag (request-response-header response "Etag"))) (substring etag 1 -1))) ; remove the quotes
         (last-modified-version (when-let ((version (request-response-header response "Last-Modified-Version"))) (string-to-number version)))
         (links (zotero-lib--parse-links (request-response-header response "Link")))
         (raw-header (request-response--raw-header response))
         (raw-data (request-response-data response))
         (total-results (when-let ((results (request-response-header response "Total-Results"))) (string-to-number results)))
         (data (pcase content-type
                 ((and (pred stringp)
                       (pred (string-match-p "application/json.*")))
                  (zotero-lib--read-json raw-data))
                 (_
                  raw-data))))
    `(:error-thrown ,error-thrown
                    :symbol-status ,symbol-status
                    :status-code ,status-code
                    :backoff ,backoff
                    :retry-after ,retry-after
                    :content-type ,content-type
                    :etag ,etag
                    :version ,last-modified-version
                    :next-url ,(plist-get links :next)
                    :prev-url ,(plist-get links :prev)
                    :first-url ,(plist-get links :first)
                    :last-url ,(plist-get links :last)
                    :alternate-url ,(plist-get links :alternate)
                    :raw-header ,raw-header
                    :raw-data ,raw-data
                    :total-results ,total-results
                    :data ,data)))

(defun zotero-lib--authorize (handle)
  "Reauthorize Zotero and return a new request handle."
  (if (y-or-n-p (format "Invalid API key. Authorize Zotero and retry? "))
      (when-let ((token (zotero-auth-authorize))
                 (api-key (zotero-auth-api-key token)))
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

;; TODO: timeout error
(defun zotero-lib--dispatch (handle &optional total)
  "Return the response of the request to the Zotero API.

This is a recursive function that dispatches HANDLE to
`zotero-lib--request' and decides what to do next based on the
response. If the response is paginated the data is concatenated."
  (let ((response (zotero-lib--request handle)))
    ;; TODO: If the API servers are overloaded, the API may include a
    ;; "Backoff: <seconds>" HTTP header in responses, indicating that the
    ;; client should perform the minimum number of requests necessary to
    ;; maintain data consistency and then refrain from making further
    ;; requests for the number of seconds indicated. Backoff can be included
    ;; in any response, including successful ones.
    ;; REVIEW: throw an error?
    ;; (when-let ((error-thrown (plist-get response :error-thrown)))
    ;;   (signal (car error-thrown) (cdr error-thrown)))
    (pcase (plist-get response :status-code)
      ;; OK
      (200
       (if total
           (if-let ((data (plist-get response :data))
                    (total (seq-concatenate 'vector total data))
                    (next-url (plist-get response :next-url))
                    (handle (plist-put handle :url next-url)))
               (zotero-lib--dispatch handle total)
             (plist-put response :data total))
         response))
      ;; No Content
      (204
       response)
      ;; Not Modified
      (304
       response)
      ;; Bad request
      (400
       (let ((message (plist-get response :data)))
         (user-error message)))
      ;; Forbidden
      (403
       ;; TODO: 403 Forbidden File editing is denied.
       (pcase (plist-get response :data)
         ("Invalid key"
          ;; Authorize and return a new handle, but no data
          (let ((handle (zotero-lib--authorize handle)))
            (zotero-lib--dispatch handle)))
         ("Forbidden"
          ;; Offer to review the privileges and return a new handle, but no data
          (let ((handle (zotero-lib--privileges handle)))
            (zotero-lib--dispatch handle)))
         ("Write access denied"
          (error "Write access denied"))
         (message
          (user-error "Unknown authentication error: %s" message))))
      (404
       (user-error "404 Not found"))
      (409
       (user-error "409 Conflict: The target library is locked"))
      (412
       (user-error "412 Precondition Failed: The file has changed
       remotely since retrieval (i.e., the provided ETag no
       longer matches). Conflict resolution is left to the
       client"))
      (413
       (user-error "413 Request Entity Too Large: The upload
       would exceed the storage quota of the library owner"))
      (428
       (user-error "428 Precondition Required: the \"If-Match\"
       or \"If-None-Match\" header was not provided."))
      (429
       ;; If a client has made too many requests within a given time
       ;; period, the API may return 429 Too Many Requests with a
       ;; "Retry-After: <seconds>" header. Clients receiving a 429 should
       ;; wait the number of seconds indicated in the header before
       ;; retrying the request.
       (let ((seconds (plist-get response :retry-after)))
         (message "Too Many Requests: Trying again after %d seconds as specified in the Retry-After header." seconds)
         (sleep-for seconds)
         (zotero-lib--dispatch handle)))
      ;; Service unavailable
      (503
       (user-error Service" Unavailable")))))

(cl-defun zotero-lib-retrieve (&key url type id resource key api-key version format if-match if-none-match include-trashed itemtype last-modified-version linkmode locale since q qmode tag itemkey collectionkey searchkey)
  "Return the last-modified-version and the data returned by the Zotero request.
The result is a cons of (version . data).

KEY is the item key, collection key, or search key. Which key is
needed varies by resource. TYPE is \"user\" (default) for your
personal library, and \"group\" for the group libraries. ID is the
ID of the personal or group library you want to access, e.g. the
user ID (default) or group ID. Your personal library ID is
available at <https://www.zotero.org/settings/keys/>. For group
libraries, the ID can be found by opening the group's page at
<https://www.zotero.org/groups/>."
  ;; Request the specified URL or construct the endpoint of the resource
  (let* ((url (or url
                  (zotero-lib--endpoint :type type :id id :resource resource :key key)))
         (handle `(:url ,url :method "GET" :api-key ,api-key :api-version ,zotero-lib-api-version :collectionkey ,collectionkey :format ,format :if-modified-since-version ,version :itemkey ,itemkey :if-match ,if-match :if-none-match ,if-none-match :include-trashed ,include-trashed :itemtype ,itemtype :last-modified-version ,last-modified-version :linkmode ,linkmode :locale ,locale :searchkey ,searchkey :since ,since :q ,q :qmode ,qmode :tag ,tag)))
    (zotero-lib--dispatch handle)))

(cl-defun zotero-lib-submit (&key method url type id resource key data api-key version content-type expect if-match if-none-match write-token)
  "Return a plist with the response of the Zotero request.

KEY is the item key, collection key, or search key. Which key is
needed varies by resource. TYPE is \"user\" (default) for your
personal library, and \"group\" for the group libraries. ID is the
ID of the personal or group library you want to access, e.g. the
user ID (default) or group ID. Your personal library ID is
available at <https://www.zotero.org/settings/keys/>. For group
libraries, the ID can be found by opening the group's page at
<https://www.zotero.org/groups/>."
  ;; Request the specified URL or construct the endpoint of the resource
  (let* ((url (or url
                  (zotero-lib--endpoint :resource resource :key key :type type :id id)))
         (handle `(:url ,url :method ,method :api-key ,api-key :api-version ,zotero-lib-api-version :content-type ,(or content-type "application/json") :data ,data :expect "" :if-match ,if-match :if-unmodified-since-version ,version :if-none-match ,if-none-match :write-token ,write-token)))
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

(defun zotero-lib-file-attributes (file)
  "Return the attributes of FILE as a plist with `:filename', `:filesize', `:content-type', `:md5', `:mtime', and `:accessdate' props.

Note that mtime is be provided in milliseconds, not seconds."
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
                    ;; multiply by 1000
                    (* 1000)))
           ;; convert to ISO 8601 date format
           (accessdate (format-time-string "%FT%T%z" (file-attribute-access-time attributes)))
           ;; filename without its directory
           (filename (file-name-nondirectory file))
           ;; filesize in bytes
           (filesize (file-attribute-size attributes))
           (content-type (or (mailcap-file-name-to-mime-type filename) "application/octet-stream")))
      `(:filename ,filename :filesize ,filesize :content-type ,content-type :md5 ,md5 :mtime ,mtime :accessdate ,accessdate))))

(cl-defun zotero-lib-get-collections (&key type id api-key)
  "Collections in the library.
  Keyword argument TYPE is \"user\" for your personal library, and
  \"group\" for the group libraries. Keyword argument ID is the ID of
  the personal or group library you want to access, e.g. the \"user
  ID\" or \"group ID\"."
  (let ((response (zotero-lib-retrieve :type type :id id :resource "collections" :api-key api-key)))
    (plist-get response :data)))

(cl-defun zotero-lib-get-collections-top (&key type id api-key)
  "Top-level collections in the library.
  Keyword argument TYPE is \"user\" for your personal library, and
  \"group\" for the group libraries. Keyword argument ID is the ID of
  the personal or group library you want to access, e.g. the \"user
  ID\" or \"group ID\"."
  (let ((response (zotero-lib-retrieve :type type :id id :resource "collections-top" :api-key api-key)))
    (plist-get response :data)))

(cl-defun zotero-lib-get-collection (&key type id key api-key)
  "A specific collection in the library.
  KEY is the collection key. Keyword argument TYPE is \"user\" for
  your personal library, and \"group\" for the group libraries.
  Keyword argument ID is the ID of the personal or group library
  you want to access, e.g. the \"user ID\" or \"group ID\"."
  (let ((response (zotero-lib-retrieve :type type :id id :resource "collection" :key key :api-key api-key)))
    (plist-get response :data)))

(cl-defun zotero-lib-get-subcollections (&key type id key api-key)
  "Subcollections within a specific collection in the library.
  KEY is the collection key. Keyword argument TYPE is \"user\" for
  your personal library, and \"group\" for the group libraries.
  Keyword argument ID is the ID of the personal or group library
  you want to access, e.g. the \"user ID\" or \"group ID\"."
  (let ((response (zotero-lib-retrieve :type type :id id :resource "subcollections" :key key :api-key api-key)))
    (plist-get response :data)))

;; FIXME: too slow
(cl-defun zotero-lib-get-items (&key type id api-key)
  "All items in the library, excluding trashed items.
  Keyword argument TYPE is \"user\" for your personal library, and
  \"group\" for the group libraries. Keyword argument ID is the ID of
  the personal or group library you want to access, e.g. the \"user
  ID\" or \"group ID\"."
  (let ((response (zotero-lib-retrieve :type type :id id :resource "items" :api-key api-key)))
    (plist-get response :data)))

;; FIXME: too slow
(cl-defun zotero-lib-get-items-top (&key type id api-key)
  "Top-level items in the library, excluding trashed items.
  Keyword argument TYPE is \"user\" for your personal library, and
  \"group\" for the group libraries. Keyword argument ID is the ID of
  the personal or group library you want to access, e.g. the \"user
  ID\" or \"group ID\"."
  (let ((response (zotero-lib-retrieve :type type :id id :resource "items-top" :api-key api-key)))
    (plist-get response :data)))

(cl-defun zotero-lib-get-trash-items (&key type id api-key)
  "Items in the trash.
  Keyword argument TYPE is \"user\" for your personal library, and
  \"group\" for the group libraries. Keyword argument ID is the ID of
  the personal or group library you want to access, e.g. the \"user
  ID\" or \"group ID\"."
  (let ((response (zotero-lib-retrieve :type type :id id :resource "trash-items" :api-key api-key)))
    (plist-get response :data)))

(cl-defun zotero-lib-get-item (&key type id key api-key)
  "A specific item in the library.
  KEY is the item key. Keyword argument TYPE is \"user\" for
  your personal library, and \"group\" for the group libraries.
  Keyword argument ID is the ID of the personal or group library
  you want to access, e.g. the \"user ID\" or \"group ID\"."
  (let ((response (zotero-lib-retrieve :type type :id id :resource "item" :key key :api-key api-key)))
    (plist-get response :data)))

(cl-defun zotero-lib-get-item-children (&key type id key api-key)
  "Child items under a specific item.
  KEY is the item key. Keyword argument TYPE is \"user\" for
  your personal library, and \"group\" for the group libraries.
  Keyword argument ID is the ID of the personal or group library
  you want to access, e.g. the \"user ID\" or \"group ID\"."
  (let ((response (zotero-lib-retrieve :type type :id id :resource "item-children" :key key :api-key api-key)))
    (plist-get response :data)))

(cl-defun zotero-lib-get-publication-items (&key type id api-key)
  "Items in My Publications.
  Keyword argument TYPE is \"user\" for your personal library, and
  \"group\" for the group libraries. Keyword argument ID is the ID of
  the personal or group library you want to access, e.g. the \"user
  ID\" or \"group ID\"."
  (let ((response (zotero-lib-retrieve :type type :id id :resource "publication-items" :api-key api-key)))
    (plist-get response :data)))

(cl-defun zotero-lib-get-collection-items (&key type id key api-key)
  "Items within a specific collection in the library.
  KEY is the collection key. Keyword argument TYPE is \"user\" for
  your personal library, and \"group\" for the group libraries.
  Keyword argument ID is the ID of the personal or group library
  you want to access, e.g. the \"user ID\" or \"group ID\"."
  (let ((response (zotero-lib-retrieve :type type :id id :resource "collection-items" :key key :api-key api-key)))
    (plist-get response :data)))

(cl-defun zotero-lib-get-collection-items-top (&key type id key api-key)
  "Top-level items within a specific collection in the library.
  KEY is the collection key. Keyword argument TYPE is \"user\" for
  your personal library, and \"group\" for the group libraries.
  Keyword argument ID is the ID of the personal or group library
  you want to access, e.g. the \"user ID\" or \"group ID\"."
  (let ((response (zotero-lib-retrieve :type type :id id :resource "collection-items-top" :key key :api-key api-key)))
    (plist-get response :data)))

(cl-defun zotero-lib-get-searches (&key type id api-key)
  "All saved searches in the library.
  Keyword argument TYPE is \"user\" for your personal library, and
  \"group\" for the group libraries. Keyword argument ID is the ID of
  the personal or group library you want to access, e.g. the \"user
  ID\" or \"group ID\".

  Note: Only search metadata is currently available, not search results."
  (let ((response (zotero-lib-retrieve :type type :id id :resource "searches" :api-key api-key)))
    (plist-get response :data)))

(cl-defun zotero-lib-get-search (&key type id key api-key)
  "A specific saved search in the library.

  KEY is the search key. Keyword argument TYPE is \"user\" for
  your personal library, and \"group\" for the group libraries.
  Keyword argument ID is the ID of the personal or group library
  you want to access, e.g. the \"user ID\" or \"group ID\".

  Note: Only search metadata is currently available, not search results."
  (let ((response (zotero-lib-retrieve :type type :id id :resource "search" :key key :api-key api-key)))
    (plist-get response :data)))

;; FIXME: too slow
(cl-defun zotero-lib-get-all-tags (&key type id api-key)
  "All tags in the library.

  Keyword argument TYPE is \"user\" for your personal library, and
  \"group\" for the group libraries. Keyword argument ID is the ID of
  the personal or group library you want to access, e.g. the \"user
  ID\" or \"group ID\"."
  (let ((response (zotero-lib-retrieve :type type :id id :resource "tags" :api-key api-key)))
    (plist-get response :data)))

(cl-defun zotero-lib-get-tags (&key type id key api-key)
  "Tags of all types matching a specific name.

  KEY is the search key. Keyword argument TYPE is \"user\" for
  your personal library, and \"group\" for the group libraries.
  Keyword argument ID is the ID of the personal or group library
  you want to access, e.g. the \"user ID\" or \"group ID\"."
  (let ((response (zotero-lib-retrieve :type type :id id :resource "tags" :key key :api-key api-key)))
    (plist-get response :data)))

(cl-defun zotero-lib-get-item-tags (&key type id key api-key)
  "Tags associated with a specific item.

KEY is the item key. Keyword argument TYPE is \"user\" for your
personal library, and \"group\" for the group libraries. Keyword
argument ID is the ID of the personal or group library you want
to access, e.g. the \"user ID\" or \"group ID\"."
  (let ((response (zotero-lib-retrieve :type type :id id :resource "item-tags" :key key :api-key api-key)))
    (plist-get response :data)))

(cl-defun zotero-lib-get-collection-tags (&key type id key api-key)
  "Tags within a specific collection in the library.

KEY is the collection key. Keyword argument TYPE is \"user\" for
your personal library, and \"group\" for the group libraries.
Keyword argument ID is the ID of the personal or group library
you want to access, e.g. the \"user ID\" or \"group ID\"."
  (let ((response (zotero-lib-retrieve :type type :id id :resource "collection-tags" :key key :api-key api-key)))
    (plist-get response :data)))

(cl-defun zotero-lib-get-items-tags (&key type id api-key)
  "All tags in the library, with the ability to filter based on the items.

Keyword argument TYPE is \"user\" for your personal library, and
\"group\" for the group libraries. Keyword argument ID is the ID of
the personal or group library you want to access, e.g. the \"user
ID\" or \"group ID\"."
  (let ((response (zotero-lib-retrieve :type type :id id :resource "items-tags" :api-key api-key)))
    (plist-get response :data)))

(cl-defun zotero-lib-get-items-top-tags (&key type id api-key)
  "Tags assigned to top-level items.
  Keyword argument TYPE is \"user\" for your personal library, and
  \"group\" for the group libraries. Keyword argument ID is the ID of
  the personal or group library you want to access, e.g. the \"user
  ID\" or \"group ID\"."
  (let ((response (zotero-lib-retrieve :type type :id id :resource "items-top-tags" :api-key api-key)))
    (plist-get response :data)))

(cl-defun zotero-lib-get-trash-items-tags (&key type id api-key)
  "Tags assigned to items in the trash.
  Keyword argument TYPE is \"user\" for your personal library, and
  \"group\" for the group libraries. Keyword argument ID is the ID of
  the personal or group library you want to access, e.g. the \"user
  ID\" or \"group ID\"."
  (let ((response (zotero-lib-retrieve :type type :id id :resource "trash-items-tags" :api-key api-key)))
    (plist-get response :data)))

(cl-defun zotero-lib-get-collection-items-tags (&key type id key api-key)
  "Tags assigned to items in a given collection.
  KEY is the collection key. Keyword argument TYPE is \"user\" for
  your personal library, and \"group\" for the group libraries.
  Keyword argument ID is the ID of the personal or group library
  you want to access, e.g. the \"user ID\" or \"group ID\"."
  (let ((response (zotero-lib-retrieve :type type :id id :resource "collection-items-tags" :key key :api-key api-key)))
    (plist-get response :data)))

(cl-defun zotero-lib-get-collection-items-top-tags (&key type id key api-key)
  "Tags assigned to top-level items in a given collection.
  KEY is the collection key. Keyword argument TYPE is \"user\" for
  your personal library, and \"group\" for the group libraries.
  Keyword argument ID is the ID of the personal or group library
  you want to access, e.g. the \"user ID\" or \"group ID\"."
  (let ((response (zotero-lib-retrieve :type type :id id :resource "collection-items-top-tags" :key key :api-key api-key)))
    (plist-get response :data)))

(cl-defun zotero-lib-get-publication-items-tags (&key type id api-key)
  "Tags assigned to items in My Publications.
  Keyword argument TYPE is \"user\" for your personal library, and
  \"group\" for the group libraries. Keyword argument ID is the ID of
  the personal or group library you want to access, e.g. the \"user
  ID\" or \"group ID\"."
  (let ((response (zotero-lib-retrieve :type type :id id :resource "publication-items-tags" :api-key api-key)))
    (plist-get response :data)))

(cl-defun zotero-lib-get-key (&key api-key)
  "Return the user id and privileges of the API-KEY."
  (let ((response (zotero-lib-retrieve :resource "keys" :key api-key)))
    (plist-get response :data)))

(cl-defun zotero-lib-get-groups (&key id api-key)
  "Return the groups the API-KEY has access to.
Argument ID is the ID of the personal library, e.g. the \"user ID\". All
groups are returned, including public groups the key owner
belongs to even if the key doesn't have explicit permissions for
them."
  (let ((response (zotero-lib-retrieve :id id :resource "groups" :api-key api-key)))
    (plist-get response :data)))

(cl-defun zotero-lib-get-group (&key id api-key)
  "Retrieve the group metadata.
Argument ID is the ID of the group library you want to access,
e.g. the \"group ID\"."
  (let ((response (zotero-lib-retrieve :type "group" :id id :api-key api-key)))
    (plist-get response :data)))

;; REVIEW: version?
(cl-defun zotero-lib-get-all-fulltext (&key type id api-key)
  "For each item with a full-text content VERSION greater than stored locally, get the item's full-text content.
  Keyword argument TYPE is \"user\" for your personal library, and
  \"group\" for the group libraries. Keyword argument ID is the ID of
  the personal or group library you want to access, e.g. the \"user
  ID\" or \"group ID\"."
  (let ((response (zotero-lib-retrieve :resource "all-fulltext" :type type :id id :api-key api-key)))
    (plist-get response :data)))

(cl-defun zotero-lib-get-item-fulltext (&key type id key api-key)
  "Get an item's full-text content.
  KEY is the item key. Keyword argument TYPE is \"user\" for your
  personal library, and \"group\" for the group libraries. Keyword
  argument ID is the ID of the personal or group library you want
  to access, e.g. the \"user ID\" or \"group ID\"."
  (let ((response (zotero-lib-retrieve :resource "item-fulltext" :type type :id id :key key :api-key api-key)))
    (plist-get response :data)))

(cl-defun zotero-lib-get-file (&key type id key api-key)
  "Get the raw content of a specific item in the library.
  KEY is the item key. Keyword argument TYPE is \"user\" for your
  personal library, and \"group\" for the group libraries. Keyword
  argument ID is the ID of the personal or group library you want
  to access, e.g. the \"user ID\" or \"group ID\"."
  (let ((response (zotero-lib-retrieve :resource "file" :type type :id id :key key :api-key api-key)))
    (plist-get response :data)))

(cl-defun zotero-lib-get-file-hash (&key type id key api-key)
  "Get the ETag header of a specific item in the library.
  KEY is the item key. Keyword argument TYPE is \"user\" for your
  personal library, and \"group\" for the group libraries. Keyword
  argument ID is the ID of the personal or group library you want
  to access, e.g. the \"user ID\" or \"group ID\"."
  (let* ((url (zotero-lib--endpoint :type type :id id :resource "file" :key key))
         (handle `(:url ,url :method "GET" :api-key ,api-key :api-version ,zotero-lib-api-version))
         (response (zotero-lib--dispatch handle)))
    (plist-get response :etag)))

(cl-defun zotero-lib-search (&key type id resource keys include-trashed query mode since api-key)
  "Search the library. Searches titles and individual creator
fields by default. Use the MODE argument to change the mode.
Default is \"titleCreatorYear\". To include full-text content,
use \"everything\".

Keyword argument TYPE is \"user\" for your personal library, and
\"group\" for the group libraries. Keyword argument ID is the ID
of the personal or group library you want to access, e.g. the
\"user ID\" or \"group ID\"."
  (let* ((items (s-join "," keys))
         (response (zotero-lib-retrieve :resource resource :type type :id id :q query :qmode mode :itemkey items :include-trashed (if include-trashed 1 0):api-key api-key)))
    (plist-get response :data)))

(cl-defun zotero-lib-search-itemtype (&key type id resource keys query mode since api-key)
  "Search the tags in library.

Use the MODE argument to change the mode. Default is
\"contains\". To perform a left-bound search, use \"startsWith\".
Keyword argument TYPE is \"user\" for your personal library, and
\"group\" for the group libraries. Keyword argument ID is the ID
of the personal or group library you want to access, e.g. the
\"user ID\" or \"group ID\"."
  (let* ((items (s-join "," keys))
         (response (zotero-lib-retrieve :resource resource :type type :id id :itemtype query :qmode mode :itemkey items :api-key api-key)))
    (plist-get response :data)))

(cl-defun zotero-lib-search-tag (&key type id resource keys query mode since api-key)
  "Search the tags in library.

Use the MODE argument to change the mode. Default is
\"contains\". To perform a left-bound search, use \"startsWith\".
Keyword argument TYPE is \"user\" for your personal library, and
\"group\" for the group libraries. Keyword argument ID is the ID
of the personal or group library you want to access, e.g. the
\"user ID\" or \"group ID\"."
  (let* ((items (s-join "," keys))
         (response (zotero-lib-retrieve :resource resource :type type :id id :tag query :qmode mode :itemkey items :api-key api-key)))
    (plist-get response :data)))

(cl-defun zotero-lib-download-file (&key file dir type id key api-key confirm)
  "A convenient wrapper around `zotero-lib-get-file'.
  Write an attachment to disk using the optional DIR and FILE. DIR
  is directory to start with if FILE is relative
  (does not start with slash or tilde). If DIR is nil, the current
  buffer’s value of ‘default-directory’ is used. If FILE is not
  supplied, a `zotero-lib-item' call is made to determine the
  attachment filename. If successful, the full path including the
  file name is returned.

  See also URL
  `https://www.zotero.org/support/dev/web_api/v3/file_upload#ii_download_the_existing_file'."
  (let* ((response (zotero-lib-retrieve :resource "file" :type type :id id :key key :api-key api-key))
         (data (plist-get response :data))
         (coding-system-for-write 'binary)
         (filename (or file
                       (let ((item (zotero-lib-get-item :type type :id id :key key :api-key api-key)))
                         (zotero-lib-plist-get* item :data :filename))))
         (full-filename (expand-file-name filename dir)))
    (write-region data nil full-filename nil nil nil confirm)
    ;; Check the ETag header of the response to make sure it matches
    ;; the attachment item's md5 value. If it doesn't, offer to
    ;; download the attachment item again.
    (let* ((attributes (zotero-lib-file-attributes full-filename))
           (etag (plist-get response :etag)))
      (if (equal etag (plist-get attributes :md5))
          full-filename
        (if (y-or-n-p (format "MD5 value doesn't match the response header. Retry? "))
            (zotero-lib-download-file :file file :dir dir :type type :id id :key key :api-key api-key)
          (warn "Item has the wrong hash. The latest version of
  the file may be available only via WebDAV, not via Zotero File
  Storage."))))))

(cl-defun zotero-lib-attachment-attributes (&key type id key api-key)
  "Get the attachment attributes."
  (let* ((item (zotero-lib-item :type type :id id :key key :api-key api-key))
         (data (plist-get item :data)))
    `(:filename ,(plist-get data :filename) :contenttype ,(plist-get data :contentType) :md5 ,(plist-get data :md5) :mtime ,(plist-get data :mtime))))

;;;; Item Type/Field Requests

(cl-defun zotero-lib-itemtypes (&key locale since)
  "Get all item types."
  (let* ((url (concat zotero-lib-base-url "/itemTypes"))
         (response (zotero-lib-retrieve :url url :locale (or locale zotero-lib-locale) :since since)))
    (plist-get response :data)))

(cl-defun zotero-lib-itemfields (&key locale since)
  "Get all item fields."
  (let* ((url (concat zotero-lib-base-url "/itemFields"))
         (response (zotero-lib-retrieve :url url :locale (or locale zotero-lib-locale) :since since)))
    (plist-get response :data)))

(cl-defun zotero-lib-itemtypefields (&key itemtype locale since)
  "Get all valid fields for an item type."
  (let* ((url (concat zotero-lib-base-url "/itemTypeFields"))
         (response (zotero-lib-retrieve :url url :itemtype itemtype :locale (or locale zotero-lib-locale) :since since)))
    (plist-get response :data)))

(cl-defun zotero-lib-itemtypecreatortypes (&key itemtype locale since)
  "Get the valid creator types for an item type."
  (let* ((url (concat zotero-lib-base-url "/itemTypeCreatorTypes"))
         (response (zotero-lib-retrieve :url url :itemtype itemtype :locale (or locale zotero-lib-locale) :since since)))
    (plist-get response :data)))

(cl-defun zotero-lib-creatorfields (&key locale since)
  "Get the localized creator fields."
  (let* ((url (concat zotero-lib-base-url "/creatorFields"))
         (response (zotero-lib-retrieve :url url :locale (or locale zotero-lib-locale) :since since)))
    (plist-get response :data)))

(defun zotero-lib-attachment-linkmodes ()
  "Return the linkmode types."
  '("imported_file" "imported_url" "linked_file" "linked_url"))

;;;; Template Requests

(defun zotero-lib-collection-template ()
  "Return the template for a new collections."
  '(:name "" :parentCollection :json-false :relations :json-empty))

(defun zotero-lib-item-template (itemtype)
  "Get the template for a new item of ITEMTYPE."
  (let* ((url (concat zotero-lib-base-url "/items/new"))
         (response (zotero-lib-retrieve :url url :itemtype itemtype)))
    (plist-get response :data)))

(defun zotero-lib-attachment-template (linkmode)
  "Get a template for a new attachment item.
  LINKMODE should be one of:
  \"imported_file\",
  \"imported_url\",
  \"linked_file\", or
  \"linked_url\".

  See also URL
  `https://www.zotero.org/support/dev/web_api/v3/file_upload#i_get_attachment_item_template'."
  (let* ((url (concat zotero-lib-base-url "/items/new"))
         (response (zotero-lib-retrieve :url url :itemtype "attachment" :linkmode linkmode)))
    (plist-get response :data)))

;;;; Write Requests

(cl-defun zotero-lib-create-item (object &key type id api-key)
  "Create an item in the library.

  OBJECT is a plist of the new item. When creating a new item,
  first get an empty property list for the item type with
  `zotero-lib-template' (or use a cached version of the template).
  Then modify it and resubmit it to the server in an array. Note
  that when uploading the full property list, only the `:data'
  property is processed. All other properties are ignored.

  Keyword argument TYPE is \"user\" for your personal library, and
  \"group\" for the group libraries. Keyword argument ID is the ID of
  the personal or group library you want to access, e.g. the \"user
  ID\" or \"group ID\"."
  (let* ((write-token (zotero-lib--write-token))
         (json (zotero-lib-encode-object  object))
         (response (zotero-lib-submit :method "POST" :resource "items" :type type :id id :data json :content-type "application/json" :expect "" :write-token write-token :api-key api-key)))
    (plist-get response :data)))

(cl-defun zotero-lib-create-items (&key type id objects api-key)
  "Create multiple items.
  Up to 50 items can be created in a single request.

  Each object should return a plist and may be:
  a cons cell (containing a propery list),
  a buffer (read one Lisp expression from the beginning),
  a function (call it with no arguments),
  a file (read one Lisp expression from the beginning),
  a string (takes text from string, starting at the beginning).

  Keyword argument TYPE is \"user\" for your
  personal library, and \"group\" for the group libraries. Keyword
  argument ID is the ID of the personal or group library you want
  to access, e.g. the \"user ID\" or \"group ID\"."
  ;; Apparently for data > 1024 bytes CURL automatically sets "Expect:
  ;; 100-continue". The client expects the server to only fetch the
  ;; header and then send a "100" return code to get the rest of the
  ;; data. However, passing an Expect header is unsupported and will
  ;; result in a 417 Expectation Failed response. Setting "Expect: "
  ;; explicitly disables this automatic behaviour.
  (let* ((write-token (zotero-lib--write-token))
         (json (zotero-lib-encode-object objects))
         (response (zotero-lib-submit :method "POST" :resource "items" :type type :id id :data json :content-type "application/json" :expect "" :write-token write-token :api-key api-key)))
    (plist-get response :data)))

(cl-defun zotero-lib-update-item (object &key type id key version api-key)
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

KEY is the item key. Keyword argument LIBRARY is user' for your
personal library, and \"group\" for the group libraries. Keyword
argument ID is the ID of the personal or group library you want
to access, e.g. the \"user ID\" or \"group ID\"."
  (let* ((json (zotero-lib-encode-object object))
         (response (zotero-lib-submit :method "PUT" :resource "item" :type type :id id :key key :data json :content-type "application/json" :version version :expect "" :api-key api-key)))
    (plist-get response :data)))

(cl-defun zotero-lib-update-items (&key type id objects version api-key)
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

Keyword argument LIBRARY is user' for your personal library, and
\"group\" for the group libraries. Keyword argument ID is the ID of
the personal or group library you want to access, e.g. the \"user
ID\" or \"group ID\"."
  (let* ((json (zotero-lib-encode-object objects))
         (response (zotero-lib-submit :method "POST" :resource "items" :type type :id id :data json :content-type "application/json" :version version :expect "" :api-key api-key)))
    (plist-get response :data)))

(cl-defun zotero-lib-delete-item (&key type id key version api-key)
  "Delete an item.

KEY is the item key. VERSION is the last known item version.
Keyword argument LIBRARY is user' for your personal library, and
\"group\" for the group libraries. Keyword argument ID is the ID of
the personal or group library you want to access, e.g. the \"user
ID\" or \"group ID\"."
  (let ((response (zotero-lib-submit :method "DELETE" :resource "item" :type type :id id :key key :version version :api-key api-key)))
    (plist-get response :data)))

(cl-defun zotero-lib-delete-items (&key type id keys version api-key)
  "Delete multiple items.
Up to 50 items can be deleted in a single request.

KEYS are the item key. VERSION is the last known item version.
Keyword argument LIBRARY is user' for your personal library, and
\"group\" for the group libraries. Keyword argument ID is the ID of
the personal or group library you want to access, e.g. the \"user
ID\" or \"group ID\"."
  (cond
   ((eq (length keys) 1)
    (let (key (car keys))
      (zotero-lib-delete-item :type type :id id :key key :version version :api-key api-key)))
   ((> (length keys) 50)
    (user-error "Up to 50 items can be deleted in a single request."))
   (t
    (let* ((items (s-join "," keys))
           (response (zotero-lib-submit :method "DELETE" :resource "items" :type type :id id :itemkey items :version version :api-key api-key)))
      (plist-get response :data)))))

(cl-defun zotero-lib-create-collection (object &key type id version api-key)
  "Create a collection.

OBJECT is a plist of the new collection. VERSION is the
last-known library version. Keyword argument LIBRARY is user'
for your personal library, and \"group\" for the group libraries.
Keyword argument ID is the ID of the personal or group library
you want to access, e.g. the \"user ID\" or \"group ID\"."
  (let* ((json (zotero-lib-encode-object object))
         (response (zotero-lib-submit :method "POST" :resource "collections" :type type :id id :data json :content-type "application/json" :version version :expect "" :api-key api-key)))
    (plist-get response :data)))

(cl-defun zotero-lib-update-collection (object &key type id key version api-key)
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

KEY is the collection key. Keyword argument LIBRARY is user' for your
personal library, and \"group\" for the group libraries. Keyword
argument ID is the ID of the personal or group library you want
to access, e.g. the \"user ID\" or \"group ID\"."
  (let* ((json (zotero-lib-encode-object object))
         (response (zotero-lib-submit :method "PUT" :resource "collection" :type type :id id :key key :data json :content-type "application/json" :version version :expect "" :api-key api-key)))
    (plist-get response :data)))

(cl-defun zotero-lib-update-collections (&key type id objects version api-key)
  "Update existing collections in the library.
Up to 50 collections can be updated in a single request. Note
that any properties not specified will be left untouched on the
server. To erase an existing property, include it with an empty
string or false as the value.

Keyword argument LIBRARY is user' for your personal library, and
\"group\" for the group libraries. Keyword argument ID is the ID of
the personal or group library you want to access, e.g. the \"user
ID\" or \"group ID\"."
  (let* ((json (zotero-lib-encode-object objects))
         (response (zotero-lib-submit :method "POST" :resource "collections" :type type :id id :data json :content-type "application/json" :version version :expect "" :api-key api-key)))
    (plist-get response :data)))

(cl-defun zotero-lib-delete-collection (&key type id key version api-key)
  "Delete a collection.

KEY is the collection key. VERSION is the collection's current
version number. Keyword argument LIBRARY is user' for your
personal library, and \"group\" for the group libraries. Keyword
argument ID is the ID of the personal or group library you want
to access, e.g. the \"user ID\" or \"group ID\"."
  (let ((response (zotero-lib-submit :method "DELETE" :resource "collection" :type type :id id :key key :version version :api-key api-key)))
    (plist-get response :data)))

(cl-defun zotero-lib-delete-collections (&key type id keys version api-key)
  "Delete multiple collections.
KEYS are the collection keys. Up to 50 collections can be deleted in a single request.

VERSION is the last-known library version. Keyword argument
LIBRARY is user' for your personal library, and \"group\" for the
group libraries. Keyword argument ID is the ID of the personal
or group library you want to access, e.g. the \"user ID\" or
\"group ID\"."
  (cond
   ((eq (length keys) 1)
    (let (key (car keys))
      (zotero-lib-delete-collection :type type :id id :key key :version version :api-key api-key)))
   ((> (length keys) 50)
    (user-error "Up to 50 collections can be deleted in a single request."))
   (t
    (let* ((collections (s-join "," keys))
           (response (zotero-lib-submit :method "DELETE" :resource "collections" :type type :id id :collectionkey collections :version version :api-key api-key)))
      (plist-get response :data)))))

(cl-defun zotero-lib-create-search (object &key type id api-key)
  "Create a saved search.
OBJECT is a plist of the new search. Keyword argument LIBRARY is
user' for your personal library, and \"group\" for the group
libraries. Keyword argument ID is the ID of the personal or
group library you want to access, e.g. the \"user ID\" or \"group
ID\"."
  (let* ((json (zotero-lib-encode-object object))
         (response (zotero-lib-submit :method "POST" :resource "searches" :type type :id id :data json :content-type "application/json" :expect "" :api-key api-key)))
    (plist-get response :data)))

(cl-defun zotero-lib-update-searches (&key type id objects version api-key)
  "Update existing searches in the library.
Up to 50 searches can be updated in a single request. Note
that any properties not specified will be left untouched on the
server. To erase an existing property, include it with an empty
string or false as the value.

Keyword argument LIBRARY is user' for your personal library, and
\"group\" for the group libraries. Keyword argument ID is the ID of
the personal or group library you want to access, e.g. the \"user
ID\" or \"group ID\"."
  (let* ((json (zotero-lib-encode-object objects))
         (response (zotero-lib-submit :method "POST" :resource "searches" :type type :id id :data json :content-type "application/json" :expect "" :api-key api-key)))
    (plist-get response :data)))

(cl-defun zotero-lib-delete-searches (&key type id keys version api-key)
  "Delete multiple searches.
Up to 50 searches can be deleted in a single request.

KEYS are the search keys. VERSION is the last-known library
version. Keyword argument LIBRARY is user' for your personal
library, and \"group\" for the group libraries. Keyword argument ID
is the ID of the personal or group library you want to access,
e.g. the \"user ID\" or \"group ID\"."
  (cond
   ((> (length keys) 50)
    (user-error "Up to 50 searches can be deleted in a single request."))
   (t
    (let* ((searches (s-join "," keys))
           (response (zotero-lib-submit :method "DELETE" :resource "searches" :type type :id id :searchkey searches :version version :api-key api-key)))
      (plist-get :data)))))

(cl-defun zotero-lib-delete-tags (&key type id tags version api-key)
  "Delete multiple tags.
Up to 50 tags can be deleted in a single request.

TAGS are the tag strings. VERSION is the last-known library
version. Keyword argument LIBRARY is user' for your personal
library, and \"group\" for the group libraries. Keyword argument ID
is the ID of the personal or group library you want to access,
e.g. the \"user ID\" or \"group ID\"."
  (cond
   ((> (length keys) 50)
    (user-error "Up to 50 tags can be deleted in a single request."))
   (t
    (let* ((url-encoded-tags (mapcar #'url-hexify-string tags))
           (value (s-join "||" url-encoded-tags))
           (response (zotero-lib-submit :method "DELETE" :resource "tags" :type type :id id :tag value :version version :api-key api-key)))
      (plist-get response :data)))))

(cl-defun zotero-lib-delete-key (&key api-key)
  "Delete the API key."
  (let ((response (zotero-lib-submit :method "DELETE" :resource "keys" :key api-key)))
    (plist-get response :data)))

(cl-defun zotero-lib-set-item-fulltext (object &key type id key api-key)
  "Set an item's full-text content.
Return t if the full-text content was updated
successfully, else return nil.

OBJECT should be a a plist containing three props:
- `:content': the full-text content, and either
- `:indexedChars' and `:totalChars' for text documents, or
- `:indexedPages' and `:totalPages' for PDFs.

KEY is the item key. Keyword argument TYPE is \"user\" for your
personal library, and \"group\" for the group libraries. Keyword
argument ID is the ID of the personal or group library you want
to access, e.g. the \"user ID\" or \"group ID\"."
  (let* ((json (zotero-lib-encode-object object))
         (response (zotero-lib-submit :method "PUT" :resource "item-fulltext" :type type :id id :key key :data json :expect "" :api-key api-key))
         (status-code (plist-get response :status-code)))
    (if (eq status-code 204) t nil)))

;;;; File Uploads

(cl-defun zotero-lib-authorize-upload (&key type id key filename filesize md5 mtime hash api-key)
  "Get upload authorisation for a file and associate it with an item.

If authorisation was successful, return a plist with the props
`:url', `:contentType', `:prefix', `:suffix', and `:uploadKey',
to be passed to `zotero-lib-upload-file'. If the file already
exists on the server, return a plist with the prop `:exists'.

For existing attachments, use If-Match: <hash> in place of
If-None-Match: *, where <hash> is the previous MD5 hash of the
file (as provided in the ETag header when downloading it).

See also URL `https://www.zotero.org/support/dev/web_api/v3/file_upload#get_upload_authorization'."
  (let* ((data (url-build-query-string `(("filename" ,filename)
                                         ("filesize" ,filesize)
                                         ("md5" ,md5)
                                         ("mtime" ,mtime))))
         (response (if hash
                       (zotero-lib-submit :method "POST" :resource "file" :type type :id id :key key :data data :content-type "application/x-www-form-urlencoded" :api-key api-key :if-match hash)
                     (zotero-lib-submit :method "POST" :resource "file" :type type :id id :key key :data data :content-type "application/x-www-form-urlencoded" :api-key api-key :if-none-match "*"))))
    (plist-get response :data)))

(defun zotero-lib-upload-file (file url content-type prefix suffix)
  "Upload FILE.

See also URL `https://www.zotero.org/support/dev/web_api/v3/file_upload#i_post_file'

Concatenate prefix, the file contents, and suffix and POST to url
with the Content-Type header set to contentType.

prefix and suffix are strings containing multipart/form-data. In
some environments, it may be easier to work directly with the form
parameters. Add params=1 to the upload authorization request above
to retrieve the individual parameters in a params array, which will
replace contentType, prefix, and suffix.
"
  (let ((content (with-temp-buffer
                   (insert-file-contents-literally file)
                   (buffer-string))))
    (request url
      :headers `(("Content-Type" . ,content-type))
      :data (concat prefix content suffix)
      :parser #'buffer-string
      :sync t)))

(cl-defun zotero-lib-register-upload (&key type id key uploadkey hash api-key)
  "Register upload. Return t if the upload was registered
successfully, else return nil.

See also URL `https://www.zotero.org/support/dev/web_api/v3/file_upload#ii_register_upload'."
  (let* ((data (url-build-query-string `(("upload" ,uploadkey))))
         ;; For existing attachments, use If-Match: <hash>, where <hash> is
         ;; the previous MD5 hash of the file, provided as the md5 property
         ;; in the attachment item.
         (response (if hash
                       (zotero-lib-submit :method "POST" :resource "file" :type type :id id :key key :data data :content-type "application/x-www-form-urlencoded" :api-key api-key :if-match hash)
                     (zotero-lib-submit :method "POST" :resource "file" :type type :id id :key key :data data :content-type "application/x-www-form-urlencoded" :api-key api-key :if-none-match "*")))
         (status-code (plist-get response :status-code)))
    (if (eq status-code 204) t nil)))

(cl-defun zotero-lib-upload-attachment (&key type id key file hash api-key)
  "A convenient wrapper to authorize, upload and register an attachment.
Return t if success, or nil if failed."
  (message "Authorize upload...")
  (let* ((attributes (zotero-lib-file-attributes file))
         (filename (plist-get attributes :filename))
         (filesize (plist-get attributes :filesize))
         (md5 (plist-get attributes :md5))
         (mtime (plist-get attributes :mtime))
         (data (zotero-lib-authorize-upload :type type :id id :key key :api-key api-key :filename filename :filesize filesize :md5 md5 :mtime mtime :hash hash)))
    (if (eq (plist-get data :exists) 1)
        (progn
          (message "Authorize upload...already exists")
          ;; Success: return t
          t)
      (message "Authorize upload...done")
      (message "Upload file...")
      (let* ((url (plist-get data :url))
             (content-type (plist-get data :contentType))
             (prefix (plist-get data :prefix))
             (suffix (plist-get data :suffix))
             (uploadkey (plist-get data :uploadKey))
             (response (zotero-lib-upload-file file url content-type prefix suffix)))
        ;; A status-code 201 means the file was successfully uploaded.
        (if (eq (request-response-status-code response) 201)
            (progn
              (message "Upload file...done")
              (message "Register upload...")
              (let ((registered-p (zotero-lib-register-upload :type type :id id :key key :uploadkey uploadkey :hash hash :api-key api-key)))
                (if registered-p
                    (progn
                      (message "Register upload...done")
                      ;; Success: return t
                      t)
                  (message "Register upload...failed")
                  ;; Failed: return nil
                  nil)))
          (message "Uploading file...failed")
          ;; Failed: return nil
          nil)))))

(provide 'zotero-lib)

;;; zotero-lib.el ends here
