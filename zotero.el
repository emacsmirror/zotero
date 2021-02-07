;;; zotero.el --- Library for the Zotero API  -*- lexical-binding: t; -*-

;; Author: Folkert van der Beek <folkertvanderbeek@gmail.com>
;; Created: 2020-03-27
;; Version: 0.1
;; Keywords: zotero, hypermedia
;; Package-Requires: ((emacs "27.1") (ht "2.2") (oauth "1.0.4") (s "1.12.0"))
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

;; Emacs 27 is required by zotero-cache.el because of ISO 8601 time parsing.

;;; Code:

;;;; Requirements

(require 'json)
(require 'mailcap)
(require 's)
(require 'url)
(require 'zotero-auth)
(require 'zotero-json)
(require 'zotero-lib)

;;;; Variables

(defconst zotero-directory
  (or (and load-file-name
           (file-name-directory load-file-name))
      default-directory)
  "The directory from where this library was first loaded.")

(defconst zotero-version (eval-when-compile
                           (require 'lisp-mnt)
                           (lm-version (or load-file-name buffer-file-name)))
  "Version of emacs-zotero currently loaded.")

(defconst zotero-base-url "https://api.zotero.org")

(defconst zotero-api-version 3
  "API version. Version 3 is currently the default and recommended version.")

(defconst zotero-user-agent (concat "emacs-zotero/" zotero-version " (https://gitlab.com/fvdbeek/emacs-zotero/; mailto:folkertvanderbeek@gmail.com)")
  "User-Agent header that properly identifies emacs-zotero.")

(defvar zotero-rate-limit nil
  "The time in seconds (since the Unix epoch) that the rate of requests is limited by the \"Backoff\" or \"Retry-After\" header.
The time is formatted as a \"Lisp timestamp\".")

(defvar zotero-status-line-regexp
  "\\([^ ]+\\) \\([[:digit:]]\\{3\\}\\) \\(.*\\)"
  "A regular expression matching the status-line of a response message.

A status-line consists of the protocol version followed by a
numeric status code and its associated textual phrase, with each
element separated by space characters.")

(defvar zotero-fieldname-regexp
  (regexp-opt-charset (nconc (number-sequence 33 57) (number-sequence 59 126)))
  "A regular expression matching response header field names.

A field name is composed of printable US-ASCII characters (i.e.,
characters that have values between 33 and 126), except colon.")

(defvar zotero-fieldvalue-regexp
  (regexp-opt-charset (nconc '(9) (number-sequence 32 126)))
  "A regular expression matching response header field bodies.

A field value may be composed of printable US-ASCII characters as
well as the space (ASCII value 32) and horizontal tab (ASCII
value 9) characters.")

(defvar zotero-header-regexp
  (concat "^\\(" zotero-fieldname-regexp "+\\)"
          ": \\(" zotero-fieldvalue-regexp "*\\)$")
  "A regular expression matching response header fields.

Header fields are lines beginning with a field name, followed by
a colon, followed by a field value.")

(defvar zotero-link-regexp "<\\([^>]*\\)>; rel=\"\\([[:word:]]+\\)\""
  "A regular expression matching the value of the link header.")

;;;; Customization

(defgroup zotero nil
  "Emacs client for Zotero"
  :group 'external)

(defcustom zotero-timeout 30
  "Default timeout in seconds.
nil means no timeout."
  :group 'zotero-lib
  :type '(choice (integer :tag "Timeout seconds")
                 (boolean :tag "No timeout" nil)))

(defcustom zotero-locale "en-US"
  "Locale used in translations.
See the available CSL locales. Note that some styles use a fixed
locale and cannot be localized."
  :group 'zotero
  :type 'string
  :link '(url-link "https://github.com/citation-style-language/locales"))

(defcustom zotero-sort "dateModified"
  "The name of the field by which entries are sorted."
  :group 'zotero
  :type '(choice (const "dateAdded")
                 (const "dateModified")
                 (const "title")
                 (const "creator")
                 (const "publisher")
                 (const "title")
                 (const "publicationTitle")
                 (const "journalAbbreviation")
                 (const "language")
                 (const "accessDate")
                 (const "libraryCatalog")
                 (const "callNumber")
                 (const "rights")
                 (const "addedBy")
                 (const :tag "numItems (tags)" "numItems")))

(defcustom zotero-limit 100
  "The maximum number of results to return with a single request.
The number should be an integer between 1 and 100."
  :group 'zotero
  :type '(integer :validate
		  (lambda (widget)
                    (let ((int (widget-value widget)))
		      (unless (and (< int 1)
                                   (> int 100))
		        (widget-put widget :error
				    "Invalid value: must be an integer between 1 and 100.")
		        widget)))))

(defcustom zotero-linkwrap nil
  "Non-nil means to return URLs and DOIs as links."
  :group 'zotero
  :type 'boolean)

;;;; Structures
(cl-defstruct (zotero-request (:constructor zotero-request-create)
                              (:copier nil))
  "Handle for Zotero requests."
  (method nil
          :read-only t
          :documentation "HTTP request method: GET/HEAD/POST/PUT/PATCH/DELETE"
          :type 'string)
  (url nil
       :read-only nil
       :documentation "URL"
       :type 'string)
  (headers nil
           :read-only nil
           :documentation "An alist of extra headers. The CAR of
           each cons cell is the field name and the CDR is the
           field value. HEADERS has the form:
((\"Content-Type\" . \"application/x-www-form-urlencoded\"))"
           :type 'cons)
  (params nil
          :read-only nil
          :documentation "An alist of the query string that is
          part of the URL. The CAR of each cons cell is the
          parameter, CAR of the CDR is the value. PARAMS has the
          form:
((key1 val1)
 (key2 val2)
 (key3 val1 val2)
 (key4)
 (key5 \"\"))"
          :type 'cons)
  (data nil
        :read-only nil
        :documentation "Data to be sent to the server."
        :type 'string))

(cl-defstruct (zotero-response (:constructor zotero-response-create)
                               (:copier nil))
  "Relevant result of Zotero requests."
  (status-code nil
               :read-only nil
               :documentation "Status code of the response.")
  (headers nil
           :read-only nil
           :documentation "An alist of extra headers. The CAR of
           each cons cell is the field name and the CDR is the
           field value. HEADERS has the form:
((\"Content-Type\" . \"application/x-www-form-urlencoded\"))")
  (version nil
           :read-only nil
           :documentation "Current library version, as returned
           by the \"Last-Modified-Version\" header in the
           response.")
  (etag nil
        :read-only nil
        :documentation "Attachment item's md5 value, as returned
        by the \"ETag\" header in the response.")
  (data nil
        :read-only nil
        :documentation "Data returned in the response."))

;;;;; Private functions

(defun zotero--get-statusline (buffer)
  "Return the status line from a response BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (buffer-substring (point-min) (line-end-position))))

(defun zotero--get-headers (buffer)
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

(defun zotero--get-body (buffer)
  "Return the body from a response BUFFER."
  (with-current-buffer buffer
    ;; Move beyond blank line at end of headers.
    (goto-char (point-min))
    (while (progn
             (forward-line 1)
             (not (looking-at "^\r?\n"))))
    (forward-line 1)
    (buffer-substring (point) (point-max))))

(defun zotero--parse-headers (string)
  "Return an alist with the parsed response headers from STRING."
  (let ((pos 0)
        (headers ()))
    (while (string-match zotero-header-regexp string pos)
      (let ((name (match-string 1 string))
            (value (match-string 2 string)))
        (push (cons name value) headers)
        (setq pos (match-end 0))))
    (nreverse headers)))

(defun zotero--parse-links (string)
  "Return an alist with link relations in STRING.

  STRING is the HTTP \"Link\" header. The alist consists of the
  `alternate', `last', `next', `prev', and `first' links."
  (when string
    (let ((pos 0)
          (links ()))
      (while (string-match zotero-link-regexp string pos)
        (let ((rel (match-string 2 string))
              (url (match-string 1 string)))
          (push (cons rel url) links))
        (setq pos (match-end 0)))
      (nreverse links))))

(defun zotero--statusline (buffer)
  "Return an alist of the status-line of response BUFFER.

  The alist consists of the protocol version `http-version', a numeric status code `status-code' and its associated textual phrase `reason-phrase'."
  (when-let ((string (zotero--get-statusline buffer)))
    (string-match zotero-status-line-regexp string)
    `(("http-version"  . ,(match-string 1 string))
      ("status-code"   . ,(string-to-number (match-string 2 string)))
      ("reason-phrase" . ,(match-string 3 string)))))

(defun zotero--headers (buffer)
  "Return an alist with the headers from a response BUFFER."
  (when-let ((header (zotero--get-headers buffer)))
    (zotero--parse-headers header)))

(defun zotero--build-query-string (query)
  "Build a query-string.

Given a QUERY in the form:
\((key1 val1)
  (key2 val1 val2)
  (key3)
  (key4 ""))

\(This is the same format as produced by
`zotero--parse-query-string')

This will return a string \"key1=val1&key2=val1,val2&key3&key4\".
Keys and values may be strings, numbers or symbols.

One difference with `url-build-query-string' is that multiple
values in a field are comma separated instead of added as
separate fields with the same key."
  (mapconcat (lambda (field)
               (let* ((field (mapcar (lambda (elt) (url-hexify-string (format "%s" elt))) field))
                      (key (car field))
                      (val (mapconcat #'identity (cdr field) ",")))
                 (if (string-empty-p val)
                     key
                   (concat key "=" val))))
             query "&"))

(defun zotero--parse-query-string (query)
  "Parse a query-string.

Given a QUERY in the form:
\"key1=val1&key2=val1,val2&key3&key4=\"

\(This is the same format as produced by
`zotero--build-query-string')

This will return an alist
 ((\"key1\" \"val1\")
  (\"key2\" \"val1\" \"val2\")
  (\"key3\")
  (\"key4\"))."
  (let ((fields (split-string query "&")))
    (mapcar (lambda (field)
              (let* ((_ (string-match "\\([^=]+\\)=?\\(.*\\)" field))
                     (key (match-string 1 field))
                     (val (match-string 2 field))
                     (vals (unless (or (string-empty-p val)) (split-string val ","))))
                (cons key vals)))
            fields)))

(defun zotero--data (response result)
  "Concatenate the data in RESPONSE and RESULT."
  (let* ((content-type (zotero--content-type response))
         (data (if (string-match-p "application/json.*" content-type)
                   (zotero-json-read-object (zotero-response-data response))
                 (zotero-response-data response))))
    (if result (vconcat result data) data)))

(defun zotero--backoff (response)
  "Return the value of the \"Backoff\" headers in RESPONSE."
  (when-let ((headers (zotero-response-headers response))
             (value (cdr (assoc "Backoff" headers))))
    (string-to-number value)))

(defun zotero--content-type (response)
  "Return the value of the \"Content-Type\" headers in RESPONSE."
  (when-let ((headers (zotero-response-headers response))
             (value (cdr (assoc "Content-Type" headers))))
    value))

(defun zotero--etag (response)
  "Return the value of the \"ETag\" headers in RESPONSE."
  (when-let ((headers (zotero-response-headers response))
             (value (cdr (assoc "ETag" headers))))
    (substring value 1 -1))) ; remove the quotes

(defun zotero--last-modified-version (response)
  "Return the value of the \"Last-Modified-Version\" headers in RESPONSE."
  (when-let ((headers (zotero-response-headers response))
             (value (cdr (assoc "Last-Modified-Version" headers))))
    (string-to-number value)))

(defun zotero--next-url (response)
  "Return the next pagination link of the \"Link\" headers in RESPONSE."
  (when-let ((header (zotero-response-headers response))
             (links (cdr (assoc "Links" header)))
             (next (cdr (assoc "next" (zotero--parse-links links)))))
    next))

(defun zotero--retry-after (response)
  "Return the value of the \"Retry-After\" headers in RESPONSE."
  (when-let ((headers (zotero-response-headers response))
             (value (cdr (assoc "Retry-After" headers))))
    (string-to-number value)))

(defun zotero--total-results (response)
  "Return the value of the \"Total-Results\" headers in RESPONSE."
  (when-let ((headers (zotero-response-headers response))
             (value (cdr (assoc "Total-Results" headers))))
    (string-to-number value)))

(defun zotero--write-token ()
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
    (dotimes (_ 32 token)
      (let ((char (elt characters (random 36))))
        (setq token (concat token (string char)))))))

(defun zotero-handle-response ()
  "Handle response returned by `zotero--retrieve'.

Return a `zotero-response' structure."
  (let ((buffer (current-buffer)))
    (unwind-protect
        (progn
          (let* ((statusline (zotero--statusline buffer))
                 (headers (zotero--headers buffer))
                 (data (zotero--get-body buffer))
                 (status-code (cdr (assoc "status-code" statusline))))
            (zotero-response-create :status-code status-code
                                    :headers headers
                                    :data data)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun zotero--retrieve (request)
  "Retrieve REQUEST from Zotero."
  (let* ((path (zotero-request-url request))
         (query (zotero--build-query-string (zotero-request-params request)))
         (url (concat path "?" query))
         (url-request-method (zotero-request-method request))
         (url-request-data (zotero-request-data request))
         (url-request-extra-headers (zotero-request-headers request)))
    (with-current-buffer (url-retrieve-synchronously url nil nil zotero-timeout)
      (set-buffer-multibyte t) ; Necessary to handle non-ASCII characters
      (funcall #'zotero-handle-response))))

(defun zotero-dispatch (request &optional result)
  "Return the response of REQUEST to the Zotero API.

This is a recursive function that dispatches REQUEST to
`zotero--retrieve' and decides what to do next based on the
response. If the response is paginated the data is concatenated
and passed as optional argument RESULT."
  ;; If the API servers are overloaded, the API may include a "Backoff:
  ;; <seconds>" HTTP header in responses, indicating that the client should
  ;; perform the minimum number of requests necessary to maintain data
  ;; consistency and then refrain from making further requests for the number of
  ;; seconds indicated. Backoff can be included in any response, including
  ;; successful ones.
  (when zotero-rate-limit
    (let ((seconds (time-to-seconds (time-subtract zotero-rate-limit (current-time)))))
      (if (natnump seconds)
          (sleep-for seconds)
        (setq zotero-rate-limit nil))))
  (let* ((response (zotero--retrieve request))
         (status-code (zotero-response-status-code response)))
    (if-let ((seconds (zotero--backoff response)))
        (setq zotero-rate-limit (time-add (current-time) (seconds-to-time seconds)))
      (setq zotero-rate-limit nil))
    (pcase status-code
      ;; OK
      (200
       ;; If the response is paginated, it will contain a "Links" header with
       ;; link relations. Then, `zotero-dispatch' is called again with a
       ;; request to the next url and the concatenated data.
       (let ((new-result (zotero--data response result)))
         (if-let ((next-url (zotero--next-url response))
                  (params (zotero--parse-query-string (url-path-and-query (url-generic-parse-url next-url)))))
             (progn
               (setf (zotero-request-params request) params)
               (zotero-dispatch request result))
           (zotero-response-create :status-code status-code
                                   :headers (zotero-response-headers response)
                                   :version (zotero--last-modified-version response)
                                   :etag (zotero--etag response)
                                   :data new-result))))
      ;; No Content
      (204
       (zotero-response-create :status-code status-code
                               :version (zotero--last-modified-version response)
                               :data (zotero-response-data response)))
      ;; REVIEW: The function `url-http-parse-headers' already has support for
      ;; caching, and a 304 status code is never returned but implicitly redirected
      ;; to a cached version. So this case probably could be removed.
      ;; Not Modified
      (304
       (zotero-response-create :status-code status-code
                               :version (zotero--last-modified-version response)
                               :data (zotero-response-data response)))
      ;; Bad request
      (400
       (user-error (zotero-request-data response)))
      ;; Forbidden
      (403
       (pcase (zotero-response-data response)
         ("Invalid key"
          ;; Authorize and return a new handle, but no data
          (if (y-or-n-p (format "Invalid API key. Authorize Zotero and retry? "))
              (when-let ((token (zotero-auth-authorize))
                         (api-key (zotero-auth-api-key token))
                         (headers (zotero-request-headers request)))
                (setf (alist-get "Zotero-API-Key" headers nil nil #'equal) api-key)
                (zotero-dispatch request))
            (user-error "Invalid API key")))
         ("Forbidden"
          ;; Offer to review the privileges
          (if (y-or-n-p (format "Insufficient privileges. Ask a WWW browser to visit the Zotero settings and retry? "))
              (let* ((headers (zotero-request-headers request))
                     (api-key (alist-get "Zotero-API-Key" headers nil nil #'equal))
                     (url (concat "https://www.zotero.org/settings/keys/edit/" api-key)))
                (browse-url url)
                (read-string "Press enter when you have changed the privileges:")
                (zotero-dispatch request))
            (user-error "Insufficient privileges")))
         (message
          (user-error "403 %s" message))))
      (404
       (user-error "404 Not found"))
      (405
       (user-error "405 Method Not Allowed: invalid request"))
      (409
       (user-error "409 Conflict: the target library is locked"))
      (412
       (user-error "412 Precondition Failed: the file has changed
       remotely since retrieval (i.e., the provided ETag no
       longer matches). Conflict resolution is left to the
       client"))
      (413
       (let ((message (zotero-response-data response)))
         (user-error "413 Request Entity Too Large: %s" message)))
      (417
       (user-error "417 Expectation Failed: passing an \"Expect\"
       header is unsupported"))
      (428
       (user-error "428 Precondition Required: the \"If-Match\" or
       \"If-None-Match\" header was not provided"))
      (429
       ;; If a client has made too many requests within a given time
       ;; period, the API may return 429 Too Many Requests with a
       ;; "Retry-After: <seconds>" header. Clients receiving a 429 should
       ;; wait the number of seconds indicated in the header before
       ;; retrying the request.
       (let ((seconds (zotero--retry-after response)))
         (message "429 Too Many Requests: trying again after %d
         seconds as specified in the \"Retry-After\" header." seconds)
         (sleep-for seconds)
         (zotero-dispatch request)))
      (500
       (user-error "500 Internal Server Error"))
      (503
       ;; A "Retry-After" header can also be included with 503 Service
       ;; Unavailable responses when the server is undergoing maintenance.
       (if-let ((seconds (zotero--retry-after response)))
           (progn
             (message "503 Service Unavailable: trying again after %d
         seconds as specified in the \"Retry-After\" header." seconds)
             (sleep-for seconds)
             (zotero-dispatch request))
         (user-error "503 Service Unavailable"))))))

(cl-defun zotero--url (resource &optional key &key type id host)
  "Return the url from which the Zotero can access RESOURCE.

RESOURCE is one of:
  - \"collections\": collections in the library
  - \"collections-top\": top-level collections in the library
  - \"collection\": a specific collection in the library
  - \"subcollections\": subcollections within a specific collection in the library
  - \"items\": all items in the library, excluding trashed items
  - \"items-top\": top-level items in the library, excluding trashed items
  - \"trash-items\": items in the trash
  - \"item\": a specific item in the library
  - \"item-children\": child items under a specific item
  - \"publication-items\": items in My Publications
  - \"collection-items\": items within a specific collection in the library
  - \"collection-items-top\": top-level items within a specific collection in the library
  - \"searches\": all saved searches in the library
  - \"search\": a specific saved search in the library
  - \"tags\": all tags in the library, or tags of all types matching a specific name when an url encoded tag is provided
  - \"item-tags\": tags associated with a specific item
  - \"collection-tags\": tags within a specific collection in the library
  - \"items-tags\": all tags in the library, with the ability to filter based on the items
  - \"items-top-tags\": tags assigned to top-level items
  - \"trash-items-tags\": tags assigned to items in the trash
  - \"collection-items-tags\": tags assigned to items in a given collection
  - \"collection-items-top-tags\": tags assigned to top-level items in a given collection
  - \"publication-items-tags\": tags assigned to items in My Publications
  - \"keys\": the user id and privileges of the given API key
  - \"groups\": all groups the current API key has access to, including public groups the key owner belongs to even if the key doesn't have explicit permissions for them
  - \"group\": group metadata
  - \"all-fulltext\": all full-text content
  - \"item-fulltext\": an item's full-text content
  - \"file\": an item's attachment file
  - \"deleted\": all deleted data.

KEY is the item key, collection key, or search key. Which key is
needed varies by resource. Argument TYPE is \"user\" for your
personal library, and \"group\" for the group libraries. ID is
the ID of the personal or group library you want to access, e.g.
the user ID or group ID. Your personal library ID is available at
URL `https://www.zotero.org/settings/keys/'. For group libraries,
the ID can be found by opening the group's page at URL
`https://www.zotero.org/groups/'. If HOST is non-nil, use that in
stead of `zotero-base-url'."
  ;; Requests for data in a specific library begin with
  ;; /users/<userID> or /groups/<groupID>. User IDs are
  ;; different from usernames and can be found on the
  ;; API Keys page and in OAuth responses. Group IDs
  ;; are different from group names and can be
  ;; retrieved from /users/<userID>/groups.
  (let* ((library-request (pcase resource
                            ("keys" nil)
                            ("group" nil)
                            ("item-types" nil)
                            ("item-fields" nil)
                            ("item-type-fields" nil)
                            ("item-type-creator-types" nil)
                            ("creator-fields" nil)
                            ("template" nil)
                            ("schema" nil)
                            (_ t)))
         (type (when library-request (or type "user")))
         (id (when library-request (or id (zotero-auth-token-userid zotero-auth-token))))
         (prefix (when library-request
                   (pcase type
                     ("user"
                      (concat "/users/" (pcase id
                                          ((pred numberp) (number-to-string id))
                                          ((pred stringp) id))))
                     ("group"
                      (concat "/groups/" (pcase id
                                           ((pred numberp) (number-to-string id))
                                           ((pred stringp) id)))))))
         (suffix (pcase resource
                   ("collections"
                    "/collections")
                   ("collections-top"
                    "/collections/top")
                   ("collection"
                    (concat "/collections/" key))
                   ("subcollections"
                    (concat "/collections/" key "/collections"))
                   ("items"
                    "/items")
                   ("items-top"
                    "/items/top")
                   ("trash-items"
                    "/items/trash")
                   ("item"
                    (concat "/items/" key))
                   ("item-children"
                    (concat "/items/" key "/children"))
                   ("publication-items"
                    "/publications/items/")
                   ("collection-items"
                    (concat "/collections/" key "/items"))
                   ("collection-items-top"
                    (concat "/collections/" key "/items/top"))
                   ("searches"
                    "/searches")
                   ("search"
                    (concat "/searches/" key))
                   ("tags"
                    "/tags")
                   ("tags"
                    (concat "/tags/" (url-hexify-string key)))
                   ("item-tags"
                    (concat "/items/" key "/items/tags"))
                   ("collection-tags"
                    (concat "/collection/" key "/tags"))
                   ("items-tags"
                    "/items/tags")
                   ("items-top-tags"
                    "/items/top/tags")
                   ("trash-items-tags"
                    "/items/trash/tags")
                   ("collection-items-tags"
                    (concat "/items/" key "/items/tags"))
                   ("collection-items-top-tags"
                    (concat "/items/" key "/items/top/tags"))
                   ("publication-items-tags"
                    "/publications/tags")
                   ("keys"
                    (concat "/keys/" key))
                   ("groups"
                    "/groups")
                   ("group"
                    (concat "/groups/" key))
                   ("all-fulltext"
                    "/fulltext")
                   ("item-fulltext"
                    (concat "/items/" key "/fulltext"))
                   ("file"
                    (concat "/items/" key "/file"))
                   ("deleted"
                    (concat "/deleted" ))
                   ("item-types"
                    "/itemTypes")
                   ("item-fields"
                    "/itemFields")
                   ("item-type-fields"
                    "/itemTypeFields")
                   ("item-type-creator-types"
                    "/itemTypeCreatorTypes")
                   ("creator-fields"
                    "/creatorFields")
                   ("template"
                    "/items/new")
                   ("schema"
                    "/schema")
                   ;; Default
                   (_ nil))))
    (concat host prefix suffix)))

;;;; Functions

(cl-defun zotero-request (method resource &optional key &key
                                 type id api-key host headers
                                 params data no-auth)
  "Return the response of the Zotero request.

METHOD is the method to use for the request, i.e. \"GET\",
\"HEAD\" \"POST\", \"PUT\", \"PATCH\", or \"DELETE\". RESOURCE is
one of:
- \"collections\": collections in the library
- \"collections-top\": top-level collections in the library
- \"collection\": a specific collection in the library
- \"subcollections\": subcollections within a specific collection in the library
- \"items\": all items in the library, excluding trashed items
- \"items-top\": top-level items in the library, excluding trashed items
- \"trash-items\": items in the trash
- \"item\": a specific item in the library
- \"item-children\": child items under a specific item
- \"publication-items\": items in My Publications
- \"collection-items\": items within a specific collection in the library
- \"collection-items-top\": top-level items within a specific collection in the library
- \"searches\": all saved searches in the library
- \"search\": a specific saved search in the library
- \"tags\": all tags in the library, or tags of all types matching a specific name when an url encoded tag is provided
- \"item-tags\": tags associated with a specific item
- \"collection-tags\": tags within a specific collection in the library
- \"items-tags\": all tags in the library, with the ability to filter based on the items
- \"items-top-tags\": tags assigned to top-level items
- \"trash-items-tags\": tags assigned to items in the trash
- \"collection-items-tags\": tags assigned to items in a given collection
- \"collection-items-top-tags\": tags assigned to top-level items in a given collection
- \"publication-items-tags\": tags assigned to items in My Publications
- \"keys\": the user id and privileges of the given API key
- \"groups\": all groups the current API key has access to, including public groups the key owner belongs to even if the key doesn't have explicit permissions for them
- \"group\": group metadata
- \"all-fulltext\": all full-text content
- \"item-fulltext\": an item's full-text content
- \"file\": an item's attachment file
- \"deleted\": all deleted data.

KEY is the item key, collection key, or search key. Which key is
needed varies by resource. Keyword TYPE is \"user\" for your
personal library, and \"group\" for the group libraries. ID is
the ID of the personal or group library you want to access, e.g.
the user ID or group ID. Your personal library ID is available at
URL `https://www.zotero.org/settings/keys/'. For group libraries,
the ID can be found by opening the group's page at URL
`https://www.zotero.org/groups/'. API-KEY is the Zotero API key.
If HOST is non-nil, use that in stead of `zotero-base-url'.

HEADERS is an alist of extra headers. The CAR of each cons cell
is the field name and the CDR is the field value. HEADERS has the
form:
\((\"Content-Type\" . \"application/x-www-form-urlencoded\"))

PARAMS is an alist of the query string that is part of the URL.
The CAR of each cons cell is the parameter, CAR of the CDR is the
value. PARAMS has the form:
\((key1 val1)
 (key2 val2)
 (key3 val1 val2)
 (key4)
 (key5 \"\"))

DATA is the data to be sent to the server.

If NO-AUTH is non-nil, no authentication is used. Authentication
is not required for read access to public libraries.

Accessing non-public libraries requires the use of an API key. If
no API key is provided, the saved access token
`zotero-auth-token' is used or the user is redirected to Zotero
to authorize. Alternatively, `zotero-auth-authorize' can be
invoked manually to create and save the access token for future
sessions."
  (let* ((method (upcase method))
         (host (or host zotero-base-url))
         (url (zotero--url resource key :type type :id id :host host))
         (api-version (number-to-string zotero-api-version))
         (no-auth (or no-auth
                      (pcase resource
                        ("keys" t)
                        ("item-types" t)
                        ("item-fields" t)
                        ("item-type-fields" t)
                        ("item-type-creator-types" t)
                        ("creator-fields" t)
                        ("template" t)
                        ("schema" t)
                        (_ nil))))
         (api-key (unless no-auth (or api-key (zotero-auth-api-key zotero-auth-token))))
         (headers (nconc headers
                         `(("User-Agent" . ,zotero-user-agent)
                           ("Zotero-API-Version" . ,api-version))
                         (unless no-auth `(("Zotero-API-Key" . ,api-key))))))
    (zotero-dispatch (zotero-request-create :method method
                                            :url url
                                            :headers headers
                                            :params params
                                            :data data))))

;;;;; Read API functions

(defun zotero-key (&optional api-key)
  "Return user and group permissions.
Together with `zotero-groups', this allows all accessible
resources to be determined.

Optional argument API-KEY is the Zotero API key."
  (let ((api-key (or api-key (zotero-auth-api-key zotero-auth-token))))
    (zotero-request "GET" "keys" api-key :no-auth t)))

(defun zotero-delete-key (&optional api-key)
  "Delete the API-KEY."
  (let ((api-key (or api-key (zotero-auth-api-key zotero-auth-token))))
    (zotero-request "DELETE" "keys" api-key :no-auth t)))

(cl-defun zotero-groups (&key type id api-key)
  "Return all groups the API-KEY has access to.
Public groups are included, even if the key doesn't have explicit
permissions for them.

Keyword TYPE is \"user\" for your personal library, and \"group\"
for the group libraries. ID is the ID of the personal or group
library you want to access, e.g. the \"user ID\" or \"group ID\".
API-KEY is the Zotero API key."
  (zotero-request "GET" "groups" nil :type type :id id :api-key api-key))

(cl-defun zotero-group (id &key api-key)
  "Return the metadata of group ID.

Keyword API-KEY is the Zotero API key."
  (zotero-request "GET" "group" id :api-key api-key))

;;;;; Read requests

(cl-defun zotero-items (&key type id api-key)
  "Return all items, excluding trashed items.

Keyword TYPE is \"user\" for your personal library, and \"group\"
for the group libraries. ID is the ID of the personal or group
library you want to access, e.g. the \"user ID\" or \"group ID\".
API-KEY is the Zotero API key."
  (zotero-request "GET" "items" nil :type type :id id :api-key api-key))

(cl-defun zotero-top (&key type id api-key)
  "Return top-level items, excluding trashed items.

Keyword TYPE is \"user\" for your personal library, and \"group\"
for the group libraries. ID is the ID of the personal or group
library you want to access, e.g. the \"user ID\" or \"group ID\".
API-KEY is the Zotero API key."
  (zotero-request "GET" "items-top" nil :type type :id id :api-key api-key))

(cl-defun zotero-publications (&key type id api-key)
  "Return items in My Publications.

Keyword TYPE is \"user\" for your personal library, and \"group\"
for the group libraries. ID is the ID of the personal or group
library you want to access, e.g. the \"user ID\" or \"group ID\".
API-KEY is the Zotero API key."
  (zotero-request "GET" "publication-items" nil :type type :id id :api-key api-key))

(cl-defun zotero-trash (&key type id api-key)
  "Return items in the trash.

Keyword TYPE is \"user\" for your personal library, and \"group\"
for the group libraries. ID is the ID of the personal or group
library you want to access, e.g. the \"user ID\" or \"group ID\".
API-KEY is the Zotero API key."
  (zotero-request "GET" "trash-items" nil :type type :id id :api-key api-key))

(cl-defun zotero-item (key &key type id api-key)
  "Return item KEY.

If KEY is a list of item keys, return the specified items. Up to
50 items can be specified in a single request.

Keyword TYPE is \"user\" for your personal library, and \"group\"
for the group libraries. ID is the ID of the personal or group
library you want to access, e.g. the \"user ID\" or \"group ID\".
API-KEY is the Zotero API key."
  (pcase key
    ((pred stringp)
     (zotero-request "GET" "item" key :type type :id id :api-key api-key))
    ((pred consp)
     (let ((value (s-join "," key)))
       (zotero-request "GET" "items" nil :type type :id id :api-key api-key :params `(("itemKey" ,value)))))))

(cl-defun zotero-children (key &key type id api-key)
  "Return child items under item KEY.

Keyword TYPE is \"user\" for your personal library, and \"group\"
for the group libraries. ID is the ID of the personal or group
library you want to access, e.g. the \"user ID\" or \"group ID\".
API-KEY is the Zotero API key."
  (zotero-request "GET" "item-children" key :type type :id id :api-key api-key))

(cl-defun zotero-collection-items (key &key type id api-key)
  "Return all items in collection KEY.

Keyword TYPE is \"user\" for your personal library, and \"group\"
for the group libraries. ID is the ID of the personal or group
library you want to access, e.g. the \"user ID\" or \"group ID\".
API-KEY is the Zotero API key."
  (zotero-request "GET" "collection-items" key :type type :id id :api-key api-key))

(cl-defun zotero-collection-items-top (key &key type id api-key)
  "Return top-level items in collection KEY.

Keyword TYPE is \"user\" for your personal library, and \"group\"
for the group libraries. ID is the ID of the personal or group
library you want to access, e.g. the \"user ID\" or \"group ID\".
API-KEY is the Zotero API key."
  (zotero-request "GET" "collection-items-top" key :type type :id id :api-key api-key))

(cl-defun zotero-collections (&key type id api-key)
  "Return all collections.

Keyword TYPE is \"user\" for your personal library, and \"group\"
for the group libraries. ID is the ID of the personal or group
library you want to access, e.g. the \"user ID\" or \"group ID\".
API-KEY is the Zotero API key."
  (zotero-request "GET" "collections" nil :type type :id id :api-key api-key))

(cl-defun zotero-collections-top (&key type id api-key)
  "Return top-level collections.

Keyword TYPE is \"user\" for your personal library, and \"group\"
for the group libraries. ID is the ID of the personal or group
library you want to access, e.g. the \"user ID\" or \"group ID\".
API-KEY is the Zotero API key."
  (zotero-request "GET" "collections-top" :type type :id id :api-key api-key))

(cl-defun zotero-collection (key &key type id api-key)
  "Return collection KEY.

Keyword TYPE is \"user\" for your personal library, and \"group\"
for the group libraries. ID is the ID of the personal or group
library you want to access, e.g. the \"user ID\" or \"group ID\".
API-KEY is the Zotero API key."
  (zotero-request "GET" "collection" key :type type :id id :api-key api-key))

(cl-defun zotero-subcollections (key &key type id api-key)
  "Return subcollections in collection KEY.

Keyword TYPE is \"user\" for your personal library, and \"group\"
for the group libraries. ID is the ID of the personal or group
library you want to access, e.g. the \"user ID\" or \"group ID\".
API-KEY is the Zotero API key."
  (zotero-request "GET" "subcollections" key :type type :id id :api-key api-key))

;; REVIEW: is this function used?
(cl-defun zotero-attachment-attributes (key &key type id api-key)
  "Get attachment attributes of item KEY.

Keyword TYPE is \"user\" for your personal library, and \"group\"
for the group libraries. ID is the ID of the personal or group
library you want to access, e.g. the \"user ID\" or \"group ID\".
API-KEY is the Zotero API key."
  (let* ((item (zotero-item key :type type :id id :api-key api-key))
         (data (plist-get item :data)))
    `(:filename ,(plist-get data :filename) :contenttype ,(plist-get data :contentType) :md5 ,(plist-get data :md5) :mtime ,(plist-get data :mtime))))

(cl-defun zotero-file (key &key type id api-key)
  "Return the attachment file of item KEY.

Keyword TYPE is \"user\" for your personal library, and \"group\"
for the group libraries. ID is the ID of the personal or group
library you want to access, e.g. the \"user ID\" or \"group ID\".
API-KEY is the Zotero API key."
  (zotero-request "GET" "file" key :type type :id id :api-key api-key))

(cl-defun zotero-download-file (key &optional file dir confirm &key type id api-key)
  "Download the attachment file of item KEY.
This is a convenient wrapper around `zotero-file'.

Write the attachment to disk using the optional arguments DIR and
FILE. DIR is the directory to start with if FILE is relative
\(does not start with slash or tilde). If DIR is nil, the current
buffer’s value of ‘default-directory’ is used. If FILE is not
supplied, a `zotero-item' call is made to determine the
attachment filename. If successful, the full path including the
file name is returned.

If optional argument CONFIRM is non-nil, ask for confirmation
before overwriting.

Keyword TYPE is \"user\" for your personal library, and \"group\"
for the group libraries. ID is the ID of the personal or group
library you want to access, e.g. the \"user ID\" or \"group ID\".
API-KEY is the Zotero API key.

See also URL
`https://www.zotero.org/support/dev/web_api/v3/file_upload#ii_download_the_existing_file'."
  (let* ((result (zotero-file key :type type :id id :api-key api-key))
         (contents (zotero-response-data result))
         (coding-system-for-write 'binary)
         (filename (or file
                       (let* ((result (zotero-items key :type type :id id :api-key api-key))
                              (item (zotero-response-data result)))
                         (zotero-lib-plist-get* item :data :filename))))
         (full-filename (expand-file-name filename dir)))
    (write-region contents nil full-filename nil nil nil confirm)
    ;; Check the ETag header of the response to make sure it matches
    ;; the attachment item's md5 value. If it doesn't, offer to
    ;; download the attachment item again.
    (let* ((attributes (zotero-file-attributes full-filename))
           (etag (zotero-response-etag result)))
      (if (equal etag (plist-get attributes :md5))
          full-filename
        (if (y-or-n-p (format "MD5 value doesn't match the response header. Retry? "))
            (zotero-download-file key file dir confirm :type type :id id :api-key api-key)
          (warn "Item has the wrong hash. The latest version of
  the file may be available only via WebDAV, not via Zotero File
  Storage."))))))

(cl-defun zotero-tags (&key type id api-key)
  "Return all tags.

Keyword TYPE is \"user\" for your personal library, and \"group\"
for the group libraries. ID is the ID of the personal or group
library you want to access, e.g. the \"user ID\" or \"group ID\".
API-KEY is the Zotero API key."
  (zotero-request "GET" "tags" :type type :id id :api-key api-key))

(cl-defun zotero-item-tags (key &key type id api-key)
  "Return tags associated with KEY.

Keyword TYPE is \"user\" for your personal library, and \"group\"
for the group libraries. ID is the ID of the personal or group
library you want to access, e.g. the \"user ID\" or \"group ID\".
API-KEY is the Zotero API key."
  (zotero-request "GET" "item-tags" key :type type :id id :api-key api-key))

(cl-defun zotero-collection-tags (key &key type id api-key)
  "Return tags in collection KEY.

Keyword TYPE is \"user\" for your personal library, and \"group\"
for the group libraries. ID is the ID of the personal or group
library you want to access, e.g. the \"user ID\" or \"group ID\".
API-KEY is the Zotero API key."
  (zotero-request "GET" "collection-tags" key :type type :id id :api-key api-key))

(cl-defun zotero-search-items (query &optional fulltext include-trashed &key type id api-key )
  "Search all items for QUERY.

Searched are case insensitive. By default, only the \"title\",
\"creator\", and \"year\" fields are searched. If FULLTEXT is
non-nil, full-text content is included. If INCLUDE-TRASHED is
non-nil, items in the trash are included.

Keyword TYPE is \"user\" for your personal library, and \"group\"
for the group libraries. ID is the ID of the personal or group
library you want to access, e.g. the \"user ID\" or \"group ID\".
API-KEY is the Zotero API key."
  (zotero-request "GET" "items" nil :type type :id id :api-key api-key :params `((q ,query)
                                                                                 (qmode ,(if fulltext "everything" "titleCreatorYear"))
                                                                                 (includeTrashed ,(if include-trashed "1" "0")))))

(cl-defun zotero-search-tags (query &optional starts-with &key type id api-key)
  "Search all tags in the library for QUERY.

Searched are case sensitive. By default, tags including QUERY are
returned. If STARTS-WITH is non-nil, a left-bound search is
performed.

Keyword TYPE is \"user\" for your personal library, and \"group\"
for the group libraries. ID is the ID of the personal or group
library you want to access, e.g. the \"user ID\" or \"group ID\".
API-KEY is the Zotero API key."
  (zotero-request "GET" "tags" nil :type type :id id :api-key api-key :params `((q ,query)
                                                                                (qmode ,(if starts-with "startsWith" "contains")))))

;;;;; Write Requests

(cl-defun zotero-create-item (object &key type id api-key)
  "Create an item.

OBJECT is a plist of the new item. When creating a new item,
first get an empty property list for the item type with
`zotero-template' (or use a cached version of the template). Then
modify it and resubmit it to the server in an array. Note that
when uploading the full property list, only the `:data' property
is processed. All other properties are ignored.

Keyword TYPE is \"user\" for your personal library, and \"group\"
for the group libraries. ID is the ID of the personal or group
library you want to access, e.g. the \"user ID\" or \"group ID\".
API-KEY is the Zotero API key."
  (let ((write-token (zotero--write-token))
        (json (zotero-json-encode-object object)))
    (zotero-request "POST" "items" nil
                    :type type
                    :id id
                    :api-key api-key
                    :headers `(("Content-Type" . "application/json")
                               ("Zotero-Write-Token" . ,write-token))
                    :data json)))

(cl-defun zotero-create-items (objects &key type id api-key)
  "Create multiple items.

OBJECTS is a list of
objects, each of which should return a plist and may be:
- a cons cell (containing a propery list),
- a buffer (read one Lisp expression from the beginning),
- a function (call it with no arguments),
- a file (read one Lisp expression from the beginning),
- a string (takes text from string, starting at the beginning).

Up to 50 items can be created in a single request.

Keyword TYPE is \"user\" for your personal library, and \"group\"
for the group libraries. ID is the ID of the personal or group
library you want to access, e.g. the \"user ID\" or \"group ID\".
API-KEY is the Zotero API key."
  (let* ((write-token (zotero--write-token))
         (json (zotero-json-encode-object objects)))
    (zotero-request "POST" "items" nil
                    :type type
                    :id id
                    :api-key api-key
                    :headers `(("Content-Type" . "application/json")
                               ("Zotero-Write-Token" . ,write-token))
                    :data json)))

(cl-defun zotero-update-item (key object &key type id api-key)
  "Update existing item KEY.

OBJECT is a plist of the modified item.

To update an existing item, first retrieve the current version of
the item with `zotero-item'. The editable data will be found in
the `:data' property of the response. Modify it and resubmit it
to the server. When uploading the full property list, only the
`:data' property is processed.

Notes and attachments can be made child items by assigning the
parent item's key to the `:parentItem' property. If parent and
child items are created in the same POST request, the child items
must appear after the parent item in the array of items.

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

Keyword TYPE is \"user\" for your personal library, and \"group\"
for the group libraries. ID is the ID of the personal or group
library you want to access, e.g. the \"user ID\" or \"group ID\".
API-KEY is the Zotero API key."
  (let ((json (zotero-json-encode-object object)))
    (zotero-request "PUT" "item" key
                    :type type
                    :id id
                    :api-key api-key
                    :headers `(("Content-Type" . "application/json"))
                    :data json)))

(cl-defun zotero-update-items (objects &key type id api-key)
  "Update existing items.

OBJECTS is a list of
objects, each of which should return a plist and may be:
- a cons cell (containing a propery list),
- a buffer (read one Lisp expression from the beginning),
- a function (call it with no arguments),
- a file (read one Lisp expression from the beginning),
- a string (takes text from string, starting at the beginning).

Up to 50 items can be created in a single request. Note that any
properties not specified will be left untouched on the server. To
erase an existing property, include it with an empty string or
false as the value.

Follow the instructions in `zotero-create-item', but include a
key and version property in each object. If modifying editable a
property list returned from the API, these properties will
already exist and shouldn't be modified.

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

Keyword TYPE is \"user\" for your personal library, and \"group\"
for the group libraries. ID is the ID of the personal or group
library you want to access, e.g. the \"user ID\" or \"group ID\".
API-KEY is the Zotero API key."
  (let ((json (zotero-json-encode-object objects)))
    (zotero-request "POST" "items" nil
                    :type type
                    :id id
                    :api-key api-key
                    :headers `(("Content-Type" . "application/json"))
                    :data json)))


(cl-defun zotero-patch-item (key object version &key type id api-key)
  "Partially update existing item KEY.

OBJECT is a plist of the modified item, with just the properties
that have actually changed. Properties not included are left
untouched on the server. To clear a property, pass an empty
string or vector as appropriate. VERSION is the last known item
version.

Keyword TYPE is \"user\" for your personal library, and \"group\"
for the group libraries. ID is the ID of the personal or group
library you want to access, e.g. the \"user ID\" or \"group ID\".
API-KEY is the Zotero API key."
  (let ((json (zotero-json-encode-object object)))
    (zotero-request "PATCH" "item" key
                    :type type
                    :id id
                    :api-key api-key
                    :headers `(("If-Unmodified-Since-Version" . ,version))
                    :data json)))

(cl-defun zotero-delete-item (key version &key type id api-key)
  "Delete item KEY.

VERSION is the last known item version.

Keyword TYPE is \"user\" for your personal library, and \"group\"
for the group libraries. ID is the ID of the personal or group
library you want to access, e.g. the \"user ID\" or \"group ID\".
API-KEY is the Zotero API key."
  (zotero-request "DELETE" "item" key
                  :type type
                  :id id
                  :api-key api-key
                  :headers `(("If-Unmodified-Since-Version" . ,version))))

(cl-defun zotero-delete-items (keys version &key type id api-key)
  "Delete multiple items.

Up to 50 items can be deleted in a single request.

KEYS is a list of item keys. VERSION is the last known item
version.

Keyword TYPE is \"user\" for your personal library, and \"group\"
for the group libraries. ID is the ID of the personal or group
library you want to access, e.g. the \"user ID\" or \"group ID\".
API-KEY is the Zotero API key."
  (cl-assert (consp keys))
  (cond
   ((<= (length keys) 50)
    (let ((value (s-join "," keys)))
      (zotero-request "DELETE" "items" nil
                      :type type
                      :id id
                      :api-key api-key
                      :headers `(("If-Unmodified-Since-Version" . ,version))
                      :params `(("itemKey" ,value)))))
   ((> (length keys) 50)
    (user-error "Up to 50 items can be deleted in a single request"))))

(cl-defun zotero-create-collection (object &key type id api-key)
  "Create a collection.

OBJECT is a plist of the new collection. When creating a new
collection, first get an empty property list for new collections
with `zotero-collection-template' (or use a cached version of the
template).

Keyword TYPE is \"user\" for your personal library, and \"group\"
for the group libraries. ID is the ID of the personal or group
library you want to access, e.g. the \"user ID\" or \"group ID\".
API-KEY is the Zotero API key."
  (let ((write-token (zotero--write-token))
        (json (zotero-json-encode-object object)))
    (zotero-request "POST" "collections" nil
                    :type type
                    :id id
                    :api-key api-key
                    :headers `(("Content-Type" . "application/json")
                               ("Zotero-Write-Token" . ,write-token))
                    :data json)))

(cl-defun zotero-update-collection (key object &key type id api-key)
  "Update existing collection KEY.

OBJECT is a plist of the modified collection. To update an
existing collection, first retrieve the current version of the
item with `zotero-collection'. The editable data will be found in
the `:data' property of the response. Modify it and resubmit it
to the server. When uploading the full property list, only the
`:data' property is processed.

Keyword TYPE is \"user\" for your personal library, and \"group\"
for the group libraries. ID is the ID of the personal or group
library you want to access, e.g. the \"user ID\" or \"group ID\".
API-KEY is the Zotero API key."
  (let ((json (zotero-json-encode-object object)))
    (zotero-request "PUT" "collection" key
                    :type type
                    :id id
                    :api-key api-key
                    :headers `(("Content-Type" . "application/json"))
                    :data json)))

(cl-defun zotero-update-collections (objects &key type id api-key)
  "Update existing collections.

OBJECTS is a list of
objects, each of which should return a plist and may be:
- a cons cell (containing a propery list),
- a buffer (read one Lisp expression from the beginning),
- a function (call it with no arguments),
- a file (read one Lisp expression from the beginning),
- a string (takes text from string, starting at the beginning).

Up to 50 collections can be updated in a single request. Note
that any properties not specified will be left untouched on the
server. To erase an existing property, include it with an empty
string or false as the value.

Keyword TYPE is \"user\" for your personal library, and \"group\"
for the group libraries. ID is the ID of the personal or group
library you want to access, e.g. the \"user ID\" or \"group ID\".
API-KEY is the Zotero API key."
  (let ((json (zotero-json-encode-object objects)))
    (zotero-request "POST" "collections" nil
                    :type type
                    :id id
                    :api-key api-key
                    :headers `(("Content-Type" . "application/json"))
                    :data json)))

(cl-defun zotero-delete-collection (key version &key type id api-key)
  "Delete collection KEY.

VERSION is the collection's current version number.

Keyword TYPE is \"user\" for your personal library, and \"group\"
for the group libraries. ID is the ID of the personal or group
library you want to access, e.g. the \"user ID\" or \"group ID\".
API-KEY is the Zotero API key."
  (zotero-request "DELETE" "collection" key
                  :type type
                  :id id
                  :api-key api-key
                  :headers `(("If-Unmodified-Since-Version" . ,version))))

(cl-defun zotero-delete-collections (keys version &key type id api-key)
  "Delete multiple collections.

Up to 50 collections can be deleted in a single request.

KEYS is a list of collection keys. VERSION is the last-known
library version.

Keyword TYPE is \"user\" for your personal library, and \"group\"
for the group libraries. ID is the ID of the personal or group
library you want to access, e.g. the \"user ID\" or \"group ID\".
API-KEY is the Zotero API key."
  (cl-assert (consp keys))
  (cond
   ((<= (length keys) 50)
    (let ((value (s-join "," keys)))
      (zotero-request "DELETE" "collections" nil
                      :type type
                      :id id
                      :api-key api-key
                      :headers `(("If-Unmodified-Since-Version" . ,version))
                      :params `(("collectionKey" ,value)))))
   ((> (length keys) 50)
    (user-error "Up to 50 collections can be deleted in a single request"))))

(cl-defun zotero-create-search (object &key type id api-key)
  "Create a saved search.

OBJECT is a plist of the new search.

Keyword TYPE is \"user\" for your personal library, and \"group\"
for the group libraries. ID is the ID of the personal or group
library you want to access, e.g. the \"user ID\" or \"group ID\".
API-KEY is the Zotero API key."
  (let ((write-token (zotero--write-token))
        (json (zotero-json-encode-object object)))
    (zotero-request "POST" "searches" nil
                    :type type
                    :id id
                    :api-key api-key
                    :headers `(("Content-Type" . "application/json")
                               ("Zotero-Write-Token" . ,write-token))
                    :data json)))

(cl-defun zotero-update-searches (objects &key type id api-key)
  "Update existing searches in the library.

OBJECTS is a list of
objects, each of which should return a plist and may be:
- a cons cell (containing a propery list),
- a buffer (read one Lisp expression from the beginning),
- a function (call it with no arguments),
- a file (read one Lisp expression from the beginning),
- a string (takes text from string, starting at the beginning).

Up to 50 searches can be updated in a single request. Note that
any properties not specified will be left untouched on the
server. To erase an existing property, include it with an empty
string or false as the value.

Keyword TYPE is \"user\" for your personal library, and \"group\"
for the group libraries. ID is the ID of the personal or group
library you want to access, e.g. the \"user ID\" or \"group ID\".
API-KEY is the Zotero API key."
  (let ((json (zotero-json-encode-object objects)))
    (zotero-request "POST" "searches" nil
                    :type type
                    :id id
                    :api-key api-key
                    :headers `(("Content-Type" . "application/json"))
                    :data json)))

(cl-defun zotero-delete-searches (keys version &key type id api-key)
  "Delete multiple searches.

Up to 50 searches can be deleted in a single request.

KEYS are the search keys. VERSION is the last-known library
version.

Keyword TYPE is \"user\" for your personal library, and \"group\"
for the group libraries. ID is the ID of the personal or group
library you want to access, e.g. the \"user ID\" or \"group ID\".
API-KEY is the Zotero API key."
  (cl-assert (consp keys))
  (cond
   ((<= (length keys) 50)
    (let ((value (s-join "," keys)))
      (zotero-request "DELETE" "searches" nil
                      :type type
                      :id id
                      :api-key api-key
                      :headers `(("If-Unmodified-Since-Version" . ,version))
                      :params `(("searchKey" ,value)))))
   ((> (length keys) 50)
    (user-error "Up to 50 collections can be deleted in a single request"))))

(cl-defun zotero-delete-tags (tags version &key type id api-key)
  "Delete multiple tags.

Up to 50 tags can be deleted in a single request.

TAGS is a list of tag strings. VERSION is the last-known library
version.

Keyword TYPE is \"user\" for your personal library, and \"group\"
for the group libraries. ID is the ID of the personal or group
library you want to access, e.g. the \"user ID\" or \"group ID\".
API-KEY is the Zotero API key."
  (cl-assert (consp tags))
  (cond
   ((<= (length tags) 50)
    (let ((value (s-join "||" (mapcar #'url-hexify-string tags))))
      (zotero-request "DELETE" "tags" nil
                      :type type
                      :id id
                      :api-key api-key
                      :headers `(("If-Unmodified-Since-Version" . ,version))
                      :params `(("tag" ,value)))))
   ((> (length tags) 50)
    (user-error "Up to 50 tags can be deleted in a single request"))))

;;;; Item Type/Field Requests

(defun zotero-item-types (&optional locale)
  "Get all item types.

If optional argument LOCALE is provided, user-friendly type/field
names in the defined language will be returned. By default the
locale in variable `zotero-locale' is requested."
  (zotero-request "GET" "item-types" nil :no-auth t :params `(("locale" ,(or locale zotero-locale)))))

(defun zotero-item-fields (&optional locale)
  "Get all item fields.

If optional argument LOCALE is provided, user-friendly type/field
names in the defined language will be returned. By default the
locale in `zotero-locale' is requested."
  (zotero-request "GET" "item-fields" nil :no-auth t :params `(("locale" ,(or locale zotero-locale)))))

(defun zotero-item-type-fields (item-type &optional locale)
  "Get all valid fields for an item type.

ITEM-TYPE is one of the item types returned by
`zotero-item-types.' If optional argument LOCALE is provided,
user-friendly type/field names in the defined language will be
returned. By default the locale in variable `zotero-locale' is
requested."
  (zotero-request "GET" "item-type-fields" nil :no-auth t :params `(("itemType" ,item-type)
                                                                    ("locale" ,(or locale zotero-locale)))))

(defun zotero-item-type-creator-types (item-type &optional locale)
  "Get the valid creator types for an item type.

Argument ITEM-TYPE is one of the item types returned by
`zotero-item-types.' If optional argument LOCALE is provided,
user-friendly type/field names in the defined language will be
returned. By default the locale in variable `zotero-locale'
is requested."
  (zotero-request "GET" "item-type-creator-types" nil :no-auth t :params `(("itemType" ,item-type)
                                                                           ("locale" ,(or locale zotero-locale)))))

(defun zotero-creator-fields (&optional locale)
  "Get the localized creator fields.

If optional argument LOCALE is provided, user-friendly type/field
names in the defined language will be returned. By default the
locale in variable `zotero-locale' is requested."
  (zotero-request "GET" "creator-fields" nil :no-auth t :params `(("locale" ,(or locale zotero-locale)))))

(defun zotero-attachment-linkmodes ()
  "Return the linkmode types.

The Zotero API doesn't provide an endpoint to request linkmode
types, so this function returns a hard-coded list."
  '("imported_file" "imported_url" "linked_file" "linked_url"))

;;;; Template Requests

(defun zotero-collection-template ()
  "Return the template for a new collections.

The Zotero API doesn't provide an endpoint to request a template
for a new collection, so this function returns a hard-coded
template."
  '(:name "" :parentCollection :json-false :relations :json-empty))

(defun zotero-item-template (item-type)
  "Get the template for a new item of an item type.

Argument ITEM-TYPE is one of the item types returned by
`zotero-itemtypes.'"
  (zotero-request "GET" "template" nil :no-auth t :params `(("itemType" ,item-type))))

(defun zotero-attachment-template (linkmode)
  "Get a template for a new attachment item.

Argument LINKMODE is one of the linkmode types returned by
`zotero-attachment-linkmodes'."
  (zotero-request "GET" "template" nil :no-auth t :params `(("itemType" "attachment")
                                                            ("linkMode" ,linkmode))))

;;;; File Uploads

(defun zotero-file-attributes (file)
  "Return the attributes of FILE.

The result is a plist with `:filename', `:filesize',
`:content-type', `:md5', `:mtime', and `:accessdate' props to be
passed to `zotero-authorize-upload'.

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

(cl-defun zotero-authorize-upload (key filename filesize md5 mtime &optional hash &key type id api-key)
  "Get upload authorisation for a file and associate it with item KEY.

Arguments FILENAME, FILESIZE, MD5, and MTIME are the attributes
of the file and can be obtained by `zotero-file-attributes'.

For existing attachments, optional argument HASH should be the
previous MD5 hash (as provided in the ETag header
when downloading the file).

Keyword TYPE is \"user\" for your personal library, and \"group\"
for the group libraries. ID is the ID of the personal or group
library you want to access, e.g. the \"user ID\" or \"group ID\".
API-KEY is the Zotero API key.

If a status-code 200 is received, the upload was authorized or
the file already exists.

If authorisation was successful, return a plist with the props
`:url', `:contentType', `:prefix', `:suffix', and `:uploadKey',
to be passed to `zotero-upload-file'. If the file already exists
on the server, return a plist with the prop `:exists'.

See also URL
`https://www.zotero.org/support/dev/web_api/v3/file_upload#get_upload_authorization'."
  (let ((data (url-build-query-string `(("filename" ,filename)
                                        ("filesize" ,filesize)
                                        ("md5" ,md5)
                                        ("mtime" ,mtime)))))
    (zotero-request "POST" "file" key
                    :type type
                    :id id
                    :api-key api-key
                    :headers `(("Content-Type" . "application/x-www-form-urlencoded")
                               ,(if hash `("If-Match" . ,hash) `(("If-None-Match:" . "*"))))
                    :data data)))

(defun zotero-upload-file (file url content-type prefix suffix)
  "Upload FILE to Zotero.

Arguments URL, CONTENT-TYPE, PREFIX, and SUFFIX can be obtained
by `zotero-authorize-upload'.

Keyword TYPE is \"user\" for your personal library, and \"group\"
for the group libraries. ID is the ID of the personal or group
library you want to access, e.g. the \"user ID\" or \"group ID\".
API-KEY is the Zotero API key.

If a status-code 201 is received, the file was successfully
uploaded.

See also URL
`https://www.zotero.org/support/dev/web_api/v3/file_upload#i_post_file'."
  (let* ((content (with-temp-buffer
                    (insert-file-contents-literally file)
                    (buffer-string)))
         (data (concat prefix content suffix))
         (url-request-method "POST")
         (url-request-data data)
         (url-request-extra-headers `(("Content-Type" . ,content-type))))
    (with-current-buffer (url-retrieve-synchronously url nil nil zotero-timeout)
      (funcall #'zotero-handle-response))))

(cl-defun zotero-register-upload (key uploadkey &optional hash &key type id api-key)
  "Register upload to item KEY.

Argument UPLOADKEY can be obtained by `zotero-authorize-upload'.
For existing attachments, optional argument HASH should be the
previous MD5 hash (as provided in the ETag header when
downloading the file).

Keyword TYPE is \"user\" for your personal library, and \"group\"
for the group libraries. ID is the ID of the personal or group
library you want to access, e.g. the \"user ID\" or \"group ID\".
API-KEY is the Zotero API key.

If a status-code 204 is received, the upload was successfully
registered.

See also URL
`https://www.zotero.org/support/dev/web_api/v3/file_upload#ii_register_upload'."
  (let ((data (url-build-query-string `(("upload" ,uploadkey)))))
    (zotero-request "POST" "file" key
                    :type type
                    :id id
                    :api-key api-key
                    :headers `(("Content-Type" . "application/x-www-form-urlencoded")
                               ,(if hash `("If-Match" . ,hash) `(("If-None-Match:" . "*"))))
                    :data data)))

(cl-defun zotero-upload-attachment (key file &optional hash &key type id api-key)
  "Authorize, upload and register attachment FILE to item KEY.
This is a convenient wrapper around `zotero-authorize-upload',
`zotero-upload-file', and `zotero-register-upload'.

For existing attachments, optional argument HASH should be the
previous MD5 hash (as provided in the ETag header when
downloading the file).

Keyword TYPE is \"user\" for your personal library, and \"group\"
for the group libraries. ID is the ID of the personal or group
library you want to access, e.g. the \"user ID\" or \"group ID\".
API-KEY is the Zotero API key.

Return t if success, or nil if failed."
  (message "Authorizing upload...")
  (let* ((attributes (zotero-file-attributes file))
         (filename (plist-get attributes :filename))
         (filesize (plist-get attributes :filesize))
         (md5 (plist-get attributes :md5))
         (mtime (plist-get attributes :mtime))
         (result (zotero-authorize-upload key filename filesize md5 mtime hash :type type :id id :api-key api-key))
         (status-code (zotero-response-status-code result))
         (data (zotero-response-data result)))
    ;; A status-code 200 means the upload was authorized or the file already
    ;; exists.
    (if (eq status-code 200)
        (if (eq (plist-get data :exists) 1)
            (progn
              (message "Authorizing upload...file already exists")
              ;; Success: return t
              t)
          (message "Authorizing upload...done")
          (message "Upload file...")
          (let* ((url (plist-get data :url))
                 (content-type (plist-get data :contentType))
                 (prefix (plist-get data :prefix))
                 (suffix (plist-get data :suffix))
                 (uploadkey (plist-get data :uploadKey))
                 (result (zotero-upload-file file url content-type prefix suffix))
                 (status-code (zotero-response-status-code result)))
            ;; A status-code 201 means the file was successfully uploaded.
            (if (eq status-code 201)
                (progn
                  (message "Upload file...done")
                  (message "Register upload...")
                  (let* ((result (zotero-register-upload key uploadkey hash :type type :id id :api-key api-key))
                         (status-code (zotero-response-status-code result)))
                    ;; A status-code 204 means the file was successfully registered.
                    (if (eq status-code 204)
                        (progn
                          (message "Register upload...done")
                          ;; Success: return t
                          t)
                      (message "Register upload...failed")
                      ;; Failed: return nil
                      nil)))
              (message "Uploading file...failed")
              ;; Failed: return nil
              nil)))
      (message "Authorizing upload...failed")
      ;; Failed: return nil
      nil)))

(provide 'zotero)

;;; zotero.el ends here
