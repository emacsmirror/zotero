;;; zotero-auth.el --- Authorization for the Zotero API  -*- lexical-binding: t; -*-

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
;; Authorization functions for Zotero. The Oauth library is used, and advised
;; because the Zotero API needs an access token, secret, user ID and username,
;; but the original function only fetches the access token and secret.

;;; Code:

;;;; Requirements

(require 'oauth)

;;;; Variables

(defvar zotero-auth-token nil
  "The access token.")

;; The Client Key and Client Secret for use during all future OAuth handshakes
;; between emacs-zotero and zotero.org.
(defconst zotero-auth-client-key "28b59774b8e3e022a296"
  "The client key issued by Zotero.")

(defconst zotero-auth-client-secret "ed094e305ae7305ebbbc"
  "The client secret issued by Zotero.")

;; The OAuth endpoints for access to the Zotero API:
(defconst zotero-auth-request-token-endpoint "https://www.zotero.org/oauth/request"
  "Temporary Credential Request URI.")

(defconst zotero-auth-token-endpoint "https://www.zotero.org/oauth/access"
  "Token Request URI.")

(defconst zotero-auth-authorize-endpoint "https://www.zotero.org/oauth/authorize"
  "Resource Owner Authorization URI.")

(cl-defstruct (zotero-auth-token (:include oauth-t)
                                 (:constructor zotero-auth-token-create)
                                 (:copier nil))
  "Specialized token of `oauth-t' including the extra slots `userid' and `username'."
  userid username)

;;;; Methods

(defun zotero-auth-authorize ()
  "Redirect user to Zotero to authorize.
Also, save the access token info for future sessions and return it.

The access token will be valid indefinitely, unless it is revoked by the
user manually, so keys should be considered sensitive."
  (interactive)
  (zotero-auth--before-authorize-function)
  (let* ((response (oauth-authorize-app zotero-auth-client-key
                                        zotero-auth-client-secret
                                        zotero-auth-request-token-endpoint
                                        zotero-auth-token-endpoint
                                        zotero-auth-authorize-endpoint))
         (token (oauth-access-token-auth-t response)))
    (zotero-auth--after-authorize-function)
    (zotero-auth--save-token token)))

(defun zotero-auth-token (&optional force)
  "Return the access token.
If optional argument FORCE is non-nil, authorize Zotero and
obtain new token info."
  (cond
   ((or (null zotero-auth-token) force)
    ;; Not authorized or forcing: authorize Zotero
    (zotero-auth-authorize))
   ((not (zotero-auth-token-valid-p zotero-auth-token))
    ;; Invalid token: authorize Zotero
    (message "Invalid token. Forcing authorization...")
    (zotero-auth-authorize))
   (t
    ;; Already authorized: return access token
    zotero-auth-token)))

(defun zotero-auth-token-valid-p (token)
  "Return t if TOKEN is valid, else return nil.

An access token is considered valid if it is a struct type called
‘zotero-auth-token’ that contains at least the two slots
\"userID\" (the user ID) and \"oauth_token_secret\" (the API
key)."
  (when (and (zotero-auth-token-p token) (zotero-auth-token-token-secret token) (zotero-auth-token-userid token)) t))

(defun zotero-auth-api-key (token)
  "Return the Zotero API key in TOKEN.

In Zotero's case the token and secret are just the same Zotero
API key."
  (zotero-auth-token-token-secret token))

(defun zotero-auth-userid (token)
  "Return the Zotero user ID in TOKEN.

Zotero will send the userID associated with the key along too."
  (zotero-auth-token-userid token))

(defun zotero-auth-username (token)
  "Return the Zotero username in TOKEN.

Zotero will send the username associated with the key along too."
  (zotero-auth-token-username token))

;;;; Functions

(defun zotero-auth--fetch-token (arg)
  "Advice replacing `oauth-fetch-token' in package `oauth.el'.
Fetch an access token, secret, user ID and username from the
service provider. The original function only fetches the access
token and secret."
  (with-current-buffer (oauth-do-request arg)
    ;; Move beyond blank line at end of headers.
    (goto-char (point-min))
    (while (progn
             (forward-line 1)
             (not (looking-at "^\r?\n"))))
    (forward-line 1)
    (let ((token (zotero-auth-token-create))
          (pairs (url-parse-query-string (buffer-substring (point) (point-max)))))
      (dolist (pair pairs)
        (cond
         ((equal (car pair) "oauth_token")
          (setf (zotero-auth-token-token token) (cadr pair)))
         ((equal (car pair) "oauth_token_secret")
          (setf (zotero-auth-token-token-secret token) (cadr pair)))
         ((equal (car pair) "userID")
          (setf (zotero-auth-token-userid token) (cadr pair)))
         ((equal (car pair) "username")
          (setf (zotero-auth-token-username token) (cadr pair)))))
      token)))

(defun zotero-auth--before-authorize-function ()
  "Function to be run before an OAuth authorization request."
  (advice-add #'oauth-fetch-token :override #'zotero-auth--fetch-token))

(defun zotero-auth--after-authorize-function ()
  "Function to be run after an OAuth authorization request."
  (advice-remove #'oauth-fetch-token #'zotero-auth--fetch-token))

(defun zotero-auth--save-token (token)
  "Save the TOKEN for future sessions and return it."
  (customize-save-variable 'zotero-auth-token token))

(provide 'zotero-auth)

;;; zotero-auth.el ends here
