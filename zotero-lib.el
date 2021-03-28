;;; zotero-lib.el --- Library for the Zotero API  -*- lexical-binding: t; -*-

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
;; Helper functions for Zotero.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 's)
(require 'seq)
(require 'xml)

;;;; Variables
(defconst zotero-lib-arxiv-regexp (rx
                                   string-start
                                   (zero-or-one "arXiv") ; Optional arXiv identifier
                                   (zero-or-one ?:)
                                   (zero-or-one space)
                                   (group
                                    (or
                                     ;; Identifier scheme since 1 April 2007
                                     (and
                                      (repeat 4 digit) ; two-digit year and month number
                                      ?.
                                      (repeat 4 5 digit) ; zero-padded sequence number of 4- or 5-digits
                                      (zero-or-one (and ?v (one-or-more digit)))) ; a literal v followed by a version number of 1 or more digits
                                     (and
                                      (one-or-more (any lower ?-)) ; archive
                                      (zero-or-one (and ?. (repeat 2 upper))) ; optional two-letter subject class
                                      ?/
                                      (repeat 7 digit) ; two-digit year and month number, and three-digit sequence number
                                      (zero-or-one (and ?v (one-or-more digit)))))) ; a literal v followed by a version number of 1 or more digits
                                   string-end)
  "A regular expression matching the arXiv identifier format.
A leading \"arXiv\" identifier is allowed. See URL
`https://arxiv.org/help/arxiv_identifier'")

(defconst zotero-lib-doi-regexp
  (rx string-start
      (zero-or-one (or "https://doi.org/" ; Optional actionable link
                       "https://dx.doi.org/" ; Earlier syntax, still supported but no longer preferred
                       (and "doi" ; Optional DOI identifier
                            (zero-or-one ?:)
                            (zero-or-one space))))
      (group
       (or
        (and
         "10."
         (repeat 4 9 digit)
         ?/
         (one-or-more (any print)))
        (and
         "10.002/"
         (one-or-more (not space)))))
      string-end)
  "A regular expression matching the modern Crossref DOI format.
See URL `https://www.doi.org/doi_handbook/2_Numbering.html' and
URL
`https://www.crossref.org/blog/dois-and-matching-regular-expressions/'.")

(defconst zotero-lib-isbn10-regexp (rx
                                    string-start
                                    (zero-or-one "ISBN") ; Optional ISBN/ISBN-10/ISBN-13 identifier
                                    (zero-or-one (and "-1" (any "03")))
                                    (zero-or-one ?:)
                                    (zero-or-one space)
                                    (group
                                     (and
                                      (repeat 1 5 digit) ;1-5 digit group identifier
                                      (zero-or-one (any space ?-))
                                      (one-or-more digit) ; Publisher and title identifiers
                                      (zero-or-one (any space ?-))
                                      (one-or-more digit)
                                      (zero-or-one (any space ?-))
                                      (or digit ?X))) ; Check digit
                                    string-end)
  "A regular expression matching the ISBN-10 format.
A leading \"ISBN\" identifier is allowed, and ISBN parts can
optionally be separated by hyphens or spaces.")

(defconst zotero-lib-isbn13-regexp (rx
                                    string-start
                                    (zero-or-one "ISBN") ; Optional ISBN/ISBN-10/ISBN-13 identifier
                                    (zero-or-one (and "-1" (any "03")))
                                    (zero-or-one ?:)
                                    (zero-or-one space)
                                    (group
                                     (and
                                      "97" (any "89") ; ISBN-13 prefix
                                      (zero-or-one (any space ?-))
                                      (repeat 1 5 digit) ; 1-5 digit group identifier
                                      (zero-or-one (any space ?-))
                                      (one-or-more digit) ; Publisher and title identifiers
                                      (zero-or-one (any space ?-))
                                      (one-or-more digit)
                                      (zero-or-one (any space ?-))
                                      digit))  ; Check digit
                                    string-end)
  "A regular expression matching the ISBN-13 format.
The ISBN can be in either the older ISBN-10 or the current
ISBN-13 format. A leading \"ISBN\" identifier is allowed, and
ISBN parts can optionally be separated by hyphens or spaces.")

(defconst zotero-lib-pmid-regexp
  (rx string-start
      (zero-or-one (and "PMID" ; Optional PMID identifier
                        (zero-or-one ?:)
                        (zero-or-one space)))
      (group
       (repeat 7 8 digit))
      string-end)
  "A regular expression probably matching a PubMed ID.")

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

(defun zotero-lib-mergable-plist-p (plist1 plist2)
  "Return non-nil if PLIST1 and PLIST2 can be merged without conflicts.
Two plists are considered mergable when the same keys don't have different values."
  (cl-loop for (key val) on plist1 by #'cddr
           always (or (not (plist-member plist2 key))
                      (eq val :json-false)
                      (eq val :json-empty)
                      (eq (plist-get plist2 key) :json-false)
                      (eq (plist-get plist2 key) :json-empty)
                      (equal val (plist-get plist2 key)))))

(defun zotero-lib-merge-plist (plist1 plist2)
  "Merge PLIST2 into PLIST1."
  (cl-loop for (key val) on plist2 by #'cddr do
           (unless (or (eq val :json-false)
                       (eq val :json-empty))
             (plist-put plist1 key val))
           finally return plist1))

(defun zotero-lib-validate-isbn (string)
  "Check if STRING is a valid ISBN.
Return the ISBN if it is valid, else return nil.

Argument STRING can be in either the older ISBN-10 or the current
ISBN-13 format. A leading \"ISBN\" identifier is allowed, and
ISBN parts can optionally be separated by hyphens or spaces.

The format is validated by a regexp and the validity of the final
digit is checked using a checksum algorithm."
  ;; An ISBN is a unique identifier for commercial books and book-like products.
  ;; The 10-digit ISBN format was published as an international standard, ISO
  ;; 2108, in 1970. All ISBNs assigned since January 1, 2007 are 13 digits.
  ;; ISBN-10 and ISBN-13 numbers are divided into four or five elements,
  ;; respectively. Three of the elements are of variable length; the remaining
  ;; one or two elements are of fixed length. All five parts are usually
  ;; separated with hyphens or spaces. A brief description of each element
  ;; follows:
  ;; - 13-digit ISBNs start with the prefix 978 or 979.
  ;; - The group identifier identifies the language-sharing country group. It
  ;;   ranges from one to five digits long.
  ;; - The publisher identifier varies in length and is assigned by the national
  ;;   ISBN agency.
  ;; - The title identifier also varies in length and is selected by the
  ;;   publisher.
  ;; - The final character is called the check digit, and is computed using a
  ;;   checksum algorithm. An ISBN-10 check digit can be either a number from 0
  ;;   to 9 or the letter X (Roman numeral for 10), whereas an ISBN-13 check
  ;;   digit ranges from 0 to 9. The allowed characters are different because
  ;;   the two ISBN types use different checksum algorithms.

  ;; Although a regular expression can check that the final digit uses a valid
  ;; character (a digit or X), it cannot determine whether it’s correct for the
  ;; ISBN’s checksum. One of two checksum algorithms (determined by whether
  ;; you’re working with an ISBN-10 or ISBN-13) are used to provide some level
  ;; of assurance that the ISBN digits haven’t been accidentally transposed or
  ;; otherwise entered incorrectly.
  (when-let ((match (or (cadr (s-match zotero-lib-isbn13-regexp string))
                        (cadr (s-match zotero-lib-isbn10-regexp string))))
             (isbn (s-replace-all '((" " . "") ("-" . "")) match))
             ;; The elisp regexps don't provide lookaheads to limit the string
             ;; length, so the total length of the isbn has to be checked here
             (valid-isbn-p (or (and (eq (length isbn) 10)
                                    (s-matches-p zotero-lib-isbn10-regexp isbn))
                               (and (eq (length isbn) 13)
                                    (s-matches-p zotero-lib-isbn13-regexp isbn))))
             (last (s-right 1 isbn))
             (list (seq-into isbn 'list))
             (first (butlast list))
             (strings (seq-map #'string first))
             (numbers (seq-map #'string-to-number strings)))
    (pcase (length numbers)
      ;; ISBN-10 checksum
      (9
       ;; Multiply each of the first 9 isbn by a number in the descending sequence from 10 to 2
       (let* ((multiplied (seq-mapn #'* '(10 9 8 7 6 5 4 3 2) numbers))
              ;; Sum the results
              (sum (seq-reduce #'+ multiplied 0))
              ;; Divide the sum by 11
              (remainder (% sum 11))
              ;; Subtract the remainder from 11
              (result (- 11 remainder))
              ;; If the result is 11, use the number 0; if 10, use the letter X
              (check (pcase result
                       (11 "0")
                       (10 "X")
                       (_ (number-to-string result)))))
         (when (equal check last) isbn)))
      ;; ISBN-13 checksum
      (12
       ;; Multiply each of the first 12 isbn by 1 or 3, alternating as you move from left to right
       (let* ((multiplied (seq-mapn #'* '(1 3 1 3 1 3 1 3 1 3 1 3) numbers))
              ;; Sum the results
              (sum (seq-reduce #'+ multiplied 0))
              ;; Divide the sum by 10
              (remainder (% sum 10))
              ;; Subtract the remainder from 10
              (result (- 10 remainder))
              ;; If the result is 10, use the number 0
              (check (pcase result
                       (10 "0")
                       (_ (number-to-string result)))))
         (when (equal check last) isbn))))))

(defun zotero-lib-validate-arxiv (string)
  "Check if STRING is a valid arXiv identifier.
Return the arXiv identifier if it is valid, else return nil.

The scheme used by arXiv was changed in April 2007. Argument
STRING can be in either the old scheme (from 1999 to March 2007)
or the new scheme (since 1 April 2007). A leading \"arXiv\"
identifier is allowed.

The format is validated by a regexp."
  (when-let ((match (cadr (s-match zotero-lib-arxiv-regexp string))))
    match))

(defun zotero-lib-validate-doi (string)
  "Check if STRING is a valid Crossref DOI.
Return the DOI if it is valid, else return nil.

A leading \"doi\" identifier or a link (for example,
https://doi.org/10.1000/182) is allowed.

The format is validated by a regexp."
  (when-let ((match (cadr (s-match zotero-lib-doi-regexp string))))
    match))

(defun zotero-lib-validate-pmid (string)
  "Check if STRING is a valid PubMed ID (PMID).
Return the PMID if it is valid, else return nil.

A leading \"PMID\" identifier is allowed.

The format is validated by a regexp."
  (when-let ((match (cadr (s-match zotero-lib-pmid-regexp string))))
    match))

(defun zotero-lib-html-to-unicode (string)
  "Replace HTML entities with unicode in STRING."
  (with-temp-buffer
    (save-excursion (insert string))
    (xml-parse-string)))

(defun zotero-lib-remove-html-tags (string)
  "Remove all HTML tags from STRING."
  (let ((regexp "<[[:alnum:][:blank:]/=\"-]*?>"))
    (replace-regexp-in-string regexp "" string)))

(provide 'zotero-lib)

;;; zotero-lib.el ends here
