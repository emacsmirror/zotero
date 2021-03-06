(require 'zotero-lib)

(ert-deftest zotero-lib-mergable-plist-p ()
  (should (zotero-lib-mergable-plist-p '(:a 1 :b 2) '(:c 3)))
  (should (zotero-lib-mergable-plist-p '(:a 1 :b 2) '(:a 1 :c 3)))
  (should (zotero-lib-mergable-plist-p '(:a :json-false :c 3) '(:a 1 :b 2)))
  (should (zotero-lib-mergable-plist-p '(:a :json-empty :c 3) '(:a 1 :b 2)))
  (should (zotero-lib-mergable-plist-p '(:a 1 :b 2) '(:a :json-false :c 3)))
  (should (zotero-lib-mergable-plist-p '(:a 1 :b 2) '(:a :json-empty :c 3)))
  (should (not (zotero-lib-mergable-plist-p '(:a 1 :b 2) '(:a 2 :c 3)))))

(ert-deftest zotero-lib-merge-plist ()
  (should (equal (zotero-lib-merge-plist '(:a 1 :b 2) '(:c 3)) '(:a 1 :b 2 :c 3)))
  (should (equal (zotero-lib-merge-plist '(:a 1 :b 2) '(:a 1 :c 3)) '(:a 1 :b 2 :c 3)))
  (should (equal (zotero-lib-merge-plist '(:a :json-false :b 2) '(:a 1 :c 3)) '(:a 1 :b 2 :c 3)))
  (should (equal (zotero-lib-merge-plist '(:a :json-empty :b 2) '(:a 1 :c 3)) '(:a 1 :b 2 :c 3)))
  (should (equal (zotero-lib-merge-plist '(:a 1 :b 2) '(:a :json-false :c 3)) '(:a 1 :b 2 :c 3)))
  (should (equal (zotero-lib-merge-plist '(:a 1 :b 2) '(:a :json-empty :c 3)) '(:a 1 :b 2 :c 3))))

(ert-deftest zotero-lib-validate-isbn ()
  (should (zotero-lib-validate-isbn "ISBN 978-0-596-52068-7"))
  (should (zotero-lib-validate-isbn "ISBN-13: 978-0-596-52068-7"))
  (should (zotero-lib-validate-isbn "978 0 596 52068 7"))
  (should (zotero-lib-validate-isbn "9780596520687"))
  (should (zotero-lib-validate-isbn "ISBN-10 0-596-52068-9"))
  (should (zotero-lib-validate-isbn "0-596-52068-9"))
  (should-not (zotero-lib-validate-isbn "978059652068")) ; 12 digits
  (should-not (zotero-lib-validate-isbn "9770596520687")) ; wrong ISBN-13 prefix
  (should-not (zotero-lib-validate-isbn "9780596520680"))) ; wrong checksum of the final digit

(ert-deftest zotero-lib-validate-arxiv ()
  (should (zotero-lib-validate-arxiv "hep-th/9901001"))
  (should (zotero-lib-validate-arxiv "hep-th/9901001v1"))
  (should (zotero-lib-validate-arxiv "math.CA/0611800v2"))
  (should (zotero-lib-validate-arxiv "arXiv:hep-th/9901001"))
  (should (zotero-lib-validate-arxiv "arXiv:hep-th/9901001v1"))
  (should (zotero-lib-validate-arxiv "arXiv:math/0611800v2"))
  (should (zotero-lib-validate-arxiv "0704.0001"))
  (should (zotero-lib-validate-arxiv "0704.0001v1" ))
  (should (zotero-lib-validate-arxiv "1412.7878"))
  (should (zotero-lib-validate-arxiv "1501.00001"))
  (should (zotero-lib-validate-arxiv "9912.12345v2"))
  (should (zotero-lib-validate-arxiv "arXiv:0704.0001"))
  (should (zotero-lib-validate-arxiv "arXiv:0704.0001v1"))
  (should (zotero-lib-validate-arxiv "arXiv:1412.7878"))
  (should (zotero-lib-validate-arxiv "arXiv:1501.00001"))
  (should (zotero-lib-validate-arxiv "arXiv:9912.12345v2")))

(ert-deftest zotero-lib-validate-doi ()
  (should (zotero-lib-validate-doi "doi:10.1000/182"))
  (should (zotero-lib-validate-doi "https://doi.org/10.1000/182"))
  (should (zotero-lib-validate-doi "https://dx.doi.org/10.1000/182"))
  (should (zotero-lib-validate-doi "10.1000/123456"))
  (should (zotero-lib-validate-doi "10.1038/issn.1476-4687"))
  (should (zotero-lib-validate-doi "10.1000/456#789"))
  (should-not (zotero-lib-validate-doi "978-12345-99990"))) ; ISBN
