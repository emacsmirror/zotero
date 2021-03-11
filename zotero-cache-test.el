(require 'zotero-cache)

(ert-deftest zotero-cache-read-access-p ()
  (should (zotero-cache-read-access-p '(:library t)))
  (should-not (zotero-cache-read-access-p '(:library :json-false)))
  (should-not (zotero-cache-read-access-p '(:library nil))))

(ert-deftest zotero-cache-write-access-p ()
  (should (zotero-cache-write-access-p '(:library t :write t)))
  (should-not (zotero-cache-write-access-p '(:library t :write :json-false)))
  (should-not (zotero-cache-write-access-p '(:library t :write nil))))

(ert-deftest zotero-cache-read-only-p ()
  (should (zotero-cache-read-only-p '(:library t :write :json-false)))
  (should (zotero-cache-read-only-p '(:library t :write nil)))
  (should-not (zotero-cache-read-only-p '(:library t :write t)))
  (should-not (zotero-cache-read-only-p '(:library :json-false :write :json-false)))
  (should-not (zotero-cache-read-only-p '(:library nil :write nil))))
