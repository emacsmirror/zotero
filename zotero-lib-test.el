(ert-deftest zotero-lib--encode-object ()
  "Tests the."
  (should (equal (zotero-lib--encode-object '(:key :value)) "[{\"key\":\"value\"}]"))
  (should (equal (zotero-lib--encode-object '(:key nil)) "[{\"key\":null}]"))
  (should (equal (zotero-lib--encode-object '(:key :json-false)) "[{\"key\":false}]"))
  (should (equal (zotero-lib--encode-object '(:key :json-empty)) "[{\"key\":{}}]")))

(ert-deftest zotero-lib--read-json ()
  "Tests the."
  (should (equal (zotero-lib--read-json "{\"key\":\"value\"}") '(:key "value")))
  (should (equal (zotero-lib--read-json "{\"key\":null}") '(:key nil)))
  (should (equal (zotero-lib--read-json "{\"key\":false}") '(:key :json-false)))
  (should (equal (zotero-lib--read-json "{\"key\":{}}") '(:key :json-empty)))
  (should (equal (zotero-lib--read-json "[{\"key\":\"value\"}, {\"key\":null}, {\"key\":false}, {\"key\":{}}]") [(:key "value") (:key nil) (:key :json-false) (:key :json-empty)])))
