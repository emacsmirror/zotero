(require 'zotero-json)

(ert-deftest zotero-json-read ()
  "Tests JSON reading."
  (should (equal (zotero-json-read "{\"key\":\"value\"}") '(:key "value")))
  (should (equal (zotero-json-read "{\"key\":null}") '(:key nil)))
  (should (equal (zotero-json-read "{\"key\":false}") '(:key :json-false)))
  (should (equal (zotero-json-read "{\"key\":{}}") '(:key :json-empty)))
  (should (equal (let ((buffer (generate-new-buffer "temp")))
                   (with-current-buffer buffer (insert "{\"key\":\"value\"}"))
                   (prog1
                       (zotero-json-read buffer)
                     (kill-buffer buffer)))
                 '(:key "value")))
  (should (equal (zotero-json-read (lambda () "{\"key\":\"value\"}")) '(:key "value"))))

(ert-deftest zotero-json-encode-to-array ()
  "Tests JSON encoding."
  (should (equal (zotero-json-encode-to-array '(:key :value)) "[{\"key\":\"value\"}]"))
  (should (equal (zotero-json-encode-to-array '(:key nil)) "[{\"key\":null}]"))
  (should (equal (zotero-json-encode-to-array '(:key :json-false)) "[{\"key\":false}]"))
  (should (equal (zotero-json-encode-to-array '(:key :json-empty)) "[{\"key\":{}}]"))
  (should (equal (zotero-json-encode-to-array "(:key :value)") "[{\"key\":\"value\"}]"))
  (should (equal (let ((buffer (generate-new-buffer "temp")))
                   (with-current-buffer buffer (insert "(:key :value)"))
                   (prog1
                       (zotero-json-encode-to-array buffer)
                     (kill-buffer buffer)))
                 "[{\"key\":\"value\"}]"))
  (should (equal (zotero-json-encode-to-array (lambda () (list :key :value))) "[{\"key\":\"value\"}]"))
  (should (equal (zotero-json-encode-to-array '(:key :value) '(:key nil) '(:key :json-false) '(:key :json-empty))
                 "[{\"key\":\"value\"},{\"key\":null},{\"key\":false},{\"key\":{}}]")))
