#lang rackton

;; Fixture for named-fields-cross-module-test.rkt: a library exporting a
;; data type whose constructors have named fields.  An importer must
;; recover the field names from the sidecar to use keyword construction.

(provide (data-out Tree))

(data (Tree a)
  (Leaf [value : a])
  (Branch [left : (Tree a)] [right : (Tree a)]))
