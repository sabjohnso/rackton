#lang rackton

;; Fixture for constraint-families-cross-module-test.rkt: a library
;; exporting a promoted list type and a higher-order constraint family
;; over it.  The importer must recover the family from the sidecar.

(provide (data-out TList))

(data (TList a) TNil (TCons a (TList a)))

(constraint-family (All c xs)
  [c TNil         = ]
  [c (TCons x xs) = (c x) (All c xs)])
