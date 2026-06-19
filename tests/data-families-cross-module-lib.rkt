#lang rackton

;; Fixture for data-families-cross-module-test.rkt.  A data family and two
;; instances; an importer must recover the family tcon (with its inferred
;; kind) and the instance constructors to build and match values.

(provide (data-out Arr))

(data-family (Arr a))
(data-instance (Arr Boolean) (MkBits Integer))
(data-instance (Arr Integer) (MkInts String))
