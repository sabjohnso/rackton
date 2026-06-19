#lang rackton

;; Fixture for constraint-synonyms-cross-module-test.rkt: a library that
;; defines a constraint synonym.  An importer must recover it from the
;; sidecar so the synonym expands in the importer's own contexts.

(define-constraint (Stringy a) (Show a) (Eq a))
