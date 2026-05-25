#lang rackton

;; No (provide ...) form — under the new semantics this should export
;; nothing.  Importers see neither `foo` nor `bar` at runtime, and
;; neither scheme appears in the rackton-schemes sidecar.

(define foo 1)
(define bar 2)
