#lang racket/base

;; A standalone type family crosses a module boundary: its clauses (and
;; inferred kind) travel via the `rackton-schemes` sidecar, so the
;; importer reduces `(Other Red)` to `Integer` and `(Tag Green)` to
;; `Integer` exactly as the defining module would.

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

;; Reaching the test below means this block kind-checked AND the imported
;; families reduced — each define's type must match the reduced family type.
(rackton
  (require "type-families-cross-module-lib.rkt")

  (: oi (Other Red))     ; closed family: Other Red ⇒ Integer
  (define oi 5)
  (: os (Other Green))   ; Other Green ⇒ String
  (define os "hi")

  (: tg (Tag Green))     ; open family: Tag Green ⇒ Integer
  (define tg 7))

(test-case "imported standalone families reduce in the importer"
  (check-equal? oi 5)
  (check-equal? os "hi")
  (check-equal? tg 7))
