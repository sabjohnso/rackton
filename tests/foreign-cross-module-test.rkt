#lang racket/base

;; foreign works inside a #lang rackton module, and a foreign binding's
;; declared type crosses the rackton-schemes sidecar so a client
;; type-checks uses of both the wrapper and the foreign binding itself.

(require rackunit
         "../main.rkt")

(rackton
  (require "foreign-lib.rkt")

  (: w Boolean)
  (define w (mentions-rackton "I love rackton"))

  (: nw Boolean)
  (define nw (mentions-rackton "I love ocaml"))

  ;; use the re-exported foreign binding directly (its type came via the sidecar)
  (: direct Boolean)
  (define direct (contains? "abcdef" "cde")))

(test-case "foreign used inside a #lang rackton module, across the boundary"
  (check-true  w)
  (check-false nw)
  (check-true  direct))
