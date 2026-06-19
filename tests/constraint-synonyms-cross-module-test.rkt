#lang racket/base

;; A constraint synonym crosses a module boundary: the importer recovers
;; it from the sidecar and expands it both as a hypothesis (the body may
;; use the components) and as a goal (the call demands them).

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

(rackton
  (require "constraint-synonyms-cross-module-lib.rkt")

  (: described ((Stringy a) => (-> a String)))
  (define (described x) (show x))

  (: out String)
  (define out (described 5))
  (: shown String)
  (define shown (show 5)))

(test-case "an imported constraint synonym expands in the importer"
  (check-equal? out shown))
