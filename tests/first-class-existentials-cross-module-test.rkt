#lang racket/base

;; A first-class existential crosses a module boundary: the importer
;; recovers both an exported VALUE of existential type and a function
;; whose signature mentions one (their `texists` round-trips through the
;; sidecar type codec), then opens the imported value locally.

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

(rackton
  (require "first-class-existentials-cross-module-lib.rkt")

  ;; Use the imported consumer on the imported existential value.
  (: shown String)
  (define shown (render-showable a-showable))

  ;; Pack a fresh value locally and feed it to the imported consumer —
  ;; the locally-written (Exists …) must match the imported signature.
  (: shown-local String)
  (define shown-local
    (render-showable (ann "hey" (Exists (a) ((Show a) => a)))))

  ;; Open the imported existential value directly.
  (: opened String)
  (define opened (open a-showable (a x) (show x))))

(test-case "an imported existential value and consumer work across modules"
  (check-equal? shown "42")
  (check-equal? shown-local "\"hey\"")
  (check-equal? opened "42"))
