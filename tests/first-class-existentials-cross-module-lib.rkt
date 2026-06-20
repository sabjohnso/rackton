#lang rackton

;; Fixture for first-class-existentials-cross-module-test.rkt.  A
;; first-class existential type travels across the module boundary in two
;; ways: a VALUE of existential type, and a function whose SIGNATURE
;; mentions one.  The importer must recover the `texists` from the sidecar
;; (it round-trips through the type codec) to use both.

(provide a-showable render-showable)

;; A value whose witness type (Integer) is hidden behind `Show`.
(: a-showable (Exists (a) ((Show a) => a)))
(define a-showable (ann 42 (Exists (a) ((Show a) => a))))

;; A consumer that opens the existential and renders it.
(: render-showable (-> (Exists (a) ((Show a) => a)) String))
(define (render-showable e) (open e (a x) (show x)))
