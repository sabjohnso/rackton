#lang racket/base

;; A `(require (for-syntax …))` written *inside* a Rackton block must wire up
;; the macro-transformer phase, so a procedural transformer's body can call
;; `syntax-case`/`datum->syntax`/… — exactly as documented in
;; scribblings/reference/macros.scrbl's "Procedural macros" section.
;;
;; The regression this pins: front-phase macro expansion runs *during*
;; elaboration, before the compiled-output require reaches the module, so the
;; block-internal for-syntax require was arriving too late and the transformer
;; body saw `syntax-case` unbound.  `expand-user-macros` now lifts the block's
;; for-syntax requires up front.
;;
;; Crucially, this module's *own* require does NOT pull in
;; `(for-syntax racket/base)` — that would mask the bug by supplying the
;; phase-1 bindings from the enclosing module.  The phase-1 toolbox must come
;; only from the require inside the `(rackton …)` block.

(require rackunit
         "../main.rkt")

(rackton
  (provide fifteen doubled-triple both)

  ;; The phase-1 toolbox is required from *inside* the block.
  (require (for-syntax racket/base))

  ;; A procedural transformer computes 3*n at compile time and splices the
  ;; resulting literal — its body runs at phase 1 and needs `syntax-case` etc.
  (define-syntax (triple-literal stx)
    (syntax-case stx ()
      [(_ n)
       (let ([k (syntax->datum #'n)])
         (datum->syntax stx (* 3 k)))]))

  (: fifteen Integer)
  (define fifteen (triple-literal 5))

  ;; A pattern macro in the same block, composed with the procedural one.
  (define-syntax-rule (double x) (+ x x))

  (: doubled-triple Integer)
  (define doubled-triple (double (triple-literal 4)))

  ;; Hygiene still holds with the block-internal for-syntax require in play.
  (define-syntax-rule (add-via-tmp a b)
    (let ([tmp a]) (+ tmp b)))

  (: both Integer)
  (define both
    (let ([tmp 100])
      (add-via-tmp 1 tmp))))

(test-case "block-internal (require (for-syntax racket/base)) enables a procedural transformer"
  (check-equal? fifteen 15))

(test-case "procedural and pattern macros compose under a block-internal for-syntax require"
  (check-equal? doubled-triple 24))

(test-case "expansion stays hygienic with a block-internal for-syntax require"
  (check-equal? both 101))
