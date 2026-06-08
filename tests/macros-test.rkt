#lang racket/base

;; User-defined syntax (macros) in Rackton.
;;
;; Rackton macros ARE Racket macros: a macro-definition form inside a
;; Rackton block introduces a real, hygienic, phase-correct Racket
;; transformer.  Before type inference runs, a front phase drives the
;; Racket expander over the Rackton body, expanding every user-macro use
;; into core Rackton forms while leaving Rackton's own forms, variables,
;; applications, and literals intact — so `parse-top` reads exactly what
;; the user wrote, modulo macro expansion.
;;
;; This file pins the FOUNDATION: a macro can be defined and used, uses
;; nest, and expansion is hygienic (a macro-introduced binder never
;; captures a user binding of the same name).  Later files extend to
;; phase-1 helpers, macro-defining macros, top-level referential
;; transparency, and cross-module macro export.

(require rackunit
         (for-syntax racket/base)   ; phase-1 bindings for procedural transformers
         "../main.rkt")

(rackton
  ;; ----- a macro can be defined and used -----------------------------
  (define-syntax-rule (twice x) (+ x x))

  (: four Integer)
  (define four (twice 2))

  ;; ----- uses nest -----------------------------------------------------
  (define-syntax-rule (inc x) (+ x 1))

  (: six Integer)
  (define six (inc (inc (inc 3))))

  ;; ----- hygiene: a macro-introduced binder must not capture ----------
  ;; `add-via-tmp` introduces `tmp`.  The use site also binds `tmp` and
  ;; passes it as the second argument.  Hygienic expansion keeps the two
  ;; `tmp`s distinct, so the result is 1 + 100 = 101.  An unhygienic
  ;; (datum-keyed) expansion would capture, binding the inner reference
  ;; to 1 and yielding 1 + 1 = 2.
  (define-syntax-rule (add-via-tmp a b)
    (let ([tmp a]) (+ tmp b)))

  (: hygiene-check Integer)
  (define hygiene-check
    (let ([tmp 100])
      (add-via-tmp 1 tmp))))

;; ----- checks -----------------------------------------------------------

(test-case "a user macro expands and runs"
  (check-equal? four 4))

(test-case "macro uses nest"
  (check-equal? six 6))

(test-case "expansion is hygienic — introduced binder does not capture"
  (check-equal? hygiene-check 101))

;; ----- phase-1 (procedural) transformers --------------------------------
;; A `define-syntax` with a procedural body runs real Racket code at compile
;; time.  `triple-literal` computes 3*n in the transformer and splices the
;; resulting literal, proving the macro layer is the genuine Racket expander
;; (phase-1 evaluation), not a template substituter.
(rackton
  (define-syntax (triple-literal stx)
    (syntax-case stx ()
      [(_ n)
       (let ([k (syntax->datum #'n)])
         (datum->syntax stx (* 3 k)))]))

  (: fifteen Integer)
  (define fifteen (triple-literal 5)))

(test-case "procedural transformer runs at phase 1"
  (check-equal? fifteen 15))

;; ----- macro-defining macros --------------------------------------------
;; `define-doubler` expands to a NEW `define-syntax-rule`.  Using it must
;; introduce a usable macro (`dbl`), which the front phase has to discover
;; even though the definition is produced by expansion, not written directly.
(rackton
  (define-syntax-rule (define-doubler name op)
    (define-syntax-rule (name x) (op x x)))

  (define-doubler dbl +)

  (: eight Integer)
  (define eight (dbl 4)))

(test-case "a macro can define another macro"
  (check-equal? eight 8))
