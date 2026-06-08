#lang racket/base

;; Top-level referential transparency (Option B) for Rackton macros.
;;
;; A macro that references a top-level Rackton binding must resolve to that
;; binding at the macro's DEFINITION site — even when the use site shadows
;; the name with a local binding.  This is the hallmark of hygienic macros
;; (referential transparency).  It requires Rackton's top-level names to be
;; expander-visible, so the reference carries definition-site binding
;; identity rather than being resolved by bare symbol at the use site.
;;
;; Local-binding hygiene (a macro's introduced binders not capturing user
;; names) is covered in macros-test.rkt; this file pins the dual property
;; for references to top-level names.

(require rackunit
         "../main.rkt")

(rackton
  (define (base-value n) (+ n 100))

  ;; This macro references the top-level `base-value`.
  (define-syntax-rule (use-base x) (base-value x))

  ;; (1) A macro may reference a user-defined top-level binding at all.
  (: rt-plain Integer)
  (define rt-plain (use-base 5))

  ;; (2) Referential transparency: a use-site shadow of `base-value` does
  ;; NOT capture the macro's reference — it still calls the top-level one,
  ;; so the result is 5 + 100 = 105, not 5 - 100 = -95.
  (: rt-shadowed Integer)
  (define rt-shadowed
    (let ([base-value (lambda (n) (- n 100))])
      (use-base 5))))

(test-case "a macro can reference a top-level binding"
  (check-equal? rt-plain 105))

(test-case "use-site shadowing does not capture a macro's top-level reference"
  (check-equal? rt-shadowed 105))
