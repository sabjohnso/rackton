#lang racket/base

;; Codegen monomorphization for positional class-method
;; calls.  When the dispatch type resolves to a concrete tcon at
;; compile time, the call site is rewritten to invoke the
;; per-instance impl directly instead of going through the
;; runtime dispatch table.
;;
;; The optimization is invisible to behavior — every existing test
;; still passes — but the elaborator exposes a parameter listing
;; the call sites it monomorphized so this test can verify the
;; optimization actually fires.

(require rackunit
         "../main.rkt")

;; ----- behavior: existing class-method semantics preserved ------

(rackton
  ;; A user-defined class so the test exercises a non-prelude
  ;; instance — prelude instances use Racket-escape bodies and are
  ;; intentionally NOT monomorphized.
  (define-class (Tag a)
    (: tag-of (-> a Integer)))

  (define-instance (Tag Integer)
    (define (tag-of x) (+ x 100)))

  (define-instance (Tag Boolean)
    (define (tag-of b) (if b 1 0)))

  (: r-tag-int Integer)
  (define r-tag-int (tag-of 7))

  (: r-tag-bool Integer)
  (define r-tag-bool (tag-of #t))

  (: poly-tag ((Tag a) => (-> a Integer)))
  (define (poly-tag x) (tag-of x))

  (: r-poly Integer)
  (define r-poly (poly-tag #f)))

(test-case "concrete (tag-of Integer) still returns the right value"
  (check-equal? r-tag-int 107))

(test-case "concrete (tag-of Boolean) dispatches to the right instance"
  (check-equal? r-tag-bool 1))

(test-case "polymorphic tag-of still dispatches at runtime"
  (check-equal? r-poly 0))

;; ----- optimization visibility ---------------------------------

(test-case "concrete user class-method call sites are monomorphized"
  ;; The parameter is populated at elaborate-time.  Each entry is a
  ;; (method-name . impl-name) cons recording one resolved site.
  ;; We expect at least one tag-of:Integer entry.
  (define recorded (rackton-monomorphized-sites))
  (check-true
   (for/or ([entry (in-list recorded)])
     (and (eq? (car entry) 'tag-of)
          (regexp-match? #rx"Integer" (symbol->string (cdr entry)))))
   (format "no Integer tag-of monomorphization in: ~v" recorded)))
