#lang racket/base

;; Phase 58: codegen inlining of small monomorphized impls.
;;
;; Phase 57 made each user-defined positional instance method
;; available as a named $method:Tcon global and pointed concrete
;; call sites at it.  Phase 58 takes the next step: when the impl's
;; body is small and contains no class-method calls, codegen
;; substitutes the args into the body at the call site instead of
;; emitting a function call.
;;
;; The optimization is invisible to behavior; the elaborator
;; exposes a parameter listing the inlined call sites so this test
;; can verify it fires.

(require rackunit
         "../main.rkt")

(rackton
  ;; A user-defined class with a small impl body — exactly the
  ;; shape the inliner is meant to specialize.
  (define-class (Tag a)
    (: tag-of (-> a Integer)))

  (define-instance (Tag Integer)
    (define (tag-of x) (+ x 100)))

  (define-instance (Tag Boolean)
    (define (tag-of b) (if b 1 0)))

  (: r-int Integer)
  (define r-int (tag-of 7))

  (: r-bool Integer)
  (define r-bool (tag-of #t))

  ;; Polymorphic call site — type is `a` until the caller resolves
  ;; it, so neither monomorphization nor inlining can fire here.
  (: poly-tag ((Tag a) => (-> a Integer)))
  (define (poly-tag x) (tag-of x))

  (: r-poly Integer)
  (define r-poly (poly-tag #f))

  ;; A larger impl body — exercises the "not small enough" path so
  ;; the optimization stays monomorphized-but-not-inlined.
  (define-class (Score a)
    (: score (-> a Integer)))

  (define-instance (Score Integer)
    (define (score n)
      (if (< n 0)
          0
          (if (< n 10)
              (+ n 1)
              (if (< n 100)
                  (+ n 10)
                  (+ n 100))))))

  (: r-score Integer)
  (define r-score (score 5)))

(test-case "small monomorphized impl returns correct value"
  (check-equal? r-int 107)
  (check-equal? r-bool 1))

(test-case "polymorphic call still works"
  (check-equal? r-poly 0))

(test-case "large impl still works (not necessarily inlined)"
  (check-equal? r-score 6))

(test-case "tag-of Integer was inlined at the (tag-of 7) call site"
  (define recorded (rackton-inlined-sites))
  (check-true
   (for/or ([entry (in-list recorded)])
     (and (eq? (car entry) 'tag-of)
          (regexp-match? #rx"Integer" (symbol->string (cdr entry)))))
   (format "no Integer tag-of inlining in: ~v" recorded)))
