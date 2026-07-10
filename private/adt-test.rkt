#lang racket/base

;; Tests for private/adt.rkt: the runtime form `define-data-ctor` that
;; emits struct, value binding, and match expander for each data
;; constructor.

(module+ test
  (require rackunit
           rackcheck
           racket/match
           "adt.rkt")

  ;; A nullary constructor (like Maybe's `None`).
  (define-data-ctor None 0)

  ;; A nullary constructor's bare reference is the singleton VALUE, not
  ;; a procedure (contrast `Some`/`Pair` below, whose bare references
  ;; ARE constructor procedures).
  (check-false (procedure? None))
  ;; Two `None`s are equal because the struct is transparent.
  (check-equal? None None)

  ;; `None` works as a match pattern.
  (check-equal? (match None [(None) 'matched]) 'matched)

  ;; A unary constructor (like Maybe's `Some`).
  (define-data-ctor Some 1)

  ;; `(Some 42)` constructs a value; field is reachable through match.
  (check-equal? (match (Some 42) [(Some x) x]) 42)

  ;; `Some` bare is the constructor procedure.
  (check-equal? ((lambda (f) (f 7)) Some) (Some 7))

  ;; Nested patterns work.
  (check-equal? (match (Some (Some 3))
                  [(Some (Some n)) n]
                  [_ #f])
                3)

  ;; A binary constructor (for a tuple-like Pair type).
  (define-data-ctor Pair 2)

  (check-equal? (match (Pair 1 2)
                  [(Pair a b) (+ a b)])
                3)

  ;; Bare reference to a binary constructor is the procedure.
  (check-equal? ((lambda (mk) (mk 'x 'y)) Pair) (Pair 'x 'y))

  ;; An n-ary constructor is a first-class CURRIED value: its HM type is
  ;; `(-> a b T)`, so it must apply stepwise as well as all at once.
  ;; Stepwise application `((Pair 1) 2)` builds the same value as `(Pair 1 2)`.
  (check-equal? ((Pair 1) 2) (Pair 1 2))
  ;; A direct partial application returns a procedure awaiting the rest.
  (check-true (procedure? (Pair 1)))
  (check-equal? ((Pair 1) 2) (Pair 1 2))
  ;; Passed by reference and applied stepwise by a higher-order function.
  (check-equal? ((lambda (mk) ((mk 'x) 'y)) Pair) (Pair 'x 'y))
  ;; …and applied all at once through the same reference.
  (check-equal? ((lambda (mk) (mk 'x 'y)) Pair) (Pair 'x 'y))

  ;; A ternary constructor curries at every prefix arity.
  (define-data-ctor Triple 3)
  (check-equal? (((Triple 1) 2) 3)  (Triple 1 2 3))
  (check-equal? ((Triple 1 2) 3)    (Triple 1 2 3))
  (check-equal? ((Triple 1) 2 3)    (Triple 1 2 3))
  (check-equal? (Triple 1 2 3)      (Triple 1 2 3))

  ;; LAW (grouping-independence): for a constructor of arity n, every way
  ;; of splitting a saturated application into curried steps produces an
  ;; equal value.  The stored field values are opaque to the constructor,
  ;; so the law must hold for arbitrary arguments — a property, not a
  ;; handful of fixed cases.
  (define gen:arg (gen:integer-in -100000 100000))
  (check-property
   (property pair-grouping-independence ([a gen:arg] [b gen:arg])
     (equal? ((Pair a) b) (Pair a b))))
  (check-property
   (property triple-grouping-independence ([a gen:arg] [b gen:arg] [c gen:arg])
     (and (equal? (((Triple a) b) c) (Triple a b c))
          (equal? ((Triple a b) c)   (Triple a b c))
          (equal? ((Triple a) b c)   (Triple a b c))))))
