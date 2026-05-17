#lang racket/base

;; Tests for private/adt.rkt: the runtime form `define-data-ctor` that
;; emits struct, value binding, and match expander for each data
;; constructor.

(module+ test
  (require rackunit
           racket/match
           "adt.rkt")

  ;; A nullary constructor (like Maybe's `None`).
  (define-data-ctor None 0)

  ;; `None` can be used as a value reference.
  (check-pred procedure? (lambda () None))
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
  (check-equal? ((lambda (mk) (mk 'x 'y)) Pair) (Pair 'x 'y)))
