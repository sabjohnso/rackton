#lang racket/base

;; End-to-end tests for the `match*` surface form: an expression that
;; pattern-matches on several scrutinees at once.  It is the honest
;; N-ary generalization of `match` — the arity follows the parenthesized
;; scrutinee list, and every clause leads with a parenthesized list of
;; that many patterns.  Parse → infer → codegen → execute.  ADT results
;; are unwrapped to Integers inside Rackton so the host-side checks
;; compare plain numbers.
;;
;; Unlike `case-lambda`, `match*` matches arbitrary expressions in place
;; rather than a lambda's arguments, so the scrutinees here are ordinary
;; sub-expressions, not parameters.

(require rackunit
         "../main.rkt")

(rackton
  (data (Maybe a) None (Some a))

  (define (unwrap d m)
    (match m
      [(None)   d]
      [(Some v) v]))

  ;; Two scrutinees, matched at once.  The scrutinees are arbitrary
  ;; expressions, not parameters.
  (define (add-maybe a b)
    (match* (a b)
      [((Some x) (Some y)) (Some (+ x y))]
      [(_ _)               None]))

  (define r1 (unwrap -1 (add-maybe (Some 1) (Some 2))))
  (define r2 (unwrap -1 (add-maybe None (Some 2))))

  ;; Single-scrutinee `match*` (arity 1): a strict generalization of
  ;; `match`.  The lone constructor-pattern argument needs its own
  ;; parens — `((Some x))`, mirroring `case-lambda`.
  (define (maybe-or-zero m)
    (match* (m)
      [(None)     0]
      [((Some x)) x]))

  (define s1 (maybe-or-zero (Some 7)))
  (define s2 (maybe-or-zero None))

  ;; Guard clause (`:when`), as in `match`.
  (define (classify n)
    (match* (n)
      [(k) :when (> k 0)  1]
      [(k) :when (< k 0) -1]
      [(_)                 0]))

  (define g1 (classify 5))
  (define g2 (classify -3))
  (define g3 (classify 0))

  ;; Scrutinees may be non-trivial expressions evaluated in place.
  (define (sum-or a b)
    (match* ((+ a 1) (+ b 1))
      [(1 1) 0]
      [(p q) (+ p q)]))

  (define e1 (sum-or 0 0))
  (define e2 (sum-or 2 3)))

;; The `rackton` block splices its definitions into this module's scope,
;; so the checks reference r1, s1, … directly (as in end-to-end-test).

(test-case "match* two-scrutinee adder"
  (check-equal? r1 3)
  (check-equal? r2 -1))

(test-case "match* single-scrutinee form"
  (check-equal? s1 7)
  (check-equal? s2 0))

(test-case "match* :when guards"
  (check-equal? g1 1)
  (check-equal? g2 -1)
  (check-equal? g3 0))

(test-case "match* over computed scrutinees"
  (check-equal? e1 0)
  (check-equal? e2 7))
