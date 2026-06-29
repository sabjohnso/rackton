#lang racket/base

;; End-to-end tests for the `case-lambda` / `case-λ` surface form: an
;; anonymous function that pattern-matches on all of its arguments at
;; once.  Parse → infer → codegen → execute.  ADT results are unwrapped
;; to Integers inside Rackton so the host-side checks compare plain
;; numbers.
;;
;; The first element of each clause is the parameter list, so a lone
;; constructor-pattern argument needs its own parens: `((Some x))`,
;; mirroring the two-argument `((Some x) (Some y))`.

(require rackunit
         "../main.rkt")

(rackton
  (data (Maybe a) None (Some a))

  (define (unwrap d m)
    (match m
      [(None)   d]
      [(Some v) v]))

  ;; Two-argument form: add two Maybe values, short-circuiting on None.
  (define add-maybe
    (case-lambda
      [((Some x) (Some y)) (Some (+ x y))]
      [(_ _)               None]))

  (define r1 (unwrap -1 (add-maybe (Some 1) (Some 2))))
  (define r2 (unwrap -1 (add-maybe None (Some 2))))

  ;; Single-argument form with the `case-λ` spelling.
  (define maybe-or-zero
    (case-λ
      [(None)     0]
      [((Some x)) x]))

  (define s1 (maybe-or-zero (Some 7)))
  (define s2 (maybe-or-zero None))

  ;; Guard clause (`:when`), as in `match`.
  (define classify
    (case-lambda
      [(n) :when (> n 0)  1]
      [(n) :when (< n 0) -1]
      [(_)                 0]))

  (define g1 (classify 5))
  (define g2 (classify -3))
  (define g3 (classify 0)))

;; The `rackton` block splices its definitions into this module's scope,
;; so the checks reference r1, s1, … directly (as in end-to-end-test).

(test-case "case-lambda two-argument adder"
  (check-equal? r1 3)
  (check-equal? r2 -1))

(test-case "case-λ single-argument form"
  (check-equal? s1 7)
  (check-equal? s2 0))

(test-case "case-lambda :when guards"
  (check-equal? g1 1)
  (check-equal? g2 -1)
  (check-equal? g3 0))
