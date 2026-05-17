#lang racket/base

;; Rackton — public entry point for the (rackton ...) macro form.
;;
;; This module re-exports everything a downstream user needs to embed
;; Rackton code inside a regular Racket module:
;;
;;   (require rackton)
;;
;;   (rackton
;;     (define-data (Maybe a) None (Some a))
;;     (: from-just (-> a (-> (Maybe a) a)))
;;     (define (from-just d m)
;;       (match m [(None) d] [(Some x) x])))
;;
;; The supported subset (Phase 1) covers:
;;   literals (Integer / Boolean / String),
;;   lambda / application, let, if, ascription, match,
;;   define / declare (:) / define-data,
;;   Hindley–Milner inference with let-polymorphism,
;;   ADTs with pattern matching.

(require "private/elaborate.rkt"
         "private/adt.rkt"
         "private/dict.rkt"
         "private/prelude-runtime.rkt"
         (except-in racket/match ==))

(provide rackton

         ;; runtime support exposed for the macro's output
         define-data-ctor
         define-class-method
         register-instance-method!
         match

         ;; prelude — class methods, ADTs, and combinators
         (all-from-out "private/prelude-runtime.rkt"))

(module+ test
  (require rackunit)
  (rackton
    (define (id x) x)
    (define (compose f g) (lambda (x) (f (g x))))

    (: fact (-> Integer Integer))
    (define (fact n)
      (if (= n 0) 1 (* n (fact (- n 1)))))

    (define-data (Maybe a) None (Some a))

    (: from-maybe (-> a (-> (Maybe a) a)))
    (define (from-maybe d m)
      (match m
        [(None)   d]
        [(Some x) x]))

    (: map-maybe (-> (-> a b) (-> (Maybe a) (Maybe b))))
    (define (map-maybe f m)
      (match m
        [(None)   None]
        [(Some x) (Some (f x))])))

  (check-equal? (id 42) 42)
  (check-equal? ((compose (lambda (n) (* n 2)) (lambda (n) (+ n 1))) 5) 12)
  (check-equal? (fact 5)  120)
  (check-equal? (fact 6)  720)
  (check-equal? (from-maybe 0 None) 0)
  (check-equal? (from-maybe 0 (Some 7)) 7)
  (check-equal? (map-maybe (lambda (n) (* n n)) (Some 4)) (Some 16))
  (check-equal? (map-maybe (lambda (n) (* n n)) None) None))
