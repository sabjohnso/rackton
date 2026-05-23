#lang racket/base

;; Phase 38: deriving menu rounded out with Traversable, Bifunctor,
;; Semigroup, and Monoid.

(require rackunit
         "../main.rkt")

(rackton
  ;; ----- 38.B Bifunctor on a custom two-tparam ADT -------------

  (define-data (Either2 a b) (Lft a) (Rgt b)
    #:deriving Bifunctor)

  (: mapped-left  (Either2 Integer String))
  (define mapped-left  (bimap (lambda (n) (+ n 1)) (lambda (s) (<> s "!")) (Lft 41)))

  (: mapped-right (Either2 Integer String))
  (define mapped-right (bimap (lambda (n) (+ n 1)) (lambda (s) (<> s "!")) (Rgt "ok")))

  ;; ----- 38.C Semigroup on a single-ctor record ----------------

  (define-struct (Log a)
    [entries : (List a)]
    [tag     : String]
    #:deriving Semigroup)

  (: combined-logs (Log Integer))
  (define combined-logs
    (<> (Log (Cons 1 (Cons 2 Nil)) "left:")
        (Log (Cons 3 Nil)           "right")))

  ;; ----- 38.D Monoid on a single-ctor record -------------------

  (define-struct Counter
    [hits   : (List Integer)]
    [label  : String]
    #:deriving Semigroup Monoid)

  (: empty-counter Counter)
  (define empty-counter mempty)

  (: combined-counter Counter)
  (define combined-counter
    (<> (Counter (Cons 1 Nil) "a")
        (Counter (Cons 2 Nil) "b"))))

;; ---------- assertions ---------------------------------------

(test-case "Bifunctor on Either2"
  (check-equal? mapped-left  (Lft 42))
  (check-equal? mapped-right (Rgt "ok!")))

(test-case "Semigroup on a record combines fields pairwise"
  (check-equal? combined-logs
                (Log (Cons 1 (Cons 2 (Cons 3 Nil))) "left:right")))

(test-case "Monoid mempty is element-wise empty"
  (check-equal? empty-counter (Counter Nil "")))

(test-case "Monoid + Semigroup combine pairwise"
  (check-equal? combined-counter
                (Counter (Cons 1 (Cons 2 Nil)) "ab")))
