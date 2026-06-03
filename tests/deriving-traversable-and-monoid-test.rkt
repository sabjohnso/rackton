#lang rackton

;; Deriving menu rounded out with Traversable, Bifunctor, Semigroup,
;; and Monoid.

(require "../unit.rkt")

;; ----- Bifunctor on a custom two-tparam ADT -------------

(data (Either2 a b) (Lft a) (Rgt b)
  #:deriving Bifunctor Eq Show)

(: mapped-left  (Either2 Integer String))
(define mapped-left  (bimap (lambda (n) (+ n 1)) (lambda (s) (mappend s "!")) (Lft 41)))

(: mapped-right (Either2 Integer String))
(define mapped-right (bimap (lambda (n) (+ n 1)) (lambda (s) (mappend s "!")) (Rgt "ok")))

;; ----- Semigroup on a single-ctor record ----------------

(struct (Log a)
  [entries : (List a)]
  [tag     : String]
  #:deriving Semigroup Eq Show)

(: combined-logs (Log Integer))
(define combined-logs
  (mappend (Log (Cons 1 (Cons 2 Nil)) "left:")
      (Log (Cons 3 Nil)           "right")))

;; ----- Monoid on a single-ctor record -------------------

(struct Counter
  [hits   : (List Integer)]
  [label  : String]
  #:deriving Semigroup Monoid Eq Show)

(: empty-counter Counter)
(define empty-counter mempty)

(: combined-counter Counter)
(define combined-counter
  (mappend (Counter (Cons 1 Nil) "a")
      (Counter (Cons 2 Nil) "b")))

;; ---------- assertions ---------------------------------------

(: suite (List Test))
(define suite
  (list
   (it "Bifunctor on Either2"
       (all-checks
        (list (check-equal? mapped-left  (Lft 42))
              (check-equal? mapped-right (Rgt "ok!")))))
   (it "Semigroup on a record combines fields pairwise"
       (check-equal? combined-logs
                     (Log (Cons 1 (Cons 2 (Cons 3 Nil))) "left:right")))
   (it "Monoid mempty is element-wise empty"
       (check-equal? empty-counter (Counter Nil "")))
   (it "Monoid + Semigroup combine pairwise"
       (check-equal? combined-counter
                     (Counter (Cons 1 (Cons 2 Nil)) "ab")))))

(: _ran Unit)
(define _ran (run-io (run-suite "deriving-traversable-and-monoid" suite)))
