#lang rackton

;; `List` is a Monad: `flatmap` is concatMap (bind each element to a list
;; and concatenate), `join` flattens one level of nesting, and
;; do-notation over List gives list-comprehension / cartesian-product
;; semantics.  This pins the prelude's `(Monad List)` instance.

(require "../unit.rkt")

;; flatmap f xs = concat (map f xs)
(: dup (-> Integer (List Integer)))
(define (dup x) (list x x))

(: dupped (List Integer))
(define dupped (flatmap dup (list 1 2 3)))

;; join flattens one level
(: nested (List (List Integer)))
(define nested (list (list 1 2) (list 3) (list 4 5)))

(: flattened (List Integer))
(define flattened (join nested))

;; do-notation over List == cartesian product
(: pairs (List (Pair Integer Integer)))
(define pairs
  (do [x <- (list 1 2)]
      [y <- (list 10 20)]
    (pure (MkPair x y))))

;; flatmap to the empty list short-circuits that branch
(: with-empty (List Integer))
(define with-empty
  (flatmap (lambda (x) (if (== x 2) Nil (list x))) (list 1 2 3)))

(: suite (List Test))
(define suite
  (list
   (it "flatmap over List is concatMap"
       (check-equal? dupped (Cons 1 (Cons 1 (Cons 2 (Cons 2 (Cons 3 (Cons 3 Nil))))))))
   (it "join flattens one level of nesting"
       (check-equal? flattened (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil)))))))
   (it "do over List is the cartesian product"
       (check-equal? pairs
                     (Cons (MkPair 1 10)
                           (Cons (MkPair 1 20)
                                 (Cons (MkPair 2 10)
                                       (Cons (MkPair 2 20) Nil))))))
   (it "flatmap to Nil drops that element"
       (check-equal? with-empty (Cons 1 (Cons 3 Nil))))))

(: _ran Unit)
(define _ran (run-io (run-suite "list-monad" suite)))
