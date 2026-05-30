#lang rackton

;; rackton/data/traversable — Data.Traversable.  `traverse` is the
;; prelude Traversable class method; these are the derived forms.

(provide (all-defined-out))

;; sequence-a: turn a structure of actions into an action of a
;; structure (Haskell sequenceA).
(: sequence-a ((Applicative f) (Traversable t) => (-> (t (f a)) (f (t a)))))
(define (sequence-a t) (traverse (lambda (x) x) t))

;; for-t: traverse with arguments flipped (Haskell `for`).
(: for-t ((Applicative f) (Traversable t) => (-> (t a) (-> (-> a (f b)) (f (t b))))))
(define (for-t t f) (traverse f t))
