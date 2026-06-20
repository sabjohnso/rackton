#lang rackton

;; rackton/data/set — Data.Set.  Immutable sets.  The `Set` type and the
;; constructor primitives (empty-set / set-insert) are promoted into the
;; prelude (so the #{..} literal needs no import); this module adds the
;; rest of Data.Set.  The derived runtime lives in
;; private/containers-runtime (Racket immutable hashes) reached via
;; `foreign`; elements compare by structural equality.

(provide (all-defined-out))

(foreign set-member? (-> a (-> (Set a) Boolean))
         #:from rackton/private/containers-runtime)
(foreign set-delete (-> a (-> (Set a) (Set a)))
         #:from rackton/private/containers-runtime)
(foreign set-size (-> (Set a) Integer)
         #:from rackton/private/containers-runtime)
(foreign set-to-list (-> (Set a) (List a))
         #:from rackton/private/containers-runtime)

;; ===== Data.Set parity =============================================
;;
;; Pure Rackton over the foreign primitives.  Elements compare by the
;; runtime's structural equality (no @racket[(Eq a)] / @racket[(Ord a)]
;; constraint).

(: set-empty? (-> (Set a) Boolean))
(define (set-empty? s) (== (set-size s) 0))

(: set-singleton (-> a (Set a)))
(define (set-singleton x) (set-insert x empty-set))

(: set-from-list (-> (List a) (Set a)))
(define (set-from-list xs)
  (foldr (lambda (x s) (set-insert x s)) empty-set xs))

;; s1 ∪ s2 — every element of s1 added to s2.
(: set-union (-> (Set a) (-> (Set a) (Set a))))
(define (set-union s1 s2)
  (foldr (lambda (x s) (set-insert x s)) s2 (set-to-list s1)))

(: set-intersection (-> (Set a) (-> (Set a) (Set a))))
(define (set-intersection s1 s2)
  (set-from-list (filter (lambda (x) (set-member? x s2)) (set-to-list s1))))

(: set-difference (-> (Set a) (-> (Set a) (Set a))))
(define (set-difference s1 s2)
  (foldr (lambda (x s) (set-delete x s)) s1 (set-to-list s2)))

(: set-subset? (-> (Set a) (-> (Set a) Boolean)))
(define (set-subset? s1 s2) (set-empty? (set-difference s1 s2)))

(: set-disjoint? (-> (Set a) (-> (Set a) Boolean)))
(define (set-disjoint? s1 s2) (set-empty? (set-intersection s1 s2)))

(: set-map (-> (-> a b) (-> (Set a) (Set b))))
(define (set-map f s) (set-from-list (fmap f (set-to-list s))))

(: set-filter (-> (-> a Boolean) (-> (Set a) (Set a))))
(define (set-filter p s) (set-from-list (filter p (set-to-list s))))

(: set-foldr (-> (-> a (-> b b)) (-> b (-> (Set a) b))))
(define (set-foldr f z s) (foldr f z (set-to-list s)))
