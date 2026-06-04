#lang rackton

;; rackton/data/either — Data.Either, over the prelude's @racket[Either]
;; type (@racket[Left] / @racket[Right]).  The @racket[Functor] /
;; @racket[Applicative] / @racket[Monad] / @racket[Bifunctor] instances
;; for @racket[Either] live in the prelude; these are the non-class
;; eliminators, predicates, and collectors.  For result/success-flavored
;; code that prefers @tt{Ok}/@tt{Err} naming, see rackton/data/result.

(provide (all-defined-out))

;; Eliminator: @racket[(either f g r)] applies @racket[f] to a
;; @racket[Left] payload, @racket[g] to a @racket[Right] payload.
(: either (-> (-> a c) (-> (-> b c) (-> (Either a b) c))))
(define (either f g r)
  (match r
    [(Left  a) (f a)]
    [(Right b) (g b)]))

(: is-left (-> (Either a b) Boolean))
(define (is-left r) (match r [(Left _) #t] [(Right _) #f]))

(: is-right (-> (Either a b) Boolean))
(define (is-right r) (match r [(Left _) #f] [(Right _) #t]))

;; @racket[(from-left default r)] — the Left payload, or @racket[default].
(: from-left (-> a (-> (Either a b) a)))
(define (from-left d r) (match r [(Left a) a] [(Right _) d]))

;; @racket[(from-right default r)] — the Right payload, or @racket[default].
(: from-right (-> b (-> (Either a b) b)))
(define (from-right d r) (match r [(Right b) b] [(Left _) d]))

;; All Left payloads (Haskell @tt{lefts}), order preserved.
(: lefts (-> (List (Either a b)) (List a)))
(define (lefts rs)
  (match rs
    [(Nil)              Nil]
    [(Cons (Left a) t)  (Cons a (lefts t))]
    [(Cons (Right _) t) (lefts t)]))

;; All Right payloads (Haskell @tt{rights}), order preserved.
(: rights (-> (List (Either a b)) (List b)))
(define (rights rs)
  (match rs
    [(Nil)              Nil]
    [(Cons (Right b) t) (Cons b (rights t))]
    [(Cons (Left _) t)  (rights t)]))

;; @racket[(partition-eithers rs)] — @racket[(Pair lefts rights)]
;; (Haskell @tt{partitionEithers}), order preserved within each side.
(: partition-eithers (-> (List (Either a b)) (Pair (List a) (List b))))
(define (partition-eithers rs)
  (foldr (lambda (r acc)
           (match r
             [(Left  a) (Pair (Cons a (fst acc)) (snd acc))]
             [(Right b) (Pair (fst acc) (Cons b (snd acc)))]))
         (Pair Nil Nil)
         rs))

;; @racket[Right] → @racket[Some]; @racket[Left] → @racket[None].
(: right->maybe (-> (Either a b) (Maybe b)))
(define (right->maybe r) (match r [(Right b) (Some b)] [(Left _) None]))

;; @racket[(maybe->either left m)] — @racket[Some]→@racket[Right];
;; @racket[None]→@racket[(Left left)].
(: maybe->either (-> a (-> (Maybe b) (Either a b))))
(define (maybe->either l m) (match m [(Some b) (Right b)] [(None) (Left l)]))
