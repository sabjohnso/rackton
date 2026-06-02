#lang rackton

;; rackton/data/either — Data.Either, over the prelude's @racket[Result]
;; type: @racket[Err] is Haskell's @tt{Left}, @racket[Ok] is @tt{Right}.
;; The @racket[Functor] / @racket[Applicative] / @racket[Monad] /
;; @racket[Bifunctor] instances for @racket[Result] are in the prelude;
;; these are the non-class eliminators and collectors.

(provide (all-defined-out))

;; Eliminator: @racket[(either f g r)] applies @racket[f] to an
;; @racket[Err] payload, @racket[g] to an @racket[Ok] payload.
(: either (-> (-> e c) (-> (-> a c) (-> (Result e a) c))))
(define (either f g r)
  (match r
    [(Err e) (f e)]
    [(Ok  a) (g a)]))

(: is-ok (-> (Result e a) Boolean))
(define (is-ok r) (match r [(Ok _) #t] [(Err _) #f]))

(: is-err (-> (Result e a) Boolean))
(define (is-err r) (match r [(Ok _) #f] [(Err _) #t]))

;; @racket[(from-ok default r)] — the Ok payload, or @racket[default].
(: from-ok (-> a (-> (Result e a) a)))
(define (from-ok d r) (match r [(Ok a) a] [(Err _) d]))

;; @racket[(from-err default r)] — the Err payload, or @racket[default].
(: from-err (-> e (-> (Result e a) e)))
(define (from-err d r) (match r [(Err e) e] [(Ok _) d]))

;; All Ok payloads (Haskell @tt{rights}).
(: oks (-> (List (Result e a)) (List a)))
(define (oks rs)
  (match rs
    [(Nil)            Nil]
    [(Cons (Ok a) t)  (Cons a (oks t))]
    [(Cons (Err _) t) (oks t)]))

;; All Err payloads (Haskell @tt{lefts}).
(: errs (-> (List (Result e a)) (List e)))
(define (errs rs)
  (match rs
    [(Nil)            Nil]
    [(Cons (Err e) t) (Cons e (errs t))]
    [(Cons (Ok _) t)  (errs t)]))

;; @racket[(partition-results rs)] — @racket[(Pair errs oks)]
;; (Haskell @tt{partitionEithers}), order preserved.
(: partition-results (-> (List (Result e a)) (Pair (List e) (List a))))
(define (partition-results rs)
  (foldr (lambda (r acc)
           (match r
             [(Err e) (Pair (Cons e (fst acc)) (snd acc))]
             [(Ok  a) (Pair (fst acc) (Cons a (snd acc)))]))
         (Pair Nil Nil)
         rs))

;; @racket[Ok] → @racket[Some]; @racket[Err] → @racket[None].
(: ok->maybe (-> (Result e a) (Maybe a)))
(define (ok->maybe r) (match r [(Ok a) (Some a)] [(Err _) None]))

;; @racket[(maybe->result e m)] — @racket[Some]→@racket[Ok];
;; @racket[None]→@racket[(Err e)].
(: maybe->result (-> e (-> (Maybe a) (Result e a))))
(define (maybe->result e m) (match m [(Some a) (Ok a)] [(None) (Err e)]))
