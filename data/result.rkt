#lang rackton

;; rackton/data/result — a result/success-flavored coproduct.
;;
;; @racket[Result] is isomorphic to the prelude's @racket[Either]
;; (@racket[Err] <-> @racket[Left], @racket[Ok] <-> @racket[Right]) but a
;; DISTINCT nominal type, for code where the @tt{Ok}/@tt{Err} naming reads
;; better than @tt{Left}/@tt{Right} — denoting an operation's success or
;; failure.  Its @racket[Functor] / @racket[Applicative] / @racket[Monad]
;; / @racket[Bifunctor] / @racket[Eq] / @racket[Show] instances live here
;; (they are NOT inherited from @racket[Either]); @racket[result->either]
;; and @racket[either->result] bridge to the prelude coproduct so the
;; arrow / @racket[Coprod] machinery (which is defined over
;; @racket[Either]) stays reachable.

(provide (all-defined-out))

(data (Result e a) (Err e) (Ok a))

;; ----- class instances --------------------------------------------
;; The error type is fixed; we map over the success type.

(instance (Functor (Result e))
  (define (fmap f r)
    (match r
      [(Err x) (Err x)]
      [(Ok  v) (Ok (f v))])))

(instance (Applicative (Result e))
  (define (pure x) (Ok x))
  (define (fapply rf rx)
    (match rf
      [(Err e) (Err e)]
      [(Ok  f) (fmap f rx)])))

(instance (Monad (Result e))
  (define (flatmap f r)
    (match r
      [(Err x) (Err x)]
      [(Ok  v) (f v)])))

(instance (Bifunctor Result)
  (define (bimap f g r)
    (match r
      [(Err e) (Err (f e))]
      [(Ok  v) (Ok  (g v))])))

(instance ((Eq e) (Eq a) => (Eq (Result e a)))
  (define (== r1 r2)
    (match r1
      [(Err x) (match r2 [(Err y) (== x y)] [(Ok  _) #f])]
      [(Ok  x) (match r2 [(Err _) #f]        [(Ok  y) (== x y)])])))

(instance ((Show e) (Show a) => (Show (Result e a)))
  (define (show r)
    (match r
      [(Err x) (string-append "(Err " (string-append (show x) ")"))]
      [(Ok  x) (string-append "(Ok " (string-append (show x) ")"))])))

;; ----- eliminator / predicates / extraction -----------------------

;; @racket[(result f g r)] applies @racket[f] to an @racket[Err] payload,
;; @racket[g] to an @racket[Ok] payload.
(: result (-> (-> e c) (-> (-> a c) (-> (Result e a) c))))
(define (result f g r)
  (match r
    [(Err e) (f e)]
    [(Ok  a) (g a)]))

(: ok? (-> (Result e a) Boolean))
(define (ok? r) (match r [(Ok _) #t] [(Err _) #f]))

(: err? (-> (Result e a) Boolean))
(define (err? r) (match r [(Ok _) #f] [(Err _) #t]))

;; @racket[(from-ok default r)] — the Ok payload, or @racket[default].
(: from-ok (-> a (-> (Result e a) a)))
(define (from-ok d r) (match r [(Ok a) a] [(Err _) d]))

;; @racket[(from-err default r)] — the Err payload, or @racket[default].
(: from-err (-> e (-> (Result e a) e)))
(define (from-err d r) (match r [(Err e) e] [(Ok _) d]))

;; All Ok payloads, order preserved.
(: oks (-> (List (Result e a)) (List a)))
(define (oks rs)
  (match rs
    [(Nil)            Nil]
    [(Cons (Ok a) t)  (Cons a (oks t))]
    [(Cons (Err _) t) (oks t)]))

;; All Err payloads, order preserved.
(: errs (-> (List (Result e a)) (List e)))
(define (errs rs)
  (match rs
    [(Nil)            Nil]
    [(Cons (Err e) t) (Cons e (errs t))]
    [(Cons (Ok _) t)  (errs t)]))

;; @racket[(partition-results rs)] — @racket[(Pair errs oks)], order
;; preserved within each side.
(: partition-results (-> (List (Result e a)) (Pair (List e) (List a))))
(define (partition-results rs)
  (foldr (lambda (r acc)
           (match r
             [(Err e) (Pair (Cons e (fst acc)) (snd acc))]
             [(Ok  a) (Pair (fst acc) (Cons a (snd acc)))]))
         (Pair Nil Nil)
         rs))

;; ----- Maybe interop ----------------------------------------------

;; @racket[Ok] → @racket[Some]; @racket[Err] → @racket[None].
(: ok->maybe (-> (Result e a) (Maybe a)))
(define (ok->maybe r) (match r [(Ok a) (Some a)] [(Err _) None]))

;; @racket[(maybe->result e m)] — @racket[Some]→@racket[Ok];
;; @racket[None]→@racket[(Err e)].
(: maybe->result (-> e (-> (Maybe a) (Result e a))))
(define (maybe->result e m) (match m [(Some a) (Ok a)] [(None) (Err e)]))

;; ----- bridge to / from the prelude Either ------------------------

;; @racket[Err] → @racket[Left]; @racket[Ok] → @racket[Right].
(: result->either (-> (Result e a) (Either e a)))
(define (result->either r) (match r [(Err e) (Left e)] [(Ok a) (Right a)]))

;; @racket[Left] → @racket[Err]; @racket[Right] → @racket[Ok].
(: either->result (-> (Either e a) (Result e a)))
(define (either->result x) (match x [(Left e) (Err e)] [(Right a) (Ok a)]))
