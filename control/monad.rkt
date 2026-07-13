#lang rackton

;; rackton/control/monad — Control.Monad.  Combinators that work over
;; any @racket[(Monad m)]: monadic mapping / sequencing / folding.  The
;; @racket[Monad] class itself, plus @racket[join] / @racket[when] /
;; @racket[unless] / @racket[void], live in the prelude; these build on
;; @racket[flatmap] (via @racket[do]) and @racket[pure].
;;
;; (This is rackton/control/monad — the file; the transformer modules
;; live under rackton/control/monad/ — the directory.  Both coexist.)

(provide (all-defined-out))

;; map-m: apply a monadic action to each element, collecting results.
;; (Haskell @tt{mapM} / @tt{traverse} specialised to lists.)
(: map-m ((Monad m) => (-> (-> a (m b)) (-> (List a) (m (List b))))))
(define (map-m f xs)
  (match xs
    [(Nil)      (pure Nil)]
    [(Cons h t) (let& ([b  (f h)]
                       [bs (map-m f t)])
                  (pure (Cons b bs)))]))

;; for-m: map-m with arguments flipped.
(: for-m ((Monad m) => (-> (List a) (-> (-> a (m b)) (m (List b))))))
(define (for-m xs f) (map-m f xs))

;; sequence-m: run a list of actions left to right, collecting results.
(: sequence-m ((Monad m) => (-> (List (m a)) (m (List a)))))
(define (sequence-m ms)
  (match ms
    [(Nil)      (pure Nil)]
    [(Cons h t) (let& ([x  h]
                       [xs (sequence-m t)])
                  (pure (Cons x xs)))]))

;; fold-m: left fold with a monadic step (Haskell @tt{foldM}).
(: fold-m ((Monad m) => (-> (-> b (-> a (m b))) (-> b (-> (List a) (m b))))))
(define (fold-m f z xs)
  (match xs
    [(Nil)      (pure z)]
    [(Cons h t) (let& ([z2 (f z h)])
                  (fold-m f z2 t))]))

;; replicate-m: run an action n times, collecting results.
(: replicate-m ((Monad m) => (-> Integer (-> (m a) (m (List a))))))
(define (replicate-m n act)
  (if (<= n 0)
      (pure Nil)
      (let& ([x  act]
             [xs (replicate-m (- n 1) act)])
        (pure (Cons x xs)))))

;; filter-m: keep elements whose monadic predicate yields #t.
(: filter-m ((Monad m) => (-> (-> a (m Boolean)) (-> (List a) (m (List a))))))
(define (filter-m p xs)
  (match xs
    [(Nil)      (pure Nil)]
    [(Cons h t) (let& ([keep (p h)]
                       [rest (filter-m p t)])
                  (pure (if keep (Cons h rest) rest)))]))
