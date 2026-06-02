#lang rackton

;; rackton/numeric/natural — Numeric.Natural.  A newtype over the
;; prelude's @racket[Integer] constrained to non-negative values, with
;; the Eq / Ord / Show / Num instances Haskell's @racket[Natural]
;; carries.  Construction is checked: @racket[num-to-natural] returns
;; @racket[None] for a negative input.  Following Haskell, the partial
;; Num operations on @racket[Natural] (subtraction below zero, negating
;; a positive) @racket[panic] rather than wrap around — a @racket[Natural]
;; never holds a negative.

(provide (all-defined-out))

(newtype Natural (Natural Integer))

;; --- construction / projection -------------------------------------

;; Integer -> (Maybe Natural); None when the input is negative.
(: num-to-natural (-> Integer (Maybe Natural)))
(define (num-to-natural n)
  (if (< n 0) None (Some (Natural n))))

(: num-from-natural (-> Natural Integer))
(define (num-from-natural x) (match x [(Natural n) n]))

;; --- Eq / Ord / Show -----------------------------------------------

(instance (Eq Natural)
  (define (== a b)
    (match a [(Natural x) (match b [(Natural y) (== x y)])])))

(instance (Ord Natural)
  (define (< a b)
    (match a [(Natural x) (match b [(Natural y) (< x y)])])))

(instance (Show Natural)
  (define (show x) (match x [(Natural n) (show n)])))

;; --- Num -----------------------------------------------------------
;; Addition and multiplication stay within the naturals; subtraction
;; and negate are partial and panic on underflow (Haskell semantics).

(instance (Num Natural)
  (define (+ a b)
    (match a [(Natural x) (match b [(Natural y) (Natural (+ x y))])]))
  (define (* a b)
    (match a [(Natural x) (match b [(Natural y) (Natural (* x y))])]))
  (define (- a b)
    (match a [(Natural x)
              (match b [(Natural y)
                        (if (< x y)
                            (panic "Natural subtraction below zero")
                            (Natural (- x y)))])]))
  (define (abs x) x)
  (define (negate x)
    (match x [(Natural n)
              (if (== n 0) (Natural 0) (panic "negate of a positive Natural"))])))
