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

(require rackton/data/coerce)

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

;; --- Num (decomposed into the algebraic lattice) -------------------
;; Addition and multiplication stay within the naturals and are exact,
;; so Natural reaches the additive abelian-group and multiplicative
;; commutative-monoid nodes; subtraction and negate are partial and
;; panic on underflow (Haskell semantics), which is a lawful (if partial)
;; inverse.
(instance (Additive-Magma Natural)
  (define (+ a b)
    (match a [(Natural x) (match b [(Natural y) (Natural (+ x y))])])))
(instance (Additive-Semigroup Natural))
(instance (Additive-Unital-Magma Natural)
  (define zero (Natural 0)))
(instance (Additive-Loop Natural)
  (define (negate x)
    (match x [(Natural n)
              (if (== n 0) (Natural 0) (panic "negate of a positive Natural"))]))
  (define (- a b)
    (match a [(Natural x)
              (match b [(Natural y)
                        (if (< x y)
                            (panic "Natural subtraction below zero")
                            (Natural (- x y)))])])))
(instance (Additive-Commutative-Loop Natural))
(instance (Multiplicative-Magma Natural)
  (define (* a b)
    (match a [(Natural x) (match b [(Natural y) (Natural (* x y))])])))
(instance (Multiplicative-Semigroup Natural))
(instance (Multiplicative-Unital-Magma Natural)
  (define one (Natural 1)))
(instance (Multiplicative-Commutative-Unital-Magma Natural))
(instance (Abs Natural)
  (define (abs x) x))

;; --- Coerce --------------------------------------------------------
;; Natural -> Integer is total and lossless.  The reverse (Integer ->
;; Natural) is partial (negatives have no Natural), and a coercion
;; cannot signal failure, so that direction stays with the checked
;; `num-to-natural : Integer -> (Maybe Natural)`.
(instance (Coerce Natural Integer)
  (define (coerce x) (num-from-natural x)))
