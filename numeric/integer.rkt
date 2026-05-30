#lang rackton

;; rackton/numeric/integer — Integral helper operations over the
;; prelude's @racket[Integer].  The prelude ships the @racket[Integral]
;; class (@racket[div] / @racket[mod] / @racket[quot] / @racket[rem])
;; and @racket[Num] (@racket[+] / @racket[-] / @racket[*] /
;; @racket[abs] / @racket[negate]); these are the derived combinators
;; Haskell exposes from @tt{GHC.Real} / @tt{Prelude}.
;;
;; Names are prefixed @racket[num-] so they don't shadow racket/base's
;; @racket[gcd] / @racket[lcm] / @racket[even?] / @racket[odd?], which
;; keeps those reachable inside @racket[(racket …)] escapes.

(provide (all-defined-out))

;; --- parity --------------------------------------------------------

(: num-even? (-> Integer Boolean))
(define (num-even? n) (== (mod n 2) 0))

(: num-odd? (-> Integer Boolean))
(define (num-odd? n) (not (num-even? n)))

;; --- sign ----------------------------------------------------------

(: num-signum (-> Integer Integer))
(define (num-signum n)
  (if (< n 0) -1 (if (== n 0) 0 1)))

;; --- gcd / lcm -----------------------------------------------------

;; Euclid's algorithm; the base case takes @racket[abs] so the result
;; is non-negative even for negative inputs (matching Haskell `gcd`).
(: num-gcd (-> Integer (-> Integer Integer)))
(define (num-gcd a b)
  (if (== b 0) (abs a) (num-gcd b (mod a b))))

;; lcm a b = |a * b| / gcd a b, with the zero case pinned to 0.
(: num-lcm (-> Integer (-> Integer Integer)))
(define (num-lcm a b)
  (if (== a 0)
      0
      (if (== b 0)
          0
          (abs (* (quot a (num-gcd a b)) b)))))

;; --- factorial / power ---------------------------------------------

(: num-factorial (-> Integer Integer))
(define (num-factorial n)
  (if (== n 0) 1 (* n (num-factorial (- n 1)))))

;; integer exponentiation b^e for e >= 0.
(: num-int-pow (-> Integer (-> Integer Integer)))
(define (num-int-pow b e)
  (if (== e 0) 1 (* b (num-int-pow b (- e 1)))))

;; --- conversion ----------------------------------------------------

;; Haskell `fromIntegral` is polymorphic in its target; the prelude
;; only ships Integer -> Float, so this is the concrete instance.
(: num-from-integral (-> Integer Float))
(define (num-from-integral n) (integer->float n))
