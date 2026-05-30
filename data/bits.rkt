#lang rackton

;; rackton/data/bits — bitwise operations over the prelude's
;; @racket[Integer], in the spirit of Haskell's Data.Bits.  Rackton has
;; one integral type, so these are plain functions rather than a
;; @racket[Bits] class; each is a single @racket[(racket …)] escape to
;; the host's @racket[bitwise-*] / @racket[arithmetic-shift].
;;
;; Names are @racket[bit-]-prefixed so they don't shadow racket/base's
;; @racket[bitwise-and] / @racket[arithmetic-shift] inside escapes
;; (the same collision discipline as data/map's @racket[map-] prefix).
;;
;; Integers are two's-complement of unbounded width: @racket[bit-not]
;; of a non-negative value is negative, and @racket[bit-count] is
;; defined for non-negative inputs (a negative has infinitely many set
;; bits in this representation).

(provide (all-defined-out))

;; --- logical ops ---------------------------------------------------

(: bit-and (-> Integer (-> Integer Integer)))
(define (bit-and a b) (racket Integer (a b) (bitwise-and a b)))

(: bit-or (-> Integer (-> Integer Integer)))
(define (bit-or a b) (racket Integer (a b) (bitwise-ior a b)))

(: bit-xor (-> Integer (-> Integer Integer)))
(define (bit-xor a b) (racket Integer (a b) (bitwise-xor a b)))

;; bitwise complement (Haskell `complement`).
(: bit-not (-> Integer Integer))
(define (bit-not a) (racket Integer (a) (bitwise-not a)))

;; --- shifts --------------------------------------------------------
;; Shift counts are non-negative; left fills with zeros, right is the
;; arithmetic (sign-extending) shift.

(: bit-shift-left (-> Integer (-> Integer Integer)))
(define (bit-shift-left a n) (racket Integer (a n) (arithmetic-shift a n)))

;; A negative arithmetic-shift count shifts right; negate in Rackton
;; (the prelude's `negate`) rather than inside the escape, where `-`
;; resolves to the prelude's binary `-`, not racket's negation.
(: bit-shift-right (-> Integer (-> Integer Integer)))
(define (bit-shift-right a n) (bit-shift-left a (negate n)))

;; --- single-bit operations -----------------------------------------
;; Bit positions are 0-indexed from the least-significant bit.

(: bit-test (-> Integer (-> Integer Boolean)))
(define (bit-test a i) (racket Boolean (a i) (bitwise-bit-set? a i)))

(: bit-set (-> Integer (-> Integer Integer)))
(define (bit-set a i) (racket Integer (a i) (bitwise-ior a (arithmetic-shift 1 i))))

(: bit-clear (-> Integer (-> Integer Integer)))
(define (bit-clear a i)
  (racket Integer (a i) (bitwise-and a (bitwise-not (arithmetic-shift 1 i)))))

;; --- population count ----------------------------------------------
;; Number of set bits in a non-negative Integer (Haskell `popCount`).

(: bit-count (-> Integer Integer))
(define (bit-count a)
  (racket Integer (a)
    (let loop ([n a] [c 0])
      (if (= n 0)
          c
          (loop (arithmetic-shift n -1) (+ c (bitwise-and n 1)))))))
