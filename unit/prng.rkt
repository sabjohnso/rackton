#lang rackton

;; rackton/unit — a pure, seeded, splittable pseudo-random generator.
;;
;; Property testing must be reproducible: generation takes no IO, and a
;; failing case is replayed from a printable starting `Integer` seed.
;; The state is a 64-bit LCG (the SplitMix / PCG multiplier and odd
;; increment).  Racket Integers are bignums, so a 2^64 modulus needs no
;; bitwise ops — `mod` and `quot` stand in for masking/shifting.
;;
;; This is adequate for test-case generation, not statistical rigor:
;; with no bitwise ops the output mixing is arithmetic-only and the
;; `mod` range reduction has the usual mod bias for large spans.
;;
;; Public API: Seed, seed-from, next-seed, split-seed, seed-value,
;; seed-int-range.

(provide (data-out Seed)
         seed-from
         next-seed
         split-seed
         seed-value
         seed-int-range)

;; 64-bit LCG state, kept in [0, 2^64).
(data Seed (Seed Integer))

(: two64 Integer)
(define two64 18446744073709551616)

(: lcg-mult Integer)
(define lcg-mult 6364136223846793005)

(: lcg-inc Integer)
(define lcg-inc 1442695040888963407)

;; Build a seed from any printable Integer (the user-visible handle).
(: seed-from (-> Integer Seed))
(define (seed-from n) (Seed (mod (abs n) two64)))

;; Advance the LCG one step.
(: next-seed (-> Seed Seed))
(define (next-seed s)
  (match s
    [(Seed x) (Seed (mod (+ (* x lcg-mult) lcg-inc) two64))]))

;; Extract a well-mixed value from a seed.  Arithmetic-only avalanche
;; (no xorshift available): fold the high half into the low half, then
;; multiply and fold again.
(: seed-value (-> Seed Integer))
(define (seed-value s)
  (match s
    [(Seed x)
     (let ([h (mod (* (+ x (quot x 4294967296)) lcg-mult) two64)])
       (mod (+ h (quot h 65536)) two64))]))

;; Derive two independent sub-seeds.  The second is reseeded from a
;; value-derived offset so the two streams diverge immediately.
(: split-seed (-> Seed (Pair Seed Seed)))
(define (split-seed s)
  (let ([s1 (next-seed s)])
    (let ([s2 (next-seed (Seed (mod (+ (seed-value s1) lcg-inc) two64)))])
      (Pair s1 s2))))

;; A value in the inclusive range [lo, hi] (assumes hi >= lo).
(: seed-int-range (-> Seed (-> Integer (-> Integer Integer))))
(define (seed-int-range s lo hi)
  (let ([span (+ (- hi lo) 1)])
    (+ lo (mod (seed-value s) span))))
