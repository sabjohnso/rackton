#lang rackton

;; rackton/system/random — System.Random.  Two layers:
;;
;;  * IO conveniences (random-integer / random-float and the inclusive
;;    randomR-style helpers) backed by the host RNG; and
;;  * a PURE, splittable StdGen — SplitMix64, the same algorithm
;;    Haskell's `random` uses for StdGen — implemented in plain 64-bit
;;    integer arithmetic (masked with mod 2^64), so a seed reproduces a
;;    sequence with no IO.  `split` here is best-effort: it keeps the
;;    golden gamma fixed and derives two decorrelated child seeds by
;;    mixing, rather than running the full mixGamma odd-gamma machinery.

(require rackton/data/bits)
(provide (all-defined-out))

;; --- IO RNG (host-backed) ------------------------------------------

;; random-integer lo hi: a uniform random Integer in the half-open
;; range [lo, hi) (hi exclusive; hi must be greater than lo).
(foreign random-integer (-> Integer (-> Integer (IO Integer)))
         #:from rackton/private/prelude-runtime)

;; random-float: a uniform random Float in [0, 1).
(foreign random-float (IO Float)
         #:from rackton/private/prelude-runtime)

;; random-r-integer lo hi: uniform Integer in the INCLUSIVE range
;; [lo, hi] (Haskell randomRIO), built on the half-open primitive.
(: random-r-integer (-> Integer (-> Integer (IO Integer))))
(define (random-r-integer lo hi) (random-integer lo (+ hi 1)))

;; random-r-float lo hi: uniform Float in [lo, hi].
(: random-r-float (-> Float (-> Float (IO Float))))
(define (random-r-float lo hi)
  (do [u <- random-float]
      (pure (+ lo (* (- hi lo) u)))))

;; --- pure splittable StdGen (SplitMix64) ---------------------------

;; A generator is (seed, gamma) with gamma odd.
(data StdGen (StdGen Integer Integer))

;; 2^64, and the SplitMix constants.
(: sm-mod Integer)    (define sm-mod    18446744073709551616)
(: sm-gamma Integer)  (define sm-gamma  11400714819323198485) ; 0x9e3779b97f4a7c15
(: sm-c1 Integer)     (define sm-c1     13787848793156543929) ; 0xbf58476d1ce4e5b9
(: sm-c2 Integer)     (define sm-c2     10723151780598845931) ; 0x94d049bb133111eb

(: mask64 (-> Integer Integer))
(define (mask64 x) (mod x sm-mod))

;; The SplitMix64 finalizer: avalanche a 64-bit word.
(: sm-mix64 (-> Integer Integer))
(define (sm-mix64 z0)
  (let ([z1 (mask64 (* (bit-xor z0 (bit-shift-right z0 30)) sm-c1))])
    (let ([z2 (mask64 (* (bit-xor z1 (bit-shift-right z1 27)) sm-c2))])
      (bit-xor z2 (bit-shift-right z2 31)))))

;; mkStdGen: seed a generator from any Integer.
(: mk-std-gen (-> Integer StdGen))
(define (mk-std-gen s) (StdGen (sm-mix64 (mask64 s)) sm-gamma))

;; next-word: the next 64-bit value and the advanced generator.
(: next-word (-> StdGen (Pair Integer StdGen)))
(define (next-word g)
  (match g
    [(StdGen seed gamma)
     (let ([seed2 (mask64 (+ seed gamma))])
       (Pair (sm-mix64 seed2) (StdGen seed2 gamma)))]))

;; randomR lo hi: a uniform Integer in the INCLUSIVE range [lo, hi] and
;; the advanced generator (slight modulo bias — best-effort).
(: random-r (-> Integer (-> Integer (-> StdGen (Pair Integer StdGen)))))
(define (random-r lo hi g)
  (match (next-word g)
    [(Pair w g2) (Pair (+ lo (mod w (+ (- hi lo) 1))) g2)]))

;; split: two decorrelated generators derived from g.
(: split (-> StdGen (Pair StdGen StdGen)))
(define (split g)
  (match (next-word g)
    [(Pair w1 g2)
     (match (next-word g2)
       [(Pair w2 _)
        (Pair (StdGen (sm-mix64 w1) sm-gamma)
                (StdGen (sm-mix64 w2) sm-gamma))])]))
