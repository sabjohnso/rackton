#lang rackton

;; rackton/system/random — pure splittable StdGen (SplitMix64) plus
;; randomR-style IO conveniences.

(require rackton/system/random
         "../unit.rkt")

;; determinism: same seed -> same value
(: det Boolean)
(define det
  (match (random-r 1 100 (mk-std-gen 42))
    [(Pair v1 _)
     (match (random-r 1 100 (mk-std-gen 42))
       [(Pair v2 _) (== v1 v2)])]))

;; value lands in the inclusive range
(: in-range Boolean)
(define in-range
  (match (random-r 1 100 (mk-std-gen 7))
    [(Pair v _) (if (>= v 1) (<= v 100) #f)]))

;; the returned generator advances (next draw differs from the first)
(: advances Boolean)
(define advances
  (match (random-r 0 1000000000 (mk-std-gen 99))
    [(Pair a g2)
     (match (random-r 0 1000000000 g2)
       [(Pair b _) (/= a b)])]))

;; split-gen yields two decorrelated generators
(: split-diff Boolean)
(define split-diff
  (match (split-gen (mk-std-gen 123))
    [(Pair gl gr)
     (match (random-r 0 1000000000 gl)
       [(Pair a _)
        (match (random-r 0 1000000000 gr)
          [(Pair b _) (/= a b)])])]))

;; randomRIO-style IO, inclusive
(: rio (IO Integer)) (define rio (random-r-integer 5 5))
(: rfo (IO Float))   (define rfo (random-r-float 2.0 2.0))

(: r-rio Integer) (define r-rio (run-io rio))
(: r-rfo Float)   (define r-rfo (run-io rfo))

(: suite (List Test))
(define suite
  (list
    (it "pure StdGen"
        (all-checks
          (list (check-true det)
                (check-true in-range)
                (check-true advances)
                (check-true split-diff))))
    (it "randomR IO conveniences"
        (all-checks
          (list (check-equal? r-rio 5)
                (check-true (< (abs (- r-rfo 2.0)) 1e-9)))))))

(: test-main (IO Unit))
(define test-main (run-suite "rackton/system/random" suite))
