#lang rackton

;; rackton/numeric/integer — Integral helper operations over Integer
;; (gcd, lcm, signum, parity, factorial, integer power).

(require rackton/numeric/integer
         "../unit.rkt")

(: even-4  Boolean) (define even-4  (num-even? 4))
(: even-5  Boolean) (define even-5  (num-even? 5))
(: odd-3   Boolean) (define odd-3   (num-odd? 3))

(: sig-neg Integer) (define sig-neg (num-signum -5))
(: sig-zero Integer)(define sig-zero (num-signum 0))
(: sig-pos Integer) (define sig-pos (num-signum 7))

(: gcd-12-8 Integer)(define gcd-12-8 (num-gcd 12 8))
(: gcd-0-5  Integer)(define gcd-0-5  (num-gcd 0 5))
(: lcm-4-6  Integer)(define lcm-4-6  (num-lcm 4 6))
(: lcm-0-5  Integer)(define lcm-0-5  (num-lcm 0 5))

(: fact-5   Integer)(define fact-5   (num-factorial 5))
(: fact-0   Integer)(define fact-0   (num-factorial 0))

(: pow-2-10 Integer)(define pow-2-10 (num-int-pow 2 10))
(: pow-3-0  Integer)(define pow-3-0  (num-int-pow 3 0))

(: from-3   Float)  (define from-3   (num-from-integral 3))

;; ---------- assertions ---------------------------------------

(: suite (List Test))
(define suite
  (list
   (it "parity"
       (all-checks
        (list (check-true  even-4)
              (check-false even-5)
              (check-true  odd-3))))
   (it "signum"
       (all-checks
        (list (check-equal? sig-neg -1)
              (check-equal? sig-zero 0)
              (check-equal? sig-pos 1))))
   (it "gcd / lcm"
       (all-checks
        (list (check-equal? gcd-12-8 4)
              (check-equal? gcd-0-5 5)
              (check-equal? lcm-4-6 12)
              (check-equal? lcm-0-5 0))))
   (it "factorial"
       (all-checks
        (list (check-equal? fact-5 120)
              (check-equal? fact-0 1))))
   (it "integer power"
       (all-checks
        (list (check-equal? pow-2-10 1024)
              (check-equal? pow-3-0 1))))
   (it "from-integral"
       (check-equal? from-3 3.0))))

(: _ran Unit)
(define _ran (run-io (run-suite "numeric-integer" suite)))
