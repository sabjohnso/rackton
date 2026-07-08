#lang rackton

;; rackton/numeric/real — Floating / RealFrac extras: inverse and
;; hyperbolic trig, log-with-base, properFraction.

(require rackton/numeric/real
         "../unit.rkt")

(: asin-0 Float) (define asin-0 (num-asin 0.0))
(: acos-1 Float) (define acos-1 (num-acos 1.0))
(: atan-0 Float) (define atan-0 (num-atan 0.0))

(: sinh-0 Float) (define sinh-0 (num-sinh 0.0))
(: cosh-0 Float) (define cosh-0 (num-cosh 0.0))
(: tanh-0 Float) (define tanh-0 (num-tanh 0.0))

(: lb-2-8   Float) (define lb-2-8   (num-log-base 2.0 8.0))
(: lb-10-1k Float) (define lb-10-1k (num-log-base 10.0 1000.0))

;; properFraction truncates toward zero: (whole, fractional)
(: pf-pos-i Integer) (define pf-pos-i (match (num-proper-fraction 3.7)  [(Pair i _) i]))
(: pf-pos-f Float)   (define pf-pos-f (match (num-proper-fraction 3.7)  [(Pair _ f) f]))
(: pf-neg-i Integer) (define pf-neg-i (match (num-proper-fraction -3.7) [(Pair i _) i]))
(: pf-neg-f Float)   (define pf-neg-f (match (num-proper-fraction -3.7) [(Pair _ f) f]))

;; ---------- assertions ---------------------------------------

(: suite (List Test))
(define suite
  (list
    (it "inverse trig"
        (all-checks
          (list (check-true (< (abs (- asin-0 0.0)) 1e-9))
                (check-true (< (abs (- acos-1 0.0)) 1e-9))
                (check-true (< (abs (- atan-0 0.0)) 1e-9)))))
    (it "hyperbolic trig"
        (all-checks
          (list (check-true (< (abs (- sinh-0 0.0)) 1e-9))
                (check-true (< (abs (- cosh-0 1.0)) 1e-9))
                (check-true (< (abs (- tanh-0 0.0)) 1e-9)))))
    (it "log base"
        (all-checks
          (list (check-true (< (abs (- lb-2-8   3.0)) 1e-9))
                (check-true (< (abs (- lb-10-1k 3.0)) 1e-9)))))
    (it "proper fraction"
        (all-checks
          (list (check-equal? pf-pos-i 3)
                (check-true (< (abs (- pf-pos-f 0.7)) 1e-9))
                (check-equal? pf-neg-i -3)
                (check-true (< (abs (- pf-neg-f -0.7)) 1e-9)))))))

(: test-main (IO Unit))
(define test-main (run-suite "numeric-real" suite))
