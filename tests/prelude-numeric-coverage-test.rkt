#lang rackton

;; Coverage-gap fills (Coverage.org Phase 4) — numeric tower.
;;
;; The dispatch coverage matrix flagged prelude numeric instances no test
;; exercised: the whole tower was tested on Float (and Num/Ord/Show on
;; Rational/Complex), but never:
;;   - Fractional on Rational / Complex  (`float-div`)
;;   - RealFrac  on Rational             (`floor-real`/`ceiling-real`/…)
;;   - Floating  on Complex              (`sqrt`/`exp`/…)
;; These are prelude instances (never monomorphized), so a concrete call
;; would have dispatched — and none did.  They work; they just had no
;; test.  Results below were confirmed by hand before pinning.

(require "../unit.rkt"
         rackton/data/complex)   ;; complex literals (3.0+4.0i)

;; ----- Fractional / RealFrac on Rational ----------------------------

;; float-div is true division on the field of rationals: 3/2 ÷ 1/4 = 6.
(: frac-rat String) (define frac-rat (show (float-div 3/2 1/4)))

(: floor-rat    Integer) (define floor-rat    (floor-real 7/2))    ; 3
(: ceil-rat     Integer) (define ceil-rat     (ceiling-real 7/2))  ; 4
(: round-rat    Integer) (define round-rat    (round-real 7/2))    ; 4 (to even)
(: trunc-rat    Integer) (define trunc-rat    (truncate-real 7/2)) ; 3 (toward 0)
(: trunc-rat-neg Integer)(define trunc-rat-neg (truncate-real -7/2)); -3

;; ----- Fractional / Floating on Complex -----------------------------

;; (4+2i) / 2 = 2+1i
(: frac-cplx String) (define frac-cplx (show (float-div 4.0+2.0i 2.0+0.0i)))
;; sqrt(-1) = i ;  exp(0) = 1
(: sqrt-cplx String) (define sqrt-cplx (show (sqrt -1.0+0.0i)))
(: exp-cplx  String) (define exp-cplx  (show (exp 0.0+0.0i)))

;; ----- suite --------------------------------------------------------

(: suite Test)
(define suite
  (describe "previously-untested prelude numeric-tower instances"
            (it "Fractional Rational" (check-equal? frac-rat "6"))
            (it "RealFrac Rational"
                (all-checks (list (check-equal? floor-rat 3)
                                  (check-equal? ceil-rat  4)
                                  (check-equal? round-rat 4)
                                  (check-equal? trunc-rat 3)
                                  (check-equal? trunc-rat-neg -3))))
            (it "Fractional Complex" (check-equal? frac-cplx "2.0+1.0i"))
            (it "Floating Complex"
                (all-checks (list (check-equal? sqrt-cplx "0.0+1.0i")
                                  (check-equal? exp-cplx  "1.0+0.0i"))))))

(: test-main (IO Unit))
(define test-main (run-suite-tree suite))
