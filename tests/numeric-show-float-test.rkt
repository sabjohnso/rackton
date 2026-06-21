#lang rackton

;; rackton/numeric/show — float formatters (Numeric's showFFloat /
;; showEFloat / showGFloat).  Precision is (Maybe Integer): the number
;; of digits after the point, or None for full precision.

(require rackton/numeric/show
         "../unit.rkt")

;; fixed-point
(: f-2   String) (define f-2   (num-show-f-float (Some 2) 3.14159))
(: f-none String)(define f-none (num-show-f-float None 2.5))

;; scientific (Haskell-style exponent: no '+' or leading zeros)
(: e-3   String) (define e-3   (num-show-e-float (Some 3) 245.7))
(: e-neg String) (define e-neg (num-show-e-float (Some 2) 0.0247))

;; general: fixed inside [0.1, 1e7), scientific outside
(: g-fix String) (define g-fix (num-show-g-float (Some 2) 3.14159))
(: g-big String) (define g-big (num-show-g-float (Some 2) 12345678.0))
(: g-sml String) (define g-sml (num-show-g-float (Some 2) 0.001))

(: suite (List Test))
(define suite
  (list
   (it "showFFloat"
       (all-checks
        (list (check-equal? f-2   "3.14")
              (check-equal? f-none "2.5"))))
   (it "showEFloat"
       (all-checks
        (list (check-equal? e-3   "2.457e2")
              (check-equal? e-neg "2.47e-2"))))
   (it "showGFloat"
       (all-checks
        (list (check-equal? g-fix "3.14")
              (check-equal? g-big "1.23e7")
              (check-equal? g-sml "1.00e-3"))))))

(: main Unit)
(define main (run-io (run-suite "rackton/numeric/show (float)" suite)))
