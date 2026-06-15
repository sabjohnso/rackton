#lang rackton

;; rackton/control/comonad — Comonad (dual of Monad) and ComonadApply.
;; `extract` is the dual of `pure`; `duplicate`/`extend` are dual to
;; `join`/`flatmap`.  Identity is the trivial comonad.

(require rackton/control/comonad
         rackton/control/apply
         "../unit.rkt")

;; --- extract / duplicate / extend over Identity ---------------------
(: i-extract Integer)
(define i-extract (extract (Identity 42)))

;; duplicate wraps a second layer
(: i-dup Integer)
(define i-dup (run-identity (run-identity (duplicate (Identity 7)))))

;; extend f w = fmap f (duplicate w); here f = extract, so it round-trips
(: i-extend Integer)
(define i-extend (run-identity (extend (lambda (w) (extract w)) (Identity 9))))

;; extend with a real co-Kleisli arrow (counts the wrapped value's parity)
(: i-extend2 Boolean)
(define i-extend2
  (run-identity (extend (lambda (w) (== 0 (mod (extract w) 2))) (Identity 10))))

;; --- ComonadApply over Identity (coapply defaults to apply) ---------
(: i-coapply Integer)
(define i-coapply
  (run-identity (coapply (Identity (lambda (x) (+ x 1))) (Identity 41))))

(: suite (List Test))
(define suite
  (list
   (it "extract unwraps Identity"
       (check-equal? i-extract 42))
   (it "duplicate adds a layer"
       (check-equal? i-dup 7))
   (it "extend with extract round-trips"
       (check-equal? i-extend 9))
   (it "extend applies a co-Kleisli arrow"
       (check-equal? i-extend2 #t))
   (it "coapply over Identity"
       (check-equal? i-coapply 42))))

(: _ran Unit)
(define _ran (run-io (run-suite "rackton/control/comonad" suite)))
