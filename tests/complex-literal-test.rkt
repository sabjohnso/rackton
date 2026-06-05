#lang rackton

;; Complex literal syntax and the exact-complex type.
;;
;;   3.0+4.0i  reads as an inexact Complex   (Float components)
;;   3+4i      reads as an exact ComplexExact (Integer components)
;;
;; Racket's reader already parses both; these tests pin that inference
;; types them, that arithmetic / Eq / Show work, and that the exact
;; type carries its own constructor, accessors, and derived ops.  The
;; 3-4-5 triangle keeps the exact norm (25) and the inexact magnitude
;; (5.0) clean.

(require rackton/data/complex
         "../unit.rkt")

;; ----- inexact complex literals (Complex) -------------------

(: zlit Complex)     (define zlit 3.0+4.0i)
(: zmk Complex)      (define zmk (make-complex 3.0 4.0))
(: zlit-re Float)    (define zlit-re (real-part zlit))
(: zlit-im Float)    (define zlit-im (imag-part zlit))
(: zlit-sum Complex) (define zlit-sum (+ 1.0+2.0i 2.0+4.0i))
(: zlit-show String) (define zlit-show (show 3.0+4.0i))

;; ----- exact complex literals (ComplexExact) ----------------

(: e1 ComplexExact)      (define e1 3+4i)
(: e1mk ComplexExact)    (define e1mk (make-complex-exact 3 4))
(: e1-re Integer)        (define e1-re (real-part-exact e1))
(: e1-im Integer)        (define e1-im (imag-part-exact e1))
(: e-sum ComplexExact)   (define e-sum (+ 1+2i 2+2i))   ;; = 3+4i
(: e-show String)        (define e-show (show 3+4i))

;; ----- derived exact operations -----------------------------

(: e-conj ComplexExact)  (define e-conj (conjugate-exact 3+4i))   ;; 3-4i
(: e-conj-im Integer)    (define e-conj-im (imag-part-exact e-conj))
(: e-norm Integer)       (define e-norm (complex-exact-norm 3+4i))
(: e->c Complex)         (define e->c (complex-exact->complex 3+4i))
(: e->c-re Float)        (define e->c-re (real-part e->c))

;; ----- literal pattern match on an exact complex ------------

(: classify-e (-> ComplexExact String))
(define (classify-e z)
  (match z
    [3+4i "three-four"]
    [_    "other"]))

(: cls String)       (define cls (classify-e 3+4i))
(: cls-other String) (define cls-other (classify-e 1+1i))

;; ---------- assertions --------------------------------------

(: suite (List Test))
(define suite
  (list
   (it "inexact complex literal 3.0+4.0i"
       (all-checks
        (list (check-equal? zlit zmk)
              (check-equal? zlit-re 3.0)
              (check-equal? zlit-im 4.0)
              (check-equal? zlit-sum (make-complex 3.0 6.0))
              (check-equal? zlit-show "3.0+4.0i"))))
   (it "exact complex literal 3+4i"
       (all-checks
        (list (check-equal? e1 e1mk)
              (check-equal? e1-re 3)
              (check-equal? e1-im 4)
              (check-equal? e-sum (make-complex-exact 3 4))
              (check-equal? e-show "3+4i"))))
   (it "derived exact operations"
       (all-checks
        (list (check-equal? e-conj-im -4)
              (check-equal? e-norm 25)
              (check-equal? e->c-re 3.0))))
   (it "exact complex literal pattern"
       (all-checks
        (list (check-equal? cls "three-four")
              (check-equal? cls-other "other"))))))

(: _ran Unit)
(define _ran (run-io (run-suite "complex literals" suite)))
