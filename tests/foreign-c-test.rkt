#lang rackton

;; rackton/foreign/c — curated bindings to libm functions that aren't in
;; the prelude, demonstrating external C-function binding via `foreign`.

(require rackton/foreign/c
         "../unit.rkt")

(: r-cbrt   Float) (define r-cbrt   (c-cbrt 27.0))
(: r-hypot  Float) (define r-hypot  (c-hypot 3.0 4.0))
(: r-expm1  Float) (define r-expm1  (c-expm1 0.0))
(: r-log1p  Float) (define r-log1p  (c-log1p 0.0))
(: r-tgamma Float) (define r-tgamma (c-tgamma 5.0))   ; Γ(5) = 4! = 24
(: r-erf    Float) (define r-erf    (c-erf 0.0))

;; ---------- assertions ---------------------------------------

(: suite (List Test))
(define suite
  (list
   (it "single-argument libm"
       (all-checks
        (list (check-true (< (abs (- r-cbrt 3.0)) 1e-9))
              (check-true (< (abs (- r-expm1 0.0)) 1e-9))
              (check-true (< (abs (- r-log1p 0.0)) 1e-9))
              (check-true (< (abs (- r-tgamma 24.0)) 1e-9))
              (check-true (< (abs (- r-erf 0.0)) 1e-9)))))
   (it "two-argument libm"
       (check-true (< (abs (- r-hypot 5.0)) 1e-9)))))

(: _ran Unit)
(define _ran (run-io (run-suite "foreign-c" suite)))
