#lang racket/base

;; rackton/foreign/c — curated bindings to libm functions that aren't in
;; the prelude, demonstrating external C-function binding via `foreign`.

(require rackunit
         "../main.rkt")

(rackton
  (require rackton/foreign/c)

  (: r-cbrt   Float) (define r-cbrt   (c-cbrt 27.0))
  (: r-hypot  Float) (define r-hypot  (c-hypot 3.0 4.0))
  (: r-expm1  Float) (define r-expm1  (c-expm1 0.0))
  (: r-log1p  Float) (define r-log1p  (c-log1p 0.0))
  (: r-tgamma Float) (define r-tgamma (c-tgamma 5.0))   ; Γ(5) = 4! = 24
  (: r-erf    Float) (define r-erf    (c-erf 0.0)))

;; ---------- assertions ---------------------------------------

(test-case "single-argument libm"
  (check-= r-cbrt 3.0 1e-9)
  (check-= r-expm1 0.0 1e-9)
  (check-= r-log1p 0.0 1e-9)
  (check-= r-tgamma 24.0 1e-9)
  (check-= r-erf 0.0 1e-9))

(test-case "two-argument libm"
  (check-= r-hypot 5.0 1e-9))
