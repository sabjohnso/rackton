#lang rackton

;; (class-out C) — exports the class plus every method.
;; `helper` is intentionally not exported.  Box is exported via
;; (data-out ...) so a client can construct values to call the method
;; on.

(define-class (Sized a)
  (: my-size (-> a Integer)))

(define-data Sphere (MkSphere Integer))

(define-instance (Sized Sphere)
  (define (my-size s)
    (match s
      [(MkSphere n) n])))

(: helper (-> Integer Integer))
(define (helper n) (+ n 1))

(provide (class-out Sized)
         (data-out Sphere))
