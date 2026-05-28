#lang rackton

;; (protocol-out C) — exports the class plus every method.
;; `helper` is intentionally not exported.  Box is exported via
;; (data-out ...) so a client can construct values to call the method
;; on.

(protocol (Sized a)
  (: my-size (-> a Integer)))

(data Sphere (MkSphere Integer))

(instance (Sized Sphere)
  (define (my-size s)
    (match s
      [(MkSphere n) n])))

(: helper (-> Integer Integer))
(define (helper n) (+ n 1))

(provide (protocol-out Sized)
         (data-out Sphere))
