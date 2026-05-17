#lang racket/base

;; Multi-parameter type classes.  Runtime dispatch is still on the
;; first argument whose type mentions a class parameter; the other
;; parameter(s) are resolved at compile time.  An ascription
;; disambiguates the result type when needed.

(require rackunit
         "../main.rkt")

(rackton
  (define-class (Convertible a b)
    (: convert (-> a b)))

  (define-instance (Convertible Integer String)
    (define (convert n) (show n)))

  (define-instance (Convertible Boolean String)
    (define (convert b) (if b "yes" "no")))

  (: int-to-string (-> Integer String))
  (define (int-to-string n) (convert n))

  (: bool-to-string (-> Boolean String))
  (define (bool-to-string b) (convert b)))

(test-case "multi-parameter class dispatches by first arg's type"
  (check-equal? (int-to-string 42)    "42")
  (check-equal? (bool-to-string #t)   "yes")
  (check-equal? (bool-to-string #f)   "no"))
