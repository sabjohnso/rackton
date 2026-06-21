#lang rackton

;; Multi-parameter type classes.  Runtime dispatch is still on the
;; first argument whose type mentions a class parameter; the other
;; parameter(s) are resolved at compile time.  An ascription
;; disambiguates the result type when needed.

(require "../unit.rkt")

(protocol (Convertible a b)
  (: convert (-> a b)))

(instance (Convertible Integer String)
  (define (convert n) (show n)))

(instance (Convertible Boolean String)
  (define (convert b) (if b "yes" "no")))

(: int-to-string (-> Integer String))
(define (int-to-string n) (convert n))

(: bool-to-string (-> Boolean String))
(define (bool-to-string b) (convert b))

(: suite (List Test))
(define suite
  (list
   (it "multi-parameter class dispatches by first arg's type"
       (all-checks
        (list (check-equal? (int-to-string 42)    "42")
              (check-equal? (bool-to-string #t)   "yes")
              (check-equal? (bool-to-string #f)   "no"))))))

(: main Unit)
(define main (run-io (run-suite "multiparam" suite)))
