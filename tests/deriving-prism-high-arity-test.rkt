#lang racket/base

;; A multi-field prism focuses a flat VARIADIC tuple `(tuple x …)`, so
;; there is no arity limit (the old Pair + Tuple3..Tuple7 ladder capped
;; it at 7).  A ctor with 8 — or more — fields derives a Prism whose
;; focus is an 8-tuple, previews/reviews round-trip through `tref`.

(require rackunit
         "../main.rkt")

(rackton
 (require rackton/data/lens)

 (data Huge
   (H8 Integer Integer Integer Integer Integer Integer Integer Integer)
   :deriving Prism)

 ;; preview yields the flat 8-tuple; pull its last element via tref.
 (: eighth Integer)
 (define eighth
   (match (preview Huge-H8-prism (H8 1 2 3 4 5 6 7 8))
     [(Some t) (tref t 7)]
     [(None)   0]))

 ;; review rebuilds the constructor from a flat 8-tuple.
 (: round-ok Boolean)
 (define round-ok
   (match (review Huge-H8-prism (tuple 1 2 3 4 5 6 7 8))
     [(H8 _ _ _ _ _ _ _ h) (== h 8)])))

(test-case "8-field prism focuses a flat 8-tuple and round-trips"
  (check-equal? eighth 8)
  (check-true round-ok))
