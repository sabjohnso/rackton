#lang rackton

;; `#:deriving Ord` synthesises an Ord instance with lexicographic
;; comparison on fields and ctor-index comparison across constructors.
;; Ord implies Eq, so Ord-deriving also derives Eq.

(require "../unit.rkt")

(data Color Red Green Blue #:deriving Ord Show)

(define c1 (< Red Green))    ; #t  (Red comes first)
(define c2 (< Blue Green))   ; #f
(define c3 (== Red Red))     ; #t  (derived Eq via Ord)
(define c4 (< Red Red))      ; #f

;; Parametric data type with Ord-comparable contents
(data (Pair2 a)
  (MkPair2 a a)
  #:deriving Ord)

(define p-less (< (MkPair2 1 2) (MkPair2 1 3)))    ; #t (second field)
(define p-eq   (== (MkPair2 1 2) (MkPair2 1 2)))   ; #t
(define p-gt   (< (MkPair2 2 1) (MkPair2 1 9)))    ; #f (first field decides)

(: suite (List Test))
(define suite
  (list
   (it "ctor-index ordering"
       (all-checks
        (list (check-true  c1)
              (check-false c2)
              (check-true  c3)
              (check-false c4))))
   (it "lexicographic field ordering on parametric data"
       (all-checks
        (list (check-true  p-less)
              (check-true  p-eq)
              (check-false p-gt))))))

(: _ran Unit)
(define _ran (run-io (run-suite "deriving Ord" suite)))
