#lang rackton

;; data-families.rkt — a data family chooses the runtime representation
;; per index.
;;
;; `SetOf a` has no constructors of its own; each `data-instance` gives a
;; representation for one element type — Booleans pack into the bits of a
;; single Integer, Integers use an explicit list.  A match at a concrete
;; instance sees only that instance's constructor.
;;
;; Run it with `racket examples/data-families.rkt`.

(data-family (SetOf a))
(data-instance (SetOf Boolean) (BitSet  Integer))            ;; packed bits
(data-instance (SetOf Integer) (ListSet (List Integer)))     ;; explicit list

;; Count members — each instance is read through its own constructor.
(: int-count (-> (SetOf Integer) Integer))
(define (int-count s) (match s [(ListSet xs) (length xs)]))

;; popcount of the packed Booleans: 0b011 ⇒ {false, true} present.
(: bool-bits (-> (SetOf Boolean) Integer))
(define (bool-bits s) (match s [(BitSet n) n]))

(: ints (SetOf Integer))
(define ints (ListSet (list 2 3 5 7)))

(: bools (SetOf Boolean))
(define bools (BitSet 3))

(: main (IO Unit))
(define main
  (do [_ <- (println (string-append "integer set size  = " (show (int-count ints))))]
    (println (string-append "boolean set bits  = " (show (bool-bits bools))))))
