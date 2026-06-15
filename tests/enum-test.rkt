#lang rackton

;; Tests for the prelude `Enum` protocol and its `Integer` instance.
;; `Enum` maps a type's values to/from the integers (`enum->integer` /
;; `integer->enum`), steps them (`succ` / `pred`), and ranges over them
;; (`enum-from-to` / `enum-from-then-to`).  For `Integer` both
;; conversions are the identity, so `succ`/`pred` are +1/-1 and the
;; ranges are ordinary integer sequences.

(require "../unit.rkt")

;; integer->enum is return-typed (a appears only in the result), so the
;; signature pins this resolution to the (Enum Integer) instance.
(: five Integer)
(define five (integer->enum 5))

(: suite (List Test))
(define suite
  (list
   (it "succ / pred on Integer"
       (all-checks
        (list (check-equal? (succ 41) 42)
              (check-equal? (pred 42) 41))))
   (it "enum->integer is the identity for Integer"
       (check-equal? (enum->integer 7) 7))
   (it "integer->enum resolves return-typed to Integer"
       (check-equal? five 5))
   (it "round-trip integer->enum . enum->integer"
       (check-equal? (integer->enum (enum->integer 99)) 99))
   (it "enum-from-to is an inclusive range"
       (check-equal? (enum-from-to 1 5) (list 1 2 3 4 5)))
   (it "enum-from-to is empty when lo > hi"
       (check-equal? (enum-from-to 5 1) Nil))
   (it "enum-from-to with lo == hi is a singleton"
       (check-equal? (enum-from-to 3 3) (list 3)))
   (it "enum-from-then-to steps by the given delta (ascending)"
       (check-equal? (enum-from-then-to 1 3 9) (list 1 3 5 7 9)))
   (it "enum-from-then-to descends when the step is negative"
       (check-equal? (enum-from-then-to 5 4 1) (list 5 4 3 2 1)))
   (it "enum-from-then-to stops before overshooting hi"
       (check-equal? (enum-from-then-to 1 3 8) (list 1 3 5 7)))))

(: _ran Unit)
(define _ran (run-io (run-suite "enum" suite)))
