#lang rackton

;; Behaviour of the prelude `Enum` protocol and its `Integer` instance.
;;
;; The tests read as sentences: a `describe` names a subject and the
;; nested `it` states what it does.  One `describe` per method relates
;; the methods, all under a single top-level `describe` naming the
;; subject they belong to, so the report reads
;; "the Integer instance of the Enum protocol > succ > steps to the next
;; integer".

(require "../unit.rkt")

;; integer->enum is return-typed (`a` appears only in the result), so the
;; annotation below is what pins this call to the (Enum Integer) instance.
(: five Integer)
(define five (integer->enum 5))

(: enum-integer Test)
(define enum-integer
  (describe "the Integer instance of the Enum protocol"

    (describe "succ"
      (it "steps to the next integer"
          (check-equal? (succ 41) 42)))

    (describe "pred"
      (it "steps to the previous integer"
          (check-equal? (pred 42) 41)))

    (describe "enum->integer"
      (it "is the identity for Integer"
          (check-equal? (enum->integer 7) 7)))

    (describe "integer->enum"
      (it "resolves by its return type to the Integer instance"
          (check-equal? five 5))
      (it "is a left inverse of enum->integer"
          (check-equal? (integer->enum (enum->integer 99)) 99)))

    (describe "enum-from-to"
      (it "enumerates an inclusive range"
          (check-equal? (enum-from-to 1 5) (list 1 2 3 4 5)))
      (it "is empty when the low bound exceeds the high"
          (check-equal? (enum-from-to 5 1) Nil))
      (it "yields a single element when the bounds are equal"
          (check-equal? (enum-from-to 3 3) (list 3))))

    (describe "enum-from-then-to"
      (it "steps by the delta between the first two bounds"
          (check-equal? (enum-from-then-to 1 3 9) (list 1 3 5 7 9)))
      (it "descends when the step is negative"
          (check-equal? (enum-from-then-to 5 4 1) (list 5 4 3 2 1)))
      (it "stops before overshooting the high bound"
          (check-equal? (enum-from-then-to 1 3 8) (list 1 3 5 7))))))

(: main Unit)
(define main (run-io (run-suite-tree enum-integer)))
