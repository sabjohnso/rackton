#lang rackton

;; Prelude exercise: Eq with a default neq, Ord as a subclass of Eq,
;; instances over Integer and Maybe.  Class methods use distinct names
;; (`eq`, `neq`, `lt`, `gt`) so they don't shadow the builtin operators
;; the instances need to call.

(require "../unit.rkt")

(protocol (Eq a)
          (: eq  (-> a (-> a Boolean)))
          (: neq (-> a (-> a Boolean)))
          (define (neq x y)
            (if (eq x y) #f #t)))

(protocol (Ord [a => Eq])
          (: lt (-> a (-> a Boolean)))
          (: gt (-> a (-> a Boolean)))
          (define (gt x y) (lt y x)))

(instance (Eq Integer)
  (define (eq x y) (= x y)))

(instance (Ord Integer)
  (define (lt x y) (< x y)))

(data (Maybe a) None (Some a))

(instance ((Eq a) => (Eq (Maybe a)))
  (define (eq x y)
    (match x
      [(None)
       (match y [(None) #t] [(Some _) #f])]
      [(Some xv)
       (match y [(None) #f] [(Some yv) (eq xv yv)])])))

;; A None annotated at a concrete element type, so the Eq (Maybe a)
;; instance resolves to a ground type (the bare None is polymorphic).
(: none-int (Maybe Integer))
(define none-int None)

(: suite (List Test))
(define suite
  (list
    (it "default neq dispatches via eq"
        (all-checks
          (list (check-true  (neq 1 2))
                (check-false (neq 1 1)))))
    (it "Ord gt via default that calls lt"
        (all-checks
          (list (check-true  (gt 3 1))
                (check-false (gt 1 3)))))
    (it "Eq carries through ADT"
        (all-checks
          (list (check-true  (eq none-int none-int))
                (check-false (eq none-int (Some 1)))
                (check-true  (neq (Some 1) (Some 2)))
                (check-false (neq (Some 1) (Some 1))))))))

(: test-main (IO Unit))
(define test-main (run-suite "prelude (user Eq/Ord)" suite))
