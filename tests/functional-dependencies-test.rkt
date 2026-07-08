#lang rackton

;; End-to-end: functional dependencies + merge sort.

(require rackton/data/list
         "../unit.rkt")

;; A multi-param class whose second parameter is functionally
;; determined by the first.  With FD improvement, calling `convert`
;; on an Integer determines the output type at compile time, so
;; downstream uses don't need ascriptions.
(protocol (Convert a b)
          (:fundep a -> b)
          (: convert (-> a b)))

(instance (Convert Integer String)
  (define (convert n) (show n)))

(instance (Convert Boolean Integer)
  (define (convert b) (if b 1 0)))

;; The result type of `convert` is determined by its argument.
;; Without the FD, this would be ambiguous: Convert Integer ? and
;; we'd need (ann ... String) at every call.
(: int->str (-> Integer String))
(define (int->str n) (convert n))

(: bool->int (-> Boolean Integer))
(define (bool->int b) (convert b))

;; Merge sort over polymorphic Ord (just verify correctness on a
;; longer list than earlier tests used).
(define many
  (Cons 5 (Cons 12 (Cons 3 (Cons 8 (Cons 1 (Cons 11
                                                 (Cons 7 (Cons 2 (Cons 9 (Cons 4 (Cons 10 (Cons 6 Nil)))))))))))))

(: sorted-many (List Integer))
(define sorted-many (sort many))

;; ----- assertions ----------------------------------------

(: suite (List Test))
(define suite
  (list
    (it "FD determines output type"
        (all-checks
          (list (check-equal? (int->str 42)   "42")
                (check-equal? (bool->int #t)  1)
                (check-equal? (bool->int #f)  0))))
    (it "Merge sort orders the input list"
        (check-equal?
          sorted-many
          (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 (Cons 6
                                                        (Cons 7 (Cons 8 (Cons 9 (Cons 10 (Cons 11 (Cons 12 Nil))))))))))))))))

(: test-main (IO Unit))
(define test-main (run-suite "functional-dependencies" suite))
