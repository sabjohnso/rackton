#lang rackton

;; Sanity check on the small stdlib: not, and, or, length,
;; foldr, filter.

(require "../unit.rkt")

(define b (not (not #t)))
(define a-and (and #t (not #f)))
(define a-or  (or  #f #t))

(define xs (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))))

(define n  (length xs))
(define sm (foldr (lambda (a b) (+ a b)) 0 xs))
;; filter to elements strictly greater than 2
(define big (filter (lambda (i) (< 2 i)) xs))
(define len-of-big (length big))

(: suite (List Test))
(define suite
  (list
    (it "Boolean ops"
        (all-checks
          (list (check-true  b)
                (check-true  a-and)
                (check-true  a-or))))
    (it "List helpers"
        (all-checks
          (list (check-equal? n 4)
                (check-equal? sm 10)
                (check-equal? len-of-big 2)
                (check-equal? big (Cons 3 (Cons 4 Nil))))))))

(: test-main (IO Unit))
(define test-main (run-suite "stdlib" suite))
