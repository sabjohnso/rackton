#lang rackton

;; A user multi-parameter class whose instance head is the function arrow
;; `(->)` must compile and dispatch.  This exercises the codegen path
;; `tags-for-instance-head`, which special-cases the primitive tycons but
;; needs a case for `->` (a procedure value dispatches as the `->` tycon).
;; The prelude's own `(Arrow (->))` instances dodge this because their
;; runtime impls are hand-registered rather than codegen'd, so this is the
;; first *user* `->`-headed instance.

(require "../unit.rkt")

(protocol (TwoP (cat :: (-> * (-> * *))) (p :: (-> * (-> * *))))
          (:fundep cat -> p)
          (: tp-first (-> (cat a b) (cat (p a c) (p b c)))))

(instance (TwoP (->) Pair)
  (define (tp-first f)
    (lambda (pr) (match pr [(Pair a c) (Pair (f a) c)]))))

(: inc (-> Integer Integer))
(define (inc x) (+ x 1))

(: r (Pair Integer Integer))
(define r ((tp-first inc) (Pair 3 100)))

(: suite (List Test))
(define suite
  (list (it "(->)-headed multi-param instance compiles and dispatches"
            (check-equal? r (Pair 4 100)))))

(: test-main (IO Unit))
(define test-main (run-suite "instance-head-arrow" suite))
