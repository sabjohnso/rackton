#lang rackton

;; Library module exporting an abstract `Counter` type.  The
;; constructor `MkCounter` is NOT re-exported because of the
;; :abstract flag.  Clients use the public API (`make-counter`,
;; `inc-counter`, `counter-value`) and can't see the ctor.

(provide (all-defined-out))

(data Counter
  (MkCounter Integer)
  :abstract)

(: make-counter (-> Integer Counter))
(define (make-counter n) (MkCounter n))

(: inc-counter (-> Counter Counter))
(define (inc-counter c)
  (match c
    [(MkCounter n) (MkCounter (+ n 1))]))

(: counter-value (-> Counter Integer))
(define (counter-value c)
  (match c
    [(MkCounter n) n]))
