#lang rackton

;; type-level-recursion.rkt — a type family that recurses over promoted
;; data, computing a type-level result.
;;
;; `Plus` adds two promoted Peano naturals; a clause whose right-hand
;; side mentions the family recurses, bounded by a fuel budget.  We index
;; a `Counted` value by a Peano "size" and let `combine` track the sum of
;; the sizes IN THE TYPE while adding the payloads at runtime.
;;
;; Run it with `racket examples/type-level-recursion.rkt`.

;; ----- promoted Peano naturals + type-level addition ----------------

(data Peano PZ (PS Peano))

(type-family (Plus a b)
  [PZ     b = b]
  [(PS n) b = (PS (Plus n b))])

;; ----- a size-indexed value ----------------------------------------

(data (Counted n) (MkCounted Integer))

;; The result's size index is `(Plus m n)` — computed by the recursive
;; family — while the payloads are summed at runtime.
(: combine (-> (Counted m) (Counted n) (Counted (Plus m n))))
(define (combine a b)
  (match a [(MkCounted x)
    (match b [(MkCounted y) (MkCounted (+ x y))])]))

(: amount (-> (Counted n) Integer))
(define (amount c) (match c [(MkCounted x) x]))

(: one (Counted (PS PZ)))            ;; size 1
(define one (MkCounted 10))

(: two (Counted (PS (PS PZ))))       ;; size 2
(define two (MkCounted 20))

;; `three`'s declared size is `(Plus 1 2)`; it type-checks only because
;; the family reduces that to `(PS (PS (PS PZ)))` — size 3.
(: three (Counted (Plus (PS PZ) (PS (PS PZ)))))
(define three (combine one two))

(: main (IO Unit))
(define main
  (do [_ <- (println (string-append "payload sum   = " (show (amount three))))]
      (println "size index (Plus 1 2) reduced to 3 at compile time")))

(define _go (run-io main))
