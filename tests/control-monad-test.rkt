#lang racket/base

;; rackton/control/monad — Control.Monad combinators over any (Monad m).
;; Exercised here at the Maybe and List monads.

(require rackunit
         "../main.rkt")

(rackton
  (require rackton/control/monad)

  ;; map-m over Maybe: short-circuits on None.
  (: mm-ok  (Maybe (List Integer)))
  (define mm-ok  (map-m (lambda (x) (if (> x 0) (Some x) None)) (list 1 2 3)))
  (: mm-bad (Maybe (List Integer)))
  (define mm-bad (map-m (lambda (x) (if (> x 0) (Some x) None)) (list 1 0 3)))

  ;; sequence-m over Maybe
  (: seq-ok  (Maybe (List Integer)))
  (define seq-ok  (sequence-m (list (Some 1) (Some 2))))
  (: seq-bad (Maybe (List Integer)))
  (define seq-bad (sequence-m (list (Some 1) None (Some 3))))

  ;; for-m is flipped map-m
  (: form (Maybe (List Integer)))
  (define form (for-m (list 1 2) (lambda (x) (Some (* x 10)))))

  ;; fold-m over Maybe
  (: fm (Maybe Integer))
  (define fm (fold-m (lambda (acc x) (if (>= x 0) (Some (+ acc x)) None)) 0 (list 1 2 3)))

  ;; replicate-m over Maybe
  (: rm (Maybe (List Integer)))
  (define rm (replicate-m 3 (Some 7)))

  ;; filter-m over Maybe
  (: flm (Maybe (List Integer)))
  (define flm (filter-m (lambda (x) (Some (> x 1))) (list 1 2 3)))

  ;; sequence-m over the List monad (cartesian product)
  (: seq-list (List (List Integer)))
  (define seq-list (sequence-m (list (list 1 2) (list 3 4)))))

;; ---------- assertions ---------------------------------------

(define (lst . xs) (let loop ([xs xs]) (if (null? xs) Nil (Cons (car xs) (loop (cdr xs))))))

(test-case "map-m / sequence-m / for-m over Maybe"
  (check-equal? mm-ok  (Some (lst 1 2 3)))
  (check-equal? mm-bad None)
  (check-equal? seq-ok (Some (lst 1 2)))
  (check-equal? seq-bad None)
  (check-equal? form   (Some (lst 10 20))))

(test-case "fold-m / replicate-m / filter-m over Maybe"
  (check-equal? fm  (Some 6))
  (check-equal? rm  (Some (lst 7 7 7)))
  (check-equal? flm (Some (lst 2 3))))

(test-case "sequence-m over the List monad (cartesian)"
  (check-equal? seq-list
                (lst (lst 1 3) (lst 1 4) (lst 2 3) (lst 2 4))))
