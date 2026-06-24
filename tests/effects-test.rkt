#lang rackton

;; Tests for rackton/effects.  Increment 1: the row algebra and the monad
;; core.  Pure (empty-row) computations run; a computation that uses effects
;; ACCUMULATES them in its row type (the annotations below are the test —
;; they only type-check if `Union` reduces componentwise).

(require "../unit.rkt"
         "../effects.rkt")

;; pure computations: the row stays empty, so run-eff accepts them
(: prog-pure (Eff (EffRow Absent Absent) Integer))
(define prog-pure (ebind (epure 5) (lambda (x) (epure (* x 2)))))

;; tell then throw: BOTH effects land in the row (Union accumulates them)
(: prog-both (Eff (EffRow Present Present) Integer))
(define prog-both
  (ebind (tell "step")
         (lambda (u) (ebind (throw "boom") (lambda (z) (epure 1))))))

;; throw alone: Except only
(: prog-throw (Eff (EffRow Present Absent) Integer))
(define prog-throw (ebind (throw "no") (lambda (z) (epure 1))))

(: core-tests Test)
(define core-tests
  (group-of "rackton/effects — core"
    (list
     (it "epure then run-eff yields the value"
       (check-equal? (run-eff (epure 7)) 7))
     (it "a pure ebind chain runs"
       (check-equal? (run-eff prog-pure) 10))
     ;; prog-both / prog-throw only compile if the row accumulates correctly;
     ;; reaching here means the Union type family reduced as intended.
     (it "row accumulation type-checks (Union is componentwise)"
       (check-true #t)))))

;; ----- increment 2: handlers discharge effects, then run -------------

;; a failing computation: tell then throw
(: failing (Eff (EffRow Present Present) Integer))
(define failing
  (ebind (tell "begin")
         (lambda (u) (ebind (throw "boom") (lambda (z) (epure 99))))))

;; a succeeding computation: log twice, return
(: logging (Eff (EffRow Absent Present) Integer))
(define logging
  (ebind (tell "a") (lambda (u) (ebind (tell "b") (lambda (v) (epure 42))))))

;; discharge both effects on `failing`, then run
(: failing-result (Pair (Either String Integer) (List String)))
(define failing-result (run-eff (handle-writer (handle-except failing))))

;; discharge the writer on `logging`, then run
(: logging-result (Pair Integer (List String)))
(define logging-result (run-eff (handle-writer logging)))

(: handler-tests Test)
(define handler-tests
  (group-of "rackton/effects — handlers discharge to the empty row"
    (list
     (it "writer handler collects the log in order"
       (check-true (match logging-result
                     [(Pair v log) (and (== v 42) (== log (list "a" "b")))])))
     (it "except handler turns failure into a value; writer keeps the log"
       (check-true (match failing-result
                     [(Pair res log)
                      (and (match res [(Left e) (== e "boom")] [(Right z) #f])
                           (== log (list "begin")))]))))))

(: laws Test)
(define laws
  (group-of "rackton/effects — graded-monad identity laws"
    (list
     (it "left identity: ebind (epure x) k = k x"
       (check-equal? (run-eff (ebind (epure 5) (lambda (n) (epure (+ n 1)))))
                     (run-eff (epure 6))))
     (it "right identity: ebind e epure = e"
       (check-true
         (match (run-eff (handle-writer (ebind logging epure)))
           [(Pair v1 l1)
            (match logging-result
              [(Pair v2 l2) (and (== v1 v2) (== l1 l2))])]))))))

(: suite Test)
(define suite (group-of "rackton/effects" (list core-tests handler-tests laws)))

(: main Unit)
(define main (run-io (run-suite-tree suite)))
