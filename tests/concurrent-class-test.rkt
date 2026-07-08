#lang rackton

;; Concurrent class.  Polymorphic fork/await/yield over
;; arbitrary monads; one Concurrent IO instance.

(require "../unit.rkt")

;; ----- 43.A direct IO use of fork-c / await-c ----------------

(: parallel-sum (IO Integer))
(define parallel-sum
  (do [f1 <- (fork-c (pure 10))]
    [f2 <- (fork-c (pure 32))]
    [a  <- (await-c f1)]
    [b  <- (await-c f2)]
    (pure (+ a b))))

;; ----- 43.B polymorphic-monad parallelization ----------------
;; A helper that takes two computations and runs them in parallel,
;; returning their pair.  Polymorphic over any Concurrent monad —
;; today instantiated only to IO.

(: par-pair ((Concurrent m) => (-> (m a) (-> (m b) (m (Pair a b))))))
(define (par-pair ma mb)
  (do [fa <- (fork-c ma)]
    [fb <- (fork-c mb)]
    [a  <- (await-c fa)]
    [b  <- (await-c fb)]
    (pure (Pair a b))))

(: par-pair-result (IO (Pair Integer String)))
(define par-pair-result
  (par-pair (pure 7) (pure "ok")))

;; ----- 43.C yield-c is callable but has no observable effect --
;; Just make sure it type-checks and produces Unit.

(: yield-result (IO Unit))
(define yield-result yield-c)

;; ---------- assertions ---------------------------------------

(: r-sum Integer)              (define r-sum (run-io parallel-sum))
(: r-par (Pair Integer String)) (define r-par (run-io par-pair-result))
(: r-yield Unit)               (define r-yield (run-io yield-result))

(: suite (List Test))
(define suite
  (list
    (it "fork-c + await-c in IO"
        (check-equal? r-sum 42))
    (it "par-pair polymorphic over Concurrent, instantiated at IO"
        (check-equal? r-par (Pair 7 "ok")))
    (it "yield-c is callable"
        (check-equal? r-yield Unit))))

(: test-main (IO Unit))
(define test-main (run-suite "concurrent-class" suite))
