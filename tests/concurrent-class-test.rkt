#lang racket/base

;; Concurrent class.  Polymorphic fork/await/yield over
;; arbitrary monads; one Concurrent IO instance.

(require rackunit
         "../main.rkt")

(rackton
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
      (pure (MkPair a b))))

  (: par-pair-result (IO (Pair Integer String)))
  (define par-pair-result
    (par-pair (pure 7) (pure "ok")))

  ;; ----- 43.C yield-c is callable but has no observable effect --
  ;; Just make sure it type-checks and produces Unit.

  (: yield-result (IO Unit))
  (define yield-result yield-c))

;; ---------- assertions ---------------------------------------

(test-case "fork-c + await-c in IO"
  (check-equal? (run-io parallel-sum) 42))

(test-case "par-pair polymorphic over Concurrent, instantiated at IO"
  (check-equal? (run-io par-pair-result) (MkPair 7 "ok")))

(test-case "yield-c is callable"
  (check-equal? (run-io yield-result) MkUnit))
