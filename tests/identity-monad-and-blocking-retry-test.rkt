#lang racket/base

;; Resolve deferred items.
;;   - Blocking retry
;;   - Identity monad + Concurrent Identity
;;   - Num/Ord refactor: abs/negate as Num; min/max as Ord

(require rackunit
         "../main.rkt")

(rackton
  ;; ----- 44.1 blocking retry ----------------------------------
  ;; A blocked transaction waits until a watched TVar changes,
  ;; then completes.  We exercise the wake-up by having a second
  ;; thread bump the TVar; the first thread's retry-until-positive
  ;; should eventually return.

  (: positive-then-double (-> (TVar Integer) (STM Integer)))
  (define (positive-then-double tv)
    (do [n <- (read-tvar tv)]
      (if (> n 0)
          (pure (* n 2))
          retry)))

  (: bump-once (-> (TVar Integer) (IO Unit)))
  (define (bump-once tv)
    (atomically
     (do [n <- (read-tvar tv)]
       (write-tvar tv (+ n 1)))))

  (: blocking-retry-result (IO Integer))
  (define blocking-retry-result
    (do [tv <- (atomically (new-tvar 0))]
        ;; Spawn a thread that bumps the TVar past zero.  The main
        ;; thread's atomically blocks on retry until that happens.
        [t  <- (fork-io (bump-once tv))]
        [v  <- (atomically (positive-then-double tv))]
        [_  <- (wait-thread t)]
      (pure v)))

  ;; ----- 44.4 Mock Concurrent via Identity --------------------

  (: par-pair ((Concurrent m) => (-> (m a) (-> (m b) (m (Pair a b))))))
  (define (par-pair ma mb)
    (do [fa <- (fork-c ma)]
        [fb <- (fork-c mb)]
        [a  <- (await-c fa)]
        [b  <- (await-c fb)]
      (pure (MkPair a b))))

  (: id-par-pair (Identity (Pair Integer String)))
  (define id-par-pair
    (par-pair (pure 7) (pure "ok")))

  (: id-par-pair-result (Pair Integer String))
  (define id-par-pair-result (run-identity id-par-pair))

  ;; ----- 44.5 Num/Ord refactor --------------------------------

  ;; abs / negate as Num methods, so they polymorphic over the
  ;; numeric tower.

  (: abs-int   Integer)
  (define abs-int   (abs -7))

  (: abs-float Float)
  (define abs-float (abs -7.5))

  (: abs-rat   Rational)
  (define abs-rat   (abs (make-rational -1 2)))

  (: neg-int   Integer)
  (define neg-int   (negate 5))

  (: neg-float Float)
  (define neg-float (negate 5.5))

  (: neg-rat   Rational)
  (define neg-rat   (negate (make-rational 1 3)))

  ;; min / max as Ord methods — same dispatch path as <.

  (: min-int   Integer)
  (define min-int (min 3 7))

  (: max-float Float)
  (define max-float (max 1.5 2.5))

  (: min-str   String)
  (define min-str (min "alpha" "beta")))

;; ---------- assertions ---------------------------------------

(test-case "STM blocking retry: main thread waits until TVar > 0"
  (check-equal? (run-io blocking-retry-result) 2))

(test-case "Mock Concurrent via Identity — par-pair runs synchronously"
  (check-equal? id-par-pair-result (MkPair 7 "ok")))

(test-case "abs polymorphic via Num: Integer / Float / Rational"
  (check-equal? abs-int    7)
  (check-equal? abs-float  7.5)
  (check-equal? abs-rat    (make-rational 1 2)))

(test-case "negate polymorphic via Num: Integer / Float / Rational"
  (check-equal? neg-int   -5)
  (check-equal? neg-float -5.5)
  (check-equal? neg-rat   (make-rational -1 3)))

(test-case "min / max polymorphic via Ord"
  (check-equal? min-int    3)
  (check-equal? max-float  2.5)
  (check-equal? min-str    "alpha"))
