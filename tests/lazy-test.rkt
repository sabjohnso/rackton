#lang rackton

;; End-to-end tests for first-class laziness: the `Lazy a` type, the
;; `delay` form, and `force` from rackton/data/lazy.
;;
;; `delay` defers a computation (call-by-need); `force` runs it at most
;; once and caches the result.  Deferral and memoization are only
;; observable through an effect, so we delay a counting IO action and
;; watch the count: it stays 0 until the first `force`, and two `force`s
;; leave it at 1 (a non-memoizing thunk would reach 2).

(require rackton/data/lazy
         "../system/ref.rkt"
         "../unit.rkt")

;; Pure value through delay/force.
(: tripled Integer)
(define tripled (force (delay (+ 1 2))))

;; (before . after) counter readings around two forces of one Lazy.
(: defer+memo (IO (Pair Integer Integer)))
(define defer+memo
  (do [r <- (make-ref 0)]
    (let ([lz (delay (run-io (do [n <- (read-ref r)]
                               [_ <- (write-ref r (+ n 1))]
                               (pure n))))])
      (do [before <- (read-ref r)]      ; 0: built but not forced (deferral)
        [_ <- (pure (force lz))]       ; force #1 — runs the effect
        [_ <- (pure (force lz))]       ; force #2 — cached, no effect
        [after <- (read-ref r)]        ; 1: memoized (non-memo would be 2)
        (pure (Pair before after))))))

(: suite (List Test))
(define suite
  (list
    (it "force (delay e) yields e's value"
        (check-equal? tripled 3))
    (it "delay defers and force memoizes"
        (check-equal? (run-io defer+memo) (Pair 0 1)))))

(: test-main (IO Unit))
(define test-main (run-suite "lazy" suite))
