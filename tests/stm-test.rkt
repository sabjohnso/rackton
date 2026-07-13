#lang rackton

;; Software transactional memory.  TVar/STM/atomically/
;; retry/or-else with optimistic concurrency control.

(require rackton/control/stm
         rackton/control/concurrent
         "../unit.rkt")

;; ----- 41.A single-threaded mutation ------------------------

(: simple-mutate (IO Integer))
(define simple-mutate
  (atomically
    (let& ([tv (new-tvar 41)]
           [_  (write-tvar tv 42)])
      (read-tvar tv))))

;; ----- 41.B two-threaded shared counter ---------------------
;; Each thread increments the same TVar 100 times in its own
;; transactions.  With proper STM serialization, the final value
;; equals 200 — no lost updates.

(: increment-tvar (-> (TVar Integer) (STM Unit)))
(define (increment-tvar tv)
  (let& ([v (read-tvar tv)])
    (write-tvar tv (+ v 1))))

(: bump-n-times (-> (TVar Integer) (-> Integer (IO Unit))))
(define (bump-n-times tv n)
  (if (== n 0)
    (pure Unit)
    (let& ([_ (atomically (increment-tvar tv))])
      (bump-n-times tv (- n 1)))))

(: concurrent-counter (IO Integer))
(define concurrent-counter
  (let& ([tv  (atomically (new-tvar 0))]
         [t1  (fork-io (bump-n-times tv 100))]
         [t2  (fork-io (bump-n-times tv 100))]
         [_   (wait-thread t1)]
         [_   (wait-thread t2)])
    (atomically (read-tvar tv))))

;; ----- 41.C or-else falls back on retry --------------------

(: orelse-result (IO Integer))
(define orelse-result
  (atomically
    (or-else retry
             (pure 7))))

;; ----- 41.D do-notation chain over STM ---------------------

(: do-chain-result (IO Integer))
(define do-chain-result
  (atomically
    (let& ([a (new-tvar 1)]
           [b (new-tvar 2)]
           [_ (write-tvar a 10)]
           [_ (write-tvar b 20)]
           [av (read-tvar a)]
           [bv (read-tvar b)])
      (pure (+ av bv)))))

(: suite (List Test))
(define suite
  (list
    (it "single-threaded TVar mutation"
        (check-equal? (run-io simple-mutate) 42))
    (it "concurrent counter: 2 threads × 100 increments = 200"
        (check-equal? (run-io concurrent-counter) 200))
    (it "or-else falls back on retry"
        (check-equal? (run-io orelse-result) 7))
    (it "do-notation chain over STM"
        (check-equal? (run-io do-chain-result) 30))))

(: test-main (IO Unit))
(define test-main (run-suite "STM" suite))
