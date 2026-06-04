#lang rackton

;; rackton/control/monad/trans — MonadTrans (lift) and MonadIO (lift-io)
;; across the transformer stack.

(require rackton/control/monad/trans
         rackton/data/result
         "../unit.rkt")

;; lift a pure inner action into a transformer (MonadTrans)
(: lift-st (Maybe (Pair Integer Integer)))
(define lift-st
  ((run-state-t (ann (lift (Some 5)) (StateT Integer Maybe Integer))) 0))

(: lift-ex (Maybe (Result String Integer)))
(define lift-ex
  (run-except-t (ann (lift (Some 9)) (ExceptT String Maybe Integer))))

;; lift-io into single-layer stacks (MonadIO)
(: io-st (IO (Pair Integer Integer)))
(define io-st
  ((run-state-t (ann (lift-io (pure-io 7)) (StateT Integer IO Integer))) 0))

(: io-ex (IO (Result String Integer)))
(define io-ex
  (run-except-t (ann (lift-io (pure-io 8)) (ExceptT String IO Integer))))

(: io-wr (IO (Pair String Integer)))
(define io-wr
  (run-writer-t (ann (lift-io (pure-io 3)) (WriterT String IO Integer))))

;; lift-io through a TWO-layer stack: StateT over (ExceptT over IO)
(: io-nested (IO (Result String (Pair Integer Integer))))
(define io-nested
  (run-except-t
   ((run-state-t (ann (lift-io (pure-io 1))
                      (StateT Integer (ExceptT String IO) Integer)))
    0)))

;; ---------- assertions ---------------------------------------

(: r-io-st (Pair Integer Integer))           (define r-io-st (run-io io-st))
(: r-io-ex (Result String Integer))          (define r-io-ex (run-io io-ex))
(: r-io-wr (Pair String Integer))            (define r-io-wr (run-io io-wr))
(: r-io-nested (Result String (Pair Integer Integer)))
(define r-io-nested (run-io io-nested))

(: suite (List Test))
(define suite
  (list
   (it "lift (MonadTrans)"
       (all-checks
        (list (check-equal? lift-st (Some (Pair 0 5)))
              (check-equal? lift-ex (Some (Ok 9))))))
   (it "lift-io (MonadIO) single layer"
       (all-checks
        (list (check-equal? r-io-st (Pair 0 7))
              (check-equal? r-io-ex (Ok 8))
              (check-equal? r-io-wr (Pair "" 3)))))
   (it "lift-io through a two-layer stack"
       (check-equal? r-io-nested (Ok (Pair 0 1))))))

(: _ran Unit)
(define _ran (run-io (run-suite "control-monad-trans" suite)))
