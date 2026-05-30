#lang racket/base

;; rackton/control/monad/trans — MonadTrans (lift) and MonadIO (lift-io)
;; across the transformer stack.

(require rackunit
         "../main.rkt")

(rackton
  (require rackton/control/monad/trans)

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
      0))))

;; ---------- assertions ---------------------------------------

(test-case "lift (MonadTrans)"
  (check-equal? lift-st (Some (MkPair 0 5)))
  (check-equal? lift-ex (Some (Ok 9))))

(test-case "lift-io (MonadIO) single layer"
  (check-equal? (run-io io-st) (MkPair 0 7))
  (check-equal? (run-io io-ex) (Ok 8))
  (check-equal? (run-io io-wr) (MkPair "" 3)))

(test-case "lift-io through a two-layer stack"
  (check-equal? (run-io io-nested) (Ok (MkPair 0 1))))
