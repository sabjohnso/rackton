#lang rackton

;; Regression: return-typed methods of a CONDITIONAL instance on a
;; transformer resolve with NO governing signature.  `get-st` (a
;; return-typed value), `modify-st`, and `lift-io` are methods of the
;; dict-parameterized instances
;;   (Monad m)   => (MonadState s (StateT s m))
;;   (MonadIO m) => (MonadIO    (StateT s m))
;; so their impls need the inner monad's dict.  A consumer that uses them
;; without writing a `(MonadState s m) (MonadIO m) =>` signature must still
;; work: inference infers those constraints itself and the needs-dict
;; machinery threads the inner-monad dict into each method — whether the
;; use is monomorphized to one concrete inner monad or compiled
;; polymorphically across several.
;;
;; (The IO content is irrelevant here — what is under test is that the
;; return-typed methods DISPATCH at `StateT s m`, which previously risked a
;; runtime "no instance registered for return-typed method at type StateT"
;; when no constraint governed the use.)

(require rackton/control/monad/state
         rackton/control/monad/trans
         "../unit.rkt")

;; ----- the issue's exact shape: no signature on `game` -------------
;; `get-st` in the Nil arm is a return-typed VALUE (the trickiest — there
;; is no runtime argument to dispatch on); the Cons arm threads `lift-io`
;; (MonadIO) and `modify-st` (MonadState) through a `let&`.
(define (game Nil) get-st)
(define (game (Cons x more))
  (let& ([_ (lift-io (pure Unit))]
         [_ (modify-st (lambda (s) (+ s x)))])
    (game more)))

;; monomorphized through use at StateT Integer IO
(: prog-io (StateT Integer IO Integer))
(define prog-io (game (Cons 10 (Cons 20 Nil))))
(: io-result (Pair Integer Integer))
(define io-result (run-io ((run-state-t prog-io) 0)))

;; ----- the same shape used at TWO inner monads --------------------
;; `count` uses only MonadState (no MonadIO), so it is valid at any
;; `StateT s m` with `Monad m`.  Using it at BOTH `StateT _ IO` and
;; `StateT _ Maybe` rules out monomorphization to a single inner monad and
;; forces a polymorphic, dict-passing compile.
(define (count Nil) get-st)
(define (count (Cons x more))
  (let& ([_ (modify-st (lambda (s) (+ s x)))])
    (count more)))

(: c-io (StateT Integer IO Integer))
(define c-io (count (Cons 1 (Cons 2 Nil))))
(: c-io-result (Pair Integer Integer))
(define c-io-result (run-io ((run-state-t c-io) 0)))

(: c-maybe (StateT Integer Maybe Integer))
(define c-maybe (count (Cons 100 Nil)))
(: c-maybe-result (Maybe (Pair Integer Integer)))
(define c-maybe-result ((run-state-t c-maybe) 5))

;; ----- assertions -------------------------------------------------
(: suite (List Test))
(define suite
  (list
    (it "no-signature get-st/modify-st/lift-io resolve at StateT _ IO"
        (check-equal? io-result (Pair 30 30)))
    (it "no-signature modify-st/get-st resolve at StateT _ IO"
        (check-equal? c-io-result (Pair 3 3)))
    (it "the same no-signature function also resolves at StateT _ Maybe"
        (check-equal? c-maybe-result (Some (Pair 105 105))))))

(: test-main (IO Unit))
(define test-main (run-suite "mtl-no-signature-return-typed" suite))
