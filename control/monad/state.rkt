#lang rackton

;; rackton/control/monad/state — Control.Monad.State.  The (non-
;; transformer) State monad, moved out of the auto-prelude (Phase 2
;; slim).  Pure Rackton — the module regenerates its runtime; no host
;; runtime needed.  (The MonadState class + the StateT instance stay in
;; the prelude / the transformer family.)

(provide (all-defined-out))

(newtype (State s a)
  (MkState (-> s (Pair s a))))

(: run-state    (-> (State s a) (-> s (Pair s a))))
(: eval-state   (-> (State s a) (-> s a)))
(: exec-state   (-> (State s a) (-> s s)))
(: get-state    (State s s))
(: put-state    (-> s (State s Unit)))
(: modify-state (-> (-> s s) (State s Unit)))

(define (run-state st)
  (match st [(MkState f) f]))

(define (eval-state st s)
  (match ((run-state st) s) [(MkPair _ a) a]))

(define (exec-state st s)
  (match ((run-state st) s) [(MkPair s2 _) s2]))

(define get-state    (MkState (lambda (s) (MkPair s s))))
(define (put-state s) (MkState (lambda (_) (MkPair s MkUnit))))
(define (modify-state f) (MkState (lambda (s) (MkPair (f s) MkUnit))))

(instance (Functor (State s))
  (define (fmap f st)
    (MkState (lambda (s)
               (match ((run-state st) s)
                 [(MkPair s2 a) (MkPair s2 (f a))])))))

(instance (Applicative (State s))
  (define (pure a) (MkState (lambda (s) (MkPair s a))))
  (define (fapply sf sa)
    (MkState (lambda (s)
               (match ((run-state sf) s)
                 [(MkPair s2 f)
                  (match ((run-state sa) s2)
                    [(MkPair s3 a) (MkPair s3 (f a))])])))))

(instance (Monad (State s))
  (define (flatmap f st)
    (MkState (lambda (s)
               (match ((run-state st) s)
                 [(MkPair s2 a) ((run-state (f a)) s2)])))))

(instance (MonadState s (State s))
  ;; get-st is a VALUE (= get-state); inline it rather than reference the
  ;; top-def, since instance registration (codegen phase 4) runs before
  ;; def evaluation (phase 6).  put-st/modify-st are lambdas, so their
  ;; references defer to call time.
  (define get-st        (MkState (lambda (s) (MkPair s s))))
  (define (put-st x)    (put-state x))
  (define (modify-st f) (modify-state f)))
