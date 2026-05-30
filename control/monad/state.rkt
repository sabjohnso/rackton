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

;; ===== StateT s m: state over an inner monad m =====================
;;
;; Carved out of the auto-prelude (Phase 2 slim, finding 2026-05-30).
;; This is PURE RACKTON — no hand-written host runtime is needed: the
;; value-dispatched methods (fmap/flatmap/fapply) resolve the inner
;; monad's impl by runtime dispatch on the inner value's tag, and the
;; methods that need the inner monad's return-typed `pure`
;; (pure/get/put/modify) have it threaded as a dict arg by the
;; needs-dict-body machinery (infer.rkt skolemize/tracked +
;; build-dict-skolems).  This module owns every mtl instance where
;; StateT is the OUTER transformer.

(newtype (StateT s m a)
  (MkStateT (-> s (m (Pair s a)))))

(: run-state-t    (-> (StateT s m a) (-> s (m (Pair s a)))))
(: eval-state-t   ((Functor m) => (-> (StateT s m a) (-> s (m a)))))
(: exec-state-t   ((Functor m) => (-> (StateT s m a) (-> s (m s)))))
(: get-state-t    ((Applicative m) => (StateT s m s)))
(: put-state-t    ((Applicative m) => (-> s (StateT s m Unit))))
(: modify-state-t ((Applicative m) => (-> (-> s s) (StateT s m Unit))))
(: lift-state-t   ((Functor m) => (-> (m a) (StateT s m a))))

(define (run-state-t st) (match st [(MkStateT f) f]))

(define (eval-state-t st s)
  (fmap (lambda (p) (match p [(MkPair _ a) a])) ((run-state-t st) s)))

(define (exec-state-t st s)
  (fmap (lambda (p) (match p [(MkPair s2 _) s2])) ((run-state-t st) s)))

;; get/put/modify build the StateT directly; their inner-`pure` use is
;; dict-threaded.  Authored as values/lambdas mirroring the State case.
(define get-state-t      (MkStateT (lambda (s) (pure (MkPair s s)))))
(define (put-state-t s)  (MkStateT (lambda (_) (pure (MkPair s MkUnit)))))
(define (modify-state-t f) (MkStateT (lambda (s) (pure (MkPair (f s) MkUnit)))))

;; lift carries only Functor m — no inner pure, hence no dict.
(define (lift-state-t ma)
  (MkStateT (lambda (s) (fmap (lambda (a) (MkPair s a)) ma))))

(instance ((Monad m) => (Functor (StateT s m)))
  (define (fmap f st)
    (MkStateT (lambda (s)
                (fmap (lambda (p) (match p [(MkPair s2 a) (MkPair s2 (f a))]))
                      ((run-state-t st) s))))))

(instance ((Monad m) => (Applicative (StateT s m)))
  (define (pure a) (MkStateT (lambda (s) (pure (MkPair s a)))))
  (define (fapply sf sa)
    (MkStateT (lambda (s)
                (flatmap (lambda (p1)
                           (match p1
                             [(MkPair s2 f)
                              (fmap (lambda (p2)
                                      (match p2 [(MkPair s3 a) (MkPair s3 (f a))]))
                                    ((run-state-t sa) s2))]))
                         ((run-state-t sf) s))))))

(instance ((Monad m) => (Monad (StateT s m)))
  (define (flatmap f st)
    (MkStateT (lambda (s)
                (flatmap (lambda (p) (match p [(MkPair s2 a) ((run-state-t (f a)) s2)]))
                         ((run-state-t st) s))))))

(instance ((Monad m) => (MonadState s (StateT s m)))
  ;; Inline the bodies (rather than delegate to get/put/modify-state-t)
  ;; so each method's inner `pure` is a DIRECT reference — the needs-dict
  ;; machinery threads the inner-pure dict into the instance method.
  ;; Delegating to the separate needs-dict top-defs would cross two
  ;; independent skolemizations and leave the dict arg unbound.
  (define get-st        (MkStateT (lambda (s) (pure (MkPair s s)))))
  (define (put-st x)    (MkStateT (lambda (_) (pure (MkPair x MkUnit)))))
  (define (modify-st f) (MkStateT (lambda (s) (pure (MkPair (f s) MkUnit))))))

;; ----- StateT-outer mtl pass-through instances --------------------
;; Each lifts the inner monad's effect through the state layer.

(instance ((MonadEnv r m) => (MonadEnv r (StateT s m)))
  (define ask-en     (lift-state-t ask-en))
  (define (local-en f sm)
    (MkStateT (lambda (s) (local-en f ((run-state-t sm) s))))))

(instance ((MonadWriter w m) => (MonadWriter w (StateT s m)))
  (define (tell-w x)    (lift-state-t (tell-w x)))
  (define (listen sm)   (racket (StateT s m (Pair a w)) (sm)   #f))
  (define (censor f sm) (racket (StateT s m a)          (f sm) #f)))

(instance ((MonadError e m) => (MonadError e (StateT s m)))
  (define (throw-e ev)   (lift-state-t (throw-e ev)))
  (define (catch-e sm h)
    (MkStateT (lambda (s)
                (catch-e ((run-state-t sm) s)
                         (lambda (e) ((run-state-t (h e)) s)))))))
