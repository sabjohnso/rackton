#lang rackton

;; rackton/control/monad/reader — Control.Monad.Reader.  The (non-
;; transformer) Env (Reader) monad, moved out of the auto-prelude
;; (Phase 2 slim).  Pure Rackton — the module regenerates its runtime.
;; (The MonadEnv class + the EnvT instance stay in the prelude / the
;; transformer family.)

(provide (all-defined-out))

(newtype (Env r a)
  (Env (-> r a)))

(: run-env (-> (Env r a) (-> r a)))
(: ask     (Env r r))
(: local   (-> (-> r r) (-> (Env r a) (Env r a))))

(define (run-env e) (match e [(Env f) f]))

(define ask (Env (lambda (r) r)))

(define (local f e)
  (Env (lambda (r) ((run-env e) (f r)))))

(instance (Functor (Env r))
  (define (fmap f e)
    (Env (lambda (r) (f ((run-env e) r))))))

(instance (Applicative (Env r))
  (define (pure a) (Env (lambda (_) a)))
  (define (fapply ef ea)
    (Env (lambda (r) (((run-env ef) r) ((run-env ea) r))))))

(instance (Monad (Env r))
  (define (flatmap f e)
    (Env (lambda (r) ((run-env (f ((run-env e) r))) r)))))

(instance (MonadEnv r (Env r))
  ;; ask-en is a VALUE (= ask); inline it (see the get-st note in
  ;; state.rkt — instance registration precedes def evaluation).
  (define ask-en         (Env (lambda (r) r)))
  (define (local-en f e) (local f e)))

;; ===== EnvT r m: env-passing over an inner monad m ================
;;
;; Carved out of the auto-prelude (Phase 2 slim, finding 2026-05-30).
;; Pure Rackton, like StateT: the value-dispatched methods resolve the
;; inner monad's impl by runtime dispatch on the inner value's tag, and
;; the methods that need the inner `pure` (pure/ask-en) have it threaded
;; as a dict arg.  This module owns every mtl instance where EnvT is the
;; OUTER transformer.

(newtype (EnvT r m a)
  (EnvT (-> r (m a))))

(: run-env-t  (-> (EnvT r m a) (-> r (m a))))
(: ask-t      ((Applicative m) => (EnvT r m r)))
(: local-t    (-> (-> r r) (-> (EnvT r m a) (EnvT r m a))))
(: lift-env-t (-> (m a) (EnvT r m a)))

(define (run-env-t e) (match e [(EnvT f) f]))

;; ask-t's inner `pure` is dict-threaded.
(define ask-t (EnvT (lambda (r) (pure r))))
(define (local-t f e) (EnvT (lambda (r) ((run-env-t e) (f r)))))
;; lift carries no inner pure (Functor m only).
(define (lift-env-t ma) (EnvT (lambda (_) ma)))

(instance ((Monad m) => (Functor (EnvT r m)))
  (define (fmap f e)
    (EnvT (lambda (r) (fmap f ((run-env-t e) r))))))

(instance ((Monad m) => (Applicative (EnvT r m)))
  (define (pure a) (EnvT (lambda (_) (pure a))))
  (define (fapply ef ea)
    (EnvT (lambda (r)
              (flatmap (lambda (f) (fmap f ((run-env-t ea) r)))
                       ((run-env-t ef) r))))))

(instance ((Monad m) => (Monad (EnvT r m)))
  (define (flatmap f e)
    (EnvT (lambda (r)
              (flatmap (lambda (a) ((run-env-t (f a)) r))
                       ((run-env-t e) r))))))

(instance ((Monad m) => (MonadEnv r (EnvT r m)))
  ;; ask-en is a VALUE needing inner pure; inline the body so the dict
  ;; threads directly (delegating to ask-t would cross two
  ;; skolemizations — see the get-st note in the StateT block).
  (define ask-en         (EnvT (lambda (r) (pure r))))
  (define (local-en f e) (local-t f e)))

;; ----- EnvT-outer mtl pass-through instances ----------------------

(instance ((MonadState s m) => (MonadState s (EnvT r m)))
  (define get-st        (lift-env-t get-st))
  (define (put-st x)    (lift-env-t (put-st x)))
  (define (modify-st f) (lift-env-t (modify-st f))))

(instance ((MonadWriter w m) => (MonadWriter w (EnvT r m)))
  (define (tell-w x)    (lift-env-t (tell-w x)))
  (define (listen em)   (racket (EnvT r m (Pair a w)) (em)   #f))
  (define (censor f em) (racket (EnvT r m a)          (f em) #f)))

(instance ((MonadError e m) => (MonadError e (EnvT r m)))
  (define (throw-e ev)   (lift-env-t (throw-e ev)))
  (define (catch-e em h)
    (EnvT (lambda (r)
              (catch-e ((run-env-t em) r)
                       (lambda (e) ((run-env-t (h e)) r)))))))
