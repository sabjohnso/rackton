#lang rackton

;; rackton/control/monad/reader — Control.Monad.Reader.  The (non-
;; transformer) Env (Reader) monad, moved out of the auto-prelude
;; (Phase 2 slim).  Pure Rackton — the module regenerates its runtime.
;; (The MonadEnv class + the EnvT instance stay in the prelude / the
;; transformer family.)

(provide (all-defined-out))

(newtype (Env r a)
  (MkEnv (-> r a)))

(: run-env (-> (Env r a) (-> r a)))
(: ask     (Env r r))
(: local   (-> (-> r r) (-> (Env r a) (Env r a))))

(define (run-env e) (match e [(MkEnv f) f]))

(define ask (MkEnv (lambda (r) r)))

(define (local f e)
  (MkEnv (lambda (r) ((run-env e) (f r)))))

(instance (Functor (Env r))
  (define (fmap f e)
    (MkEnv (lambda (r) (f ((run-env e) r))))))

(instance (Applicative (Env r))
  (define (pure a) (MkEnv (lambda (_) a)))
  (define (fapply ef ea)
    (MkEnv (lambda (r) (((run-env ef) r) ((run-env ea) r))))))

(instance (Monad (Env r))
  (define (flatmap f e)
    (MkEnv (lambda (r) ((run-env (f ((run-env e) r))) r)))))

(instance (MonadEnv r (Env r))
  ;; ask-en is a VALUE (= ask); inline it (see the get-st note in
  ;; state.rkt — instance registration precedes def evaluation).
  (define ask-en         (MkEnv (lambda (r) r)))
  (define (local-en f e) (local f e)))
