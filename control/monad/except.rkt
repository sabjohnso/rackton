#lang rackton

;; rackton/control/monad/except — Control.Monad.Except.  The ExceptT
;; transformer (typed exceptions over an inner monad), carved out of the
;; auto-prelude (Phase 2 slim, finding 2026-05-30).  Pure Rackton.
;;
;; ExceptT is the one transformer whose value-dispatched methods
;; (fapply / flatmap / catch-e) call the INNER monad's `pure` — to
;; rewrap Ok/Err results.  At a runtime-dispatched call site there is no
;; compile-time dict to thread, so codegen registers those methods to
;; derive the inner pure from the dispatch-arg WITNESS
;; (inner-pure-from-witness), and emits a pure-via-witness deriver for
;; ExceptT so nested stacks (ExceptT over ExceptT) reconstruct it.
;; This module owns every mtl instance where ExceptT is the OUTER
;; transformer.  (ExceptT over Identity plays the role of a bare Except.)

(require rackton/data/result)
(provide (all-defined-out))

(newtype (ExceptT e m a)
  (ExceptT (m (Result e a))))

(: run-except-t  (-> (ExceptT e m a) (m (Result e a))))
(: throw-error   ((Applicative m) => (-> e (ExceptT e m a))))
(: catch-error   ((Monad m) => (-> (ExceptT e m a)
                                   (-> (-> e (ExceptT e m a))
                                       (ExceptT e m a)))))
(: lift-except-t ((Functor m) => (-> (m a) (ExceptT e m a))))

(define (run-except-t e) (match e [(ExceptT m) m]))

;; throw-error / catch-error thread the inner `pure` as a dict at their
;; (concrete) call sites.
(define (throw-error e) (ExceptT (pure (Err e))))
(define (catch-error ea handler)
  (ExceptT
   (flatmap (lambda (r)
              (match r
                [(Err e) (run-except-t (handler e))]
                [(Ok  v) (pure (Ok v))]))
            (run-except-t ea))))

;; lift carries Functor m only — no inner pure, hence no dict.
(define (lift-except-t ma) (ExceptT (fmap Ok ma)))

(instance ((Functor m) => (Functor (ExceptT e m)))
  (define (fmap f ea)
    (ExceptT (fmap (lambda (r) (match r [(Err x) (Err x)] [(Ok v) (Ok (f v))]))
                     (run-except-t ea)))))

(instance ((Monad m) => (Applicative (ExceptT e m)))
  (define (pure a) (ExceptT (pure (Ok a))))
  (define (fapply ef ea)
    (ExceptT
     (flatmap (lambda (rf)
                (match rf
                  [(Err x) (pure (Err x))]
                  [(Ok  f)
                   (fmap (lambda (ra)
                           (match ra [(Err x) (Err x)] [(Ok a) (Ok (f a))]))
                         (run-except-t ea))]))
              (run-except-t ef)))))

(instance ((Monad m) => (Monad (ExceptT e m)))
  (define (flatmap f ea)
    (ExceptT
     (flatmap (lambda (r)
                (match r
                  [(Err e) (pure (Err e))]
                  [(Ok  a) (run-except-t (f a))]))
              (run-except-t ea)))))

(instance ((Monad m) => (MonadError e (ExceptT e m)))
  ;; Inline throw-e/catch-e (delegating to the needs-dict throw-error/
  ;; catch-error top-defs would cross skolemizations).
  (define (throw-e e) (ExceptT (pure (Err e))))
  (define (catch-e ea handler)
    (ExceptT
     (flatmap (lambda (r)
                (match r
                  [(Err e) (run-except-t (handler e))]
                  [(Ok  v) (pure (Ok v))]))
              (run-except-t ea)))))

;; ----- ExceptT-outer mtl pass-through instances -------------------
;; These lift through lift-except-t, which is Functor-only (not
;; needs-dict), so delegating is safe — no inlining needed.

(instance ((MonadState s m) => (MonadState s (ExceptT e m)))
  (define get-st        (lift-except-t get-st))
  (define (put-st x)    (lift-except-t (put-st x)))
  (define (modify-st f) (lift-except-t (modify-st f))))

(instance ((MonadEnv r m) => (MonadEnv r (ExceptT e m)))
  (define ask-en     (lift-except-t ask-en))
  (define (local-en f em)
    (ExceptT (local-en f (run-except-t em)))))

(instance ((MonadWriter w m) => (MonadWriter w (ExceptT e m)))
  (define (tell-w x)    (lift-except-t (tell-w x)))
  (define (listen ex)   (racket (ExceptT e m (Pair a w)) (ex)   #f))
  (define (censor f ex) (racket (ExceptT e m a)          (f ex) #f)))
