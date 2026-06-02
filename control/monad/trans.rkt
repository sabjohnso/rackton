#lang rackton

;; rackton/control/monad/trans — Control.Monad.Trans.Class /
;; Control.Monad.IO.Class.  The MonadTrans and MonadIO classes live in
;; the prelude (their methods are return-typed); this module supplies
;; the instances for the four transformers.  Requires each transformer
;; module for its type + lift-*-t helper.

(require rackton/control/monad/state
         rackton/control/monad/reader
         rackton/control/monad/writer
         rackton/control/monad/except)

;; Re-export the transformer modules so `(require …/trans)` brings the
;; whole stack (types, runners, lift-*-t) plus these lift / lift-io
;; instances in one import.
(provide (all-defined-out)
         (all-from-out rackton/control/monad/state)
         (all-from-out rackton/control/monad/reader)
         (all-from-out rackton/control/monad/writer)
         (all-from-out rackton/control/monad/except))

;; ----- MonadTrans: lift an inner action one layer up ---------------

(instance (MonadTrans (StateT s))
  (define (lift ma) (lift-state-t ma)))

(instance (MonadTrans (EnvT r))
  (define (lift ma) (lift-env-t ma)))

;; WriterT needs the log's mempty, so its lift is inlined (delegating to
;; the needs-dict lift-writer-t would cross skolemizations).
(instance ((Monoid w) => (MonadTrans (WriterT w)))
  (define (lift ma) (WriterT (fmap (lambda (a) (Pair mempty a)) ma))))

(instance (MonadTrans (ExceptT e))
  (define (lift ma) (lift-except-t ma)))

;; ----- MonadIO: lift an IO action through the stack ----------------
;; Each transformer lifts the inner monad's lift-io one layer.

(instance ((MonadIO m) => (MonadIO (StateT s m)))
  (define (lift-io io) (lift-state-t (lift-io io))))

(instance ((MonadIO m) => (MonadIO (EnvT r m)))
  (define (lift-io io) (lift-env-t (lift-io io))))

(instance ((MonadIO m) (Monoid w) => (MonadIO (WriterT w m)))
  (define (lift-io io)
    (WriterT (fmap (lambda (a) (Pair mempty a)) (lift-io io)))))

(instance ((MonadIO m) => (MonadIO (ExceptT e m)))
  (define (lift-io io) (lift-except-t (lift-io io))))
