#lang rackton

;; rackton/control/monad/writer — Control.Monad.Writer.  The WriterT
;; transformer (an accumulating writer over an inner monad), carved out
;; of the auto-prelude (Phase 2 slim, finding 2026-05-30).  Pure
;; Rackton — the value-dispatched methods resolve the inner monad's impl
;; and the log Monoid's `<>` by runtime dispatch on the relevant value's
;; tag; the methods that need the inner `pure` and/or the log's `mempty`
;; (pure / tell / the lifted mtl methods) have those threaded as dict
;; args by the needs-dict-body machinery.  This module owns every mtl
;; instance where WriterT is the OUTER transformer.
;;
;; (There is no non-transformer Writer here yet — WriterT over Identity
;; serves that role, mirroring the StateT/EnvT split.  The MonadWriter
;; class itself stays in the prelude.)

(provide (all-defined-out))

(newtype (WriterT w m a)
  (MkWriterT (m (Pair w a))))

(: run-writer-t  (-> (WriterT w m a) (m (Pair w a))))
(: eval-writer-t ((Functor m) => (-> (WriterT w m a) (m a))))
(: exec-writer-t ((Functor m) => (-> (WriterT w m a) (m w))))
(: tell          ((Applicative m) => (-> w (WriterT w m Unit))))
(: lift-writer-t ((Functor m) (Monoid w) => (-> (m a) (WriterT w m a))))

(define (run-writer-t w) (match w [(MkWriterT m) m]))

(define (eval-writer-t w)
  (fmap (lambda (p) (match p [(MkPair _ a) a])) (run-writer-t w)))
(define (exec-writer-t w)
  (fmap (lambda (p) (match p [(MkPair w0 _) w0])) (run-writer-t w)))

;; tell's inner `pure` is dict-threaded; lift-writer-t's `mempty` is.
(define (tell w) (MkWriterT (pure (MkPair w MkUnit))))
(define (lift-writer-t ma)
  (MkWriterT (fmap (lambda (a) (MkPair mempty a)) ma)))

(instance ((Functor m) => (Functor (WriterT w m)))
  (define (fmap f wa)
    (MkWriterT (fmap (lambda (p) (match p [(MkPair w0 a) (MkPair w0 (f a))]))
                     (run-writer-t wa)))))

(instance ((Monad m) (Monoid w) => (Applicative (WriterT w m)))
  ;; pure threads BOTH inner pure and the log's mempty.
  (define (pure a) (MkWriterT (pure (MkPair mempty a))))
  (define (fapply wf wa)
    (MkWriterT
     (flatmap (lambda (p1)
                (match p1
                  [(MkPair w1 f)
                   (fmap (lambda (p2)
                           (match p2 [(MkPair w2 a) (MkPair (<> w1 w2) (f a))]))
                         (run-writer-t wa))]))
              (run-writer-t wf)))))

(instance ((Monad m) (Semigroup w) => (Monad (WriterT w m)))
  (define (flatmap f wa)
    (MkWriterT
     (flatmap (lambda (p1)
                (match p1
                  [(MkPair w1 a)
                   (fmap (lambda (p2)
                           (match p2 [(MkPair w2 b) (MkPair (<> w1 w2) b)]))
                         (run-writer-t (f a)))]))
              (run-writer-t wa)))))

(instance ((Monoid w) (Monad m) => (MonadWriter w (WriterT w m)))
  ;; tell-w inlines tell (delegating to the needs-dict `tell` top-def
  ;; would cross two skolemizations — see the StateT block's note).
  (define (tell-w x)    (MkWriterT (pure (MkPair x MkUnit))))
  (define (listen wm)
    (MkWriterT (fmap (lambda (p) (match p [(MkPair w a) (MkPair w (MkPair a w))]))
                     (run-writer-t wm))))
  (define (censor f wm)
    (MkWriterT (fmap (lambda (p) (match p [(MkPair w a) (MkPair (f w) a)]))
                     (run-writer-t wm)))))

;; ----- WriterT-outer mtl pass-through instances -------------------
;; The lift-through-the-log methods inline lift-writer-t's body (it is a
;; needs-dict top-def — Monoid w's mempty — so delegating would cross
;; skolemizations).  The genuinely value-dispatched methods (local-en /
;; catch-e) recurse on the inner monad value directly.

(instance ((MonadState s m) (Monoid w) => (MonadState s (WriterT w m)))
  (define get-st        (MkWriterT (fmap (lambda (a) (MkPair mempty a)) get-st)))
  (define (put-st x)    (MkWriterT (fmap (lambda (a) (MkPair mempty a)) (put-st x))))
  (define (modify-st f) (MkWriterT (fmap (lambda (a) (MkPair mempty a)) (modify-st f)))))

(instance ((MonadEnv r m) (Monoid w) => (MonadEnv r (WriterT w m)))
  (define ask-en     (MkWriterT (fmap (lambda (a) (MkPair mempty a)) ask-en)))
  (define (local-en f wm)
    (MkWriterT (local-en f (run-writer-t wm)))))

(instance ((MonadError e m) (Monoid w) => (MonadError e (WriterT w m)))
  (define (throw-e ev)
    (MkWriterT (fmap (lambda (a) (MkPair mempty a)) (throw-e ev))))
  (define (catch-e wm h)
    (MkWriterT (catch-e (run-writer-t wm)
                        (lambda (e) (run-writer-t (h e)))))))
