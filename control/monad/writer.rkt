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
  (WriterT (m (Pair w a))))

(: run-writer-t  (-> (WriterT w m a) (m (Pair w a))))
(: eval-writer-t ((Functor m) => (-> (WriterT w m a) (m a))))
(: exec-writer-t ((Functor m) => (-> (WriterT w m a) (m w))))
(: tell          ((Applicative m) => (-> w (WriterT w m Unit))))
(: lift-writer-t ((Functor m) (Monoid w) => (-> (m a) (WriterT w m a))))

(define (run-writer-t w) (match w [(WriterT m) m]))

(define (eval-writer-t w)
  (fmap (lambda (p) (match p [(Pair _ a) a])) (run-writer-t w)))
(define (exec-writer-t w)
  (fmap (lambda (p) (match p [(Pair w0 _) w0])) (run-writer-t w)))

;; tell's inner `pure` is dict-threaded; lift-writer-t's `mempty` is.
(define (tell w) (WriterT (pure (Pair w Unit))))
(define (lift-writer-t ma)
  (WriterT (fmap (lambda (a) (Pair mempty a)) ma)))

(instance ((Functor m) => (Functor (WriterT w m)))
  (define (fmap f wa)
    (WriterT (fmap (lambda (p) (match p [(Pair w0 a) (Pair w0 (f a))]))
                     (run-writer-t wa)))))

(instance ((Monad m) (Monoid w) => (Applicative (WriterT w m)))
  ;; pure threads BOTH inner pure and the log's mempty.
  (define (pure a) (WriterT (pure (Pair mempty a))))
  (define (fapply wf wa)
    (WriterT
     (flatmap (lambda (p1)
                (match p1
                  [(Pair w1 f)
                   (fmap (lambda (p2)
                           (match p2 [(Pair w2 a) (Pair (<> w1 w2) (f a))]))
                         (run-writer-t wa))]))
              (run-writer-t wf)))))

;; Monoid w (not just Semigroup w): the inherited Applicative superclass
;; needs the log's `mempty` for `pure`, so a lawful Monad (WriterT w m)
;; must carry Monoid w — matching the Applicative instance above and
;; Haskell's WriterT, whose Monad instance also requires Monoid w.
(instance ((Monad m) (Monoid w) => (Monad (WriterT w m)))
  (define (flatmap f wa)
    (WriterT
     (flatmap (lambda (p1)
                (match p1
                  [(Pair w1 a)
                   (fmap (lambda (p2)
                           (match p2 [(Pair w2 b) (Pair (<> w1 w2) b)]))
                         (run-writer-t (f a)))]))
              (run-writer-t wa)))))

(instance ((Monoid w) (Monad m) => (MonadWriter w (WriterT w m)))
  ;; tell-w inlines tell (delegating to the needs-dict `tell` top-def
  ;; would cross two skolemizations — see the StateT block's note).
  (define (tell-w x)    (WriterT (pure (Pair x Unit))))
  (define (listen wm)
    (WriterT (fmap (lambda (p) (match p [(Pair w a) (Pair w (Pair a w))]))
                     (run-writer-t wm))))
  (define (censor f wm)
    (WriterT (fmap (lambda (p) (match p [(Pair w a) (Pair (f w) a)]))
                     (run-writer-t wm)))))

;; ----- WriterT-outer mtl pass-through instances -------------------
;; The lift-through-the-log methods inline lift-writer-t's body (it is a
;; needs-dict top-def — Monoid w's mempty — so delegating would cross
;; skolemizations).  The genuinely value-dispatched methods (local-en /
;; catch-e) recurse on the inner monad value directly.

(instance ((MonadState s m) (Monoid w) => (MonadState s (WriterT w m)))
  (define get-st        (WriterT (fmap (lambda (a) (Pair mempty a)) get-st)))
  (define (put-st x)    (WriterT (fmap (lambda (a) (Pair mempty a)) (put-st x))))
  (define (modify-st f) (WriterT (fmap (lambda (a) (Pair mempty a)) (modify-st f)))))

(instance ((MonadEnv r m) (Monoid w) => (MonadEnv r (WriterT w m)))
  (define ask-en     (WriterT (fmap (lambda (a) (Pair mempty a)) ask-en)))
  (define (local-en f wm)
    (WriterT (local-en f (run-writer-t wm)))))

(instance ((MonadError e m) (Monoid w) => (MonadError e (WriterT w m)))
  (define (throw-e ev)
    (WriterT (fmap (lambda (a) (Pair mempty a)) (throw-e ev))))
  (define (catch-e wm h)
    (WriterT (catch-e (run-writer-t wm)
                        (lambda (e) (run-writer-t (h e)))))))
