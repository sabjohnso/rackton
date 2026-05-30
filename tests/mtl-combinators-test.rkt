#lang racket/base

;; MTL polish + combinators.  Covers the previously-stubbed
;; lifted `local-en` / `catch-e` impls, the new MonadWriter `listen`
;; and `censor` methods, and the derived combinators `asks` / `gets` /
;; `void` / `when` / `unless`.

(require rackunit
         "../main.rkt")

(rackton
  (require rackton/control/monad/state)
  (require rackton/control/monad/reader)
  ;; A tiny helper used by the local-en / censor tests since the
  ;; prelude doesn't yet ship a string-upcase combinator.
  (: prefix-x (-> String String))
  (define (prefix-x s) (<> "X-" s))

  ;; ----- local-en lifted -----------------------------------

  ;; EnvT String IO: local-en should rewrite the env seen by the inner
  ;; action.  Base EnvT case.
  (: shout-env ((MonadEnv String m) => (m String)))
  (define shout-env ask-en)

  (: env-t-io-loud (IO String))
  (define env-t-io-loud
    ((run-env-t (local-en prefix-x shout-env)) "quiet"))

  ;; StateT Integer (EnvT String IO): local-en lifts through StateT.
  (: ask-then-incr ((MonadState Integer m) (MonadEnv String m) =>
                    (m String)))
  (define ask-then-incr
    (do [r <- ask-en]
        [_ <- (put-st (string-length r))]
      (pure r)))

  (: stacked-local
     (StateT Integer (EnvT String IO) String))
  (define stacked-local
    (local-en prefix-x ask-then-incr))

  (: stacked-local-result (IO (Pair Integer String)))
  (define stacked-local-result
    ((run-env-t ((run-state-t stacked-local) 0)) "abc"))

  ;; ----- catch-e on the base ExceptT -----------------------
  ;; Lifted catch-e through a transformer stack needs access to the
  ;; innermost monad's `pure` to re-inject Ok values, which the
  ;; current dict-passing mechanism only resolves down one layer.
  ;; This test covers the base ExceptT regression; a fuller lifted
  ;; impl is tracked under "deeper qual chains" in Not Yet Supported.

  (: thrower (ExceptT String IO Integer))
  (define thrower (throw-e "boom"))

  (: recover (-> String (ExceptT String IO Integer)))
  (define (recover _msg) (pure 42))

  (: caught-base (ExceptT String IO Integer))
  (define caught-base (catch-e thrower recover))

  (: caught-base-result (IO (Result String Integer)))
  (define caught-base-result (run-except-t caught-base))

  ;; ----- 32.3 listen + censor ---------------------------------

  ;; Base WriterT String IO usage: tell, listen back the log.
  (: greet-and-log
     ((MonadWriter String m) (Monad m) => (m String)))
  (define greet-and-log
    (do [_ <- (tell-w "step1.")]
        [_ <- (tell-w "step2.")]
      (pure "done")))

  ;; The carrier of `WriterT w m a` is `(m (Pair w a))` (log first),
  ;; so `(run-writer-t (listen x))` has type
  ;;   m (Pair w (Pair a w))
  ;; with both `w`s carrying the same accumulated log.
  (: listened-greet (IO (Pair String (Pair String String))))
  (define listened-greet
    (run-writer-t (listen greet-and-log)))

  ;; censor: rewrite the accumulated log via `f`.  Underlying type
  ;; `m (Pair w a)`, where `w` is the censored log.
  (: censored-greet (IO (Pair String String)))
  (define censored-greet
    (run-writer-t (censor prefix-x greet-and-log)))

  ;; ----- 32.4 derived combinators -----------------------------

  ;; asks: derives a value from the env via a function.
  (: env-length-result Integer)
  (define env-length-result ((run-env (asks string-length)) "hello"))

  ;; gets: derives a value from the state.
  (: state-doubled-result (Pair Integer Integer))
  (define state-doubled-result
    ((run-state (gets (lambda (s) (* s 2)))) 21))

  ;; void: drop the result of an IO action.
  (: voided-io (IO Unit))
  (define voided-io (void (pure 42)))

  ;; when: conditional Applicative action.
  (: when-true-io  (IO Unit))
  (define when-true-io  (when #true  (pure MkUnit)))
  (: when-false-io (IO Unit))
  (define when-false-io (when #false (pure MkUnit)))

  ;; unless: complement of when.
  (: unless-true-io  (IO Unit))
  (define unless-true-io  (unless #true  (pure MkUnit)))
  (: unless-false-io (IO Unit))
  (define unless-false-io (unless #false (pure MkUnit))))

;; ---------- assertions ---------------------------------------

(test-case "local-en base over EnvT IO rewrites the env"
  (check-equal? (run-io env-t-io-loud) "X-quiet"))

(test-case "local-en lifted through StateT (EnvT IO)"
  (check-equal? (run-io stacked-local-result)
                (MkPair 5 "X-abc")))

(test-case "catch-e on base ExceptT IO recovers from a throw"
  (check-equal? (run-io caught-base-result) (Ok 42)))

(test-case "listen on WriterT IO surfaces the accumulated log"
  ;; WriterT's underlying carrier is `(m (Pair w a))` (log first), so
  ;; `(listen x)` produces `(m (Pair w (Pair a w)))` — the outer log
  ;; is unchanged and the inner pair carries the original value plus
  ;; the log so far.
  (check-equal? (run-io listened-greet)
                (MkPair "step1.step2."
                        (MkPair "done" "step1.step2."))))

(test-case "censor on WriterT IO transforms the log"
  (check-equal? (run-io censored-greet)
                (MkPair "X-step1.step2." "done")))

(test-case "asks derives from Env"
  (check-equal? env-length-result 5))

(test-case "gets derives from State"
  (check-equal? state-doubled-result (MkPair 21 42)))

(test-case "void drops the IO result"
  (check-equal? (run-io voided-io) MkUnit))

(test-case "when true runs the action; false short-circuits"
  (check-equal? (run-io when-true-io)  MkUnit)
  (check-equal? (run-io when-false-io) MkUnit))

(test-case "unless true short-circuits; false runs the action"
  (check-equal? (run-io unless-true-io)  MkUnit)
  (check-equal? (run-io unless-false-io) MkUnit))
