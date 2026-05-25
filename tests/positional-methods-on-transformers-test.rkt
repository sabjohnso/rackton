#lang racket/base

;; Runtime resolvers for transformer-side positional class methods —
;; catch-e on the lifted transformers (StateT/EnvT/WriterT), plus
;; listen / censor / local-en on the transformer ctors.  Builds on
;; the pure-via-witness + ExceptT registrations covered elsewhere.

(require rackunit
         "../main.rkt")

(rackton
  ;; ----- WriterT do-notation in a polymorphic body --------
  ;; The MonadWriter inferred body's `do` chain runtime-dispatches
  ;; `>>=` on MkWriterT — already registered.  This is a regression
  ;; guard.

  (: log-and-add ((MonadWriter String m) => (-> Integer (m Integer))))
  (define (log-and-add n)
    (do [_ <- (tell-w "added.")]
      (pure (+ n 1))))

  (: wt-add (WriterT String IO Integer))
  (define wt-add (log-and-add 10))

  (: wt-add-result (IO (Pair String Integer)))
  (define wt-add-result (run-writer-t wt-add))

  ;; ----- listen / censor in a polymorphic body ------------
  ;; Calls listen/censor inside a (MonadWriter w m) =>, then runs at
  ;; WriterT String IO.  Both methods must dispatch at runtime on
  ;; MkWriterT.

  (: traced-then-listen
     ((MonadWriter String m) => (m (Pair Integer String))))
  (define traced-then-listen
    (listen
     (do [_ <- (tell-w "x.")]
         [_ <- (tell-w "y.")]
       (pure 7))))

  (: listen-result (IO (Pair String (Pair Integer String))))
  (define listen-result (run-writer-t traced-then-listen))

  (: prefix-bang (-> String String))
  (define (prefix-bang s) (<> "!" s))

  (: censored-trace
     ((MonadWriter String m) => (m Integer)))
  (define censored-trace
    (censor prefix-bang
            (do [_ <- (tell-w "step.")]
              (pure 99))))

  (: censor-result (IO (Pair String Integer)))
  (define censor-result (run-writer-t censored-trace))

  ;; ----- catch-e through WriterT (ExceptT IO) -------------
  ;; The WriterT-over-ExceptT lifted MonadError instance must catch
  ;; via the runtime catch-e dispatcher on the inner ExceptT value.

  (: thrower-wt (WriterT String (ExceptT String IO) Integer))
  (define thrower-wt
    (do [_ <- (tell-w "before-throw.")]
        [_ <- (ann (throw-e "boom")
                   (WriterT String (ExceptT String IO) Integer))]
        (pure 1)))

  (: recover-wt
     (-> String (WriterT String (ExceptT String IO) Integer)))
  (define (recover-wt _) (pure 42))

  (: caught-wt (WriterT String (ExceptT String IO) Integer))
  (define caught-wt (catch-e thrower-wt recover-wt))

  (: caught-wt-result (IO (Result String (Pair String Integer))))
  (define caught-wt-result (run-except-t (run-writer-t caught-wt)))

  ;; ----- local-en in a polymorphic body over StateT (Env _) -----
  ;; Polymorphic `(MonadEnv String m) => ...` body, then run at
  ;; StateT Integer (Env String).  The runtime `local-en` dispatch
  ;; on MkStateT recurses via runtime local-en on the inner Env.

  (: announce-env ((MonadEnv String m) => (m String)))
  (define announce-env
    (do [r <- ask-en]
      (pure (<> "hi " r))))

  (: local-stack (StateT Integer (Env String) String))
  (define local-stack
    (local-en prefix-bang announce-env))

  (: local-stack-result (Env String (Pair Integer String)))
  (define local-stack-result ((run-state-t local-stack) 0)))

;; ---------- assertions ---------------------------------------

(test-case "do-notation chain in MonadWriter polymorphic body (WriterT IO)"
  (check-equal? (run-io wt-add-result)
                (MkPair "added." 11)))

(test-case "listen in polymorphic MonadWriter body (WriterT IO)"
  (check-equal? (run-io listen-result)
                (MkPair "x.y." (MkPair 7 "x.y."))))

(test-case "censor in polymorphic MonadWriter body (WriterT IO)"
  (check-equal? (run-io censor-result)
                (MkPair "!step." 99)))

(test-case "catch-e lifted through WriterT (ExceptT IO)"
  (check-equal? (run-io caught-wt-result)
                (Ok (MkPair "" 42))))

(test-case "local-en in polymorphic body over StateT (Env String)"
  (check-equal? ((run-env local-stack-result) "world")
                (MkPair 0 "hi !world")))
