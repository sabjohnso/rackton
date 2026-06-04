#lang racket/base

;; Mtl-style classes — MonadState, MonadEnv, MonadWriter,
;; MonadError.  Polymorphic effect code runs against multiple
;; transformer stacks.

(require rackunit
         "../main.rkt")

(rackton
  (require rackton/control/monad/state)
  (require rackton/control/monad/reader)
  (require rackton/control/monad/writer)
  (require rackton/control/monad/except)
  (require rackton/data/result)
  ;; ----- MonadState polymorphic increment -----------------
  (: incr-by ((MonadState Integer m) => (-> Integer (m Unit))))
  (define (incr-by n)
    (do [k <- get-st]
      (put-st (+ k n))))

  ;; Against plain State
  (: state-after-incr (State Integer Unit))
  (define state-after-incr (incr-by 5))

  (: state-result (Pair Integer Unit))
  (define state-result ((run-state state-after-incr) 10))

  ;; Against StateT Integer IO
  (: state-t-io-incr (StateT Integer IO Unit))
  (define state-t-io-incr (incr-by 3))

  (: state-t-io-result (IO (Pair Integer Unit)))
  (define state-t-io-result ((run-state-t state-t-io-incr) 100))

  ;; Against lifted EnvT String (State Integer)
  (: env-of-state-incr (EnvT String (State Integer) Unit))
  (define env-of-state-incr (incr-by 7))

  (: env-of-state-result (State Integer Unit))
  (define env-of-state-result ((run-env-t env-of-state-incr) "config"))

  ;; ----- MonadEnv polymorphic reader -----------------------
  (: greet-env ((MonadEnv String m) => (m String)))
  (define greet-env
    (do [name <- ask-en]
      (pure (mappend "hello, " name))))

  ;; Against plain Env
  (: env-greet-result String)
  (define env-greet-result ((run-env greet-env) "world"))

  ;; Against EnvT String IO
  (: env-t-io-greet-result (IO String))
  (define env-t-io-greet-result ((run-env-t greet-env) "io-world"))

  ;; Against lifted StateT Integer (Env String)
  (: state-of-env-greet (StateT Integer (Env String) String))
  (define state-of-env-greet greet-env)

  (: state-of-env-greet-result (Env String (Pair Integer String)))
  (define state-of-env-greet-result ((run-state-t state-of-env-greet) 0))

  ;; ----- MonadError polymorphic throw -----------------------
  (: must-be-positive ((MonadError String m) => (-> Integer (m Integer))))
  (define (must-be-positive n)
    (if (> n 0)
        (pure n)
        (throw-e "non-positive")))

  ;; Against ExceptT String IO
  (: ok-result (IO (Result String Integer)))
  (define ok-result (run-except-t (must-be-positive 7)))

  (: err-result (IO (Result String Integer)))
  (define err-result (run-except-t (must-be-positive -1)))

  ;; ----- Nested stack: StateT s (EnvT r IO) ----------------
  ;; Uses both MonadState and MonadEnv via lifted instances.

  (: stateful-greet ((MonadState Integer m) (MonadEnv String m) =>
                     (m String)))
  (define stateful-greet
    (do [_    <- (incr-by 1)]
        [n    <- get-st]
        [name <- ask-en]
      (pure (mappend name (mappend ": " (integer->string n))))))

  (: nested-stack-result (IO (Pair Integer String)))
  (define nested-stack-result
    ((run-env-t ((run-state-t (ann stateful-greet
                                   (StateT Integer (EnvT String IO) String)))
                 0))
     "tag")))

;; ---------- assertions ---------------------------------------

(test-case "MonadState over State"
  (check-equal? state-result (Pair 15 Unit)))

(test-case "MonadState over StateT IO"
  (check-equal? (run-io state-t-io-result) (Pair 103 Unit)))

(test-case "MonadState lifted over EnvT (State Integer)"
  (check-equal? ((run-state env-of-state-result) 5)
                (Pair 12 Unit)))

(test-case "MonadEnv over Env"
  (check-equal? env-greet-result "hello, world"))

(test-case "MonadEnv over EnvT IO"
  (check-equal? (run-io env-t-io-greet-result) "hello, io-world"))

(test-case "MonadEnv lifted over StateT (Env r)"
  (check-equal? ((run-env state-of-env-greet-result) "from-env")
                (Pair 0 "hello, from-env")))

(test-case "MonadError throw + ok path over ExceptT IO"
  (check-equal? (run-io ok-result)  (Ok 7))
  (check-equal? (run-io err-result) (Err "non-positive")))

(test-case "Nested stack: StateT over EnvT IO with both effects"
  (check-equal? (run-io nested-stack-result)
                (Pair 1 "tag: 1")))
