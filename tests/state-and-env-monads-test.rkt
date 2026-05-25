#lang racket/base

;; State, Env, StateT, EnvT.

(require rackunit
         "../main.rkt")

(rackton
  ;; ----- State: a counter ----------------------------------
  (: tick (State Integer Integer))
  (define tick
    (do [n <- get-state]
        [_ <- (put-state (+ n 1))]
      (pure n)))

  (: three-ticks (State Integer (List Integer)))
  (define three-ticks
    (do [a <- tick]
        [b <- tick]
        [c <- tick]
      (pure (Cons a (Cons b (Cons c Nil))))))

  (: ticked (Pair Integer (List Integer)))
  (define ticked ((run-state three-ticks) 10))

  ;; ----- Env: read + transform env -------------------------
  (: greet (Env String String))
  (define greet
    (do [name <- ask]
      (pure (<> "hello, " name))))

  (: shouted (Env String String))
  (define shouted (local (lambda (s) (<> s "!!")) greet))

  (: greeted String)
  (define greeted ((run-env greet) "world"))

  (: greeted-loud String)
  (define greeted-loud ((run-env shouted) "world"))

  ;; ----- StateT over Maybe: short-circuit -----------------
  (: safe-div (-> Integer (-> Integer (StateT Integer Maybe Integer))))
  (define (safe-div num den)
    (if (== den 0)
        (lift-state-t None)
        (do [acc <- get-state-t]
            [_   <- (put-state-t (+ acc 1))]
          (pure (div num den)))))

  (: ok-chain (Maybe (Pair Integer Integer)))
  (define ok-chain
    ((run-state-t (do [a <- (safe-div 20 4)]
                      [b <- (safe-div 10 a)]
                    (pure (+ a b))))
     0))

  (: bad-chain (Maybe (Pair Integer Integer)))
  (define bad-chain
    ((run-state-t (do [a <- (safe-div 20 4)]
                      [_ <- (safe-div 1 0)]
                    (pure a)))
     0))

  ;; ----- EnvT over IO: read config in IO ------------------
  (: shouted-io (EnvT String IO String))
  (define shouted-io
    (do [name <- ask-t]
      (pure (<> "[CONFIG] " name))))

  ;; ----- lift-state-t round-trip --------------------------
  (: rounded (Maybe (Pair Integer Integer)))
  (define rounded
    ((run-state-t (lift-state-t (Some 99))) 7)))

;; ---------- assertions -------------------------------------

(test-case "State: three-ticks counts 10→13, collects 10,11,12"
  (check-equal? ticked
                (MkPair 13 (Cons 10 (Cons 11 (Cons 12 Nil))))))

(test-case "Env: reads the environment"
  (check-equal? greeted "hello, world"))

(test-case "Env local modifies the env"
  (check-equal? greeted-loud "hello, world!!"))

(test-case "StateT/Maybe success path threads state"
  ;; safe-div 20 4 = 5 with state→1
  ;; safe-div 10 5 = 2 with state→2
  ;; result = 5 + 2 = 7
  (check-equal? ok-chain (Some (MkPair 2 7))))

(test-case "StateT/Maybe short-circuits on inner None"
  (check-equal? bad-chain None))

(test-case "EnvT/IO threads config through IO"
  (check-equal? (run-io ((run-env-t shouted-io) "settings.cfg"))
                "[CONFIG] settings.cfg"))

(test-case "lift-state-t wraps an inner-monad action"
  (check-equal? rounded (Some (MkPair 7 99))))
