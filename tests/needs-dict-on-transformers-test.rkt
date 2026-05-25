#lang racket/base

;; Phase 33: runtime resolvers for needs-dict instances.  Make
;; `do`-notation and `catch-e` work on transformer stacks where the
;; inner monad's instance is itself needs-dict — primarily ExceptT
;; over IO, and ExceptT over ExceptT for nested-error code.

(require rackunit
         "../main.rkt")

(rackton
  ;; ----- 33.A regression: base ExceptT >>= --------------------
  ;; Phase 32 ran a one-line catch-e on base ExceptT.  This test
  ;; pulls the >>= path through `do`-notation directly to guard
  ;; against a regression in the registration.

  (: ok-then  (ExceptT String IO Integer))
  (define ok-then
    (do [x <- (pure 1)]
        [y <- (pure 2)]
      (pure (+ x y))))

  (: ok-then-result (IO (Result String Integer)))
  (define ok-then-result (run-except-t ok-then))

  ;; ----- 33.B nested ExceptT >>= ------------------------------
  ;; The inner monad is ExceptT String IO (itself needs-dict).
  ;; Without Phase 33's runtime resolver, >>= on MkExceptT fails.

  (: nested-ok (ExceptT String (ExceptT String IO) Integer))
  (define nested-ok
    (do [x <- (pure 10)]
      (pure (+ x 5))))

  (: nested-ok-result (IO (Result String (Result String Integer))))
  (define nested-ok-result (run-except-t (run-except-t nested-ok)))

  ;; ----- 33.C catch-e through StateT (ExceptT IO) --------------
  ;; The Phase 32 deferral: lifted catch-e through one transformer
  ;; over ExceptT.

  (: thrower-st (StateT Integer (ExceptT String IO) Integer))
  (define thrower-st
    (do [_ <- (put-st 99)]
        [_ <- (ann (throw-e "boom")
                   (StateT Integer (ExceptT String IO) Integer))]
        get-st))

  (: recover-st
     (-> String (StateT Integer (ExceptT String IO) Integer)))
  (define (recover-st _msg) (pure 7))

  (: caught-st (StateT Integer (ExceptT String IO) Integer))
  (define caught-st (catch-e thrower-st recover-st))

  (: caught-st-result (IO (Result String (Pair Integer Integer))))
  (define caught-st-result (run-except-t ((run-state-t caught-st) 0)))

  ;; ----- 33.D catch-e on base ExceptT (Phase 32 regression) ----

  (: thrower-base (ExceptT String IO Integer))
  (define thrower-base (throw-e "boom"))

  (: recover-base (-> String (ExceptT String IO Integer)))
  (define (recover-base _) (pure 42))

  (: caught-base (ExceptT String IO Integer))
  (define caught-base (catch-e thrower-base recover-base))

  (: caught-base-result (IO (Result String Integer)))
  (define caught-base-result (run-except-t caught-base))

  ;; ----- 33.E nested ExceptT catch via runtime resolver --------
  ;; Two layers of ExceptT (with distinct error types) over IO.
  ;; The inner-pure derivation has to walk MkExceptT → MkExceptT →
  ;; $io.  Throw at the OUTER ExceptT's error layer; catch with a
  ;; handler that returns a pure value.

  (: outer-throw (ExceptT String (ExceptT Integer IO) Integer))
  (define outer-throw (throw-e "outer-boom"))

  (: outer-recover
     (-> String (ExceptT String (ExceptT Integer IO) Integer)))
  (define (outer-recover _) (pure 99))

  (: caught-outer (ExceptT String (ExceptT Integer IO) Integer))
  (define caught-outer (catch-e outer-throw outer-recover))

  (: caught-outer-result (IO (Result Integer (Result String Integer))))
  (define caught-outer-result
    (run-except-t (run-except-t caught-outer))))

;; ---------- assertions ---------------------------------------

(test-case "do-notation chain on base ExceptT IO"
  (check-equal? (run-io ok-then-result) (Ok 3)))

(test-case "do-notation chain on nested ExceptT (ExceptT IO)"
  (check-equal? (run-io nested-ok-result) (Ok (Ok 15))))

(test-case "catch-e lifted through StateT (ExceptT IO)"
  (check-equal? (run-io caught-st-result)
                (Ok (MkPair 0 7))))

(test-case "catch-e base ExceptT IO (regression)"
  (check-equal? (run-io caught-base-result) (Ok 42)))

(test-case "catch-e on nested ExceptT (ExceptT IO) via runtime resolver"
  (check-equal? (run-io caught-outer-result)
                (Ok (Ok 99))))
