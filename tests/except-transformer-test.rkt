#lang racket/base

;; Phase-27: class-method dict-passing + ExceptT.

(require rackunit
         "../main.rkt")

(rackton
  ;; ----- ExceptT String IO --------------------------------

  (: divide-safely (-> Integer (-> Integer (ExceptT String IO Integer))))
  (define (divide-safely num den)
    (if (== den 0)
        (throw-error "division by zero")
        (pure (div num den))))

  (: succ-call (ExceptT String IO Integer))
  (define succ-call (divide-safely 20 4))

  (: fail-call (ExceptT String IO Integer))
  (define fail-call (divide-safely 1 0))

  (: succ-result (IO (Result String Integer)))
  (define succ-result (run-except-t succ-call))

  (: fail-result (IO (Result String Integer)))
  (define fail-result (run-except-t fail-call))

  ;; ----- bind short-circuits on Err -----------------------
  (: chained (ExceptT String IO Integer))
  (define chained
    (do [a <- (divide-safely 100 5)]
        [b <- (divide-safely a 0)]    ;; throws — rest skipped
        [c <- (divide-safely a 2)]
      (pure (+ b c))))

  (: chained-result (IO (Result String Integer)))
  (define chained-result (run-except-t chained))

  ;; ----- catch-error recovery ----------------------------
  (: recovered (ExceptT String IO Integer))
  (define recovered
    (catch-error fail-call (lambda (_) (pure 999))))

  (: recovered-result (IO (Result String Integer)))
  (define recovered-result (run-except-t recovered))

  ;; ----- ExceptT over Maybe ------------------------------
  (: maybe-throw (ExceptT String Maybe Integer))
  (define maybe-throw (throw-error "bad"))

  (: maybe-throw-result (Maybe (Result String Integer)))
  (define maybe-throw-result (run-except-t maybe-throw))

  (: maybe-lifted (ExceptT String Maybe Integer))
  (define maybe-lifted (lift-except-t (Some 7)))

  (: maybe-lifted-result (Maybe (Result String Integer)))
  (define maybe-lifted-result (run-except-t maybe-lifted))

  (: maybe-none-lifted (ExceptT String Maybe Integer))
  (define maybe-none-lifted (lift-except-t None))

  (: maybe-none-result (Maybe (Result String Integer)))
  (define maybe-none-result (run-except-t maybe-none-lifted))

  ;; ----- ExceptT do-chain over Maybe with bind short-circuit
  (: maybe-chained (ExceptT String Maybe Integer))
  (define maybe-chained
    (do [_ <- (throw-error "from-mid")]
      (pure 99)))

  (: maybe-chained-result (Maybe (Result String Integer)))
  (define maybe-chained-result (run-except-t maybe-chained)))

;; ---------- assertions ----------------------------------

(test-case "ExceptT success path"
  (check-equal? (run-io succ-result) (Ok 5)))

(test-case "ExceptT throw-error short-circuits"
  (check-equal? (run-io fail-result) (Err "division by zero")))

(test-case "bind short-circuits on the thrown error"
  (check-equal? (run-io chained-result) (Err "division by zero")))

(test-case "catch-error recovers from a thrown error"
  (check-equal? (run-io recovered-result) (Ok 999)))

(test-case "ExceptT over Maybe: thrown error visible as outer Some (Err …)"
  (check-equal? maybe-throw-result (Some (Err "bad"))))

(test-case "ExceptT over Maybe: lift Some yields Some (Ok …)"
  (check-equal? maybe-lifted-result (Some (Ok 7))))

(test-case "ExceptT over Maybe: lift None yields outer None"
  (check-equal? maybe-none-result None))

(test-case "ExceptT over Maybe: bind on Err short-circuits within outer Some"
  (check-equal? maybe-chained-result (Some (Err "from-mid"))))
