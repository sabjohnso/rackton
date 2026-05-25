#lang racket/base

;; End-to-end: Float arithmetic, conversions, sqrt;
;; try / raise-io for structured error recovery.

(require rackunit
         racket/port
         "../main.rkt")

(rackton
  ;; Float literals + arithmetic.  Use values whose binary repr is
  ;; exact to avoid floating-point round-off in assertions.
  (define half 0.5)
  (define two  2.0)
  (define sum-f  (+ half two))         ;; 2.5
  (define prod-f (* half two))         ;; 1.0
  (define is-half-less (< half 1.0))   ;; #t

  ;; Fractional division
  (define quotient-f (float-div 7.0 2.0))

  ;; sqrt
  (define root-9 (sqrt 9.0))

  ;; Conversions
  (define one-float (integer->float 1))
  (define from-half (float->integer half))     ;; truncates → 0

  ;; Show Float
  (define half-shown (show half))

  ;; Try over IO catches panics.
  (: pinch (-> Integer (IO Integer)))
  (define (pinch n)
    (if (< n 0)
        (raise-io "no negatives allowed")
        (pure-io n)))

  (: safe-pinch (-> Integer (IO (Result String Integer))))
  (define (safe-pinch n) (try (pinch n))))

;; ----- Float arithmetic ---------------------------------------

(test-case "Float arithmetic dispatches on tag"
  (check-equal? sum-f 2.5)
  (check-equal? prod-f 1.0)
  (check-true  is-half-less))

(test-case "Fractional division"
  (check-equal? quotient-f 3.5))

(test-case "sqrt"
  (check-equal? root-9 3.0))

(test-case "Conversions"
  (check-equal? one-float 1.0)
  (check-equal? from-half 0))

(test-case "Show Float"
  (check-equal? half-shown "0.5"))

;; ----- try / raise-io -----------------------------------------

(test-case "try wraps successful actions in Ok"
  (check-equal? (run-io (safe-pinch 4)) (Ok 4)))

(test-case "try captures panic / raise-io as Err"
  (define res (run-io (safe-pinch -1)))
  (check-pred (lambda (r) (match r [(Err _) #t] [_ #f])) res))
