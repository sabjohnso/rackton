#lang rackton

;; System surface: random, time, env vars, argv, filesystem.

(require rackton/system
         "../unit.rkt")

;; A dice roll constrained between 1 and 6 inclusive.
(: roll-die (IO Integer))
(define roll-die (random-integer 1 7))

;; Wall-clock value
(: now (IO Integer))
(define now current-time-seconds)

;; Read an environment variable, defaulting to "default"
(: env-or-default (-> String (-> String (IO String))))
(define (env-or-default name fallback)
  (do [maybe-v <- (getenv name)]
    (pure-io
     (match maybe-v
       [(None)   fallback]
       [(Some v) v]))))

;; Round-trip createDirectoryIfMissing + list-directory (idempotent)
(: dir-roundtrip (-> String (IO Integer)))
(define (dir-roundtrip path)
  (do [_       <- (create-directory-if-missing path)]
      [entries <- (list-directory path)]
    (pure-io (length entries))))

;; ----- value-level smoke checks --------------------------------

(: out-roll Integer) (define out-roll (run-io roll-die))
(: out-now Integer)  (define out-now (run-io now))
(: out-env String)   (define out-env (run-io (env-or-default "RACKTON_PHASE_15_UNSET_QQ" "fallback")))
(: out-dir Integer)  (define out-dir (run-io (dir-roundtrip "/tmp/rackton-p15-fresh-dir-zz")))

(: suite (List Test))
(define suite
  (list
   (it "random-integer is within bounds"
       (check-true (and (<= 1 out-roll) (< out-roll 7))))
   (it "current-time-seconds yields a recent epoch value"
       (all-checks
        (list (check-true (> out-now 1577836800))
              (check-true (< out-now 32503680000)))))
   (it "getenv with a definitely-missing key returns None"
       (check-equal? out-env "fallback"))
   (it "make-directory + list-directory"
       (check-equal? out-dir 0))))

(: _ran Unit)
(define _ran (run-io (run-suite "rackton/system primitives" suite)))
