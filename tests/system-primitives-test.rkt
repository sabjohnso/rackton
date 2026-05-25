#lang racket/base

;; System surface: random, time, env vars, argv, filesystem.

(require rackunit
         racket/file
         "../main.rkt")

(rackton
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

  ;; Round-trip make-directory + list-directory + delete-file
  (: dir-roundtrip (-> String (IO Integer)))
  (define (dir-roundtrip path)
    (do [_       <- (make-directory path)]
        [entries <- (list-directory path)]
      (pure-io (length entries)))))

;; ----- value-level smoke checks --------------------------------

(test-case "random-integer is within bounds"
  (define out (run-io roll-die))
  (check-true  (and (<= 1 out) (<  out 7))))

(test-case "current-time-seconds yields a recent epoch value"
  (define out (run-io now))
  ;; Some sanity bound: after 2020-01-01, before 3000-01-01.
  (check-true (> out 1577836800))
  (check-true (< out 32503680000)))

(test-case "getenv with a definitely-missing key returns None"
  (define out (run-io (env-or-default "RACKTON_PHASE_15_UNSET_QQ" "fallback")))
  (check-equal? out "fallback"))

(test-case "make-directory + list-directory + delete-file"
  (define tmp (make-temporary-file "rackton-p15-~a" 'directory))
  ;; The tmp dir already exists; remove it first so our rackton code
  ;; creates a fresh nested subdir.
  (delete-directory tmp)
  (check-equal? (run-io (dir-roundtrip (path->string tmp))) 0)
  (delete-directory tmp))
