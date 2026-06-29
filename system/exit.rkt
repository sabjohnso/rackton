#lang rackton

;; rackton/system/exit — System.Exit.  Terminate the process with a
;; status code.  ExitCode is a plain Rackton data type; the single
;; runtime primitive (exit-with-code) lives in private/prelude-runtime
;; and is reached via `foreign`.

(provide (all-defined-out))

;; ExitSuccess is status 0; (ExitFailure n) is status n (conventionally
;; non-zero).
(data ExitCode
  ExitSuccess
  (ExitFailure Integer))

;; The host primitive: exit with a raw status code.  `exit` never
;; returns, so the result type is free (Haskell's `exitWith :: ExitCode
;; -> IO a`).
(foreign exit-with-code (-> Integer (IO a))
         :from rackton/private/prelude-runtime)

;; exitWith: terminate with the status named by an ExitCode.
(: exit-with (-> ExitCode (IO a)))
(define (exit-with code)
  (match code
    [(ExitSuccess)    (exit-with-code 0)]
    [(ExitFailure n)  (exit-with-code n)]))

;; exitSuccess: terminate with status 0.
(: exit-success (IO a))
(define exit-success (exit-with-code 0))

;; exitFailure: terminate with status 1.
(: exit-failure (IO a))
(define exit-failure (exit-with-code 1))
