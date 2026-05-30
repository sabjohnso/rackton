#lang racket/base

;; rackton/system/exit — exitSuccess / exitFailure / exitWith.
;; Calling exit in-process would kill the test runner, so each case
;; runs a tiny #lang rackton fixture in a fresh `racket` subprocess and
;; checks the process exit status.

(require rackunit
         racket/system
         racket/file
         racket/port)

(define racket-bin (find-executable-path "racket"))

(define (exit-code-of body)
  (define f (make-temporary-file "rackton-exit-~a.rkt"))
  (call-with-output-file f #:exists 'replace
    (lambda (out) (write-string body out)))
  (begin0
    (parameterize ([current-output-port (open-output-nowhere)]
                   [current-error-port  (open-output-nowhere)])
      (system*/exit-code racket-bin (path->string f)))
    (delete-file f)))

(define (fixture action-expr)
  (string-append
   "#lang rackton\n"
   "(require rackton/system/exit)\n"
   "(define done (run-io (ann " action-expr " (IO Unit))))\n"))

(test-case "exit-with (ExitFailure n) exits with status n"
  (check-equal? (exit-code-of (fixture "(exit-with (ExitFailure 3))")) 3))

(test-case "exit-success exits 0"
  (check-equal? (exit-code-of (fixture "exit-success")) 0))

(test-case "exit-failure exits 1"
  (check-equal? (exit-code-of (fixture "exit-failure")) 1))
