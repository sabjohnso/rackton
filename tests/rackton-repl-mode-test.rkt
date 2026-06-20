#lang racket/base

;; Embedding the Rackton REPL in the host Racket REPL.
;;
;; `(require rackton/repl)` switches the running `racket` REPL into
;; Rackton mode: each subsequent form is read and rewritten into a call
;; to `rackton-process`, which threads it through the same kernel the
;; standalone Rackton REPL uses, printing `value :: Type`.  The switch is
;; done by replacing `current-read-interaction` (which only a live REPL
;; calls), so `eval`, module loading, and these tests are unaffected.

(require rackunit
         racket/port
         "../repl.rkt")

;; ----- the reader defers a form to a rackton-process call -----------
;;
;; The reader uses `read-syntax` so the bracket/brace literals' paren-shape
;; survives.  That syntax can't ride through a `(quote …)` (which would
;; flatten it to a datum), so the reader stashes the read form and emits a
;; nullary `(rackton-process-pending)` that consumes it.

(test-case "the interaction reader emits a deferred rackton-process call"
  (check-equal?
   (syntax->datum (rackton-interaction-read 'src (open-input-string "(define x 5)")))
   '(rackton-process-pending)))

(test-case "the interaction reader passes EOF through"
  (check-pred eof-object?
              (rackton-interaction-read 'src (open-input-string ""))))

(test-case "the deferred call evaluates the read form, threading state"
  (rackton-repl-reset!)
  (void (rackton-interaction-read 'src (open-input-string "(define x 5)")))
  (check-regexp-match
   #rx"x ::"
   (with-output-to-string (lambda () (rackton-process-pending)))))

(test-case "the embedded reader carries a bracket literal through to eval"
  ;; under plain `read` this would be (1 2 3) and fail to apply 1
  (rackton-repl-reset!)
  (void (rackton-interaction-read 'src (open-input-string "[1 2 3]")))
  (check-regexp-match
   #rx"List Integer"
   (with-output-to-string (lambda () (rackton-process-pending)))))

(test-case "a comma command keeps a bracket-literal argument through to eval"
  (rackton-repl-reset!)
  (void (rackton-interaction-read 'src (open-input-string ",type [1 2 3]\n")))
  (check-regexp-match
   #rx"List Integer"
   (with-output-to-string (lambda () (rackton-process-pending)))))

;; ----- rackton-process evaluates as Rackton, threading state --------

(test-case "definitions, then expressions referring to them, print value :: Type"
  (rackton-repl-reset!)
  (check-regexp-match #rx"x ::"
                      (with-output-to-string (lambda () (rackton-process '(define x 5)))))
  ;; x persists across interactions via the kernel state
  (check-regexp-match #rx"5 ::"
                      (with-output-to-string (lambda () (rackton-process 'x))))
  (check-regexp-match #rx"3 ::"
                      (with-output-to-string (lambda () (rackton-process '(+ 1 2))))))

(test-case "a type error is reported, not raised"
  (rackton-repl-reset!)
  (check-regexp-match
   #rx"error"
   (with-output-to-string (lambda () (rackton-process '(+ 1 "oops"))))))

;; ----- entering installs the reader; :quit / exit restores it -------

;; NOTE: requiring this module auto-enters Rackton mode (the feature), so
;; these tests normalize with `exit!` first, then re-enter at the end to
;; leave the load-time state intact.

(test-case "enter! installs the interaction reader; exit! restores the prior one"
  (rackton-repl-exit!)
  (define before (current-read-interaction))
  (check-not-eq? before rackton-interaction-read)
  (rackton-repl-enter!)
  (check-eq? (current-read-interaction) rackton-interaction-read)
  (rackton-repl-exit!)
  (check-eq? (current-read-interaction) before)
  (rackton-repl-enter!))

(test-case ",quit from within Rackton mode drops back to the Racket reader"
  (rackton-repl-exit!)
  (define before (current-read-interaction))
  (rackton-repl-enter!)
  (rackton-repl-reset!)
  (void (with-output-to-string (lambda () (rackton-process '(unquote quit)))))
  (check-eq? (current-read-interaction) before)
  (rackton-repl-enter!))

(test-case "re-entering after ,quit resumes the same session"
  ;; `,quit` only restores the Racket reader; the kernel state lives on,
  ;; so a definition made before the quit is still bound after re-entry.
  (rackton-repl-exit!)
  (rackton-repl-enter!)
  (rackton-repl-reset!)
  (void (with-output-to-string (lambda () (rackton-process '(define survivor 7)))))
  (void (with-output-to-string (lambda () (rackton-process '(unquote quit)))))
  (rackton-repl-enter!)
  (check-regexp-match
   #rx"7 ::"
   (with-output-to-string (lambda () (rackton-process 'survivor)))))
