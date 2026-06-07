#lang racket/base

;; Rackton REPL — user-facing entry point.
;;
;; Two ways in:
;;   - `racket -l rackton/repl` boots the standalone loop (`module+ main`).
;;   - `(require rackton/repl)` at a running `racket` REPL switches that
;;     REPL into Rackton mode: subsequent forms are evaluated as Rackton
;;     and printed as `value :: Type`, like `typed/racket`.  `:quit`
;;     returns to the plain Racket reader.
;;
;; The switch replaces `current-read-interaction` — the procedure a live
;; REPL uses to read each interaction — with one that rewrites the form
;; into a call to `rackton-process`.  The normal evaluator then runs that
;; call, so the kernel's own `eval` is never re-entered (which is why we
;; hook the reader, not `current-eval`).  `current-read-interaction` is
;; invoked only by an interactive REPL, so `eval`, module loading, and
;; the test suite are unaffected.

(require "private/repl.rkt"
         (only-in "private/term.rkt" refresh-type-columns!))

(provide (all-from-out "private/repl.rkt")
         rackton-repl-enter!
         rackton-repl-exit!
         rackton-process
         rackton-interaction-read
         rackton-repl-reset!)

;; ----- session state -------------------------------------------------

;; The one kernel state for this embedded session, created lazily.
(define repl-state-box (box #f))

(define (ensure-state!)
  (unless (unbox repl-state-box)
    (set-box! repl-state-box (rackton-repl-init))))

;; Discard any session state (fresh prelude env).  Used on entry and by
;; tests to isolate sequences.
(define (rackton-repl-reset!)
  (set-box! repl-state-box (rackton-repl-init)))

;; ----- per-interaction processing ------------------------------------

;; Evaluate one already-read Rackton datum: step the kernel, persist the
;; new state, and print its `value :: Type` / definition / error string.
;; Returns (void) so the host REPL prints nothing of its own.  A `:quit`
;; drops back to the plain Racket reader.
(define (rackton-process datum)
  (ensure-state!)
  ;; Match the standalone REPL: track the terminal width each interaction
  ;; so wrapped type errors fit the window.
  (refresh-type-columns!)
  (define-values (state* output)
    (rackton-repl-step (unbox repl-state-box) datum))
  (set-box! repl-state-box state*)
  (display output)
  (when (rackton-repl-quit? state*)
    (rackton-repl-exit!)
    (display "; returned to the Racket reader\n"))
  (void))

;; ----- the interaction reader ----------------------------------------

;; Read one form and rewrite it into `(rackton-process 'form)`, so the
;; normal evaluator runs it through the kernel.  `rackton-process` is
;; spliced as this module's binding (hygienically), so it resolves
;; regardless of the REPL namespace's imports.
(define (rackton-interaction-read src in)
  (define form (read in))
  (if (eof-object? form)
      form
      #`(rackton-process (quote #,(datum->syntax #f form)))))

;; ----- entering / leaving Rackton mode -------------------------------

;; The reader in effect before we switched, so `:quit` / exit can restore
;; it.  #f when we are not currently installed.
(define saved-reader (box #f))

(define (rackton-repl-enter!)
  (unless (unbox saved-reader)
    (set-box! saved-reader (current-read-interaction))
    (current-read-interaction rackton-interaction-read)))

(define (rackton-repl-exit!)
  (define prev (unbox saved-reader))
  (when prev
    (current-read-interaction prev)
    (set-box! saved-reader #f)))

;; Requiring `rackton/repl` switches the host REPL into Rackton mode.
;; This only swaps `current-read-interaction` (the procedure a live REPL
;; calls to read each form); it writes nothing and touches no other
;; global, so it is inert in scripts, `eval`, and the test suite — none of
;; which read interactions.  The mode change is visible from the first
;; `value :: Type` result; `:quit` returns to the plain Racket reader.
(rackton-repl-enter!)

(module+ main
  (rackton-repl-run))
