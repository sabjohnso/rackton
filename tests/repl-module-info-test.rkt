#lang racket/base

;; REPL module info: the ,module command.
;;
;; ,module MODULE prints what a Rackton module provides (its exported
;; values with types, plus data constructors, type constructors,
;; classes, and instance count) and what modules it requires.  The
;; requires list is read from the importee's `rackton-schemes` sidecar,
;; which records the module's own `(require …)` specs.  These tests
;; drive the kernel directly, like repl-usability-test.rkt.

(require rackunit
         "../private/repl.rkt")

(define (drive-session inputs)
  (for/fold ([state (rackton-repl-init)] [out '()] #:result (values state (reverse out)))
            ([form (in-list inputs)])
    (define-values (state* o) (rackton-repl-step state form))
    (values state* (cons o out))))

(define (last-output inputs)
  (define-values (_ outs) (drive-session inputs))
  (car (reverse outs)))

;; ----- ,module on a stdlib module ---------------------------------

(test-case ",module lists what a module requires"
  ;; rackton/data/cokleisli has exactly one require: rackton/control/comonad.
  (define out (last-output '((unquote module rackton/data/cokleisli))))
  (check-regexp-match #rx"rackton/control/comonad" out))

(test-case ",module lists what a module provides, with types"
  (define out (last-output '((unquote module rackton/data/cokleisli))))
  ;; A provided value and its type signature.
  (check-regexp-match #rx"run-cokleisli" out)
  (check-regexp-match #rx"->" out))

(test-case ",module groups provides under a provides heading"
  (define out (last-output '((unquote module rackton/data/cokleisli))))
  (check-regexp-match #rx"(?i:provides)" out)
  (check-regexp-match #rx"(?i:requires)" out))

(test-case ",mod is an alias for ,module"
  (define out (last-output '((unquote mod rackton/data/cokleisli))))
  (check-regexp-match #rx"run-cokleisli" out))

;; ----- error handling ---------------------------------------------

(test-case ",module on a non-existent module reports it cleanly"
  (define out (last-output '((unquote module rackton/does/not/exist))))
  (check-regexp-match #rx"(?i:no.*module|not found|cannot)" out))

;; A relative string path cannot be anchored from the REPL — the input
;; form's source is not a file path — so resolving it raises deep in
;; `require-spec->module-path`.  That must surface as a clean message,
;; not escape and kill the kernel.  Drive a *syntax* input carrying a
;; non-path source (as a live REPL form does) to exercise the path.
(test-case ",module on an unanchorable relative path does not crash the kernel"
  (define loc (vector 'repl-stdin 1 0 1 1))
  (define input (datum->syntax #f (list 'unquote 'module "rackton/tape.rkt") loc))
  (define-values (state* out)
    (rackton-repl-step (rackton-repl-init) input))
  (check-pred string? out)
  (check-regexp-match #rx"(?i:cannot|not.*resolve|no.*module|error)" out)
  (check-false (rackton-repl-quit? state*)))

;; A string path that names a real file resolves (anchored at the
;; current directory).  Use an absolute path to an installed module so
;; the test does not depend on the working directory.
(test-case ",module on a string file path resolves a real module"
  (define path
    (path->string
     (collection-file-path "cokleisli.rkt" "rackton" "data")))
  (define out (last-output (list (list 'unquote 'module path))))
  (check-regexp-match #rx"run-cokleisli" out)
  (check-regexp-match #rx"rackton/control/comonad" out))
