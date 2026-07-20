#lang racket/base

;; REPL: the completion commands.
;;
;; ,complete PREFIX prints the completion candidates for PREFIX — the
;; session env's vars, data constructors, classes, and type
;; constructors, plus the surface keywords — one per line.  It is the
;; pipe transport for editor completion: `rackton-repl-completions`
;; already drives the terminal editor's Tab, and this command exposes
;; the same answers to a piped client.
;;
;; A piped client knows where its own point is, so the two other
;; universes a `require` spec may name get a command each:
;; ,complete-module PREFIX for collection paths and ,complete-path
;; PREFIX for relative file paths.  The terminal editor needs no such
;; split — it hands the whole entry to `rackton-repl-completions-at`,
;; which classifies the position itself.  These tests drive the kernel
;; directly.

(require rackunit
         racket/string
         racket/file
         "../private/repl.rkt")

(define (command-output cmd prefix)
  (define-values (_ o)
    (rackton-repl-step (rackton-repl-init) (list 'unquote cmd prefix)))
  o)

(define (complete-output prefix) (command-output 'complete prefix))

(define (lines out) (if (string=? out "") '() (regexp-split #rx"\n" (string-trim out))))

(test-case ",complete lists matching prelude names and keywords"
  (define out (complete-output 'ma))
  (check-regexp-match #rx"(?m:^match$)" out)   ; a surface keyword
  (check-regexp-match #rx"(?m:^max$)" out))    ; a prelude binding

(test-case ",complete lists definition keywords for a 'de' prefix"
  (define out (complete-output 'de))
  (check-regexp-match #rx"(?m:^define$)" out)
  (check-regexp-match #rx"(?m:^delay$)" out))

(test-case ",complete prints one candidate per line"
  (define out (complete-output 'de))
  (for ([line (in-list (regexp-split #rx"\n" (string-trim out)))])
    (check-regexp-match #rx"^[^ \t]+$" line)))

(test-case ",complete with no matches prints nothing"
  (check-equal? (complete-output 'zzznotaname) ""))

(test-case ",complete stays identifier-only"
  ;; A client that wants module paths must say so; ,complete's meaning
  ;; does not shift under existing users.
  (check-equal? (complete-output 'rackton/data/) ""))

;; ----- module paths ---------------------------------------------------

(test-case ",complete-module lists collection paths"
  (define out (command-output 'complete-module 'rackton/data/may))
  (check-equal? (lines out) '("rackton/data/maybe")))

(test-case ",complete-module marks a directory with a trailing slash"
  (define out (command-output 'complete-module 'rackton/da))
  (check-not-false (member "rackton/data/" (lines out))))

(test-case ",complete-module with no matches prints nothing"
  (check-equal? (command-output 'complete-module 'zzz-no-collection/) ""))

;; ----- relative paths -------------------------------------------------

(test-case ",complete-path lists files relative to the working directory"
  (define dir (make-temporary-directory))
  (display-to-file "" (build-path dir "helpers.rkt"))
  (parameterize ([current-directory dir])
    (check-equal? (lines (command-output 'complete-path 'he)) '("helpers.rkt")))
  (delete-directory/files dir))

(test-case ",complete-path accepts a string argument"
  ;; A path fragment need not read as a symbol.
  (define dir (make-temporary-directory))
  (display-to-file "" (build-path dir "helpers.rkt"))
  (parameterize ([current-directory dir])
    (check-equal? (lines (command-output 'complete-path "he")) '("helpers.rkt")))
  (delete-directory/files dir))

;; ----- the position-classifying entry point ---------------------------

;; What the terminal editor's Tab calls: the whole entry and the point,
;; answered with the region to replace and its candidates.

(define (complete-at text)
  (define-values (start cs)
    (rackton-repl-completions-at (rackton-repl-init) text (string-length text)))
  (list (substring text start) cs))

(test-case "a name outside a require completes to identifiers"
  (check-not-false (member "match" (cadr (complete-at "(ma")))))

(test-case "a module path inside a require completes to collection paths"
  (define r (complete-at "(require rackton/data/may"))
  (check-equal? (car r) "rackton/data/may"
                "the region replaced spans the whole path, slashes included")
  (check-equal? (cadr r) '("rackton/data/maybe")))

(test-case "a string inside a require completes to relative paths"
  (define dir (make-temporary-directory))
  (display-to-file "" (build-path dir "helpers.rkt"))
  (parameterize ([current-directory dir])
    (define r (complete-at "(require \"he"))
    (check-equal? (car r) "he" "the region replaced is inside the quote")
    (check-equal? (cadr r) '("helpers.rkt")))
  (delete-directory/files dir))
