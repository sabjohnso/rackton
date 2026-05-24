#lang racket/base

;; Rackton REPL — user-facing entry point.
;;
;; Re-exports the REPL kernel and, when run as `racket -l rackton/repl`,
;; boots the interactive loop directly so users don't have to type
;; the entry expression themselves.

(require "private/repl.rkt")

(provide (all-from-out "private/repl.rkt"))

(module+ main
  (rackton-repl-run))
