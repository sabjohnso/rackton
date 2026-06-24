#lang rackton

;; rackton/cmdline — umbrella over the command-line library, modeled on
;; OCaml's cmdliner.  `(require rackton/cmdline)` brings in the whole
;; interface: converters, argument info + constructors, the applicative
;; Term, the argv parser, man pages, the pure run bridge, and Cmd/eval.
;;
;; This lives at the collection root (like rackton/system) so the name
;; `rackton/cmdline` resolves to it; the per-concern modules are
;; rackton/cmdline/conv, …/arg, …/eval, etc.  Prefer the specific
;; imports in library code; the umbrella is handy for scripts.

(require rackton/cmdline/conv
         rackton/cmdline/arg
         rackton/cmdline/argval
         rackton/cmdline/term
         rackton/cmdline/parser
         rackton/cmdline/parsed
         rackton/cmdline/manpage
         rackton/cmdline/run
         rackton/cmdline/eval)

(provide (all-from-out rackton/cmdline/conv)
         (all-from-out rackton/cmdline/arg)
         (all-from-out rackton/cmdline/argval)
         (all-from-out rackton/cmdline/term)
         (all-from-out rackton/cmdline/parser)
         (all-from-out rackton/cmdline/parsed)
         (all-from-out rackton/cmdline/manpage)
         (all-from-out rackton/cmdline/run)
         (all-from-out rackton/cmdline/eval))
