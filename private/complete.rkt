#lang racket/base

;; Rackton — completion: which universe of candidates answers a point.
;;
;; Two modules supply the parts: complete-context.rkt says what kind of
;; thing belongs at the point, module-complete.rkt enumerates the module
;; paths.  This module holds the policy joining them — that a module-path
;; position is answered with collection paths, a relative-path position
;; with file paths — so the REPL and the language server share one
;; decision instead of each writing it out.
;;
;; What genuinely differs between the clients is injected: the identifier
;; lister (a live session env in the REPL, a document's analysis in the
;; server) and the directory a relative path is anchored at (the working
;; directory in the REPL, the document's own directory in the server).
;;
;; Public API:
;;   completion-answer : string nat
;;                       #:identifiers (string -> (listof string))
;;                       #:base-dir path-string
;;                    -> (values kind nat (listof string))
;;       the category at `pos`, the start of the text a candidate
;;       replaces, and the candidates themselves.

(provide completion-answer)

(require (only-in "complete-context.rkt" completion-context)
         (only-in "module-complete.rkt"
                  collection-path-completions relative-path-completions))

(define (completion-answer text pos
                           #:identifiers identifiers
                           #:base-dir [base-dir (current-directory)])
  (define-values (kind start) (completion-context text pos))
  (define prefix (substring text start pos))
  (values kind
          start
          (case kind
            [(module-path) (collection-path-completions prefix)]
            [(relative-path) (relative-path-completions prefix base-dir)]
            [else (identifiers prefix)])))
