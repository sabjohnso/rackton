#lang info
(define collection "rackton")
;; syntax-color-lib: the Racket lexer is the structural editor's
;; "syntax table" (private/repl-entry.rkt) and its coloring source.
(define deps '("base"
               "syntax-color-lib"))
(define build-deps '("scribble-lib"
                     "racket-doc"
                     "rackunit-lib"
                     "rackcheck-lib"
                     ;; sandbox-lib: scribblings/rackton-eval.rkt runs the
                     ;; doc examples in a sandbox at build time.
                     "sandbox-lib"))
(define scribblings
  '(("scribblings/rackton.scrbl"                               ()           (language))
    ("scribblings/guide/rackton-guide.scrbl"                   (multi-page) (language))
    ("scribblings/reference/rackton-reference.scrbl"           (multi-page) (language))
    ("scribblings/developer/rackton-developer.scrbl"           (multi-page))))
(define pkg-desc "A Racket adaptation of the Coalton statically-typed functional language")
;; Must be a valid Racket version (`raco setup --check-pkg-deps` rejects
;; others): a MAJOR.MINOR pair, optionally .PATCH/.BUILD, where no
;; component past the minor is a trailing zero.  A patch-zero release is
;; the two-part "0.38" (not "0.38.0"); a non-zero patch is "0.38.1".
;; The minor is the release counter.
(define version "1.1")
(define pkg-authors '(sbj))
(define license '(Apache-2.0 OR MIT))
