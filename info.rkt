#lang info
(define collection "rackton")
(define deps '("base"))
(define build-deps '("scribble-lib"
                     "racket-doc"
                     "rackunit-lib"
                     "rackcheck-lib"))
(define scribblings
  '(("scribblings/rackton.scrbl"                               ()           (language))
    ("scribblings/guide/rackton-guide.scrbl"                   (multi-page) (language))
    ("scribblings/reference/rackton-reference.scrbl"           (multi-page) (language))
    ("scribblings/developer/rackton-developer.scrbl"           (multi-page))))
(define pkg-desc "A Racket adaptation of the Coalton statically-typed functional language")
(define version "0.3")
(define pkg-authors '(sbj))
(define license '(Apache-2.0 OR MIT))
