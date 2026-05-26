#lang scribble/manual
@require[scribble/manual
         (for-label rackton
                    (only-in racket/base module))]

@title[#:tag "entry-points"]{Entry points}
Rackton code reaches the typechecker through three distinct entry
points, all of which route through a single elaboration pipeline.

@defform[#:literals (rackton)
         (rackton form ...)]{

Type-checks and compiles each @racket[form] as Rackton source, then
splices the generated Racket code back into the surrounding module at
the original site.  Multiple @racket[(rackton …)] blocks may coexist
in one Racket module.
Each block elaborates independently against the prelude (see
@secref["values"] for the prelude bindings).  Bindings from one block
are visible at runtime to a later block in the same Racket module, but
the type checker does not propagate schemes between them.

@racketblock[
(require rackton)
(rackton
  (define-data (Maybe a) None (Some a))
  (: from-maybe (-> a (-> (Maybe a) a)))
  (define (from-maybe d m)
    (match m [(None) d] [(Some x) x])))]}

@defform[#:literals (rackton/main)
         (rackton/main form ...)]{

Like @racket[rackton], but additionally emits a @racketmodfont{rackton-schemes}
sidecar submodule carrying the typing environment in serialisable form
so that other Rackton files importing this module can recover its
schemes.  A given Racket module may contain at most one
@racket[rackton/main] block.  This is the form produced by
@hash-lang[] @racketmodfont{rackton} and by
@racket[(module @#,racketidfont{name} rackton …)]; user code rarely
uses it directly.}

@section[#:tag "module-form"]{The @racket[module] form}

When @racket[rackton] is used as the language in a @racket[module]
form, the custom @racketidfont{#%module-begin} wraps every body form
in a single @racket[rackton/main] invocation:

@racketblock[
(module greet rackton
  (provide greet)
  (: greet (-> String String))
  (define (greet name)
    (racket String (name)
      (string-append "hello " name))))]

Exports are driven by the user's @racket[provide] forms inside the
body; no @racket[provide] form means nothing escapes.

@section[#:tag "lang-rackton"]{@hash-lang[] @racketmodfont{rackton}}

A file beginning with @hash-lang[] @racketmodfont{rackton} is read by
@filepath{lang/reader.rkt} into a single @racket[rackton/main]
invocation:

@codeblock|{
#lang rackton

(provide fact)
(: fact (-> Integer Integer))
(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))
}|

This interface is the canonical one for Rackton source files; the
embedded @racket[(rackton …)] form is intended for using Rackton from
inside an otherwise-Racket codebase.

Run a @hash-lang[] @racketmodfont{rackton} file directly with
@exec{racket file.rkt}, or load it from another module with
@racket[(require "file.rkt")].
