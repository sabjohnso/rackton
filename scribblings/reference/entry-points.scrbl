#lang scribble/manual
@require[scribble/manual
         (for-label rackton
                    (only-in racket/base module))
         "../rackton-eval.rkt"]
@(define ev (make-rackton-eval))

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
  (data (Maybe a) None (Some a))
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

@rackton-example[#:eval ev]{
#lang rackton

(provide fact)
(: fact (-> Integer Integer))
(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))
}

This interface is the canonical one for Rackton source files; the
embedded @racket[(rackton …)] form is intended for using Rackton from
inside an otherwise-Racket codebase.

Run a @hash-lang[] @racketmodfont{rackton} file directly with
@exec{racket file.rkt}, or load it from another module with
@racket[(require "file.rkt")].

@section[#:tag "reserved-entry-points"]{@racket[main] and @racket[test-main]}

@racket[main] and @racket[test-main] are reserved names, recognized
only in @racket[rackton/main] context (a @hash-lang[]
@racketmodfont{rackton} file, or @racket[(module @#,racketidfont{name}
rackton …)]) — not inside an embedded @racket[(rackton …)] block, where
they are ordinary names.  Each, if a module defines it, must have type
@racket[(IO Unit)] — a declared signature is checked against exactly
that type, and an undeclared one gets @racket[(IO Unit)] synthesized as
its signature, so an otherwise-ambiguous body (this language has no
type-class defaulting) still resolves.

Defining @racket[main] emits @racket[(module+ main (run-io main))];
defining @racket[test-main] emits @racket[(module+ test (run-io
test-main))].  Both piggyback on ordinary Racket/@exec{raco} behavior:
running @exec{racket file.rkt} instantiates the module's top level and
then, if present, its @racketidfont{main} submodule; @exec{raco test}
prefers a module's @racketidfont{test} submodule over requiring the
module directly.  So an application's entry point is @racket[main]:

@rackton-example[#:eval ev]{
#lang rackton

(: main (IO Unit))
(define main (println "hello, world"))
}

and a test suite's is @racket[test-main] — see
@secref["testing" #:doc '(lib "rackton/scribblings/guide/rackton-guide.scrbl")]
for @tt{rackton/unit}'s @racketidfont{run-suite}, which packages a
@racketidfont{Test} tree into exactly this shape.  Neither name's IO
runs merely because something @racket[require]s the file — only
instantiating the matching submodule runs it, which keeps requiring a
module (e.g. to reuse its types or definitions) free of side effects.
