#lang scribble/manual
@require[scribble/manual
         (for-label rackton)
         "../rackton-eval.rkt"]
@(define ev (make-rackton-eval))

@title[#:tag "quickstart"]{Quickstart}

@section{Install and run}

Rackton is a Racket package.  Install it once with:

@commandline{raco pkg install --link --auto}

from the package's working directory.  After install, the
@racketidfont{#%module-begin} hooks and @hash-lang[]
@racketmodfont{rackton} reader are both available.

@section{Hello, world}

Save the following as @filepath{hello.rkt}:

@rackton-example[#:eval ev]{
#lang rackton

(: main (IO Unit))
(define main (println "hello, world"))
}

Run it with @exec{racket hello.rkt}.  @racket[println] is typed
@racket[(-> String (IO Unit))] — it doesn't perform IO when called; it
returns an @racket[IO] action.  @racket[main] is a reserved name: a
@hash-lang[]@racketmodfont{rackton} module that defines it must give it
type @racket[(IO Unit)], and Rackton emits a @racket[(module+ main
(run-io main))] submodule that runs it — which is what
@exec{racket hello.rkt} instantiates.  @secref["testing"] covers the
matching @racket[test-main] entry point, run by @exec{raco test}.

@section{Your first typed function}

@rackton-example[#:eval ev]{
#lang rackton

(provide fact)

(: fact (-> Integer Integer))
(define (fact n)
  (if (= n 0) 1 (* n (fact (- n 1)))))
}

The @racket[:] form is the type signature.  The @racket[define] is
checked against it.  Drop the signature and Rackton still infers
@racket[(-> Integer Integer)] from the body — the integer literal
@racket[1] and the arithmetic on @racket[n] each fix one end of the
arrow.  For a body with free type variables (e.g.
@racket[(define (id x) x)]), the inferred scheme is generalised:
@racket[(All (a) (-> a a))].

@section{The REPL}

Launch the REPL with:

@commandline{racket -l rackton/repl}

@verbatim|{
λ> (+ 1 2)
3 :: Integer
λ> ,type (lambda (x) (Cons x Nil))
(lambda (x) (Cons x Nil)) :: (All (a) (-> a (List a)))
λ> ,quit
}|

REPL commands are typed with a leading comma — @litchar{,type},
@litchar{,info}, @litchar{,clear}, @litchar{,quit}, @litchar{,help}.  A
leading comma reads as an @racket[unquote], which is valid only inside a
@racket[quasiquote] — never as a top-level form — so a command can never
be mistaken for ordinary input; a bare @litchar{,} is an accepted no-op.

Tab completion, multi-line input, and history are all supported.  See
@secref["repl" #:doc '(lib "rackton/scribblings/reference/rackton-reference.scrbl")]
for the full command list.

@section{Where to next}

This guide proceeds top-down: each chapter builds on the previous.

@itemlist[
@item{@secref["interfaces"] — the three ways to embed Rackton in your
      Racket projects.}
@item{@secref["values-and-types"] — primitive types, type inference,
      and the type signature form.}
@item{@secref["pattern-matching"] — destructuring with @racket[match].}
@item{@secref["adts-and-records"] — your own data types.}
@item{@secref["type-classes"] — protocols and instances.}]

For exhaustive signatures of every binding the prelude ships, see the
@other-doc['(lib "rackton/scribblings/reference/rackton-reference.scrbl")].
