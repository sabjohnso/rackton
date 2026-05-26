#lang scribble/manual
@require[scribble/manual
         (for-label rackton)]

@title[#:tag "quickstart"]{Quickstart}

@section{Install and run}

Rackton is a Racket package.  Install it once with:

@commandline{raco pkg install --link --auto}

from the package's working directory.  After install, the
@racketidfont{#%module-begin} hooks and @hash-lang[]
@racketmodfont{rackton} reader are both available.

@section{Hello, world}

Save the following as @filepath{hello.rkt}:

@codeblock|{
#lang rackton

(define main (println "hello, world"))
(define _    (run-io main))
}|

Run it with @exec{racket hello.rkt}.  @racket[println] is typed
@racket[(-> String (IO Unit))] — it doesn't perform IO when called; it
returns an @racket[IO] action.  @racket[run-io] is the bridge that
executes the action in the surrounding Racket runtime.

@section{Your first typed function}

@codeblock|{
#lang rackton

(provide fact)

(: fact (-> Integer Integer))
(define (fact n)
  (if (= n 0) 1 (* n (fact (- n 1)))))
}|

The @racket[:] form is the type signature.  The @racket[define] is
checked against it.  Drop the signature and Rackton infers
@racket[(All (a) (-> Integer Integer))] from the body — the integer
literals and arithmetic fix the parameter and result.

@section{The REPL}

Launch the REPL with:

@commandline{racket -l rackton/repl}

@verbatim|{
rackton> (+ 1 2)
3 :: Integer
rackton> :type (lambda (x) (Cons x Nil))
(lambda (x) (Cons x Nil)) :: (∀ a) (-> a (List a))
rackton> :quit
}|

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
@item{@secref["classes"] — type classes and instances.}]

For exhaustive signatures of every binding the prelude ships, see the
@other-doc['(lib "rackton/scribblings/reference/rackton-reference.scrbl")].
