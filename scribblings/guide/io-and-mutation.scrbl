#lang scribble/manual
@require[scribble/manual
         (for-label rackton)]

@title[#:tag "io-and-mutation"]{IO, refs, and files}

Side effects in Rackton happen inside the @racket[IO] monad.
@racket[IO] is declared with no public constructors — values can only
be built by primitives.  Sequencing uses @racket[do] / @racket[flatmap]
just like any other @racket[Monad].

@section{Hello, IO}

@codeblock|{
(: greet (-> String (IO Unit)))
(define (greet name)
  (println (string-append "hello, " name)))

(define main (greet "world"))
(define _    (run-io main))
}|

@racket[println] returns an @racket[IO Unit] action.  It does
@italic{not} perform IO when called — it just builds an action.
@racket[run-io] is the bridge that hands the action to the surrounding
Racket runtime to execute.

@section{Standard streams}

@itemlist[
@item{@racket[print]      — @racket[(-> String (IO Unit))]}
@item{@racket[println]    — @racket[(-> String (IO Unit))] (adds a newline)}
@item{@racket[read-line]  — @racket[(IO String)]}
@item{@racket[pure-io]    — @racket[(-> a (IO a))] (equivalent to @racket[pure])}
@item{@racket[run-io]     — @racket[(-> (IO a) a)] (executes)}]

@section{Mutable references}

A @racket[(Ref a)] is an opaque mutable cell.  All operations are IO
actions:

@codeblock|{
(: counter-up (-> (Ref Integer) (IO Integer)))
(define (counter-up r)
  (do [n <- (read-ref r)]
      (write-ref r (+ n 1))
    (read-ref r)))
}|

The primitives are @racket[make-ref], @racket[read-ref], and
@racket[write-ref].  Because they're in @racket[IO], the type system
prevents accidental observation of mutation outside an @racket[IO]
context.

@section{File I/O}

@codeblock|{
(: read-greeting (-> String (IO String)))
(define (read-greeting path)
  (do [body <- (read-file path)]
    (pure-io (string-append "from file: " body))))
}|

The primitives are @racket[read-file], @racket[write-file], and
@racket[file-exists?].  Each is @racket[IO]-typed; the file system is
treated as an opaque source of effects.

@section{Panic and structured error recovery}

@racket[panic] terminates the program with an unrecoverable error.
Its return type is universally quantified, so it can stand in any
branch:

@codeblock|{
(: pick-positive (-> Integer Integer))
(define (pick-positive n)
  (if (< n 0) (panic "negative not allowed") n))
}|

For recoverable failures inside @racket[IO], @racket[try] catches any
exception and delivers the result as a @racket[Result]:

@codeblock|{
(: safe-read (-> String (IO (Result String String))))
(define (safe-read path)
  (try (read-file path)))

(match (run-io (safe-read "missing.txt"))
  [(Ok body) body]
  [(Err msg) "<file missing>"])
}|

@racket[raise-io] is the typed counterpart that fails an @racket[IO]
action with a message; it pairs naturally with @racket[try].

@section{System interface}

The prelude wraps a small set of common system operations as
@racket[IO] actions: @racket[random-integer], @racket[random-float],
@racket[current-time-seconds], @racket[list-directory],
@racket[getenv], @racket[argv], @racket[delete-file],
@racket[make-directory].  Each is documented in the
@secref["system" #:doc '(lib "rackton/scribblings/reference/rackton-reference.scrbl")]
section of the reference.

@section{A complete IO-driven program}

@codeblock|{
#lang rackton

(: prompt-and-greet (IO Unit))
(define prompt-and-greet
  (do (print "name? ")
      [name <- read-line]
    (println (string-append "hi, " name))))

(define _ (run-io prompt-and-greet))
}|
