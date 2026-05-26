#lang scribble/manual
@require[scribble/manual
         (for-label rackton)]

@title[#:tag "examples"]{Worked examples}

Two complete programs live in @filepath{examples/}.  Both are written
in idiomatic Rackton and exercise most of the features covered in
this guide.

@section{calc.rkt — a typed expression interpreter}

@filepath{examples/calc.rkt} is a small expression-language
interpreter.  It exercises:

@itemlist[
@item{Recursive ADTs: @racket[Sexpr] (input syntax) and @racket[Expr]
      (typed AST) parameterised by the result type.}
@item{Pattern matching with nested constructors.}
@item{@racket[(Map String Integer)] as a typing environment.}
@item{@racket[Result] for parse errors with structured messages.}
@item{@racket[IO] with @racket[read-line] / @racket[println] for the
      REPL loop.}
@item{Mutual recursion between @racket[parse-expr],
      @racket[parse-list], and @racket[parse-form], declared via
      @racket[:] signatures before any of the bodies.}
@item{The @racket[racket] escape to call into Racket's @racket[read]
      for tokenizing the input.}]

Run it:

@commandline{racket examples/calc.rkt}

@codeblock|{
calc> (+ 1 2)
3
calc> (let x 5 (* x x))
25
calc> (let x 3 (let y 4 (+ x (* y y))))
19
}|

Open the file and read it top-to-bottom — the structure mirrors a
small textbook on writing an interpreter in a typed FP language.

@section{todo.rkt — a CLI todo manager}

@filepath{examples/todo.rkt} is a command-line todo list backed by a
text file.  It exercises:

@itemlist[
@item{@racket[IO] for file I/O via @racket[read-file] /
      @racket[write-file] / @racket[file-exists?].}
@item{@racket[try] for graceful handling of missing files.}
@item{@racket[String] manipulation through
      @racket[string-split] / @racket[string-join] / @racket[string-prefix?].}
@item{@racket[List] processing with @racket[map] (via @racket[fmap]),
      @racket[filter], and @racket[reverse].}
@item{Pattern matching on @racket[argv] to dispatch by subcommand.}
@item{Module-level @racket[main] action driven by @racket[run-io].}]

Run it:

@commandline{racket examples/todo.rkt list}
@commandline{racket examples/todo.rkt add "buy milk"}
@commandline{racket examples/todo.rkt done 1}

Both examples are short enough to read in a sitting — they double as
working code and as worked exercises in style.

@section{Other source to learn from}

@itemlist[
@item{@filepath{tests/} — every feature has a feature-named test file.
      These are the most precise specification of what works and how.}
@item{@filepath{private/prelude.rkt} — the prelude is itself a Rackton
      program, defining most of the class hierarchy and the standard
      ADTs.  Reading it is one of the fastest ways to internalise the
      idioms.}]
