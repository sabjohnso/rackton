#lang scribble/manual
@require[scribble/manual
         (for-label rackton)]

@title[#:tag "examples"]{Worked examples}

Three complete programs live in @filepath{examples/}.  All are written
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
      @racket[write-file].}
@item{@racket[try] for graceful handling of a missing file (rather
      than a pre-flight @racket[file-exists?] check).}
@item{@racket[String] manipulation through
      @racket[string-split] / @racket[string-join] / @racket[string-prefix?].}
@item{@racket[List] processing with @racket[map] (via @racket[fmap]),
      @racket[filter], and @racket[snoc] (append on the right).}
@item{Pattern matching on @racket[argv] to dispatch by subcommand.}
@item{Module-level @racket[main] action driven by @racket[run-io].}]

Run it:

@commandline{racket examples/todo.rkt list}
@commandline{racket examples/todo.rkt add "buy milk"}
@commandline{racket examples/todo.rkt done 1}

Both examples are short enough to read in a sitting — they double as
working code and as worked exercises in style.

@section{word-count.rkt — the standard library across families}

@filepath{examples/word-count.rkt} counts word frequencies and prints
the five most common.  Where @filepath{calc.rkt} and
@filepath{todo.rkt} lean on the language core, this one is about the
@seclink["stdlib"
         #:doc '(lib "rackton/scribblings/reference/rackton-reference.scrbl")]{standard
library}: it pulls several families
together through the @racketmodname[rackton/batteries] umbrella.

@itemlist[
@item{@racketmodname[rackton/text/string]: @racket[words] and
      @racket[to-lower-string] to tokenise, @racket[pad-left] and
      @racket[unlines] to format.}
@item{@racketmodname[rackton/data/map]: @racket[map-insert-with] to
      accumulate counts and @racket[map-to-list] to read them back.}
@item{@racketmodname[rackton/data/list]: @racket[sort-on] and
      @racket[take] to pick the top entries.}
@item{@racket[fmap] / @racket[foldr] over @racket[List], and
      @racket[do]-notation to sequence the @racket[IO].}
@item{@racketmodname[rackton/system]: @racket[argv] and
      @racket[read-file] for input.}]

Run it (built-in sample, or a file):

@commandline{racket examples/word-count.rkt}
@commandline{racket examples/word-count.rkt some-file.txt}

@codeblock|{
    5  the
    2  fox
    2  dog
    1  end
    1  sleeps
}|

@section{Other source to learn from}

@itemlist[
@item{@filepath{tests/} — every feature has a feature-named test file.
      These are the most precise specification of what works and how.}
@item{@filepath{private/prelude.rkt} — the prelude is itself a Rackton
      program, defining most of the class hierarchy and the standard
      ADTs.  Reading it is one of the fastest ways to internalise the
      idioms.}]
