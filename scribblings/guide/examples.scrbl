#lang scribble/manual
@require[scribble/manual
         (for-label rackton
                    rackton/text/string
                    rackton/data/map
                    rackton/data/list
                    rackton/system)
         "../rackton-eval.rkt"]
@(define ev (make-rackton-eval))

@title[#:tag "examples"]{Worked examples}

Four complete programs live in @filepath{examples/}.  All are written
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

@section{expr-compiler.rkt — a type-safe compiler}

@filepath{examples/expr-compiler.rkt} compiles a small integer
expression language (@racket[Add], @racket[Sub], @racket[Mul]) to a
stack machine whose instructions are typed by the shape of the operand
stack — so the compiler can only emit stack-balanced code.  It is the
worked version of the @seclink["promoted-data"]{promoted data} and GADT
sections of @secref["advanced-types"].

@itemlist[
@item{@bold{GADTs}: @racket[Code] is indexed by an input and an output
      stack shape; each instruction constructor pins how it moves the
      stack.}
@item{@bold{Promoted data (DataKinds)}: the stack shape is a type-level
      list built from the promoted datatypes @racket[Ty] and
      @racket[Stack], so an ill-formed shape is a kind error.}
@item{@bold{Kind inference}: @racket[Code]'s kind
      (@racket[(-> Stack Stack *)]) is inferred from the constructors —
      no kind annotation is written.}
@item{@bold{Defunctionalised continuations}: each instruction threads
      the rest of the program, so @racket[compile] is a direct
      continuation-passing translation.}
@item{@bold{Polymorphic recursion}: @racket[compile] calls itself at
      different stack shapes, admitted because its type is declared.}]

Run it:

@commandline{racket examples/expr-compiler.rkt}

It prints each result followed by the compiled instruction listing:

@codeblock|{
compiling expressions to a typed stack machine:
(2 + 3) * 4     = 20
    push 2
    push 3
    add
    push 4
    mul

10 - (2 * 3)    = 4
    push 10
    push 2
    push 3
    mul
    sub

(7 - 2)*(1 + 4) = 25
    push 7
    push 2
    sub
    push 1
    push 4
    add
    mul

done.
}|

@section{Other source to learn from}

@itemlist[
@item{@filepath{tests/} — every feature has a feature-named test file.
      These are the most precise specification of what works and how.}
@item{@filepath{private/prelude.rkt} — the prelude is itself a Rackton
      program, defining most of the protocol hierarchy and the standard
      ADTs.  Reading it is one of the fastest ways to internalise the
      idioms.}]
