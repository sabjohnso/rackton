#lang scribble/manual
@require[scribble/manual
         (for-label rackton)]

@title[#:tag "racket-interop"]{Racket interoperation}

The @racket[racket] escape is the trapdoor that lets Rackton code
reach Racket's standard library beyond what the prelude exposes.

@section{The escape form}

@codeblock|{
(racket Type (var ...) body ...)
}|

Drops into raw Racket and returns a value asserted to have type
@racket[Type].  The named Rackton bindings @racket[var ...] are
spliced into @racket[body] unmodified.  Multiple @racket[body] forms
are wrapped in an implicit @racket[begin].

@codeblock|{
(: greet (-> String String))
(define (greet name)
  (racket String (name)
    (string-append "hello " name)))
}|

Inside @racket[body], @racket[name] resolves to the string the caller
passed.  Identifier resolution inside an escape follows
@filepath{lang/runtime.rkt}'s import list: names the Rackton prelude
shadows (including @racket[string-append], @racket[length],
@racket[+], @racket[map], @racket[filter], @racket[reverse], and many
others — see the @racket[except-in racket/base …] clause in
@filepath{lang/runtime.rkt}) resolve to the prelude's
@italic{binary curried} version, not @racketmodname[racket/base]'s
variadic one.  The example above happens to pass exactly two
arguments, so the curried form's full application succeeds.  If you
need @racketmodname[racket/base]'s variadic
@racket[string-append], reach for @racket[(require racket/base)] in
the surrounding module and use a renamed import inside the escape, or
chain binary calls.

@section{Type assertions are unchecked}

No type-checking is performed on @racket[body].  If the asserted
@racket[Type] is wrong, the program will produce a value of one type
that other Rackton code thinks is a different type — and you'll see
the consequence at runtime, not compile time.  Use the escape
sparingly and verify the asserted type matches what the body actually
produces.

@section{Multi-form bodies}

The body accepts any number of forms; they're wrapped in an implicit
@racket[begin], so inner @racket[(define …)], @racket[(let …)], and
other side-effecting forms work naturally:

@codeblock|{
(: report (-> Integer (IO Unit)))
(define (report n)
  (racket (IO Unit) (n)
    (define msg (format "value is ~a" n))
    (define action ($io (lambda () (displayln msg) Unit)))
    action))
}|

@section{What's in scope}

Inside @racket[body] you can use:

@itemlist[
@item{All of @racketmodname[racket/base] @italic{except} the names
      Rackton's prelude shadows.  Roughly: arithmetic operators,
      comparison operators, several list functions, several string
      functions, IO operations.  See
      @filepath{lang/runtime.rkt}'s @racket[except-in] clause for the
      authoritative list.}
@item{Any binding the surrounding Racket module @racket[require]d.}
@item{The Rackton-named bindings you list in @racket[(var ...)].}]

@section{When to use the escape}

@itemlist[
@item{@bold{Calling a Racket library} not yet wrapped by the prelude
      — e.g., @racket[racket/draw], @racket[net/url], @racket[db].}
@item{@bold{Performance-sensitive inner loops} where the typed
      version would force an allocation the Racket version avoids.}
@item{@bold{Extending the prelude} — many of the prelude's own
      bindings (@racket[string-length], @racket[+], etc.) are defined
      as escape wrappers over their Racket counterparts.}]

In every case, prefer @italic{wrapping} the escape inside a typed
helper rather than scattering escapes throughout call sites — that
way the type assertion is in one place and the rest of the code stays
type-safe.

@section{Calling Rackton from Racket}

The other direction is trivial: any Rackton @racket[define] that's
exported via @racket[provide] is a normal Racket procedure visible
to importing Racket modules.  Constructors and instance methods are
also exported as Racket values; pattern-matching from Racket is
possible via @racket[racket/match] (which works on Rackton's ADT
struct representation).
