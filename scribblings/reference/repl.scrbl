#lang scribble/manual
@require[scribble/manual
         (for-label rackton
                    (only-in racket/base input-port? string? boolean?)
                    (only-in racket/contract any/c listof))]

@title[#:tag "repl"]{The REPL}

@defmodule[rackton/repl]

Rackton ships an interactive REPL that reuses the same elaboration
pipeline as the @racket[rackton] macro, slicing it one form at a time
and carrying inference state between calls.

Run it from a shell with:

@commandline{racket -l rackton/repl}

The REPL accepts any Rackton expression or top-level form
(@racket[define], @racket[define-data], @racket[define-class], etc.)
and prints the resulting binding's type after each input.  It also
recognises a handful of meta-commands, each starting with a colon.

@section{Commands}

@deftogether[(@defidform[#:kind "REPL command" :type]
              @defidform[#:kind "REPL command" :t])]{

@racket[(:type @#,racket[_expr])] (or @racket[(:t @#,racket[_expr])])
shows the inferred type of @racket[_expr] without evaluating it.
REPL commands are written as parenthesised forms so they can flow
through the same reader as ordinary input.

@verbatim|{
λ> (:type (lambda (x) (Cons x Nil)))
(lambda (x) (Cons x Nil)) :: (All (a) (-> a (List a)))
}|}

@deftogether[(@defidform[#:kind "REPL command" :info]
              @defidform[#:kind "REPL command" :i])]{

@racket[(:info @#,racket[_name])] (or @racket[(:i @#,racket[_name])])
prints what @racket[_name] is bound to in the current environment: a
value scheme, a data constructor, a type constructor, or a class.}

@deftogether[(@defidform[#:kind "REPL command" :quit]
              @defidform[#:kind "REPL command" :q])]{

@racket[(:quit)] (or @racket[(:q)]) exits the REPL.}

@deftogether[(@defidform[#:kind "REPL command" :help]
              @defidform[#:kind "REPL command" :h])]{

@racket[(:help)] (or @racket[(:h)]) prints the command list.}

@section{Programmatic interface}

The REPL is implemented as a state machine so it can be driven by
tests as easily as by stdin.  These bindings are re-exported from
@racketmodname[rackton/repl]:

@defproc[(rackton-repl-run) any/c]{

Boots the REPL on the current input/output ports.  This is the
function invoked when @racketmodname[rackton/repl] is run as
@exec{racket -l rackton/repl}.}

@defproc[(rackton-repl-init) rackton-repl-state?]{

Returns a fresh REPL state with the prelude environment installed and
all elaboration tables empty.}

@defproc[(rackton-repl-step [state rackton-repl-state?]
                            [input any/c])
         (values rackton-repl-state? string?)]{

Processes one input form against @racket[state], returning the next
state and the formatted output string.  @racket[input] may be a
Rackton expression, a Rackton top-level form, or one of the meta
commands above.}

@defproc[(rackton-repl-state? [v any/c]) boolean?]{

Predicate recognising REPL-state values.}

@defproc[(rackton-read-form [port input-port?]) any/c]{

Reads a single Rackton form from @racket[port], handling multi-line
input.  Returns @racket[eof] on end of input.}

@defproc[(rackton-repl-completions [state rackton-repl-state?]
                                   [prefix string?])
         (listof string?)]{

Returns the list of bindings in @racket[state]'s environment whose
names begin with @racket[prefix].  Used for tab completion.}

@defproc[(rackton-repl-quit? [state rackton-repl-state?]) boolean?]{

Returns @racket[#t] if @racket[state] is a quit-requested state (i.e.
@racket[:quit] or @racket[:q] was processed).}

@section{Inspection}

The optimisation-log accessors @racket[rackton-monomorphized-sites]
and @racket[rackton-inlined-sites] are exported from
@racketmodname[rackton] itself rather than @racketmodname[rackton/repl];
see @secref["inspection"] for their signatures.
