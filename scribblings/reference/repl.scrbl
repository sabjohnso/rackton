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
(@racket[define], @racket[data], @racket[protocol], etc.)
and prints the resulting binding's type after each input.  It also
recognises a handful of meta-commands, each starting with a colon.

@section{Embedding in the host Racket REPL}

Requiring @racketmodname[rackton/repl] from a running @exec{racket} REPL
switches that REPL into Rackton mode — subsequent forms are evaluated as
Rackton and printed as @racketresultfont{value :: Type}, much like
@racketmodfont{typed/racket}:

@verbatim|{
> (require rackton/repl)
> (define (sqr x) (* x x))
> (sqr 5)
25 :: Integer
> ,quit
; returned to the Racket reader — (rackton-repl-enter!) to resume
> (+ 1 2)
3
}|

It works by replacing @racket[current-read-interaction] — the procedure a
live REPL uses to read each interaction — so it is inert in scripts, in
@racket[eval], and during module loading (none of those read
interactions).  @litchar{,quit} (or @litchar{,q}) restores the plain
Racket reader.  The session state lives on, so calling
@racket[(rackton-repl-enter!)] afterwards @emph{resumes} the same session
— every earlier definition is still bound.  Use
@racket[(rackton-repl-reset!)] for a clean slate instead.  The underlying
functions @racket[rackton-repl-enter!] and @racket[rackton-repl-exit!]
are also exported.

@section{Error formatting and terminal width}

Type-error messages wrap long types to fit the window.  At a REPL the
wrap width tracks the terminal: it is taken from the @envvar{COLUMNS}
environment variable, or from @exec{stty size} when a real terminal is
attached, falling back to a fixed 79-column default.

This applies to both the Rackton REPL (@exec{racket -l rackton/repl},
which re-checks each prompt so a mid-session resize is honored) and the
host Racket REPL — typing @racket[(rackton _form ...)] at a
@exec{racket} prompt adapts too.  Adaptation happens only for
interactive (@racket['top-level]) evaluation; batch compilation, a
@hash-lang[] @racketmodfont{rackton} module, DrRacket, and test runs keep
the fixed default, so error text stays reproducible outside the REPL.

When Racket runs without a real terminal — for example a
Geiser/@racket[racket-mode] REPL, which talks to Racket over a pipe —
@exec{stty} cannot probe a width.  Set @envvar{COLUMNS} in that case (it
is read wherever the error is formatted) to get wrapped output.

@section{Commands}

REPL commands are typed with a leading comma — @litchar{,type},
@litchar{,quit}, and so on.  A comma reads as an @racket[unquote], which
is never a valid Rackton form, so a command can never be mistaken for
ordinary input.  A bare @litchar{,} on its own is an accepted no-op.

@deftogether[(@defidform[#:kind "REPL command" type]
              @defidform[#:kind "REPL command" t])]{

@litchar{,type} @racket[_expr] (or @litchar{,t} @racket[_expr])
shows the inferred type of @racket[_expr] without evaluating it.

@verbatim|{
λ> ,type (lambda (x) (Cons x Nil))
(lambda (x) (Cons x Nil)) :: (All (a) (-> a (List a)))
}|}

@deftogether[(@defidform[#:kind "REPL command" info]
              @defidform[#:kind "REPL command" i])]{

@litchar{,info} @racket[_name] (or @litchar{,i} @racket[_name])
prints what @racket[_name] is bound to in the current environment: a
value scheme, a data constructor, a type constructor, or a class.}

@deftogether[(@defidform[#:kind "REPL command" clear]
              @defidform[#:kind "REPL command" c])]{

@litchar{,clear} (or @litchar{,c}) resets the session to a fresh prelude
environment, discarding every definition, data type, class, and instance
made since the session began.}

@deftogether[(@defidform[#:kind "REPL command" quit]
              @defidform[#:kind "REPL command" q])]{

@litchar{,quit} (or @litchar{,q}) exits the REPL.}

@deftogether[(@defidform[#:kind "REPL command" help]
              @defidform[#:kind "REPL command" h])]{

@litchar{,help} (or @litchar{,h}) prints the command list.}

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

@defproc[(rackton-parse-command-line [str string?]) any/c]{

Parses one line of input into a command datum, or @racket[#f] when the
line is not a command.  A leading comma marks a command: the rest of the
line is read as the command word and its arguments.  For example
@litchar{,type (lambda (x) x)} parses to
@racket[(list 'unquote 'type '(lambda (x) x))], and a bare @litchar{,}
parses to @racket[(list 'unquote)], the no-op.}

@defproc[(rackton-repl-completions [state rackton-repl-state?]
                                   [prefix string?])
         (listof string?)]{

Returns the list of bindings in @racket[state]'s environment whose
names begin with @racket[prefix].  Used for tab completion.}

@defproc[(rackton-repl-quit? [state rackton-repl-state?]) boolean?]{

Returns @racket[#t] if @racket[state] is a quit-requested state (i.e.
@litchar{,quit} or @litchar{,q} was processed).}

@section{Inspection}

The optimisation-log accessors @racket[rackton-monomorphized-sites]
and @racket[rackton-inlined-sites] are exported from
@racketmodname[rackton] itself rather than @racketmodname[rackton/repl];
see @secref["inspection"] for their signatures.
