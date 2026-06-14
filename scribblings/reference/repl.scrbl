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
recognises a handful of meta-commands, each starting with a comma.

@section{The structural editor}

When standard input and output are a terminal, the REPL reads input
through its own structural (paredit-style) editor.  Its contract is
paredit's: @emph{the entry stays balanced} — no ordinary keystroke
can break parenthesis structure.

@itemlist[

 @item{@bold{Electric keys} — @litchar{(} and @litchar{[} insert a
       balanced pair and put the cursor inside (inserting themselves
       literally inside strings and comments); @litchar{)} never
       inserts, it moves past the enclosing list's closing delimiter;
       @litchar{"} inserts a string pair, an escaped quote inside a
       string, or steps over the closing quote.  Backspace and
       @litchar{C-d} refuse to delete one half of a non-empty pair
       (an empty pair deletes whole; a delimiter is stepped over
       instead), and @litchar{C-k} kills to the end of the line
       without ever cutting through a delimiter.}

 @item{@bold{Structural commands} — slurp and barf in both
       directions (@litchar{C-right} / @litchar{C-left} /
       @litchar{C-M-left} / @litchar{C-M-right}), splice
       (@litchar{M-s}), raise (@litchar{M-r}), wrap (@litchar{M-(}),
       and s-expression motion (@litchar{C-M-f} / @litchar{C-M-b} /
       @litchar{C-M-u} / @litchar{C-M-d}) — the paredit command set,
       with delimiter characters read from the entry so mixed
       @litchar{()} and @litchar{[]} behave correctly.
       @litchar{,keys} prints the full table.}

 @item{@bold{Whole-form editing} — a multi-line entry is one
       buffer: arrows move anywhere in it.  @litchar{Return} accepts
       when the entry is complete and the cursor is at its end —
       with electric delimiters the text is almost always balanced,
       so position is what distinguishes “done” from “editing
       inside”; anywhere else it opens an indented line.
       @litchar{M-Return} always opens a line; @litchar{M-q}
       reindents the entry.  Pasted text (bracketed paste) is
       inserted verbatim, bypassing the electric keys.}

 @item{@bold{History} — accepted entries persist across sessions in
       @filepath{rackton-history} under @racket[(find-system-path
       'pref-dir)].  @litchar{Up} recalls history when the cursor is
       on the entry's first line (and is line motion below it);
       @litchar{M-p} searches backward for entries starting like the
       text before the cursor.}

 @item{@bold{Completion} — @litchar{Tab} completes the name before
       the cursor from the live session environment — values, data
       constructors, types, protocols — plus the surface keywords
       (@racket[define], @racket[protocol], @racket[match], …).}

 @item{@bold{Syntax coloring} — tokens color via the standard Racket
       lexer (the same lexer that drives the editor's structure).
       Rackton keyword heads, type names, and data constructors each
       color apart from ordinary identifiers — types and constructors
       are told apart by the live session environment, so a name
       starts coloring the moment its definition is accepted.  The
       palette is customizable with @litchar{,colors} and persists in
       the Racket preferences; setting the @envvar{NO_COLOR}
       environment variable disables coloring entirely.}

]

When standard input is not a terminal — pipes, scripts, dumb
terminals — the REPL falls back to plain line-by-line reading with
the same multi-line accumulation as earlier releases, so scripted use
is unchanged.

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
evaluates @racket[_expr] and shows its value and inferred type, the
value rendered the same way a bare expression's result is (data
constructors under their bare names, functions as @racketresultfont{<lambda>}).

@verbatim|{
λ> ,type (lambda (x) (Cons x Nil))
<lambda> :: (All (a) (-> a (List a)))
λ> ,type (Cons 1 Nil)
(Cons 1 Nil) :: (List Integer)
}|}

@deftogether[(@defidform[#:kind "REPL command" info]
              @defidform[#:kind "REPL command" i])]{

@litchar{,info} @racket[_name] (or @litchar{,i} @racket[_name])
prints what @racket[_name] is bound to in the current environment.  A
value or data constructor prints its scheme on one line.  A protocol lists
its parameters, superprotocols, methods (each with its scheme), and known
instances; a type constructor lists its arity, its constructors, and the
protocols it has instances of.  A type declared @racket[#:abstract] is
marked @litchar{sealed}.

@verbatim|{
λ> ,info Monad
Monad (protocol)
  parameters:   m
  superprotocols: (Applicative m)
  methods:
    flatmap :: (All (m a b) ((Monad m) => (-> (-> a (m b)) (-> (m a) (m b)))))
    join :: (All (m a) ((Monad m) => (-> (m (m a)) (m a))))
  instances: (Monad (Either a)) (Monad IO) (Monad Identity) (Monad List) (Monad Maybe)
}|}

@deftogether[(@defidform[#:kind "REPL command" source]
              @defidform[#:kind "REPL command" src])]{

@litchar{,source} @racket[_name] (or @litchar{,src} @racket[_name])
pretty-prints the input form that bound @racket[_name] — the form as
typed into the session, or, for a prelude name, its definition in the
prelude source.  A data constructor shows its @racket[data] form; a
method shows its @racket[protocol]; a protocol shows the protocol
followed by the instances the session knows (a re-evaluated instance
replaces its earlier self).  A name imported from a module has no
recorded source and says so.

@verbatim|{
λ> ,source Maybe
(data (Maybe a) None (Some a))
}|}

@deftogether[(@defidform[#:kind "REPL command" accepts]
              @defidform[#:kind "REPL command" a])]{

@litchar{,accepts} @racket[_type] (or @litchar{,a} @racket[_type])
lists the functions and data constructors in scope that accept an
argument of @racket[_type] — search by argument position, in the
spirit of Hoogle.  A candidate matches when @racket[_type] unifies
with one of its argument positions; argument positions that are
@emph{unconstrained} type variables are excluded (they accept
everything), and a candidate whose protocol constraints can never be
satisfied under the match is dropped.  A @emph{constrained} variable
participates and stands or falls with its constraints:
@litchar{,accepts Integer} lists @racket[+] because @racket[(Num
Integer)] exists, and @litchar{,accepts (List Integer)} lists
@racket[fmap] because @racket[(Functor List)] exists, while a
@racketidfont{MonadWriter}-constrained candidate is not listed for
@racket[List] queries.

@verbatim|{
λ> ,accepts (List Integer)
Cons :: (All (a) (-> a (-> (List a) (List a))))
append :: (All (a) (-> (List a) (-> (List a) (List a))))
…
sum :: (All (t) ((Foldable t) => (-> (t Integer) Integer)))
}|}

@deftogether[(@defidform[#:kind "REPL command" search]
              @defidform[#:kind "REPL command" returns])]{

@litchar{,search} @racket[_signature] searches everything in scope —
session, imports, prelude — by whole signature: a candidate matches
when it has the same arity and the types unify, either with the
arguments in order (listed first) or permuted (listed after).  A
non-arrow query finds values of that type; a string query
(@litchar{,search "fold"}) searches names.  @litchar{,returns}
@racket[_type] matches the (curried) result type instead;
bare-variable results are excluded, and protocol constraints must remain
satisfiable, as with @litchar{,accepts}.

@verbatim|{
λ> ,search (-> (-> x y) (-> (List x) (List y)))
fmap :: (All (f a b) ((Functor f) => (-> (-> a b) (-> (f a) (f b)))))
…
λ> ,returns (List Integer)
filter :: (All (a) (-> (-> a Boolean) (-> (List a) (List a))))
…
}|

The same queries run over the installed standard library from a
shell, with each match's defining module and line:

@commandline{racket -l rackton/search -- "(-> (List a) Integer)"}
@commandline{racket -l rackton/search -- --returns "(List Integer)"}
@commandline{racket -l rackton/search -- --name "stream"}}

@defidform[#:kind "REPL command" complete]{

@litchar{,complete} @racket[_prefix] prints the names that complete
@racket[_prefix] — the session's vars, data constructors, protocols, and
type constructors, plus the surface keywords — one per line, or nothing
when none match.  It is the pipe transport for editor completion: the
structural editor's Tab and a piped client (such as the Emacs inferior
REPL) draw on the same candidates.

@verbatim|{
λ> ,complete ma
mappend
match
max
…
}|}

@defidform[#:kind "REPL command" keys]{

@litchar{,keys} prints the structural editor's key bindings —
generated from the same table that drives key dispatch, so it cannot
drift from the actual behavior.}

@defidform[#:kind "REPL command" colors]{

@litchar{,colors} shows the editor's color scheme: the active scheme
name and each syntactic category (@racketidfont{paren},
@racketidfont{keyword}, @racketidfont{identifier},
@racketidfont{type}, @racketidfont{constructor},
@racketidfont{string}, @racketidfont{literal},
@racketidfont{comment}, @racketidfont{error}) with its color.
@litchar{,colors} @racket[_scheme] switches schemes
(@racketidfont{standard} or @racketidfont{plain}); @litchar{,colors}
@racket[_category] @racket[_color] overrides one category, where
@racket[_color] is one of the sixteen ANSI color names,
@racketidfont{default}, or @racketidfont{none}.  Overrides persist
across sessions (Racket preferences, key
@racketidfont{rackton-colors}) and survive scheme switches.  The
@envvar{NO_COLOR} environment variable overrides everything and
turns coloring off.}

@deftogether[(@defidform[#:kind "REPL command" clear]
              @defidform[#:kind "REPL command" c])]{

@litchar{,clear} (or @litchar{,c}) resets the session to a fresh prelude
environment, discarding every definition, data type, protocol, and instance
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
