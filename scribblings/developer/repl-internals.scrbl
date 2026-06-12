#lang scribble/manual
@require[scribble/manual
         (for-label rackton)]

@title[#:tag "repl-internals"]{REPL internals}

@filepath{private/repl.rkt} is a state-machine kernel that reuses
the same elaboration pipeline as the @racket[rackton] macro, slicing
it one form at a time and carrying inference state between calls.
Around it sit six single-purpose modules:

@itemlist[
@item{@filepath{private/repl-input.rkt} — reading: comma-command
      parsing, the line-accumulating @racket[rackton-read-form], the
      editor's entry reader and accept test, and history
      persistence.  No dependency on the kernel.}
@item{@filepath{private/repl-entry.rkt} — the structural editor's
      buffer model: an immutable (text, point) entry, a tokenizer
      backed by the standard Racket lexer (the analogue of paredit's
      syntax table — char literals and parens inside strings or
      comments are never mistaken for delimiters), context queries,
      and pure s-expression motion.}
@item{@filepath{private/repl-paredit.rkt} — the paredit commands,
      ported from paredit.el: the electric layer (@litchar{(},
      @litchar{)}, @litchar{"}, deletion, structural kill) and the
      transforms (slurp/barf both ways, splice, raise, wrap).  Every
      command is a pure entry → entry function that preserves
      balance — pinned by a rackcheck property over every command
      from every cursor position.}
@item{@filepath{private/repl-term.rkt} — the terminal shell: a pure
      key decoder, a pure layout engine, a pure editor state machine
      (history, completion, accept), and a thin imperative loop
      (stty raw mode, bracketed paste, ANSI repaint).  Its keymap is
      one declarative table that generates both dispatch and the
      @litchar{,keys} text.}
@item{@filepath{private/repl-source.rkt} — @litchar{,source}: records,
      per bound name, the input form that bound it (names taken from
      the parsed top-form AST), and lazily indexes the prelude the
      same way.}
@item{@filepath{private/repl-search.rkt} — @litchar{,accepts}:
      argument-position type search by one-shot unification, with a
      conservative constraint-satisfiability filter.}]

@section{State}

@racket[rackton-repl-state] carries everything the pipeline needs
between inputs:

@itemlist[
@item{@racket[env] — the typing environment, threaded between forms.}
@item{@racket[declared] — names with a pending @racket[:] signature
      awaiting a body.}
@item{@racket[sources] — name → the input form(s) that bound it, for
      @litchar{,source} (see @filepath{repl-source.rkt}).}
@item{@racket[infer-st] — the immutable inference state (fresh
      counter, pending preds, and the resolution tables codegen
      consumes), threaded through @racket[infer-program/phases].}
@item{@racket[nsp] — the live Racket namespace that executes
      compiled code.}
@item{@racket[expr-counter] — a counter used to generate fresh names
      for naked-expression entries (so each gets a stable name to
      reference if needed).}
@item{@racket[macros] — the names this session has bound as user
      macros; their transformers live in @racket[nsp].}
@item{@racket[quit?] — flag set by @litchar{,quit} / @litchar{,q}.}]

@section{The three-way dispatch}

Each call to @racket[rackton-repl-step] dispatches by input shape:

@itemlist[#:style 'ordered
@item{If the input is a comma command — it reads as @racket[(unquote
      _word ...)], e.g. @litchar{,type}, @litchar{,info},
      @litchar{,source}, @litchar{,accepts}, @litchar{,keys},
      @litchar{,clear}, @litchar{,quit}, @litchar{,help} or their
      shortforms — invoke @racket[handle-command].}
@item{Else if the input is a top-level form (@racket[define],
      @racket[data], @racket[newtype],
      @racket[struct], @racket[protocol],
      @racket[instance], @racket[define-alias], @racket[:],
      @racket[require]), invoke @racket[handle-top-input].}
@item{Else invoke @racket[handle-expr-input], which wraps the input
      in a fresh @racket[(define $repl-N …)] so the same machinery
      that handles top-level @racket[define] can run, then evaluates
      the resulting binding for display.}]

This three-way split keeps the kernel orthogonal to the input syntax
— the same code paths run for tests as for stdin, and for stdin as
for any future remote-REPL frontend.

@section{Tab completion}

@racket[rackton-repl-completions] walks the env's @racket[vars],
@racket[data-ctors], @racket[tcons], and @racket[classes] tables plus
the surface keywords, returning every name that starts with the given
prefix.  The structural editor calls it directly (the REPL loop hands
it a closure over the live session state); the plain line loop wires
the same function into Racket's @racket[readline]-style completer.

@section{The two input layers}

@racket[rackton-repl-run] picks an input layer at startup:
@racket[rackton-term-open] succeeds when stdin/stdout are a terminal,
and the structural-editor loop runs; otherwise the plain loop reads
line by line.  Both feed the same kernel.

In the editor, an entry is accepted as a whole:
@racket[rackton-editor-ready?] decides when @litchar{Return} may
accept (every datum reads to eof; comma commands are recognised; a
malformed-but-closed entry accepts so the kernel reports the error) —
the editor additionally requires the cursor at the entry's end — and
@racket[rackton-editor-read-datum] converts the accepted text to one
datum, routing comma lines through
@racket[rackton-parse-command-line].

In the plain loop, @racket[rackton-read-form] reads a single Rackton
form from a port.  When a partial form is parsed but more input is
needed (the parenthesis balance is non-zero), the reader prompts with
@tt{..>} and continues reading from the next line.  This is what lets
the REPL accept multi-line @racket[(define (f x) …)] forms naturally.

@section{Error handling}

Every dispatch path wraps the inner work in
@racket[(with-handlers ([exn:fail? …]) …)] that formats the exception
as an error string and returns it as the step's output.  The state
is not advanced on error — the user can retry with corrected input.

This keeps the REPL stable in the face of any compile-time or
runtime error from user code.

@section{Why a state machine?}

The REPL kernel could have been implemented as a loop — read,
elaborate, eval, print, repeat.  A state machine instead lets:

@itemlist[
@item{Tests drive the REPL programmatically, asserting on the output
      of each step.}
@item{A future GUI or remote frontend reuse the same kernel without
      pulling in stdin handling.}
@item{The launcher (@filepath{repl.rkt}) be a thin, easily-replaced
      wrapper.}]

The cost is a small amount of plumbing (passing @racket[state] and
returning the new state) that disappears behind the kernel's
interface.
