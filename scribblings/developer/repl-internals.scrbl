#lang scribble/manual
@require[scribble/manual
         (for-label rackton)]

@title[#:tag "repl-internals"]{REPL internals}

@filepath{private/repl.rkt} is a state-machine kernel that reuses
the same elaboration pipeline as the @racket[rackton] macro, slicing
it one form at a time and carrying inference state between calls.

@section{State}

@racket[rackton-repl-state] wraps every piece of mutable infrastructure
that the inference pipeline expects to find through parameters:

@itemlist[
@item{@racket[env] — the typing environment, threaded between forms.}
@item{@racket[declared] — names with a pending @racket[:] signature
      awaiting a body.}
@item{@racket[fresh-box] / @racket[preds-box] — boxes for the
      per-program fresh-counter and pending-pred bag.}
@item{@racket[method-uses] / @racket[method-resolutions] /
      @racket[method-dict-resolutions] / @racket[needs-dict-defs] /
      @racket[instance-default-bodies] — hashes for codegen consumption.}
@item{@racket[nsp] — the live Racket namespace that executes
      compiled code.}
@item{@racket[expr-counter] — a counter used to generate fresh names
      for naked-expression entries (so each gets a stable name to
      reference if needed).}
@item{@racket[quit?] — flag set by @racket[:quit] / @racket[:q].}]

@section{The three-way dispatch}

Each call to @racket[rackton-repl-step] dispatches by input shape:

@itemlist[#:style 'ordered
@item{If the input starts with a known REPL command (@racket[:type],
      @racket[:info], @racket[:quit], @racket[:help] or their
      shortforms), invoke @racket[handle-command].}
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
@racket[data-ctors], @racket[tcons], and @racket[classes] tables,
returning every binding name that starts with the given prefix.  The
launcher in @filepath{repl.rkt} wires this into Racket's
@racket[readline]-style completer.

@section{Reading multi-line input}

@racket[rackton-read-form] reads a single Rackton form from a port.
When a partial form is parsed but more input is needed (the
parenthesis balance is non-zero), the reader prompts with @tt{..>}
and continues reading from the next line.  This is what lets the
REPL accept multi-line @racket[(define (f x) …)] forms naturally.

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
