#lang scribble/manual
@require[scribble/manual
         (for-label (except-in rackton apply) rackton/incremental rackton/mono rackton/temporal)
         "../rackton-eval.rkt"]
@(define ev (make-rackton-eval))

@title[#:tag "stdlib-incremental"]{@tt{rackton/incremental} — monotone × temporal}
@defmodule[rackton/incremental]

The bridge between @racketmodname[rackton/mono] and
@racketmodname[rackton/temporal]: a guarded stream whose every tick is a
monotone least fixpoint — incremental / differential dataflow.  Neither core
module depends on the other; this is the only bridge.  Not in the
auto-prelude; @racket[require] it explicitly.

@defproc[(scan-mono [step (-> s in (Mono s s))] [seed s] [ins (Signal in)]) (Signal s)]{
  A guarded scan (needs @racket[(BoundedJoinSemilattice s)]).  At each tick,
  @racket[step] turns the previous output and the current input into a
  monotone endomap; the new output is its @racket[mono-fix], which is then
  threaded forward as the next @racket[seed].

  Two well-founded guarantees compose, with no overlap:
  @itemlist[
    @item{@bold{Across ticks}, the stream is productive — the @racket[Signal]'s
          tail is a @racket[Later], so @racket[sig-take] forces one tick at a
          time.}
    @item{@bold{Within a tick}, the value is a @racket[mono-fix], which
          stabilizes by the ascending-chain condition on a finite (or ACC)
          lattice — so every @racket[adv] yields its value FINITELY, by
          theorem, not by trust.}]

  The canonical use is incremental @tech{monotonicity}: a stream of inputs
  arriving over time, each folded into a running state whose closure is a
  monotone fixpoint — e.g. a transitive closure recomputed as edges arrive
  (see @tt{examples/incremental-dataflow.rkt}).
}

@defproc[(scan-mono-diff [step (-> s in (Mono s s))] [seed s] [ins (Signal in)]) (Signal s)]{
  The @bold{differential} variant (needs @racket[(Eq s)]): resume each tick's
  fixpoint from the PREVIOUS output (via @racket[mono-fix-from]) instead of
  from ⊥, doing incremental rather than from-scratch work.  Produces the same
  stream as @racket[scan-mono] WHEN the per-tick maps grow monotonically
  (each tick's map dominates the last) — which holds for accumulating
  dataflow, where the previous output is below the new least fixpoint.  The
  initial @racket[seed] must be a valid lower bound (e.g. ⊥).
}

@section{Example — a latch}

A minimal instance over the shipped @racket[Boolean] lattice: the new value
is @racket[(lub prev input)] (a constant map's fixpoint), so the output
latches to @racket[#t] once an input is @racket[#t] and stays there.

@rackton-example[#:eval ev #:mode 'defs #:context? #t]{
(require rackton/mono rackton/temporal rackton/incremental)
;; inputs: #f, #t, #f, #f, …
(define bools (SigCons #f (next (SigCons #t (next (sig-repeat #f))))))
(define latch (scan-mono (lambda (p i) (mono-const (lub p i))) #f bools))
}

@rackton-example[#:eval ev #:mode 'value #:context? #t]{
(sig-take 4 latch)
}
