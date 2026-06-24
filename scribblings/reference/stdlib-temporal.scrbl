#lang scribble/manual
@require[scribble/manual
         (for-label (except-in rackton apply) rackton/temporal)
         "../rackton-eval.rkt"]
@(define ev (make-rackton-eval))

@title[#:tag "stdlib-temporal"]{@tt{rackton/temporal} — guarded streams}
@defmodule[rackton/temporal]

The @tech{later} modality @racket[Later] (written ▷ in the literature) and
the @deftech{guarded} streams it supports.  A @racket[(Later a)] is a value
available "one step from now"; the only way to recurse through it is
@racket[lob], whose type is Löb's theorem (▷ as the provability □) and whose
self-reference sits UNDER a @racket[Later] — so a stream is @emph{productive}
by construction: it always yields its head before deferring its tail.  Not in
the auto-prelude; @racket[require] it explicitly.

@racket[Later] is built on the memoizing @racketmodname[rackton/data/lazy]
@racket[Lazy], so @racket[adv] caches — a stream's prefix is computed once,
not re-derived each step.

@section{The ▷ modality}

@defidform[#:kind "type" Later]{
  @racket[(Later a)] — an @racket[a] available one tick from now.  Sealed;
  build it with @racket[next] / @racket[map-later] / @racket[lob], force it
  one tick with @racket[adv].
}

@deftogether[(
  @defproc[(next [x a]) (Later a)]
  @defproc[(adv [l (Later a)]) a]
  @defproc[(map-later [g (-> a b)] [l (Later a)]) (Later b)]
  @defproc[(map-later2 [g (-> a b c)] [la (Later a)] [lb (Later b)]) (Later c)]
)]{
  @racket[next] delays a value; @racket[adv] advances one tick (memoized);
  @racket[map-later] / @racket[map-later2] are the functor / applicative.
}

@defproc[(lob [f (-> (Later a) a)]) a]{
  The guarded fixpoint: @racket[(lob f)] feeds @racket[f] a @racket[Later]
  that, when advanced, yields @racket[(lob f)] again.  Productive as long as
  @racket[f] does not advance it before producing output.  Its type is Löb's
  theorem — distinct from @racketmodname[rackton/mono]'s inductive
  @racket[mono-fix].
}

@section{Guarded streams}

@deftogether[(
  @defidform[#:kind "type" Signal]
  @defproc[(sig-head [s (Signal a)]) a]
  @defproc[(sig-tail [s (Signal a)]) (Later (Signal a))]
)]{
  @racket[(Signal a)] is a guarded stream, @racket[(SigCons a (Later (Signal a)))]
  — head now, tail later.  Because the tail's type is a @racket[Later], an
  UNGUARDED tail (a forced value where a @racket[Later] is required) is a
  type error: the modality rejects the most common non-productive mistake.
  (@racket[Signal], not @racket[Stream] — @racketmodname[rackton/data/lazy]
  already ships @racket[Stream].)
}

@deftogether[(
  @defproc[(sig-repeat [x a]) (Signal a)]
  @defproc[(sig-iterate [f (-> a a)] [x a]) (Signal a)]
  @defproc[(sig-map [g (-> a b)] [s (Signal a)]) (Signal b)]
  @defproc[(sig-zip [g (-> a b c)] [s1 (Signal a)] [s2 (Signal b)]) (Signal c)]
  @defproc[(sig-take [n Integer] [s (Signal a)]) (List a)]
)]{
  Build a constant signal, or the orbit @tt{x, f x, f (f x), …}; map and zip
  pointwise; and force the first @racket[n] values into a list.
}

@section{Example}

@rackton-example[#:eval ev #:mode 'defs #:context? #t]{
(require rackton/temporal)
;; 0, 1, 2, 3, …  — guarded recursion under map-later
(define nats (sig-iterate (lambda (n) (+ n 1)) 0))
;; Fibonacci: iterate a paired state, then project the first component
(define fibs
  (sig-map (lambda (p) (match p [(Pair a b) a]))
           (sig-iterate (lambda (p) (match p [(Pair a b) (Pair b (+ a b))]))
                        (Pair 0 1))))
}

Both are infinite; @racket[sig-take] forces only the prefix:

@rackton-example[#:eval ev #:mode 'value #:context? #t]{
(sig-take 8 fibs)
}
