#lang scribble/manual
@require[scribble/manual
         (for-label (except-in rackton apply) rackton/effects)
         "../rackton-eval.rkt"]
@(define ev (make-rackton-eval))

@title[#:tag "stdlib-effects"]{@tt{rackton/effects} — typed algebraic effects}
@defmodule[rackton/effects]

Algebraic effects as an @deftech{indexed} (graded) monad.  A computation has
type @racket[(Eff row a)] — it produces an @racket[a] while using the effects
recorded in @racket[row], a phantom type index tracked entirely at compile
time.  @racket[ebind] @emph{unions} the rows of the two computations it
sequences, so the type accumulates every effect used; handlers @emph{discharge}
a label; and @racket[run-eff] runs only a computation whose row is EMPTY — so
a forgotten handler is a type error.  Not in the auto-prelude; @racket[require]
it explicitly.

The row is a @bold{product of presence flags} — one slot per effect, each
@racket[Present] or @racket[Absent] — so @racket[Union] is componentwise and
the encoding scales by adding a slot.  v1 fixes the alphabet to
@racket[Except] (String errors) and @racket[Writer] (a String log).

A graded monad's @racket[ebind] changes the row type, so @racket[Eff] is
@bold{not} a @racket[Monad] instance and @racket[do]-notation does not apply —
chain @racket[ebind] explicitly.

@section{The effect row}

@deftogether[(
  @defidform[#:kind "type" Present]
  @defidform[#:kind "type" Absent]
  @defidform[#:kind "type" EffRow]
)]{
  A row is @racket[(EffRow ex wr)] with one presence flag per effect:
  @racket[ex] for Except, @racket[wr] for Writer, each @racket[Present] or
  @racket[Absent].  The empty row is @racket[(EffRow Absent Absent)].
}

@section{The indexed monad}

@defidform[#:kind "type" Eff]{
  @racket[(Eff row a)] — a computation using the effects in @racket[row],
  producing an @racket[a].  Sealed: the only way to obtain one is the
  operations below, so the row is always an honest record of the effects used.
}

@deftogether[(
  @defproc[(epure [x a]) (Eff (EffRow Absent Absent) a)]
  @defproc[(ebind [e (Eff r a)] [k (-> a (Eff s b))]) (Eff (Union r s) b)]
)]{
  Inject a pure value (empty row), and sequence — @racket[ebind] runs
  @racket[e], passes its result to @racket[k], and @emph{unions} the rows.
}

@deftogether[(
  @defproc[(throw [msg String]) (Eff (EffRow Present Absent) a)]
  @defproc[(tell [s String]) (Eff (EffRow Absent Present) Unit)]
)]{
  The effect operations: @racket[throw] raises an error (the Except effect);
  @racket[tell] appends to the log (the Writer effect).
}

@deftogether[(
  @defproc[(with-except [e (Eff (EffRow Absent wr) a)]) (Eff (EffRow Present wr) a)]
  @defproc[(with-writer [e (Eff (EffRow ex Absent) a)]) (Eff (EffRow ex Present) a)]
)]{
  Widen a row by adding one unused effect (sound — the row is an upper bound).
  Use it to make the two branches of an @racket[if] agree, e.g. a
  @racket[throw] branch and a pure branch.
}

@section{Handlers and run}

@deftogether[(
  @defproc[(handle-except [e (Eff r a)]) (Eff (DropExcept r) (Either String a))]
  @defproc[(handle-writer [e (Eff r a)]) (Eff (DropWriter r) (Pair a (List String)))]
)]{
  Discharge a label.  @racket[handle-except] makes the result total — it can
  no longer throw, and the @racket[Either] becomes the value.
  @racket[handle-writer] moves the accumulated log into the value.  Each
  clears its slot in the row.
}

@defproc[(run-eff [e (Eff (EffRow Absent Absent) a)]) a]{
  Run a fully-handled computation.  Gated on the EMPTY row — running a
  computation with an unhandled effect is a type error.
}

@section{Example — log and may-fail}

A checked division that logs each step and throws on a zero divisor; the row
records both effects, the handlers discharge them, and @racket[run-eff] runs
the result.

@rackton-example[#:eval ev #:mode 'defs #:context? #t]{
(require rackton/effects)

(: checked-div (-> Integer Integer (Eff (EffRow Present Present) Integer)))
(define (checked-div a b)
  (ebind (tell "div")
         (lambda (u)
           (if (== b 0)
               (throw "division by zero")
               (with-except (epure (div a b)))))))

;; 100 / 5 / 0  — throws on the second step
(: prog (Eff (EffRow Present Present) Integer))
(define prog (ebind (checked-div 100 5) (lambda (h) (checked-div h 0))))
(: run-prog (-> (Eff (EffRow Present Present) Integer)
                (Pair (Either String Integer) (List String))))
(define (run-prog p) (run-eff (handle-writer (handle-except p))))
}

The result pairs the outcome (an @racket[Either]) with the log; the failure
is caught, not thrown:

@rackton-example[#:eval ev #:mode 'value #:context? #t]{
(match (run-prog prog) [(Pair r log) r])
}
