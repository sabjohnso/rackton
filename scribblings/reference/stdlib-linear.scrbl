#lang scribble/manual
@require[scribble/manual
         (for-label (except-in rackton apply) rackton/linear)
         "../rackton-eval.rkt"]
@(define ev (make-rackton-eval))

@title[#:tag "stdlib-linear"]{@tt{rackton/linear} — substructural arrows}
@defmodule[rackton/linear]

A @deftech{substructural} (Freyd-style) arrow tower: a separate sibling of
the shipped @racket[Arrow] hierarchy that reuses the prelude's
@racket[Category] and adds a monoidal product, so morphisms can run in
parallel and — crucially — so that COPY and DISCARD become @emph{optional}
capabilities rather than baked in.  An arrow that withholds them is
@emph{linear}: it can route and transform wires but never duplicate or drop
one, enforced by which protocols it implements.

The monoidal product is a @bold{data family} (not a type family or a fundep
parameter): a data family is generative, so the unifier recovers the
category index from a tensor type structurally — which is what makes the
nullary structural operations (@racket[braid], @racket[dup], @racket[discard])
writable.  Not in the auto-prelude; @racket[require] it explicitly.

@section{The tensor and the tower}

@defidform[#:kind "type" Ten]{
  The monoidal product, a data family: @racket[(Ten cat a b)] is the product
  of @racket[a] and @racket[b] in arrow @racket[cat]'s structure.  Each
  concrete arrow supplies its own representation with a @racket[data-instance].
}

@defproc[(par [f (cat a b)] [g (cat c d)]) (cat (Ten cat a c) (Ten cat b d))]{
  The @racket[Tensored] method (requires @racket[Category]): run @racket[f]
  and @racket[g] on the two components independently (Haskell's @tt{f *** g}).
}

@deftogether[(
  @defproc[(par-first  [f (cat a b)]) (cat (Ten cat a c) (Ten cat b c))]
  @defproc[(par-second [f (cat a b)]) (cat (Ten cat c a) (Ten cat c b))]
)]{
  Derived: act on one component, leaving the other alone
  (@racket[(par f ident)] and @racket[(par ident f)]).
}

@defthing[braid (cat (Ten cat a b) (Ten cat b a))]{
  The @racket[Symmetric] method (requires @racket[Tensored]): cross the two
  components.  Involutive, and natural with respect to @racket[par].
}

@defthing[dup (cat a (Ten cat a a))]{
  The @racket[Copyable] method (requires @racket[Symmetric]): the comonoid
  comultiplication — duplicate a wire (the diagonal).  A LINEAR arrow does
  not provide it.
}

@defthing[discard (cat a Unit)]{
  The @racket[Discardable] method (requires @racket[Symmetric]): the counit —
  drop a wire to the monoidal unit @racket[Unit].  A LINEAR arrow does not
  provide it.
}

@defidform[#:kind "constraint" Cartesian]{
  The constraint synonym @racket[(Cartesian cat)] = @racket[(Symmetric cat)]
  + @racket[(Copyable cat)] + @racket[(Discardable cat)] — an arrow with the
  full cartesian structure.
}

@section{Concrete arrows}

@deftogether[(
  @defidform[#:kind "type" Lin]
  @defproc[(lin [f (-> a b)]) (Lin a b)]
  @defproc[(run-lin [l (Lin a b)]) (-> a b)]
  @defproc[(at [l (Lin a b)] [x a]) b]
)]{
  @racket[Lin] is the @emph{linear} arrow: @racket[Category] + @racket[Tensored]
  + @racket[Symmetric], and DELIBERATELY no @racket[Copyable] /
  @racket[Discardable].  So @racket[dup] / @racket[discard] at type
  @racket[Lin] is a type error — you cannot copy or drop a linear wire.
  @racket[lin] builds one from a function, @racket[at] applies it.
}

@deftogether[(
  @defidform[#:kind "type" Fn]
  @defproc[(fn [f (-> a b)]) (Fn a b)]
  @defproc[(run-fn [h (Fn a b)]) (-> a b)]
  @defproc[(at-fn [h (Fn a b)] [x a]) b]
)]{
  @racket[Fn] is the @emph{cartesian} function arrow: every capability,
  including @racket[dup] and @racket[discard].  Same runtime shape as
  @racket[Lin]; the difference is purely which protocols it implements.
}

@section{Example — a linear circuit}

@racket[braid] crosses two wires and @racket[par] transforms them side by
side, with no copying or dropping:

@rackton-example[#:eval ev #:mode 'defs #:context? #t]{
(require rackton/linear)
(define inc (lin (lambda (n) (+ n 1))))
(define dbl (lin (lambda (n) (* n 2))))
;; braid >>> (inc *** dbl): cross the wires, then transform each
(define circuit (comp (par inc dbl) braid))
(define (pr t) (match t [(LinTen a b) (Pair a b)]))
}

On the pair @tt{(3, 5)} the wires cross to @tt{(5, 3)}, then @tt{inc} and
@tt{dbl} give:

@rackton-example[#:eval ev #:mode 'value #:context? #t]{
(pr (at circuit (LinTen 3 5)))
}
