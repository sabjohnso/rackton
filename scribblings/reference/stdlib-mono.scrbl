#lang scribble/manual
@require[scribble/manual
         (for-label (except-in rackton apply) rackton/mono rackton/unit rackton/data/set)
         "../rackton-eval.rkt"]
@(define ev (make-rackton-eval))

@title[#:tag "stdlib-mono"]{@tt{rackton/mono} — monotonicity}
@defmodule[rackton/mono]

Datafun-style @deftech{monotonicity} as a library: the order-theory
protocols a monotone computation needs, a @emph{sealed} type of monotone
maps whose combinators make every constructible map monotone by
construction, and a least-fixpoint operator — the typed-Datalog / dataflow
payoff.  Not in the auto-prelude; @racket[require] it explicitly.

The discipline rides the value's type and a closed combinator algebra
rather than the typing context, so ordinary Hindley–Milner inference
carries it with no special checker support.

@section{Order-theory protocols}

@defproc[(leq [x a] [y a]) Boolean]{
  The @racket[Poset] method: a partial order on @racket[a] (@racket[Poset]
  requires @racket[Eq]).  Its @racket[#:laws] are reflexivity,
  antisymmetry, and transitivity.  The runnable bundle is
  @racket[Poset-laws].
}

@defproc[(lub [x a] [y a]) a]{
  The @racket[JoinSemilattice] method: the least upper bound (join) of
  @racket[x] and @racket[y].  Laws: commutative, associative, idempotent,
  and consistent with the order (@racket[(leq x (lub x y))]).  Bundle:
  @racket[JoinSemilattice-laws].
}

@defthing[bot a]{
  The @racket[BoundedJoinSemilattice] method: the least element, the
  identity of @racket[lub] and the seed for @racket[mono-fix].  It is
  return-typed (like @racket[mempty]); resolve it by the expected type or
  by an annotation.
}

@deftogether[(
  @defthing[Poset-laws (-> (Gen a) Test)]
  @defthing[JoinSemilattice-laws (-> (Gen a) Test)]
)]{
  Runnable property bundles auto-generated from each protocol's
  @racket[#:laws]; apply to a generator to get a @racket[Test] that checks
  an instance against every law.  (@racket[BoundedJoinSemilattice]'s only
  law uses the return-typed @racket[bot], which the bundle generator skips,
  so it has no bundle — its law is still checked at definition.)

  @racket[Boolean] ships as the canonical two-point lattice
  (@racket[lub] is @racket[or], @racket[bot] is @racket[#f]).
}

@section{The monotone arrow}

@defidform[#:kind "type" Mono]{
  A monotone map @racket[(Mono a b)].  Its constructor is @emph{sealed}
  (@racket[#:abstract]), so the only way to build one is the combinators
  below — each a monotone map that also preserves its arguments'
  monotonicity, so every @racket[Mono] you can construct is monotone.
}

@deftogether[(
  @defproc[(run-mono [m (Mono a b)]) (-> a b)]
  @defproc[(app-mono [m (Mono a b)] [x a]) b]
)]{
  The underlying function of @racket[m], and its application to @racket[x].
}

@deftogether[(
  @defthing[mono-id (Mono a a)]
  @defproc[(mono-comp [g (Mono b c)] [f (Mono a b)]) (Mono a c)]
  @defproc[(mono-const [y b]) (Mono a b)]
  @defthing[mono-fst (Mono (Pair a b) a)]
  @defthing[mono-snd (Mono (Pair a b) b)]
  @defproc[(mono-pair [f (Mono a b)] [g (Mono a c)]) (Mono a (Pair b c))]
  @defproc[(mono-join [f (Mono a s)] [g (Mono a s)]) (Mono a s)]
)]{
  The closed set of monotone-map formers: identity, composition, constants,
  the product projections and pairing, and the pointwise join
  (@racket[mono-join] needs @racket[(JoinSemilattice s)]).
}

@defproc[(unsafe-mono [f (-> a b)]) (Mono a b)]{
  The one trusted escape: wrap an arbitrary function as a @racket[Mono].
  @bold{Unsafe} — the caller asserts @racket[f] is monotone; the type
  system does not check it.  Use it only to lift a genuinely-monotone
  primitive the closed algebra cannot express, and keep each use visible.
}

@section{Least fixpoint}

@defproc[(mono-fix [f (Mono a a)]) a]{
  The least fixpoint of @racket[f], by Kleene iteration from @racket[bot]
  (needs @racket[(BoundedJoinSemilattice a)]).  Sound as the @emph{least}
  fixpoint because @racket[f] is monotone by construction; it assumes the
  carrier satisfies the ascending-chain condition (e.g. is finite) so the
  chain stabilizes.
}

@defproc[(mono-fix/fuel [fuel Integer] [f (Mono a a)]) (Maybe a)]{
  As @racket[mono-fix], but bounded: @racket[(Some r)] if the fixpoint is
  reached within @racket[fuel] steps, else @racket[None].  Use it on a
  carrier whose ascending-chain condition is not guaranteed.
}

@section{Example — transitive closure as a least fixpoint}

A one-rule Datalog program: the transitive closure of a graph is the least
relation @racket[R] with @racket[R = base ⊔ compose(base, R)].  The step is
built from the closed combinators plus one trusted leaf (relational
composition), so it is monotone, and @racket[mono-fix] runs it to its least
fixed point.

@rackton-example[#:eval ev #:mode 'defs #:context? #t]{
(require rackton/mono rackton/data/set)

(data Rel (MkRel (Set (Pair Integer Integer))))
(define (edges r) (match r [(MkRel s) s]))

(instance (Eq Rel)
  (define (== a b)
    (if (set-subset? (edges a) (edges b)) (set-subset? (edges b) (edges a)) #f)))
(instance (Poset Rel)               (define (leq a b) (set-subset? (edges a) (edges b))))
(instance (JoinSemilattice Rel)     (define (lub a b) (MkRel (set-union (edges a) (edges b)))))
(instance (BoundedJoinSemilattice Rel) (define bot (MkRel empty-set)))

(define base (MkRel (set-from-list (list (Pair 1 2) (Pair 2 3) (Pair 3 4)))))

(define (compose-rel b r)
  (MkRel (set-from-list
          (foldr (lambda (e1 acc)
                   (match e1 [(Pair x y)
                     (foldr (lambda (e2 acc2)
                              (match e2 [(Pair y2 z)
                                (if (== y y2) (Cons (Pair x z) acc2) acc2)]))
                            acc (set-to-list (edges r)))]))
                 Nil (set-to-list (edges b))))))

(define step (mono-join (mono-const base) (unsafe-mono (compose-rel base))))
(define tc (mono-fix step))
}

The closure of @tt{1→2→3→4} adds the reachability edges @tt{1→3, 2→4,
1→4}, for six in all:

@rackton-example[#:eval ev #:mode 'value #:context? #t]{
(set-size (edges tc))
}
