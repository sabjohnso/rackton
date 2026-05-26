#lang scribble/manual
@require[scribble/manual
         (for-label rackton)]

@title[#:tag "class-entailment"]{Class entailment}

@filepath{private/entail.rkt} answers the question: given a set of
hypotheses (the constraints in scope) and a candidate constraint, is
the candidate entailed?  And if so, by which instance?

@section{The entailment problem}

A typing judgement carries qualified types of the form
@racket[(C₁ … Cₙ) => τ].  When a polymorphic value is used at a
specific type, each constraint must either:

@itemlist[
@item{@bold{Resolve} to a registered instance, picking up that
      instance's own qualifiers as new constraints; or}
@item{@bold{Survive} as a residual constraint, propagated outward to
      the enclosing definition's scheme; or}
@item{@bold{Fail} — no instance and no enclosing context to absorb
      it, in which case the program is rejected.}]

@section{Instance lookup}

The environment maintains an @racket[instance-table] keyed by class
name, mapping to a list of registered instances.  Each instance
carries:

@itemlist[
@item{Its qualifier context (@racket[((Eq a) => …)]).}
@item{Its head (@racket[(Eq (Maybe a))]).}
@item{The defining module's identifier so codegen can resolve method
      calls back to the right per-instance impl.}]

To resolve a candidate constraint:

@itemlist[#:style 'ordered
@item{Look up the class in @racket[instance-table].}
@item{For each registered instance, attempt to unify the instance's
      head with the candidate constraint.  Skolem variables in the
      candidate constraint may unify with concrete types in the
      instance head (this is how @racket[(Eq (Maybe Integer))]
      resolves through the @racket[(Eq a) => (Eq (Maybe a))]
      instance).}
@item{If unification succeeds, apply the resulting substitution to
      the instance's context, adding those constraints to the
      pending-pred bag for further resolution.}
@item{Record the resolution in @racket[current-method-resolutions] so
      codegen can route the call.}]

@section{Overlap checks}

Two instances overlap when their heads can be unified — both would
resolve a constraint that matches the common type.  When
@racket[define-instance] adds an instance to the table, the entail
module checks for overlap with every existing instance for the same
class:

@itemlist[
@item{@bold{Disjoint heads} — no overlap, accept.}
@item{@bold{One head is strictly more specific} — accept, with the
      more specific one selected when both match.}
@item{@bold{Equivalent heads} — error (duplicate instance).}
@item{@bold{Genuinely overlapping but neither strictly more specific}
      — accept iff the call site can disambiguate via type
      ascription, otherwise an ambiguity error.}]

This is the same rule set GHC uses with @tt{OverlappingInstances};
the implementation is direct and small.

@section{Functional dependencies and consistency}

When a class declares @racket[#:fundep a -> b], the entail module
maintains the invariant that no two instances disagree about
@racket[b] for the same @racket[a].  Adding a new instance triggers
a check against every existing instance — if the new @racket[a]
matches an existing one but the @racket[b]s differ, the new instance
is rejected as inconsistent with the fundep.

The fundep also feeds back into resolution: at a call site where
@racket[a] is known but @racket[b] is ambiguous, the resolver looks
up @racket[a] in the fundep witness map and uses the discovered
@racket[b] to disambiguate.

@section{Module-level coherence}

Instances always escape their defining module regardless of
@racket[provide].  When a module is loaded (via @racket[require]),
its instances are merged into the importing module's instance table.
The overlap and fundep checks run on the merged table, so a coherent
program cannot import two libraries that each define
mutually-conflicting instances for the same class/type pair.

This is the Haskell tradition.  The alternative — module-local
instances — is rejected because it allows different parts of a
program to disagree about which instance to use for a given pair,
which silently breaks any data type whose representation depends on
the instance (e.g., sets keyed by @racket[Eq]).

@section{Why not dictionary passing?}

A classical implementation of type classes (Wadler & Blott, 1989)
inserts a dictionary parameter at every constrained function, and
each method call indexes into the dictionary.

Rackton instead uses @italic{first-argument runtime tag dispatch}:
every class method becomes a generic function that examines its
first value argument's struct tag and indexes into a hash table.
The dictionary never appears at the calling convention level.

The tradeoffs:

@itemlist[
@item{@bold{Pro: no dictionary plumbing.}  Polymorphic functions
      compile to ordinary Racket functions; no extra parameter, no
      eta-expansion overhead, no need to inline dictionaries for
      performance.}
@item{@bold{Pro: matches Racket's generic-function tradition.}  The
      generated code reads like ordinary Racket multimethod code.}
@item{@bold{Con: dispatch happens at runtime.}  This is a
      fixed-cost hash lookup per method call.  The
      monomorphization pass (see
      @secref["monomorphization-and-inlining"]) recovers most of
      this for concrete call sites by resolving the dispatch at
      compile time and emitting a direct call to the per-instance
      impl.}
@item{@bold{Con: return-typed methods need special handling.}
      @racket[pure] has no value-typed argument, so first-argument
      dispatch can't reach it.  Return-typed methods are resolved
      entirely at compile time via the elaborator's expected-type
      machinery, and the codegen emits a direct call to the
      per-instance impl name (@racketidfont{$pure:Maybe}, etc.).  Needs-dict
      instances (e.g., @racket[ExceptT] over an arbitrary @racket[m])
      then thread the inner @racket[pure] as a leading argument.}]
