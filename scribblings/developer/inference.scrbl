#lang scribble/manual
@require[scribble/manual
         (for-label rackton)]

@title[#:tag "inference"]{Type inference}

@filepath{private/infer.rkt} is the largest file in the codebase
(~4400 lines / ~200 KB).  It implements Algorithm W with skolemization,
GADT refinement, class-constraint collection, and the bookkeeping for
monomorphization and inlining.

@section{Algorithm W in one paragraph}

Walk the expression top-down.  At each node, produce a substitution
and a type.  At @racket[e:var], look up the scheme in the environment
and instantiate it.  At @racket[e:lam], invent a fresh @racket[α] for
the parameter, infer the body with @racket[α] in scope, return
@racket[(-> α body-type)].  At @racket[e:app], infer the function
and argument, unify the function's type with @racket[(-> arg-type
β)] for a fresh @racket[β], return @racket[β].  At @racket[e:let],
infer the right-hand side, generalise against the environment, add
the resulting scheme to the environment, infer the body.

@section{Skolemization for declared signatures}

When a @racket[(: name type)] declaration is in scope for a top-level
@racket[define], the inferer:

@itemlist[#:style 'ordered
@item{Instantiates the declared scheme's quantifiers with skolems.}
@item{Adds @racket[name : (instantiated body)] to the environment.}
@item{Infers the @racket[define]'s body, producing an inferred type.}
@item{Unifies the inferred type with the skolemised body.}
@item{Generalises the result, restoring the original scheme.}]

Skolems prevent the body from accidentally specialising a type
parameter.  If the body uses @racket[+] on an @racket[a] parameter,
the skolem-flavoured @racket[a] won't unify with @racket[Integer], and
the inferer rejects the program with "no Num instance for a".

@section{GADT refinement}

A constructor whose signature ends in @racket[(T τ₁ … τₙ)] (declared
via @racket[(Ctor : (-> field … @#,racket[(T τ₁ … τₙ)]))]) constrains
the scrutinee's type parameters to @racket[τ₁ … τₙ] when pattern-matched.
At each match clause, the inferer:

@itemlist[#:style 'ordered
@item{Instantiates the constructor's universally-quantified existentials
      with fresh skolems.}
@item{Unifies the constructor's declared return type with the scrutinee's
      type, producing a refining substitution.}
@item{Applies the refining substitution to the environment for the
      duration of the clause body.}
@item{Inverts the refinement at clause exit — any type variable
      escaping the clause that was bound by the refinement gets
      reported as a leaked skolem.}]

This is what makes a typed interpreter possible: the clause for
@racket[IntLit] knows the result is @racket[Integer], the clause for
@racket[BoolLit] knows it's @racket[Boolean], even though both have
the same scrutinee type @racket[(Term a)].

@section{Class-constraint collection and reduction}

The inferer collects @racket[qual] constraints from instantiated
schemes and accumulates them in a pending-pred bag (a box-of-list
parameter, @racket[current-pending-preds]).  At each generalisation
point — @racket[let], @racket[define], top-level — the inferer:

@itemlist[#:style 'ordered
@item{Reduces the pending constraints against the environment's
      instance table (see @secref["class-entailment"]).  Each
      constraint either resolves to a concrete instance or remains
      as a residual variable-headed constraint.}
@item{Splits the residual constraints into those mentioning only
      free-in-the-environment variables (kept, propagated outward)
      and those mentioning only free-in-this-binding variables
      (added to the binding's scheme as a context).}]

This is essentially the constraint-reduction phase of Mark Jones's
@italic{Typing Haskell in Haskell} (1999); Rackton's twist is that
the dispatch is by runtime type tag rather than by dictionary
passing — see @secref["codegen"] for how that compiles.

@section{Communicating with codegen}

Several @racket[parameterize]d boxes carry per-elaboration state
between the inferer and the codegen:

@itemlist[
@item{@racket[current-method-resolutions] — for each call-site
      identifier, the per-instance impl name resolved at compile time
      (e.g., @racket[pure] at @racket[Maybe] →
      @racketidfont{$pure:Maybe}).}
@item{@racket[current-method-dict-resolutions] — for each call site
      requiring dictionary arguments, the list of resolved dict
      values in declaration order.}
@item{@racket[current-needs-dict-defs] — definitions that themselves
      take dict arguments because their body uses methods of a
      needs-dict instance.  Populated by both the declared-signature
      path and the inferred path: when a non-declared @racket[define]
      has a lambda RHS whose generalized scheme picks up a
      return-typed-bearing constraint over a quantified tvar (e.g.
      @racket[(define (madd mx my) … (pure (+ x y)))] inferring
      @racket[(Monad m) (Num a) => …]), inference allocates a skolem
      tcon for each such tvar, calls @racket[build-dict-skolems] to
      derive the dict-arg names, retroactively records
      @racket['dict] entries on recursive @racket[e:var] references
      to the def's own name, and runs @racket[resolve-method-uses!]
      with the skolem map in scope.}
@item{@racket[current-monomorphized-sites], @racket[current-inlinable-bodies],
      and @racket[current-inlined-sites] — the monomorphization/inlining
      logs (declared in @filepath{private/monomorph-log.rkt} and
      re-exported through @filepath{infer.rkt}), accessible to user code
      via @racket[rackton-monomorphized-sites] and
      @racket[rackton-inlined-sites].}]

The codegen consults these tables when lowering each AST node; the
inferer is the sole writer.  See @secref["monomorphization-and-inlining"]
for the details of the optimisation passes.

@section{Error messages}

When unification fails, the inferer reports the two types it tried to
unify along with the source location of the offending form.  When an
instance can't be resolved, it reports the class and the type, along
with a "did you mean" suggestion that searches the visible instance
table for Levenshtein-close type names.  When a binding is unbound,
the inferer searches the entire environment for a near-match and
suggests it.

These messages are produced as @racket[exn:fail:syntax] at compile
time; ill-typed Rackton code never reaches the generated Racket
runtime.
