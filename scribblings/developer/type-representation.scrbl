#lang scribble/manual
@require[scribble/manual
         (for-label rackton)]

@title[#:tag "type-representation"]{Type representation}

@filepath{private/types.rkt} defines the type, scheme, and
substitution data structures used throughout the inferer and codegen.
Everything else operates on these.

@section{The type ADT}

@itemlist[
@item{@racket[tvar] — a type variable, identified by its name
      (symbol).  Skolemness is tracked separately via the
      @racket[scheme-vars] / inference-time skolem set, not on the
      struct itself.}
@item{@racket[tcon] — a type constructor (e.g., @racket[Integer],
      @racket[Maybe]).  Constructors are first-class; their kinds
      come from the environment's @racket[tcons] table.}
@item{@racket[tapp] — type application (e.g., @racket[(Maybe a)]
      is @racket[(tapp (tcon 'Maybe) (list (tvar 'a)))]).}
@item{@racket[tforall] — explicit universal quantification.  Body
      is a @racket[qual] or an ordinary type.}
@item{@racket[qual] — qualified type: a list of @racket[pred]
      constraints followed by a body.  The Haskell @racket[(C a) => τ]
      shape.}
@item{@racket[pred] — a single class constraint @racket[(C τ ...)],
      with the class name and argument types.}]

The names without the @racket[ty:] prefix are the internal type
representation in @filepath{private/types.rkt}.  The
@racketidfont{ty:}-prefixed structs in @filepath{private/surface.rkt}
are the @italic{source-level} AST nodes built by the surface parser
before inference begins; they are converted into @racket[tvar] /
@racket[tcon] / @racket[tapp] during elaboration.

@section{Schemes}

A scheme is the surface presentation of a polymorphic type: zero or
more bound type variables, zero or more constraints, and a body
type.  Schemes are stored alongside bindings in the environment's
@racket[vars] table.

The two operations on schemes are:

@itemlist[
@item{@bold{Instantiation} — fresh type variables are substituted for
      the scheme's bound variables; the body is returned along with
      the freshly-instantiated constraints.  This is what every use
      site does.}
@item{@bold{Generalisation} — take a type and the set of variables
      free in the surrounding environment; return a scheme that
      quantifies over the type variables free in the type but not
      free in the environment.  This is what @racket[let] and
      @racket[define] do.}]

@section{Substitutions}

A substitution is a finite map from type-variable names to types.
The composition operation is left-biased: @racket[(subst-compose σ₁ σ₂)]
first applies @racket[σ₂], then @racket[σ₁] to the result.

The two key operations:

@itemlist[
@item{@racket[apply-subst] — replaces every @racket[tvar] in a type
      with its image under the substitution.  Recurses through
      @racket[tapp] and chases through @racket[qual] and
      @racket[tforall] (with care for binding).  @racket[apply-subst/scheme]
      is the scheme-aware variant.}
@item{@racket[type-vars] / @racket[scheme-free-vars] / @racket[pred-vars]
      — the free type-variable names of a type, a scheme, and a
      predicate, respectively.}]

Substitutions are the universal currency of unification (see
@secref["unification"]) and inference.

@section{Skolems}

A skolem is a type variable that the type checker treats as a fresh,
distinct, opaque constant.  Skolems appear in two contexts:

@itemlist[
@item{@bold{Declared signatures.}  When checking a binding against a
      @racket[:] declaration, the declared scheme's universally
      quantified variables are skolemised — replaced by fresh
      constants — and the body's inferred type is unified against
      the skolemised body.  This ensures the body actually has the
      promised polymorphic type rather than accidentally specialising
      one of the type parameters.}
@item{@bold{GADT refinement.}  Matching on a GADT constructor whose
      @racket[: (-> field … Result)] signature refines its result
      unifies the scrutinee's type parameters against that result type,
      often binding
      a parameter to a concrete type.  The bound parameter behaves as
      a skolem within the clause.}]

Skolems are represented as @racket[tcon]s with synthetic, freshly-generated
names rather than as a flag on @racket[tvar].  The unifier treats them as
ordinary constructors, so they unify only with themselves — which is exactly
the rigidity skolemisation requires.

@section[#:tag "dev-kinds"]{Kinds}

Kinds classify types.  Two constructors:

@itemlist[
@item{@racket[kind-star] — the kind of ordinary types (@racket[Integer],
      @racket[(Maybe Integer)], etc.).}
@item{@racket[kind-arr] — type-constructor kinds.  Written
      @racket[(-> k1 k2)] in source.}]

Kind checking happens during class declaration (every parameter has
a kind, defaulting to @racket[kind-star]) and during type-constructor
application (the head's kind must accept the argument's kind).
