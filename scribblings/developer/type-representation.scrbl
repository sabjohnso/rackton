#lang scribble/manual
@require[scribble/manual
         (for-label rackton)]

@title[#:tag "type-representation"]{Type representation}

@filepath{private/types.rkt} defines the type, scheme, and
substitution data structures used throughout the inferer and codegen.
Everything else operates on these.

@section{The type ADT}

@itemlist[
@item{@racket[ty:var] — a type variable, identified by its name
      (symbol) and an optional skolem flag.}
@item{@racket[ty:con] — a type constructor (e.g., @racket[Integer],
      @racket[Maybe]).  Constructors are first-class; their kinds
      come from the environment's @racket[tcons] table.}
@item{@racket[ty:app] — type application (e.g., @racket[(Maybe a)]
      is @racket[(ty:app (ty:con 'Maybe) (list (ty:var 'a)))]).}
@item{@racket[ty:forall] — explicit universal quantification.  Body
      is a @racket[ty:qual] or an ordinary type.}
@item{@racket[ty:qual] — qualified type: a list of constraints
      followed by a body.  The Haskell @racket[(C a) => τ] shape.}]

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
The composition operation is left-biased: @racket[(compose σ₁ σ₂)]
first applies @racket[σ₂], then @racket[σ₁] to the result.

The two key operations:

@itemlist[
@item{@racket[apply-sub] — replaces every @racket[ty:var] in a type
      with its image under the substitution.  Recurses through
      @racket[ty:app] and chases through @racket[ty:qual] and
      @racket[ty:forall] (with care for binding).}
@item{@racket[free-vars] — the set of type-variable names occurring
      in a type, scheme, or environment.}]

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
@item{@bold{GADT refinement.}  Matching on a GADT constructor with a
      @racket[#:returns] clause unifies the scrutinee's type
      parameters against the constructor's return type, often binding
      a parameter to a concrete type.  The bound parameter behaves as
      a skolem within the clause.}]

The skolem flag on @racket[ty:var] discriminates the two purposes; the
unifier treats skolems as rigid (unifiable only with themselves).

@section{Kinds}

Kinds classify types.  Two constructors:

@itemlist[
@item{@racket[k:star] — the kind of ordinary types (@racket[Integer],
      @racket[(Maybe Integer)], etc.).}
@item{@racket[k:arr] — type-constructor kinds.  Written
      @racket[(-> k1 k2)] in source.}]

Kind checking happens during class declaration (every parameter has
a kind, defaulting to @racket[k:star]) and during type-constructor
application (the head's kind must accept the argument's kind).
