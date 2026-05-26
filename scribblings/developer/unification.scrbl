#lang scribble/manual
@require[scribble/manual
         (for-label rackton)]

@title[#:tag "unification"]{Unification}

@filepath{private/unify.rkt} implements Robinson's first-order
unification algorithm over Rackton's type AST.  It is the only
component allowed to invent new bindings of type variables to types.

@section{Algorithm}

Given two types @racket[τ₁] and @racket[τ₂], unification produces
either a substitution @racket[σ] such that
@racket[(apply-sub σ τ₁) = (apply-sub σ τ₂)], or an error.  The
recursive rules:

@itemlist[
@item{@bold{Variable vs.\ anything.}  If @racket[τ₁] is a non-skolem
      @racket[ty:var] @racket[α], bind @racket[α] to @racket[τ₂]
      after an occurs check.}
@item{@bold{Anything vs.\ variable.}  Symmetric.}
@item{@bold{Constructor vs.\ constructor.}  If both are @racket[ty:con]
      with the same name, success with the empty substitution.
      Different names: error.}
@item{@bold{Application vs.\ application.}  Unify the heads; apply the
      resulting substitution to both argument lists; unify
      element-wise.}
@item{@bold{Skolem vs.\ anything else.}  Error — skolems unify only
      with themselves.}]

@section{The occurs check}

Before binding @racket[α] to @racket[τ], the unifier verifies that
@racket[α] does not occur in @racket[τ].  Without this, infinite
types like @racket[α = (List α)] would be admitted, and the codegen
would produce non-terminating dispatch logic.

@section{Error reporting}

A unification failure produces a value containing both types and the
position in the recursion where the mismatch arose.  The inferer
catches the failure and decorates it with the source location of the
offending form before re-raising.

@section{Why first-order?}

Rackton has higher-rank polymorphism but @italic{not} higher-order
unification.  That is: @racket[α] cannot be a function-typed
variable that ranges over functions; it ranges only over types in
the strict sense.  This keeps unification decidable and unique.  The
rank-N restriction (explicit @racket[All] inside parameter types) is
exactly the cost of avoiding higher-order unification.

@section{What about constraints?}

@racket[ty:qual] constraints are NOT unified by this module.  The
inferer strips constraints off before calling the unifier; the
constraints are accumulated separately in the pending-pred bag and
resolved against the instance table (see @secref["class-entailment"]).
Mixing the two would obscure both processes.
