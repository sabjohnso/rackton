#lang scribble/manual
@require[scribble/manual
         (for-label rackton)]

@title[#:tag "monomorphization-and-inlining"]{Monomorphization and inlining}

Class-method calls go through a runtime dispatcher by default
(see @secref["codegen"]).  Two optimisation passes recover most of
the cost for monomorphic call sites:

@itemlist[
@item{@bold{Monomorphization} — at each call site where the
      dispatch argument's type is concrete, the inferer resolves the
      instance at compile time and codegen emits a direct call to the
      per-instance impl, bypassing the runtime dispatcher.}
@item{@bold{Inlining} — for sufficiently small per-instance impl
      bodies, the codegen substitutes the body in place of the call,
      skipping the function-call overhead entirely.}]

Both passes log their actions to parameters that user code can
inspect via @racket[rackton-monomorphized-sites] and
@racket[rackton-inlined-sites].

@section{Monomorphization}

The inferer runs a post-pass over the typed-core AST after constraint
reduction.  For each class-method call site, if the dispatch argument
has a concrete type @racket[T] (no free variables, no skolems), the
inferer:

@itemlist[#:style 'ordered
@item{Looks up the instance for the call's class at @racket[T].}
@item{Records the resolution in @racket[current-method-resolutions]:
      @racket[((call-site-id) → (impl-name))].}
@item{If the instance is qualified (e.g.\ @racket[(Monad m) =>
      (Monad (ExceptT e m))]), the inferer also resolves the inner
      constraints recursively, building a list of "dict args" — the
      per-instance impls of the inner classes' return-typed methods.
      This list is stored in @racket[current-method-dict-resolutions].}]

Codegen consults the resolution table when lowering each call.
A resolved call gets a direct call to the impl name; an unresolved
call falls back to the dispatcher.

@bold{What this is NOT:}  the call is still a function call.  The
inferer does not specialise the @italic{caller} — a polymorphic
function used at @racket[Integer] doesn't get a separate
Integer-specialised compilation.  The monomorphization is purely
local to each call site.

@section{Inlining}

After codegen has lowered each call to either a dispatch call or a
direct impl call, a second pass examines each direct call:

@itemlist[#:style 'ordered
@item{Look up the impl's body in @racket[current-inlinable-bodies].}
@item{If the body is "small" (a few primitives, no recursion, no
      complex control flow), substitute it in place of the call.
      Otherwise, leave the call.}
@item{Record the inlining in @racket[current-inlined-sites].}]

The size heuristic is conservative: function calls, simple
arithmetic, and pattern matches on small ADTs are inlinable; loops
and recursion are not.  The point is to remove the call overhead for
hot operations like @racket[+], not to perform aggressive program
specialisation.

@section{Why the logs?}

Tests can assert which call sites the inferer monomorphized and which
the codegen inlined, pinning the optimisation behaviour as part of the
language's contract.  Without the logs, an optimisation regression
would silently degrade performance without breaking any tests.

@racket[tests/monomorphization-test.rkt] and
@racket[tests/impl-inlining-test.rkt] are the corresponding behavioural
tests.

@section{Limitations}

@itemlist[
@item{Polymorphic-monad code (e.g., functions with a
      @racket[(Monad m) =>] context) cannot be monomorphized — the
      dispatch argument's type is variable.  The dispatcher handles
      the call at runtime.}
@item{Return-typed methods (@racket[pure], @racket[mempty]) are
      @italic{always} compile-time resolved — they have no value-typed
      argument and so cannot use the runtime dispatcher.  The
      elaborator either succeeds in resolving them or reports an
      ambiguity error.}
@item{Needs-dict instance bodies (methods whose impl needs an inner
      class's impl) can be monomorphized only when the inner class is
      itself resolved at compile time.  When the inner class is
      polymorphic, the call falls back to the runtime dispatcher
      that consults the @racket[pure-via-witness] machinery (see
      @filepath{private/prelude-runtime.rkt}).}]
