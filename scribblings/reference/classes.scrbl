#lang scribble/manual
@require[scribble/manual
         (for-label rackton)]

@title[#:tag "classes"]{Built-in type classes}
@declare-exporting[rackton]

This chapter lists every type class shipped with the Rackton prelude
along with the methods declared by each.  Method signatures are shown
in @racket[defproc] form for readability; in the underlying type system
each method has the curried scheme @racket[(All (a ...) ((C a ...) => τ))]
where @racket[τ] is the curried form of the listed signature.  All
methods are correspondingly callable as partial applications.

@section{Equality and ordering}

@defidform[#:kind "class" Eq]{

Decidable equality.

@deftogether[(
  @defproc[(== [x a] [y a]) Boolean]
  @defproc[(/= [x a] [y a]) Boolean])]{

Equality and disequality.  The default for @racket[/=] negates
@racket[==].}

Built-in instances: @racket[Integer], @racket[Float], @racket[Rational],
@racket[Complex], @racket[Boolean], @racket[String], @racket[Char],
@racket[Bytes].}

@defidform[#:kind "class" Ord]{

Total ordering.  Superclass: @racket[Eq].

@deftogether[(
  @defproc[(< [x a] [y a]) Boolean]
  @defproc[(> [x a] [y a]) Boolean]
  @defproc[(<= [x a] [y a]) Boolean]
  @defproc[(>= [x a] [y a]) Boolean]
  @defproc[(min [x a] [y a]) a]
  @defproc[(max [x a] [y a]) a])]{

Comparison and selection.}

Built-in instances: @racket[Integer], @racket[Float], @racket[Rational],
@racket[String], @racket[Char].}

@section{Numeric hierarchy}

@defidform[#:kind "class" Num]{

Additive and multiplicative arithmetic.

@deftogether[(
  @defproc[(+ [x a] [y a]) a]
  @defproc[(- [x a] [y a]) a]
  @defproc[(* [x a] [y a]) a]
  @defproc[(abs [x a]) a]
  @defproc[(negate [x a]) a])]{

The basic arithmetic operations.}

Built-in instances: @racket[Integer], @racket[Float], @racket[Rational],
@racket[Complex].}

@defidform[#:kind "class" Fractional]{

Reals supporting division.  Superclass: @racket[Num].

@defproc[(float-div [x a] [y a]) a]{

Real division.}

Built-in instances: @racket[Float], @racket[Rational], @racket[Complex].}

@defidform[#:kind "class" Integral]{

Integer-style arithmetic.  Superclass: @racket[Num].

@deftogether[(
  @defproc[(div  [x a] [y a]) a]
  @defproc[(mod  [x a] [y a]) a]
  @defproc[(quot [x a] [y a]) a]
  @defproc[(rem  [x a] [y a]) a])]{

Integer division, modulus, truncating quotient, and remainder.}

Built-in instance: @racket[Integer].}

@defidform[#:kind "class" Real]{

Reals (totally ordered numbers).  Superclasses: @racket[Num],
@racket[Ord].

@defproc[(to-rational [x a]) Rational]{

Lossless conversion to @racket[Rational] when possible.}

Built-in instances: @racket[Integer], @racket[Float], @racket[Rational].}

@defidform[#:kind "class" Floating]{

Reals with transcendental functions.  Superclass: @racket[Fractional].

@deftogether[(
  @defthing[#:kind "method" pi a]
  @defproc[(exp  [x a]) a]
  @defproc[(log  [x a]) a]
  @defproc[(sqrt [x a]) a]
  @defproc[(sin  [x a]) a]
  @defproc[(cos  [x a]) a]
  @defproc[(tan  [x a]) a]
  @defproc[(**   [x a] [y a]) a])]{

@racket[pi] is a return-typed nullary method (resolve at use site with
@racket[ann] if ambiguous).  Built-in instances: @racket[Float],
@racket[Complex].}}

@defidform[#:kind "class" RealFrac]{

Reals with integer-rounding operations.  Superclasses: @racket[Real],
@racket[Fractional].

@deftogether[(
  @defproc[(floor-real    [x a]) Integer]
  @defproc[(ceiling-real  [x a]) Integer]
  @defproc[(round-real    [x a]) Integer]
  @defproc[(truncate-real [x a]) Integer])]{

Convert to @racket[Integer] in four different rounding modes.}

Built-in instances: @racket[Float], @racket[Rational].}

@defidform[#:kind "class" RealFloat]{

Floating-point reals.  Superclasses: @racket[RealFrac],
@racket[Floating].

@deftogether[(
  @defproc[(is-nan?      [x a]) Boolean]
  @defproc[(is-infinite? [x a]) Boolean]
  @defproc[(atan2        [y a] [x a]) a])]{

NaN and infinity tests, plus quadrant-aware arctangent.}

Built-in instance: @racket[Float].}

@section{Display}

@defidform[#:kind "class" Show]{

Conversion to a printable @racket[String].

@defproc[(show [x a]) String]{

Render @racket[x] for display.}

Built-in instances: @racket[Integer], @racket[Float], @racket[Rational],
@racket[Complex], @racket[Boolean], @racket[String], @racket[Char],
@racket[Bytes].  Derived @racket[Show] is available on any
@racket[define-data] via @racket[#:deriving Show].}

@section{Functor hierarchy}

@defidform[#:kind "class" Functor]{

Type constructors @racket[(f :: (-> * *))] supporting a map.

Built-in instances: @racket[Maybe], @racket[List], @racket[Result e],
@racket[IO], @racket[STM], @racket[Identity], @racket[State s],
@racket[Env r], @racket[StateT s m], @racket[EnvT r m],
@racket[WriterT w m], @racket[ExceptT e m].}

@defproc[(fmap [g (-> a b)] [fa (f a)]) (f b)]{

Lift a function under the functor.}

@defidform[#:kind "class" Applicative]{

Functors with a context-introducing @racket[pure] and a way to apply
context-wrapped functions.  Superclass: @racket[Functor].

@deftogether[(
  @defproc[(pure    [a a])                                          (f a)]
  @defproc[(fapply  [fg (f (-> a b))] [fa (f a)])                   (f b)]
  @defproc[(liftA2  [g (-> a (-> b c))] [fa (f a)] [fb (f b)])      (f c)]
  @defproc[(product [fa (f a)] [fb (f b)])                          (f (Pair a b))])]{

@racket[pure] is return-typed (the inferer resolves the @racket[f]
from the call's expected type); use @racket[ann] when ambiguous.

@racket[fapply], @racket[liftA2], and @racket[product] are arranged in
a default cycle (@racket[fapply] ← @racket[product] ← @racket[liftA2]
← @racket[fapply]).  An instance must define at least one; the other
two are derived.  Omitting all three is a compile-time error.}

Built-in instances: all the @racket[Functor] instances above.}

@defidform[#:kind "class" Monad]{

Computations that sequence with binding.  Superclass:
@racket[Applicative].

@deftogether[(
  @defproc[(flatmap [k (-> a (f b))] [m (f a)])  (f b)]
  @defproc[(join    [mma (f (f a))])             (f a)])]{

@racket[flatmap] is monadic bind with the continuation as the first
argument (sugared via @racket[do]); it matches @tt{flip (>>=)} from
Haskell.  @racket[join] collapses one layer of monadic nesting.  An
instance must define at least one of @racket[flatmap] or @racket[join];
the other is derived
(@racket[(flatmap f ma)] = @racket[(join (fmap f ma))],
@racket[(join mma)] = @racket[(flatmap (lambda (m) m) mma)]).  Omitting
both is a compile-time error.}

Built-in instances: all the @racket[Applicative] instances above.
For @racket[STM], compose actions monadically and then run the result
under @racket[atomically].}

@section{Folding and traversal}

@defidform[#:kind "class" Foldable]{

Type constructors that can be folded down to a summary.

@deftogether[(
  @defproc[(foldr   [g (-> a (-> b b))] [z b] [t (t a)]) b]
  @defproc[(length  [t (t a)])                            Integer]
  @defproc[(to-list [t (t a)])                            (List a)])]{

@racket[foldr] is the fundamental operation; @racket[length] and
@racket[to-list] have default implementations derived from it.}

Built-in instances: @racket[List], @racket[Maybe].  Derived via
@racket[#:deriving Foldable] on @racket[define-data].}

@defidform[#:kind "class" Traversable]{

Containers that can be walked applicatively.

@defproc[(traverse [g (-> a (f b))] [t (t a)]) (f (t b))]{

The @racket[f] in the result must satisfy @racket[Applicative].}

Built-in instances: @racket[List], @racket[Maybe].  Derived via
@racket[#:deriving Traversable].}

@defidform[#:kind "class" Bifunctor]{

Type constructors @racket[(p :: (-> * (-> * *)))] supporting a
two-sided map.

@deftogether[(
  @defproc[(bimap  [g (-> a c)] [h (-> b d)] [pab (p a b)]) (p c d)]
  @defproc[(first  [g (-> a c)] [pab (p a b)])               (p c b)]
  @defproc[(second [h (-> b d)] [pab (p a b)])               (p a d)])]{

@racket[first] and @racket[second] have defaults expressed via
@racket[bimap].}

Built-in instances: @racket[Pair], @racket[Result].  Derived via
@racket[#:deriving Bifunctor].}

@section{Semigroup and Monoid}

@defidform[#:kind "class" Semigroup]{

Types with an associative combining operation.

@defproc[(<> [x a] [y a]) a]{

The semigroup operation.}

Built-in instances: @racket[String] (concatenation), @racket[List]
(append), @racket[Sum] (addition), @racket[Product] (multiplication).
Derived via @racket[#:deriving Semigroup].}

@defidform[#:kind "class" Monoid]{

Semigroups with an identity.  Superclass: @racket[Semigroup].

@defthing[#:kind "method" mempty a]{

The identity element.  Return-typed; use @racket[ann] when
ambiguous.}

Built-in instances: @racket[String], @racket[List], @racket[Sum],
@racket[Product].  Derived via @racket[#:deriving Monoid].}

@section{MTL-style monadic classes}

These classes abstract over the state / reader / writer / error
effects so a single function body can run against any transformer
stack offering the effect.

@defidform[#:kind "class" MonadState]{

@racket[(MonadState s m)]: monads supporting access to a single
mutable state of type @racket[s].  Superclass: @racket[Monad m].
Functional dependency: @racket[m -> s].

@deftogether[(
  @defthing[#:kind "method" get-st (m s)]
  @defproc[(put-st [s s])           (m Unit)]
  @defproc[(modify-st [f (-> s s)]) (m Unit)])]{

@racket[get-st] is return-typed.  Built-in instances:
@racket[State s], @racket[StateT s m] for any @racket[Monad m], plus
lifted @racket[StateT] instances through the other transformers.

For working directly with the @racket[State] monad rather than
polymorphically, see also the non-class helpers @racket[get-state],
@racket[put-state], and @racket[modify-state].}}

@defidform[#:kind "class" MonadEnv]{

@racket[(MonadEnv r m)]: monads supporting access to a read-only
environment of type @racket[r].  Superclass: @racket[Monad m].
Functional dependency: @racket[m -> r].

@deftogether[(
  @defthing[#:kind "method" ask-en                       (m r)]
  @defproc[(local-en [f (-> r r)] [k (m a)])             (m a)])]{

@racket[ask-en] is return-typed.  Built-in instances: @racket[Env r],
@racket[EnvT r m] for any @racket[Monad m], plus lifted instances
through the other transformers.

For working directly with the @racket[Env] monad, see the non-class
helpers @racket[ask] and @racket[local].}}

@defidform[#:kind "class" MonadWriter]{

@racket[(MonadWriter w m)]: monads supporting an append-only log of
type @racket[w].  Superclasses: @racket[Monad m], @racket[Monoid w].
Functional dependency: @racket[m -> w].

@deftogether[(
  @defproc[(tell-w [w w])      (m Unit)]
  @defproc[(listen [k (m a)])  (m (Pair a w))]
  @defproc[(censor [f (-> w w)] [k (m a)]) (m a)])]{

Built-in instance: @racket[WriterT w m].

For working directly with @racket[WriterT] rather than polymorphically,
see the non-class helper @racket[tell].}}

@defidform[#:kind "class" MonadError]{

@racket[(MonadError e m)]: monads supporting typed short-circuiting
errors of type @racket[e].  Superclass: @racket[Monad m].  Functional
dependency: @racket[m -> e].

@deftogether[(
  @defproc[(throw-e [e e])                        (m a)]
  @defproc[(catch-e [k (m a)] [h (-> e (m a))])    (m a)])]{

Built-in instance: @racket[ExceptT e m].

For working directly with @racket[ExceptT] rather than polymorphically,
see the non-class helpers @racket[throw-error] and @racket[catch-error].}}

@defidform[#:kind "class" Concurrent]{

@racket[(Concurrent m)]: monads supporting forked async computations.
Superclass: @racket[Monad m].

@deftogether[(
  @defproc[(fork-c  [k (m a)])      (m (Future a))]
  @defproc[(await-c [fut (Future a)]) (m a)]
  @defthing[#:kind "method" yield-c (m Unit)])]{

@racket[await-c] and @racket[yield-c] are return-typed.  Built-in
instances: @racket[IO] (real OS threads via @racket[fork-io]),
@racket[Identity] (deterministic, single-threaded).}}
