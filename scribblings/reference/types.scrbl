#lang scribble/manual
@require[scribble/manual
         (for-label rackton)]

@title[#:tag "types"]{Built-in types}
This chapter enumerates every type constructor and every data
constructor that the Rackton prelude makes available without any user
declaration.  Type classes and their methods are documented in
@secref["classes"]; the value bindings that operate on these types are
in @secref["values"].

@section{Primitive types}

@defidform[#:kind "type" Integer]{

Arbitrary-precision integers.  Literals like @racket[42] and
@racket[-7] have type @racket[Integer].  Instances: @racket[Num],
@racket[Eq], @racket[Ord], @racket[Show], @racket[Integral],
@racket[Real].}

@defidform[#:kind "type" Float]{

Inexact reals (Racket's @racket[flonum]s).  Literals with a fractional
part or exponent (@racket[3.14], @racket[1e10]) have type
@racket[Float].  Instances: @racket[Num], @racket[Eq], @racket[Ord],
@racket[Show], @racket[Fractional], @racket[Floating], @racket[Real],
@racket[RealFrac], @racket[RealFloat].}

@defidform[#:kind "type" Rational]{

Exact non-integer rationals.  Built with @racket[make-rational].
Instances: @racket[Num], @racket[Eq], @racket[Ord], @racket[Show],
@racket[Fractional], @racket[Real], @racket[RealFrac].}

@defidform[#:kind "type" Complex]{

Complex numbers (Racket's @racket[number?] with non-zero imaginary
part).  Built with @racket[make-complex].  Instances: @racket[Num],
@racket[Eq], @racket[Show], @racket[Fractional], @racket[Floating].}

@defidform[#:kind "type" Boolean]{

The two-valued type with constructors @racket[#t] (true) and
@racket[#f] (false).  Instances: @racket[Eq], @racket[Show].}

@defidform[#:kind "type" String]{

Immutable strings (Racket's @racket[immutable?] strings).  Literals
like @racket["hello"] have type @racket[String].  Instances:
@racket[Eq], @racket[Ord], @racket[Show], @racket[Semigroup],
@racket[Monoid].}

@defidform[#:kind "type" Char]{

Unicode code points (Racket's @racket[char?]).  Literals like
@racket[#\A] have type @racket[Char].  Instances: @racket[Eq],
@racket[Ord], @racket[Show].}

@defidform[#:kind "type" Bytes]{

Immutable byte strings (Racket's @racket[bytes?]).  Literals like
@racket[#"hello"] have type @racket[Bytes].  Instances: @racket[Eq],
@racket[Show].}

@defidform[#:kind "type & constructor" Unit]{

A nullary type with a single value; the type and its sole constructor
share the name @racket[Unit].  Used as the result type of
side-effecting computations.}

@section{Sum and product types}

@defidform[#:kind "type" Maybe]{

Optional values: @racket[(Maybe a)] is either present or absent.

@deftogether[(@defidform[#:kind "constructor" None]
              @defidform[#:kind "constructor" Some])]{

@racket[None : (Maybe a)] is the absent case; @racket[Some : (-> a (Maybe a))]
wraps a present value.}

Instances: @racket[Functor], @racket[Applicative], @racket[Monad],
@racket[Foldable], @racket[Traversable].}

@defidform[#:kind "type" List]{

Inductive list type with constructors @racket[Nil] and @racket[Cons].

@deftogether[(@defidform[#:kind "constructor" Nil]
              @defidform[#:kind "constructor" Cons])]{

@racket[Nil : (List a)] is the empty list; @racket[Cons : (-> a (-> (List a) (List a)))]
prepends an element.}

Instances: @racket[Functor], @racket[Applicative], @racket[Monad],
@racket[Foldable], @racket[Traversable], @racket[Semigroup],
@racket[Monoid].}

@defidform[#:kind "type & constructor" Pair]{

Two-element product type; the type and its constructor share the name.
@racket[Pair : (-> a (-> b (Pair a b)))] builds a pair.  Use
@racket[fst] / @racket[snd] / @racket[swap] to project.

Instances: @racket[Bifunctor].}

@defidform[#:kind "type" Result]{

Tagged-union for fallible computations.  @racket[(Result e a)] is
either an error of type @racket[e] or a success of type @racket[a].

@deftogether[(@defidform[#:kind "constructor" Err]
              @defidform[#:kind "constructor" Ok])]{

@racket[Err : (-> e (Result e a))] / @racket[Ok : (-> a (Result e a))].}

Instances: @racket[Functor], @racket[Applicative], @racket[Monad]
(over the @racket[a]; @racket[e] is fixed per chain),
@racket[Bifunctor].}

@section{Monoid wrappers}

@para{@bold{Module} — @racket[(require rackton/data/monoid)]
(see @secref["stdlib"]).}

@defidform[#:kind "type & constructor" Sum]{

Newtype wrapper marking an @racket[Integer] for additive monoid
behaviour; the type and its constructor share the name.
@racket[Sum : (-> Integer Sum)].

Instance: @racket[Semigroup] / @racket[Monoid] (@racket[mempty] is
@racket[(Sum 0)]; @racket[<>] adds).}

@defidform[#:kind "type & constructor" Product]{

Newtype wrapper marking an @racket[Integer] for multiplicative monoid
behaviour; the type and its constructor share the name.
@racket[Product : (-> Integer Product)].

Instance: @racket[Semigroup] / @racket[Monoid] (@racket[mempty] is
@racket[(Product 1)]; @racket[<>] multiplies).}

@section{IO, references, and concurrency}

@para{@bold{Module} — @racket[IO] is in the prelude.  @racket[Ref] is in
@racketmodname[rackton/system]; @racket[MVar] and @racket[Chan] are in
@racketmodname[rackton/control/concurrent] (see @secref["stdlib"]).}

@defidform[#:kind "type" IO]{

The IO monad — a computation that may perform side effects when
executed.  Values of @racket[(IO a)] are first-class: build them up
with @racket[do] / @racket[flatmap], run them with @racket[run-io].
Instances: @racket[Functor], @racket[Applicative], @racket[Monad],
@racket[Concurrent].  No public constructor.}

@defidform[#:kind "type" Ref]{

A mutable cell.  All operations are @racket[IO] actions; the value is
opaque.  See @racket[make-ref], @racket[read-ref], @racket[write-ref].}

@defidform[#:kind "type" MVar]{

A synchronised mutable variable: @racket[(MVar a)] is either filled
with a value of type @racket[a] or empty.  @racket[take-mvar] blocks
on empty; @racket[put-mvar] blocks on full.  See @racket[new-mvar],
@racket[new-empty-mvar], @racket[take-mvar], @racket[put-mvar],
@racket[read-mvar], @racket[modify-mvar].}

@defidform[#:kind "type" Chan]{

An unbounded asynchronous channel.  Sends never block; receives block
until a value is available.  See @racket[new-chan], @racket[send-chan],
@racket[recv-chan].}

@defidform[#:kind "type" ThreadId]{

The handle returned by @racket[fork-io]: an opaque token identifying
a running OS-level thread.  @racket[wait-thread] joins on the
underlying thread.  See also @racket[Future] (a sibling abstraction
used by the @racket[Concurrent] class).}

@defidform[#:kind "type" Future]{

The result of a concurrent computation spawned with @racket[fork-c].
Read via the @racket[Concurrent] class's @racket[await-c] method.}

@section{Software transactional memory}

@para{@bold{Module} — @racket[(require rackton/control/stm)]
(see @secref["stdlib"]).}

@defidform[#:kind "type" TVar]{

A transactional variable.  Reads and writes are deferred until commit
time; on a conflict, the transaction restarts.  See @racket[new-tvar],
@racket[read-tvar], @racket[write-tvar].}

@defidform[#:kind "type" STM]{

The STM monad — a transactional computation that may read and write
TVars.  Run a transaction with @racket[atomically].  Instances:
@racket[Functor], @racket[Applicative], @racket[Monad].}

@section{Optics}

@para{@bold{Module} — @racket[(require rackton/data/lens)]
(see @secref["stdlib"]).}

@defidform[#:kind "type & constructor" Lens]{

A functional getter/setter pair: @racket[(Lens s a)] focuses an
@racket[a] inside an @racket[s].  The type and its constructor share the
name.  @racket[Lens : (-> (-> s a) (-> s (-> a s)) (Lens s a))].  See
@racket[view], @racket[set], @racket[over], @racket[lens-compose].}

@defidform[#:kind "type & constructor" Prism]{

A pattern for matching one branch of a sum type: @racket[(Prism s a)]
either extracts or builds.  The type and its constructor share the name.
@racket[Prism : (-> (-> s (Maybe a)) (-> a s) (Prism s a))].  See
@racket[preview], @racket[review].}

@defidform[#:kind "type & constructor" Traversal]{

A polymorphic walk that visits zero or more positions.
@racket[(Traversal s a)] is the lens-of-many.  The type and its
constructor share the name.
@racket[Traversal : (-> (-> s (List a)) (-> (-> a a) (-> s s)) (Traversal s a))].
See @racket[to-list-of], @racket[over-of], @racket[list-traversal],
@racket[lens-as-traversal].}

@section{Monad transformers and the Identity monad}

@para{@bold{Modules} — @racket[Identity] is in the prelude.  The
transformers live under @tt{rackton/control/monad}:
@racket[StateT] in @tt{…/state}, @racket[EnvT] in @tt{…/reader},
@racket[WriterT] in @tt{…/writer}, @racket[ExceptT] in @tt{…/except}
(see @secref["stdlib"]).}

@defidform[#:kind "type & constructor" Identity]{

The identity monad: @racket[(Identity a)] wraps a single @racket[a]
with no effects.  Useful as the base for transformer stacks.  The type
and its constructor share the name.
@racket[Identity : (-> a (Identity a))].  Unwrap with
@racket[run-identity].

Instances: @racket[Functor], @racket[Applicative], @racket[Monad],
@racket[Concurrent].}

@defidform[#:kind "type & constructor" State]{

The state monad: @racket[(State s a)] is a function from a state
@racket[s] to a pair of a new state and a result.  The type and its
constructor share the name.
@racket[State : (-> (-> s (Pair s a)) (State s a))].  See
@racket[run-state], @racket[eval-state], @racket[exec-state],
@racket[get-state], @racket[put-state], @racket[modify-state].

Instances: @racket[Functor], @racket[Applicative], @racket[Monad],
@racket[MonadState].}

@defidform[#:kind "type & constructor" Env]{

The reader / environment monad: @racket[(Env r a)] is a function from
an environment @racket[r] to an @racket[a].  The type and its
constructor share the name.
@racket[Env : (-> (-> r a) (Env r a))].  See @racket[run-env],
@racket[ask], @racket[local].

Instances: @racket[Functor], @racket[Applicative], @racket[Monad],
@racket[MonadEnv].}

@defidform[#:kind "type & constructor" StateT]{

State monad transformer: @racket[(StateT s m a)] threads state
@racket[s] through an inner monad @racket[m].  The type and its
constructor share the name.
@racket[StateT : (-> (-> s (m (Pair s a))) (StateT s m a))].  See
@racket[run-state-t], @racket[eval-state-t], @racket[exec-state-t],
@racket[get-state-t], @racket[put-state-t], @racket[modify-state-t],
@racket[lift-state-t].

Instances: @racket[Functor], @racket[Applicative], @racket[Monad],
@racket[MonadState] (when @racket[m] is a @racket[Monad]).}

@defidform[#:kind "type & constructor" EnvT]{

Reader monad transformer: @racket[(EnvT r m a)] threads an
environment @racket[r] through an inner monad @racket[m].  The type and
its constructor share the name.
@racket[EnvT : (-> (-> r (m a)) (EnvT r m a))].  See
@racket[run-env-t], @racket[ask-t], @racket[local-t], @racket[lift-env-t].

Instances: @racket[Functor], @racket[Applicative], @racket[Monad],
@racket[MonadEnv] (when @racket[m] is a @racket[Monad]).}

@defidform[#:kind "type & constructor" WriterT]{

Writer monad transformer: @racket[(WriterT w m a)] threads a log
@racket[w] (which must be a @racket[Monoid]) through an inner monad
@racket[m].  The type and its constructor share the name.
@racket[WriterT : (-> (m (Pair w a)) (WriterT w m a))].  See
@racket[run-writer-t], @racket[eval-writer-t], @racket[exec-writer-t],
@racket[tell], @racket[lift-writer-t].

Instances: @racket[Functor], @racket[Applicative], @racket[Monad],
@racket[MonadWriter] (when @racket[m] is a @racket[Monad] and
@racket[w] is a @racket[Monoid]).}

@defidform[#:kind "type & constructor" ExceptT]{

Exception monad transformer: @racket[(ExceptT e m a)] threads a
short-circuiting error of type @racket[e] through an inner monad
@racket[m].  The type and its constructor share the name.
@racket[ExceptT : (-> (m (Result e a)) (ExceptT e m a))].  See
@racket[run-except-t], @racket[throw-error], @racket[catch-error],
@racket[lift-except-t].

Instances: @racket[Functor], @racket[Applicative], @racket[Monad],
@racket[MonadError] (when @racket[m] is a @racket[Monad]).}

@section{Immutable containers}

@defidform[#:kind "type" Map]{

@racket[(Map k v)] is an immutable mapping from @racket[k] to
@racket[v], backed by Racket's @racket[equal?]-hash.  Every operation
returns a new map.  See @racket[empty-map], @racket[map-insert],
@racket[map-lookup], @racket[map-delete], @racket[map-keys],
@racket[map-values], @racket[map-size], @racket[map-fold].}

@defidform[#:kind "type" Set]{

@racket[(Set a)] is an immutable set of @racket[a].  See
@racket[empty-set], @racket[set-insert], @racket[set-member?],
@racket[set-delete], @racket[set-size], @racket[set-to-list].}

@defidform[#:kind "type" Ptr]{

@racket[(Ptr a)] is an opaque, typed pointer to raw memory holding a
value of type @racket[a] (the @racket[a] is a phantom tag).  It lives in
the prelude because @racket[Storable]'s @racket[peek] is return-typed,
but its operations — allocation, @racket[peek]/@racket[poke], pointer
arithmetic, C strings — are in @racketmodname[rackton/foreign/ptr] and
are @bold{unsafe}.}
