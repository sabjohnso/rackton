#lang scribble/manual
@require[scribble/manual
         (for-label rackton rackton/data/result)]

@title[#:tag "types"]{Built-in types}
This chapter enumerates every type constructor and every data
constructor that the Rackton prelude makes available without any user
declaration.  Protocols (Haskell's @italic{type classes}) and their
methods are documented in
@secref["classes"]; the value bindings that operate on these types are
in @secref["values"].

@section[#:tag "ref-primitive-types"]{Primitive types}

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

Inexact complex numbers (Racket's @racket[number?] with a non-zero
imaginary part and inexact components).  Written as a literal with an
imaginary suffix, like @racket[3.0+4.0i], or built with
@racket[make-complex].  Instances: @racket[Num], @racket[Eq],
@racket[Show], @racket[Fractional], @racket[Floating].}

@defidform[#:kind "type" ComplexExact]{

Exact complex numbers — the exact counterpart of @racket[Complex],
backed by Racket's exact non-real numbers.  An exact imaginary literal
such as @racket[3+4i] has type @racket[ComplexExact]; values are also
built with @racket[make-complex-exact] from two @racket[Integer]
components (the Gaussian integers).  The accessors
@racket[real-part-exact] and @racket[imag-part-exact] recover the
components.  Instances: @racket[Num], @racket[Eq], @racket[Show] — no
@racket[Ord] (complex numbers have no total order) and no
@racket[Fractional] / @racket[Floating] (those leave the exact-integer
world).

Like @racket[Rational], the type is closed only up to Racket's exact
arithmetic: a result whose imaginary part cancels to zero collapses to a
real (for instance @racket[(* 3+4i 3-4i)] is the @racket[Integer]
@racket[25]), and dividing two exact complex numbers yields exact
rational components that the integer accessors do not describe — widen
with @racket[complex-exact->complex] first.}

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

@defidform[#:kind "type" Symbol]{

Interned symbols (Racket's @racket[symbol?]).  A @racket[Symbol]
literal is written as a quoted identifier, like @racket['foo].
Convert to and from text with @racket[symbol->string] and
@racket[string->symbol].  Instances: @racket[Eq], @racket[Ord],
@racket[Show].}

@deftogether[(
@defform[#:kind "type & constructor" #:link-target? #f #:id Unit #:literals (data)
         (data Unit
           Unit)]
@defthing[#:kind "type & constructor" Unit Unit])]{

A nullary type with a single value; the type and its sole constructor
share the name @racket[Unit].  Used as the result type of
side-effecting computations.}

@section{Sum and product types}

@deftogether[(
@defform[#:kind "type" #:link-target? #f #:id Maybe #:literals (data None Some)
         (data (Maybe a)
           None
           (Some a))]
@defidform[#:kind "type" Maybe]
@defthing[#:kind "constructor" None (Maybe a)]
@defthing[#:kind "constructor" Some (-> a (Maybe a))])]{

Optional values: @racket[(Maybe a)] is either present or absent.
@racket[None] is the absent case; @racket[Some] wraps a present value.

Instances: @racket[Functor], @racket[Applicative], @racket[Monad],
@racket[Foldable], @racket[Traversable].}

@deftogether[(
@defform[#:kind "type" #:link-target? #f #:id List #:literals (data Nil Cons List)
         (data (List a)
           Nil
           (Cons a (List a)))]
@defidform[#:kind "type" List]
@defthing[#:kind "constructor" Nil (List a)]
@defthing[#:kind "constructor" Cons (-> a (-> (List a) (List a)))])]{

Inductive list type.  @racket[Nil] is the empty list; @racket[Cons]
prepends an element.

Instances: @racket[Functor], @racket[Applicative], @racket[Monad],
@racket[Foldable], @racket[Traversable], @racket[Semigroup],
@racket[Monoid].}

@deftogether[(
@defform[#:kind "type & constructor" #:link-target? #f #:id Pair #:literals (data)
         (data (Pair a b)
           (Pair a b))]
@defthing[#:kind "type & constructor" Pair (-> a (-> b (Pair a b)))])]{

The binary tuple: a two-element product whose type and constructor share
the name.  @racket[(Pair a b)] is definitionally the two-element
@racket[Tuple] — @racket[(Pair a b)] ≡ @racket[(Tuple a b)] and
@racket[(Pair x y)] ≡ @racket[(tuple x y)] — so @racket[tref], the tuple
@racket[Eq] / @racket[Ord] / @racket[Show], and the projections
@racket[fst] / @racket[snd] / @racket[swap] all apply.  Unlike the
variadic @racket[Tuple], @racket[Pair] is also a binary type constructor,
which is what lets it carry higher-kinded instances.

Instances: @racket[Bifunctor], @racket[Prod] (and the tuple
@racket[Eq] / @racket[Ord] / @racket[Show]).}

@defidform[#:kind "type" Tuple]{

The variadic, heterogeneous, fixed-arity product type: @racket[(Tuple τ
...)] for any number of element types.  Build one with @racket[(tuple
elem ...)] and read an element with @racket[(tref t n)] (the index
@racket[n] is a literal, bounds-checked at compile time).  A
@racket[(Tuple a b)] of arity two is a @racket[Pair].  There is no arity
limit, so @racket[Tuple] supersedes the old fixed @tt{Tuple3}…@tt{Tuple7}
focus types for multi-field @racket[:deriving Prism].

@racket[(Tuple τ ...)] has @racket[Eq], @racket[Ord], and @racket[Show]
whenever every element type @racket[τ] does.}

@defidform[#:kind "type" Array]{

A fixed-size array: @racket[(Array n a)] holds exactly @racket[n]
elements of type @racket[a], with the size @racket[n] carried in the
type as a type-level @racket[Nat] (so it is checked at compile time).
Build one with @racket[array] or @racket[build-array]; read an element
with @racket[aref].  Multidimensional arrays are nested —
@racket[(Array n (Array m a))] is an @racket[n]×@racket[m] grid — and
@racket[flatten-major] / @racket[flatten-minor] collapse one level into a
flat @racket[(Array (* n m) a)].  The element layout is hidden; only the
size and element type are observable.}

@deftogether[(
@defform[#:kind "type" #:link-target? #f #:id Either #:literals (data Left Right)
         (data (Either a b)
           (Left a)
           (Right b))]
@defidform[#:kind "type" Either]
@defthing[#:kind "constructor" Left (-> a (Either a b))]
@defthing[#:kind "constructor" Right (-> b (Either a b))])]{

Tagged-union coproduct.  @racket[(Either a b)] is either a @racket[Left]
of type @racket[a] or a @racket[Right] of type @racket[b].  By convention
the @racket[Left] carries an error and the @racket[Right] a success, but
@racket[Either] is the neutral, foundational coproduct — it is what the
arrow @racket[Coprod] / @racket[ArrowChoice] machinery is defined over.
For code that reads better with @tt{Ok}/@tt{Err} naming, the isomorphic
@racket[Result] lives in @racketmodname[rackton/data/result].

Instances: @racket[Functor], @racket[Applicative], @racket[Monad]
(over the @racket[b]; @racket[a] is fixed per chain),
@racket[Bifunctor], @racket[Coprod].}

@section{Monoid wrappers}

@para{@bold{Module} — @racket[(require rackton/data/monoid)]
(see @secref["stdlib"]).}

@deftogether[(
@defform[#:kind "type & constructor" #:link-target? #f #:id Sum #:literals (newtype Integer)
         (newtype Sum
           (Sum Integer))]
@defthing[#:kind "type & constructor" Sum (-> Integer Sum)])]{

Newtype wrapper marking an @racket[Integer] for additive monoid
behaviour; the type and its constructor share the name.

Instance: @racket[Semigroup] / @racket[Monoid] (@racket[mempty] is
@racket[(Sum 0)]; @racket[mappend] adds).}

@deftogether[(
@defform[#:kind "type & constructor" #:link-target? #f #:id Product #:literals (newtype Integer)
         (newtype Product
           (Product Integer))]
@defthing[#:kind "type & constructor" Product (-> Integer Product)])]{

Newtype wrapper marking an @racket[Integer] for multiplicative monoid
behaviour; the type and its constructor share the name.

Instance: @racket[Semigroup] / @racket[Monoid] (@racket[mempty] is
@racket[(Product 1)]; @racket[mappend] multiplies).}

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
used by the @racket[Concurrent] protocol).}

@defidform[#:kind "type" Future]{

The result of a concurrent computation spawned with @racket[fork-c].
Read via the @racket[Concurrent] protocol's @racket[await-c] method.}

@section[#:tag "ref-stm"]{Software transactional memory}

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

@section[#:tag "ref-optics"]{Optics}

@para{@bold{Module} — @racket[(require rackton/data/lens)]
(see @secref["stdlib"]).}

@deftogether[(
@defform[#:kind "type & constructor" #:link-target? #f #:id Lens #:literals (data ->)
         (data (Lens s a)
           (Lens (-> s a) (-> s (-> a s))))]
@defthing[#:kind "type & constructor" Lens (-> (-> s a) (-> s (-> a s)) (Lens s a))])]{

A functional getter/setter pair: @racket[(Lens s a)] focuses an
@racket[a] inside an @racket[s].  The type and its constructor share the
name.  See
@racket[view], @racket[set], @racket[over], @racket[lens-compose].}

@deftogether[(
@defform[#:kind "type & constructor" #:link-target? #f #:id Prism #:literals (data Maybe ->)
         (data (Prism s a)
           (Prism (-> s (Maybe a)) (-> a s)))]
@defthing[#:kind "type & constructor" Prism (-> (-> s (Maybe a)) (-> a s) (Prism s a))])]{

A pattern for matching one branch of a sum type: @racket[(Prism s a)]
either extracts or builds.  The type and its constructor share the name.
See @racket[preview], @racket[review].}

@deftogether[(
@defform[#:kind "type & constructor" #:link-target? #f #:id Traversal #:literals (data List ->)
         (data (Traversal s a)
           (Traversal (-> s (List a))
                      (-> (-> a a) (-> s s))))]
@defthing[#:kind "type & constructor" Traversal (-> (-> s (List a)) (-> (-> a a) (-> s s)) (Traversal s a))])]{

A polymorphic walk that visits zero or more positions.
@racket[(Traversal s a)] is the lens-of-many.  The type and its
constructor share the name.
See @racket[to-list-of], @racket[over-of], @racket[list-traversal],
@racket[lens-as-traversal].}

@section{Monad transformers and the Identity monad}

@para{@bold{Modules} — @racket[Identity] is in the prelude.  The
transformers live under @tt{rackton/control/monad}:
@racket[StateT] in @tt{…/state}, @racket[EnvT] in @tt{…/reader},
@racket[WriterT] in @tt{…/writer}, @racket[ExceptT] in @tt{…/except}
(see @secref["stdlib"]).}

@deftogether[(
@defform[#:kind "type & constructor" #:link-target? #f #:id Identity #:literals (data)
         (data (Identity a)
           (Identity a))]
@defthing[#:kind "type & constructor" Identity (-> a (Identity a))])]{

The identity monad: @racket[(Identity a)] wraps a single @racket[a]
with no effects.  Useful as the base for transformer stacks.  The type
and its constructor share the name.  Unwrap with @racket[run-identity].

Instances: @racket[Functor], @racket[Applicative], @racket[Monad],
@racket[Concurrent].}

@deftogether[(
@defform[#:kind "type & constructor" #:link-target? #f #:id State #:literals (newtype Pair ->)
         (newtype (State s a)
           (State (-> s (Pair s a))))]
@defthing[#:kind "type & constructor" State (-> (-> s (Pair s a)) (State s a))])]{

The state monad: @racket[(State s a)] is a function from a state
@racket[s] to a pair of a new state and a result.  The type and its
constructor share the name.  See
@racket[run-state], @racket[eval-state], @racket[exec-state],
@racket[get-state], @racket[put-state], @racket[modify-state].

Instances: @racket[Functor], @racket[Applicative], @racket[Monad],
@racket[MonadState].}

@deftogether[(
@defform[#:kind "type & constructor" #:link-target? #f #:id Env #:literals (newtype ->)
         (newtype (Env r a)
           (Env (-> r a)))]
@defthing[#:kind "type & constructor" Env (-> (-> r a) (Env r a))])]{

The reader / environment monad: @racket[(Env r a)] is a function from
an environment @racket[r] to an @racket[a].  The type and its
constructor share the name.  See @racket[run-env],
@racket[ask], @racket[local].

Instances: @racket[Functor], @racket[Applicative], @racket[Monad],
@racket[MonadEnv].}

@deftogether[(
@defform[#:kind "type & constructor" #:link-target? #f #:id StateT #:literals (newtype Pair ->)
         (newtype (StateT s m a)
           (StateT (-> s (m (Pair s a)))))]
@defthing[#:kind "type & constructor" StateT (-> (-> s (m (Pair s a))) (StateT s m a))])]{

State monad transformer: @racket[(StateT s m a)] threads state
@racket[s] through an inner monad @racket[m].  The type and its
constructor share the name.  See
@racket[run-state-t], @racket[eval-state-t], @racket[exec-state-t],
@racket[get-state-t], @racket[put-state-t], @racket[modify-state-t],
@racket[lift-state-t].

Instances: @racket[Functor], @racket[Applicative], @racket[Monad],
@racket[MonadState] (when @racket[m] is a @racket[Monad]).}

@deftogether[(
@defform[#:kind "type & constructor" #:link-target? #f #:id EnvT #:literals (newtype ->)
         (newtype (EnvT r m a)
           (EnvT (-> r (m a))))]
@defthing[#:kind "type & constructor" EnvT (-> (-> r (m a)) (EnvT r m a))])]{

Reader monad transformer: @racket[(EnvT r m a)] threads an
environment @racket[r] through an inner monad @racket[m].  The type and
its constructor share the name.  See
@racket[run-env-t], @racket[ask-t], @racket[local-t], @racket[lift-env-t].

Instances: @racket[Functor], @racket[Applicative], @racket[Monad],
@racket[MonadEnv] (when @racket[m] is a @racket[Monad]).}

@deftogether[(
@defform[#:kind "type & constructor" #:link-target? #f #:id WriterT #:literals (newtype Pair)
         (newtype (WriterT w m a)
           (WriterT (m (Pair w a))))]
@defthing[#:kind "type & constructor" WriterT (-> (m (Pair w a)) (WriterT w m a))])]{

Writer monad transformer: @racket[(WriterT w m a)] threads a log
@racket[w] (which must be a @racket[Monoid]) through an inner monad
@racket[m].  The type and its constructor share the name.  See
@racket[run-writer-t], @racket[eval-writer-t], @racket[exec-writer-t],
@racket[tell], @racket[lift-writer-t].

Instances: @racket[Functor], @racket[Applicative], @racket[Monad],
@racket[MonadWriter] (when @racket[m] is a @racket[Monad] and
@racket[w] is a @racket[Monoid]).}

@deftogether[(
@defform[#:kind "type & constructor" #:link-target? #f #:id ExceptT #:literals (newtype Result)
         (newtype (ExceptT e m a)
           (ExceptT (m (Result e a))))]
@defthing[#:kind "type & constructor" ExceptT (-> (m (Result e a)) (ExceptT e m a))])]{

Exception monad transformer: @racket[(ExceptT e m a)] threads a
short-circuiting error of type @racket[e] through an inner monad
@racket[m].  The type and its constructor share the name.  See
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
