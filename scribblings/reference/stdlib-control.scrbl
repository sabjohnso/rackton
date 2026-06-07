#lang scribble/manual
@require[scribble/manual
         (for-label rackton rackton/control/applicative rackton/control/concurrent rackton/control/monad rackton/control/monad/except rackton/control/monad/reader rackton/control/monad/state rackton/control/monad/writer rackton/control/stm rackton/data/result)]

@title[#:tag "stdlib-control"]{@tt{rackton/control} — applicative, monad, transformers}

@section{rackton/control/applicative}
@defmodule[rackton/control/applicative #:no-declare]
@declare-exporting[rackton/control/applicative]

Control.Applicative helpers. The core @racket[pure], @racket[fapply], and
@racket[liftA2] operations are prelude @racket[Applicative] methods; this
module adds the higher-arity lift.

@defproc[(lift-a3 [g (-> a (-> b (-> c d)))] [fa (f a)] [fb (f b)] [fc (f c)]) (f d)]{
Applies a curried 3-ary function @racket[g] under an @racket[Applicative]
@racket[f], for any @racket[(Applicative f)].}


@section{rackton/control/concurrent}
@defmodule[rackton/control/concurrent #:no-declare]
@declare-exporting[rackton/control/concurrent]

Control.Concurrent: threads, MVars, and channels, factored out of the
auto-prelude. These are thin wrappers over Racket's threads, semaphores, and
async channels, with the runtime living in @tt{rackton/private/prelude-runtime}
and reached through @racket[foreign].

@defidform[#:kind "type" ThreadId]{The identity of a running thread, as returned
by @racket[fork-io].}

@defidform[#:kind "type" MVar]{A mutable, synchronizing location holding a value
of type @racket[a]; it may be full or empty.}

@defidform[#:kind "type" Chan]{An unbounded asynchronous channel carrying values
of type @racket[a].}

@defproc[(fork-io [action (IO a)]) (IO ThreadId)]{Spawns @racket[action] in a new
thread and returns its @racket[ThreadId].}

@defproc[(wait-thread [tid ThreadId]) (IO Unit)]{Blocks until the thread named by
@racket[tid] has finished.}

@defproc[(new-mvar [x a]) (IO (MVar a))]{Creates a new @racket[MVar] holding the
initial value @racket[x].}

@defthing[new-empty-mvar (IO (MVar a))]{Creates a new, initially empty
@racket[MVar].}

@defproc[(take-mvar [mv (MVar a)]) (IO a)]{Removes and returns the value held in
@racket[mv], blocking while it is empty and leaving it empty afterward.}

@defproc[(put-mvar [mv (MVar a)] [x a]) (IO Unit)]{Stores @racket[x] in
@racket[mv], blocking while it is full.}

@defproc[(read-mvar [mv (MVar a)]) (IO a)]{Returns the value held in @racket[mv]
without emptying it, blocking while it is empty.}

@defproc[(modify-mvar [mv (MVar a)] [f (-> a a)]) (IO Unit)]{Atomically replaces
the contents of @racket[mv] with @racket[f] applied to its current value.}

@defthing[new-chan (IO (Chan a))]{Creates a new, empty asynchronous channel.}

@defproc[(send-chan [ch (Chan a)] [x a]) (IO Unit)]{Sends @racket[x] on channel
@racket[ch] without blocking.}

@defproc[(recv-chan [ch (Chan a)]) (IO a)]{Receives the next value from channel
@racket[ch], blocking until one is available.}


@section{rackton/control/monad}
@defmodule[rackton/control/monad #:no-declare]
@declare-exporting[rackton/control/monad]

Control.Monad combinators that work over any @racket[(Monad m)]: monadic
mapping, sequencing, folding, replication, and filtering. These build on
@racket[flatmap] (via @racket[do]) and @racket[pure]; the @racket[Monad]
class itself, plus @racket[join], @tt{when}, @tt{unless}, and
@tt{void}, live in the prelude.

@defproc[(map-m [f (-> a (m b))] [xs (List a)]) (m (List b))]{
  Apply a monadic action to each element of @racket[xs], collecting the
  results (Haskell @tt{mapM} / @tt{traverse} specialised to lists).}

@defproc[(for-m [xs (List a)] [f (-> a (m b))]) (m (List b))]{
  @racket[map-m] with its arguments flipped.}

@defproc[(sequence-m [ms (List (m a))]) (m (List a))]{
  Run a list of actions left to right, collecting their results.}

@defproc[(fold-m [f (-> b (-> a (m b)))] [z b] [xs (List a)]) (m b)]{
  Left fold over @racket[xs] with a monadic step function (Haskell @tt{foldM}).}

@defproc[(replicate-m [n Integer] [act (m a)]) (m (List a))]{
  Run @racket[act] @racket[n] times, collecting the results.}

@defproc[(filter-m [p (-> a (m Boolean))] [xs (List a)]) (m (List a))]{
  Keep the elements of @racket[xs] whose monadic predicate yields true.}


@section{rackton/control/monad/except}
@defmodule[rackton/control/monad/except #:no-declare]
@declare-exporting[rackton/control/monad/except]

The @racket[ExceptT] monad transformer: typed exceptions layered over an
inner monad. Provides @racket[Functor], @racket[Applicative], @racket[Monad],
and @racket[MonadError] instances for @racket[ExceptT], along with
@racket[MonadState], @racket[MonadEnv], and @racket[MonadWriter] pass-through
instances when @racket[ExceptT] is the outer transformer. @racket[ExceptT]
over @tt{Identity} plays the role of a bare @racket[Except].

@deftogether[(
@defform[#:kind "type & constructor" #:link-target? #f #:id ExceptT #:literals (newtype Result)
         (newtype (ExceptT e m a)
           (ExceptT (m (Result e a))))]
@defthing[#:kind "type & constructor" ExceptT (-> (m (Result e a)) (ExceptT e m a))])]{
  The exception transformer, wrapping an inner-monad computation that yields a
  @racket[Result].}

@defproc[(run-except-t [e (ExceptT e m a)]) (m (Result e a))]{
  Unwrap an @racket[ExceptT] action to its underlying inner-monad computation
  of a @racket[Result].}

@defproc[(throw-error [e e]) (ExceptT e m a)]{
  Raise @racket[e] as a typed error, producing a failing @racket[ExceptT]
  action; requires @racket[(Applicative m)].}

@defproc[(catch-error [ea (ExceptT e m a)] [handler (-> e (ExceptT e m a))]) (ExceptT e m a)]{
  Run @racket[ea], passing any raised error to @racket[handler] for recovery;
  requires @racket[(Monad m)].}

@defproc[(lift-except-t [ma (m a)]) (ExceptT e m a)]{
  Lift an inner-monad action @racket[ma] into @racket[ExceptT], treating its
  result as a success; requires @racket[(Functor m)].}


@section{rackton/control/monad/reader}
@defmodule[rackton/control/monad/reader #:no-declare]
@declare-exporting[rackton/control/monad/reader]

The (non-transformer) @racket[Env] (Reader) monad together with its
@racket[EnvT] transformer variant, which threads a read-only environment
@racket[r] over an inner monad @racket[m]. Provides @racket[Functor],
@racket[Applicative], @racket[Monad], and @racket[MonadEnv] instances for both,
plus @tt{MonadState}/@tt{MonadWriter}/@tt{MonadError} pass-through instances for
@racket[EnvT].

@deftogether[(
@defform[#:kind "type & constructor" #:link-target? #f #:id Env #:literals (newtype ->)
         (newtype (Env r a)
           (Env (-> r a)))]
@defthing[#:kind "type & constructor" Env (-> (-> r a) (Env r a))])]{
The reader monad: a computation that reads a
shared environment @racket[r] to produce an @racket[a].}

@defproc[(run-env [e (Env r a)]) (-> r a)]{Unwraps an @racket[Env] back into its
underlying environment-consuming function.}

@defthing[ask (Env r r)]{Retrieves the current environment as the computation's
result.}

@defproc[(local [f (-> r r)] [e (Env r a)]) (Env r a)]{Runs @racket[e] in an
environment locally transformed by @racket[f].}

@deftogether[(
@defform[#:kind "type & constructor" #:link-target? #f #:id EnvT #:literals (newtype ->)
         (newtype (EnvT r m a)
           (EnvT (-> r (m a))))]
@defthing[#:kind "type & constructor" EnvT (-> (-> r (m a)) (EnvT r m a))])]{
The reader monad transformer: env-passing over an
inner monad @racket[m].}

@defproc[(run-env-t [e (EnvT r m a)]) (-> r (m a))]{Unwraps an @racket[EnvT] back
into its underlying environment-consuming function.}

@defthing[ask-t (EnvT r m r)]{Retrieves the current environment, lifted into the
inner monad via its @racket[pure] (requires @racket[(Applicative m)]).}

@defproc[(local-t [f (-> r r)] [e (EnvT r m a)]) (EnvT r m a)]{Runs @racket[e] in
an environment locally transformed by @racket[f].}

@defproc[(lift-env-t [ma (m a)]) (EnvT r m a)]{Lifts an inner-monad action into
@racket[EnvT], ignoring the environment.}


@section{rackton/control/monad/state}
@defmodule[rackton/control/monad/state #:no-declare]
@declare-exporting[rackton/control/monad/state]

The (non-transformer) @racket[State] monad together with the
@racket[StateT] monad transformer that layers state over an inner monad.
Pure Rackton: the module regenerates its own runtime and provides the
@tt{Functor}/@tt{Applicative}/@tt{Monad}/@tt{MonadState} instances for
both, plus the @racket[StateT]-outer pass-through instances for
@tt{MonadEnv}/@tt{MonadWriter}/@tt{MonadError}.

@deftogether[(
@defform[#:kind "type & constructor" #:link-target? #f #:id State #:literals (newtype Pair ->)
         (newtype (State s a)
           (State (-> s (Pair s a))))]
@defthing[#:kind "type & constructor" State (-> (-> s (Pair s a)) (State s a))])]{
  The state monad threading a value of type
  @racket[s] through a computation that yields an @racket[a].}

@defproc[(run-state [st (State s a)]) (-> s (Pair s a))]{Unwraps a
  @racket[State] action into its underlying state-transition function.}

@defproc[(eval-state [st (State s a)] [s s]) a]{Runs @racket[st] from
  initial state @racket[s] and returns only the result value.}

@defproc[(exec-state [st (State s a)] [s s]) s]{Runs @racket[st] from
  initial state @racket[s] and returns only the final state.}

@defthing[get-state (State s s)]{Retrieves the current state as the
  result value.}

@defproc[(put-state [s s]) (State s Unit)]{Replaces the current state
  with @racket[s].}

@defproc[(modify-state [f (-> s s)]) (State s Unit)]{Applies @racket[f]
  to the current state.}

@deftogether[(
@defform[#:kind "type & constructor" #:link-target? #f #:id StateT #:literals (newtype Pair ->)
         (newtype (StateT s m a)
           (StateT (-> s (m (Pair s a)))))]
@defthing[#:kind "type & constructor" StateT (-> (-> s (m (Pair s a))) (StateT s m a))])]{
  The state monad transformer: state of
  type @racket[s] layered over an inner monad @racket[m].}

@defproc[(run-state-t [st (StateT s m a)]) (-> s (m (Pair s a)))]{Unwraps
  a @racket[StateT] action into its underlying state-transition function.}

@defproc[(eval-state-t [st (StateT s m a)] [s s]) (m a)]{Runs @racket[st]
  from initial state @racket[s], keeping only the result value (requires
  @tt{Functor} @racket[m]).}

@defproc[(exec-state-t [st (StateT s m a)] [s s]) (m s)]{Runs @racket[st]
  from initial state @racket[s], keeping only the final state (requires
  @tt{Functor} @racket[m]).}

@defthing[get-state-t (StateT s m s)]{Retrieves the current state as the
  result value (requires @tt{Applicative} @racket[m]).}

@defproc[(put-state-t [s s]) (StateT s m Unit)]{Replaces the current state
  with @racket[s] (requires @tt{Applicative} @racket[m]).}

@defproc[(modify-state-t [f (-> s s)]) (StateT s m Unit)]{Applies
  @racket[f] to the current state (requires @tt{Applicative} @racket[m]).}

@defproc[(lift-state-t [ma (m a)]) (StateT s m a)]{Lifts an inner-monad
  action into @racket[StateT], leaving the state unchanged (requires
  @tt{Functor} @racket[m]).}


@section{rackton/control/monad/writer}
@defmodule[rackton/control/monad/writer #:no-declare]
@declare-exporting[rackton/control/monad/writer]

The @racket[WriterT] transformer: an accumulating writer over an inner
monad. This module owns every mtl instance where @racket[WriterT] is the
outer transformer; the non-transformer @tt{Writer} role is served by
@racket[WriterT] over @tt{Identity}, while the @tt{MonadWriter} class
itself lives in the prelude.

@deftogether[(
@defform[#:kind "type & constructor" #:link-target? #f #:id WriterT #:literals (newtype Pair)
         (newtype (WriterT w m a)
           (WriterT (m (Pair w a))))]
@defthing[#:kind "type & constructor" WriterT (-> (m (Pair w a)) (WriterT w m a))])]{
  The writer-transformer type
  @racket[(WriterT w m a)], accumulating a log of type @racket[w] over the
  inner monad @racket[m] around a result of type @racket[a].}

@defproc[(run-writer-t [w (WriterT w m a)]) (m (Pair w a))]{Unwraps a
  @racket[WriterT], exposing the inner computation that produces the
  @racket[(Pair w a)] of log and result.}

@defproc[(eval-writer-t [w (WriterT w m a)]) (m a)]{Runs the writer and
  keeps only the result, discarding the accumulated log. Requires
  @racket[(Functor m)].}

@defproc[(exec-writer-t [w (WriterT w m a)]) (m w)]{Runs the writer and
  keeps only the accumulated log, discarding the result. Requires
  @racket[(Functor m)].}

@defproc[(tell [w w]) (WriterT w m Unit)]{Appends @racket[w] to the log,
  producing a @racket[WriterT] with a @racket[Unit] result. Requires
  @racket[(Applicative m)].}

@defproc[(lift-writer-t [ma (m a)]) (WriterT w m a)]{Lifts an inner-monad
  computation into @racket[WriterT], pairing its result with the empty log
  @racket[mempty]. Requires @racket[(Functor m)] and @racket[(Monoid w)].}


@section{rackton/control/stm}
@defmodule[rackton/control/stm #:no-declare]
@declare-exporting[rackton/control/stm]

Software transactional memory (@tt{Control.Concurrent.STM}). Provides
transactional variables and an @racket[STM] monad whose transaction log,
commit lock, and version checks are implemented by the host runtime and
reached through @racket[foreign]; @racket[STM] is an opaque type whose
values carry the @tt{$stm} runtime dispatch tag. @racket[STM] has
@tt{Functor}, @tt{Applicative}, and @tt{Monad} instances.

@defidform[#:kind "type" TVar]{A transactional variable holding a value of
type @racket[a], read and written only inside an @racket[STM] transaction.}

@defidform[#:kind "type" STM]{A transaction in the software-transactional-memory
monad producing a result of type @racket[a].}

@deftogether[(@defproc[(new-tvar [x a]) (STM (TVar a))]
              @defproc[(read-tvar [v (TVar a)]) (STM a)]
              @defproc[(write-tvar [v (TVar a)] [x a]) (STM Unit)])]{
  Create a new @racket[TVar] holding @racket[x], read the current value of a
  @racket[TVar], and write @racket[x] into a @racket[TVar], respectively, each
  as an @racket[STM] action.}

@defthing[retry (STM a)]{Abort and re-run the current transaction, blocking
until one of the @racket[TVar]s it read has changed.}

@defproc[(or-else [a (STM a)] [b (STM a)]) (STM a)]{Run @racket[a]; if it
calls @racket[retry], run @racket[b] instead.}

@defproc[(atomically [m (STM a)]) (IO a)]{Run the transaction @racket[m]
atomically, committing its effects as a single indivisible step.}

@deftogether[(@defproc[(stm-fmap [f (-> a b)] [s (STM a)]) (STM b)]
              @defproc[(stm-pure [x a]) (STM a)]
              @defproc[(stm-ap [sf (STM (-> a b))] [sa (STM a)]) (STM b)]
              @defproc[(stm-bind [f (-> a (STM b))] [s (STM a)]) (STM b)])]{
  Host-runtime implementations backing the @tt{Functor}, @tt{Applicative}, and
  @tt{Monad} instances for @racket[STM].}



@section{rackton/control/monad/trans}
@defmodule[rackton/control/monad/trans #:no-declare]
Re-exports the four monad transformers (@racket[StateT], @racket[EnvT],
@racket[WriterT], @racket[ExceptT]) as a single import and supplies their
@tt{MonadTrans} (@tt{lift}) and @tt{MonadIO} (@tt{lift-io}) instances.
