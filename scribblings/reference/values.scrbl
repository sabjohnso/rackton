#lang scribble/manual
@require[scribble/manual
         (for-label rackton)]

@title[#:tag "values"]{Built-in values}
This chapter documents every value binding that the Rackton prelude
makes available without any user declaration.  Bindings are grouped by
role: combinators, numeric helpers, strings, characters and bytes,
lists, pairs, immutable maps and sets, optics, IO and effects, and the
several monad-specific accessors that complement the polymorphic
@secref["classes"] entries.

@section[#:tag "prelude"]{Generic combinators}

@defproc[(id [x a]) a]{The identity function.}

@defproc[(const [x a]) (-> b a)]{

Constant-function builder: @racket[((const x) y)] is @racket[x] for
any @racket[y].}

@defproc[(flip [f (-> a (-> b c))]) (-> b (-> a c))]{

Swaps the first two arguments of a binary curried function.}

@defproc[(compose [f (-> b c)] [g (-> a b)]) (-> a c)]{

Function composition: @racket[((compose f g) x)] is @racket[(f (g x))].}

@defproc[(panic [msg String]) a]{

Terminates the program with @racket[msg].  Its return type is
universally quantified, so it can stand in for a value of any type
when used in an unreachable branch.}

@section[#:tag "numeric-helpers"]{Numeric helpers}

These functions complement the @racket[Num] / @racket[Integral] /
@racket[Fractional] class methods.  See @secref["classes"] for
@racket[+], @racket[-], @racket[*], @racket[abs], @racket[negate],
@racket[div], @racket[mod], @racket[quot], @racket[rem],
@racket[float-div], @racket[exp], @racket[log], @racket[sqrt],
@racket[sin], @racket[cos], @racket[tan], @racket[**], @racket[pi],
@racket[is-nan?], @racket[is-infinite?], @racket[atan2],
@racket[floor-real], @racket[ceiling-real], @racket[round-real],
@racket[truncate-real], @racket[to-rational], and @racket[min] /
@racket[max].

@defproc[(integer->string [n Integer]) String]{

Decimal rendering of an integer.}

@defproc[(string->integer [s String]) (Maybe Integer)]{

Parses a string as a decimal integer.  Returns @racket[None] if
@racket[s] is not a valid integer literal.}

@defproc[(integer->float [n Integer]) Float]{

Exact-to-inexact conversion.}

@defproc[(float->integer [x Float]) Integer]{

Truncates toward zero, then converts to exact.}

@defproc[(abs-float [x Float]) Float]{

Absolute value specialised to @racket[Float].}

@defproc[(make-rational [n Integer] [d Integer]) Rational]{

Constructs an exact rational @racket[n/d].}

@defproc[(make-complex [re Float] [im Float]) Complex]{

Constructs a complex number with the given real and imaginary parts.}

@defproc[(get-sum     [s Sum])     Integer]{Extracts the wrapped integer.}
@defproc[(get-product [p Product]) Integer]{Extracts the wrapped integer.}

@section{Sum helper}

@defproc[(sum [xs (t Integer)]) Integer]{

The sum of the integers in any @racket[Foldable] container, using
@racket[foldr] with @racket[+] and @racket[0].}

@section{MTL combinators}

@defproc[(asks [f (-> r a)]) (m a)]{

In any @racket[(MonadEnv r m)], reads the environment and applies
@racket[f].  Equivalent to @racket[(fmap f ask-en)].}

@defproc[(gets [f (-> s a)]) (m a)]{

In any @racket[(MonadState s m)], reads the state and applies
@racket[f].  Equivalent to @racket[(fmap f get-st)].}

@defproc[(mconcat [xs (List a)]) a]{

In any @racket[(Monoid a)], folds @racket[xs] with @racket[<>] and
@racket[mempty].}

@section[#:tag "strings"]{Strings}

In addition to the @racket[Eq] / @racket[Ord] / @racket[Show] /
@racket[Semigroup] / @racket[Monoid] instances, the prelude ships:

@defproc[(string-length [s String]) Integer]{

The length of @racket[s] in characters.}

@defproc[(string-append [a String] [b String]) String]{

Concatenation.  See also @racket[<>] (the @racket[Semigroup] method)
which is the polymorphic equivalent.}

@defproc[(substring [s String] [start Integer] [end Integer]) String]{

The substring of @racket[s] from @racket[start] (inclusive) to
@racket[end] (exclusive).}

@defproc[(string-prefix? [pre String] [s String]) Boolean]{

@racket[#t] iff @racket[s] starts with @racket[pre].}

@defproc[(string-split [sep String] [s String]) (List String)]{

Splits @racket[s] on every occurrence of @racket[sep].}

@defproc[(string-join [sep String] [parts (List String)]) String]{

Inverse of @racket[string-split].}

@section[#:tag "chars"]{Characters and bytes}

@defproc[(char->string [c Char]) String]{

A one-character string.}

@defproc[(string->chars [s String]) (List Char)]{

Decomposes a string into its characters.}

@defproc[(chars->string [cs (List Char)]) String]{

Inverse of @racket[string->chars].}

@defproc[(string->bytes [s String]) Bytes]{

UTF-8 encode.}

@defproc[(bytes->string [b Bytes]) (Maybe String)]{

UTF-8 decode.  Returns @racket[None] on invalid bytes.}

@section[#:tag "lists"]{Lists}

The @racket[List] type's @racket[Functor] / @racket[Monad] /
@racket[Foldable] / @racket[Traversable] / @racket[Semigroup] /
@racket[Monoid] instances cover most needs.  These additional
operations are specialised to lists:

@defproc[(reverse [xs (List a)]) (List a)]{Reverses a list.}

@defproc[(append [xs (List a)] [ys (List a)]) (List a)]{

Concatenates two lists.  See also @racket[<>] (the @racket[Semigroup]
instance for @racket[List]) which is the polymorphic equivalent.}

@defproc[(filter [p (-> a Boolean)] [xs (List a)]) (List a)]{

Retains only the elements satisfying @racket[p].}

@defproc[(sort [xs (List a)]) (List a)]{

Stable merge sort.  Requires @racket[(Ord a)].}

@defproc[(zip [as (List a)] [bs (List b)]) (List (Pair a b))]{

Pairs corresponding elements, truncating to the shorter list.}

@defproc[(take [n Integer] [xs (List a)]) (List a)]{

The first @racket[n] elements.  Yields the whole list if
@racket[n] ≥ @racket[(length xs)].}

@defproc[(drop [n Integer] [xs (List a)]) (List a)]{

All but the first @racket[n] elements.}

@defproc[(find [p (-> a Boolean)] [xs (List a)]) (Maybe a)]{

The first element satisfying @racket[p], or @racket[None].}

@defproc[(concat-map [f (-> a (List b))] [xs (List a)]) (List b)]{

Maps then flattens.}

@defproc[(group-by [key (-> a k)] [xs (List a)]) (Map k (List a))]{

Buckets @racket[xs] by @racket[(key x)].  Requires @racket[(Eq k)].}

@section[#:tag "pairs"]{Pairs}

@defproc[(fst  [p (Pair a b)]) a]{First projection.}
@defproc[(snd  [p (Pair a b)]) b]{Second projection.}
@defproc[(swap [p (Pair a b)]) (Pair b a)]{Swaps the two fields.}

@section[#:tag "maps"]{Immutable Map}

@defthing[empty-map (Map k v)]{

The empty map.  Returns a fresh empty value per call site, polymorphic
in @racket[k] and @racket[v].}

@defproc[(map-insert [k k] [v v] [m (Map k v)]) (Map k v)]{

Returns a new map with @racket[k] bound to @racket[v].  Requires
@racket[(Eq k)].}

@defproc[(map-lookup [k k] [m (Map k v)]) (Maybe v)]{

@racket[(Some v)] if @racket[k] is bound, otherwise @racket[None].}

@defproc[(map-delete [k k] [m (Map k v)]) (Map k v)]{

Removes @racket[k] from @racket[m] (no-op if absent).}

@defproc[(map-keys   [m (Map k v)]) (List k)]{Keys in unspecified order.}
@defproc[(map-values [m (Map k v)]) (List v)]{Values in unspecified order.}
@defproc[(map-size   [m (Map k v)]) Integer]{Number of entries.}

@defproc[(map-fold [f (-> k (-> v (-> b b)))] [z b] [m (Map k v)]) b]{

Folds over the entries with @racket[f] applied curried-style.}

@section[#:tag "sets"]{Immutable Set}

@defthing[empty-set (Set a)]{The empty set.}

@defproc[(set-insert  [x a] [s (Set a)]) (Set a)]{Add an element.}
@defproc[(set-member? [x a] [s (Set a)]) Boolean]{Membership test.}
@defproc[(set-delete  [x a] [s (Set a)]) (Set a)]{Remove an element.}
@defproc[(set-size    [s (Set a)]) Integer]{Cardinality.}
@defproc[(set-to-list [s (Set a)]) (List a)]{Elements in unspecified order.}

@section[#:tag "io"]{IO}

@defproc[(pure-io [x a]) (IO a)]{

Lifts @racket[x] into the @racket[IO] monad — performs no side effects
when run.  Equivalent to @racket[pure] specialised to @racket[IO].}

@defproc[(run-io [k (IO a)]) a]{

Executes the action and returns its result.  This is the bridge from
the typed Rackton world to the surrounding Racket runtime; it is
typically called once, at the top level of a program.}

@subsection{Standard streams}

@defproc[(print    [s String]) (IO Unit)]{Writes @racket[s] without a trailing newline.}
@defproc[(println  [s String]) (IO Unit)]{Writes @racket[s] followed by a newline.}
@defthing[read-line (IO String)]{Reads one line from standard input.}

@subsection{Mutable references}

A @racket[(Ref a)] is an opaque mutable cell.  All operations are
@racket[IO] actions.

@defproc[(make-ref  [v a]) (IO (Ref a))]{Allocate a new cell.}
@defproc[(read-ref  [r (Ref a)]) (IO a)]{Read the current value.}
@defproc[(write-ref [r (Ref a)] [v a]) (IO Unit)]{Overwrite the cell.}

@subsection{Files}

@defproc[(read-file    [path String]) (IO String)]{Reads the file's contents.}
@defproc[(write-file   [path String] [contents String]) (IO Unit)]{Replaces the file's contents.}
@defproc[(file-exists? [path String]) (IO Boolean)]{Tests for file existence.}

@subsection{Error recovery}

@defproc[(try [k (IO a)]) (IO (Result String a))]{

Runs @racket[k] and catches any Racket-level exception, delivering the
result as @racket[(Result String a)].}

@defproc[(raise-io [msg String]) (IO a)]{

Fails an @racket[IO] action with @racket[msg].  Paired naturally with
@racket[try].}

@section[#:tag "concurrency"]{Concurrency}

These primitives back the @racket[Concurrent] class's @racket[IO]
instance and are also usable directly.

@subsection{Threads}

@defproc[(fork-io    [k (IO a)]) (IO (Future a))]{

Spawns a thread running @racket[k]; returns a @racket[Future] that
yields the result.}

@defproc[(wait-thread [t (Future a)]) (IO Unit)]{

Blocks until @racket[t] completes.}

@subsection{MVars}

A @racket[(MVar a)] is a synchronised mutable variable.  Take blocks
on empty; put blocks on full.

@defproc[(new-mvar       [v a]) (IO (MVar a))]{Allocate filled with @racket[v].}
@defthing[new-empty-mvar (IO (MVar a))]{Allocate empty.}
@defproc[(take-mvar      [m (MVar a)]) (IO a)]{Empty and return the contents.}
@defproc[(put-mvar       [m (MVar a)] [v a]) (IO Unit)]{Fill (blocks if full).}
@defproc[(read-mvar      [m (MVar a)]) (IO a)]{Read without emptying.}
@defproc[(modify-mvar    [m (MVar a)] [f (-> a a)]) (IO Unit)]{Atomically replace via @racket[f].}

@subsection{Async channels}

Unbounded async channels.  Sends never block; receives block when
empty.

@defthing[new-chan  (IO (Chan a))]{Allocate a new channel.}
@defproc[(send-chan [ch (Chan a)] [v a]) (IO Unit)]{Enqueue.}
@defproc[(recv-chan [ch (Chan a)]) (IO a)]{Dequeue, blocking if empty.}

@subsection[#:tag "stm-primitives"]{Software transactional memory primitives}

@defproc[(new-tvar    [v a])                (STM (TVar a))]{Allocate a transactional variable.}
@defproc[(read-tvar   [t (TVar a)])         (STM a)]{Read inside a transaction.}
@defproc[(write-tvar  [t (TVar a)] [v a])   (STM Unit)]{Write inside a transaction.}
@defthing[retry       (STM a)]{

Abandons the current transaction and blocks until any TVar it read
has been written to, then restarts.}

@defproc[(or-else     [k1 (STM a)] [k2 (STM a)]) (STM a)]{

If @racket[k1] calls @racket[retry], runs @racket[k2] instead.}

@defproc[(atomically  [k (STM a)]) (IO a)]{

Runs an STM transaction.  On conflict, restarts; the result is
returned exactly once.}

@section[#:tag "monad-helpers"]{Monad-specific helpers}

The @racket[State], @racket[Env], @racket[Writer], and @racket[Except]
families each ship a small set of non-class accessors and runners.
These complement (but do not replace) the polymorphic class methods.

@subsection{State}

@defproc[(run-state    [k (State s a)]) (-> s (Pair s a))]{Unwrap to the underlying function.}
@defproc[(eval-state   [k (State s a)] [s s]) a]{Run and return only the result.}
@defproc[(exec-state   [k (State s a)] [s s]) s]{Run and return only the final state.}
@defthing[get-state    (State s s)]{State-monad-specific @racket[get-st] (also reachable as the class method).}
@defproc[(put-state    [v s]) (State s Unit)]{State-monad-specific @racket[put-st].}
@defproc[(modify-state [f (-> s s)]) (State s Unit)]{State-monad-specific @racket[modify-st].}

@subsection{Env (Reader)}

@defproc[(run-env [k (Env r a)]) (-> r a)]{Unwrap to the underlying function.}
@defthing[ask     (Env r r)]{Env-monad-specific @racket[ask-en].}
@defproc[(local   [f (-> r r)] [k (Env r a)]) (Env r a)]{Env-monad-specific @racket[local-en].}

@subsection{StateT}

@defproc[(run-state-t    [k (StateT s m a)]) (-> s (m (Pair s a)))]{Unwrap.}
@defproc[(eval-state-t   [k (StateT s m a)] [s s]) (m a)]{Run; project the result.}
@defproc[(exec-state-t   [k (StateT s m a)] [s s]) (m s)]{Run; project the final state.}
@defproc[(get-state-t    [inner-pure (-> a (m a))]) (StateT s m s)]{Constructor for the StateT version of @racket[get-st].  The dict arg is inserted automatically at use sites.}
@defproc[(put-state-t    [inner-pure (-> a (m a))] [v s]) (StateT s m Unit)]{Same for @racket[put-st].}
@defproc[(modify-state-t [inner-pure (-> a (m a))] [f (-> s s)]) (StateT s m Unit)]{Same for @racket[modify-st].}
@defproc[(lift-state-t   [ma (m a)]) (StateT s m a)]{Lift an inner action.}

@subsection{EnvT}

@defproc[(run-env-t  [k (EnvT r m a)]) (-> r (m a))]{Unwrap.}
@defproc[(ask-t      [inner-pure (-> a (m a))]) (EnvT r m r)]{EnvT @racket[ask-en].}
@defproc[(local-t    [f (-> r r)] [k (EnvT r m a)]) (EnvT r m a)]{EnvT @racket[local-en].}
@defproc[(lift-env-t [ma (m a)]) (EnvT r m a)]{Lift an inner action.}

@subsection{WriterT}

@defproc[(run-writer-t   [k (WriterT w m a)]) (m (Pair w a))]{Unwrap.}
@defproc[(eval-writer-t  [k (WriterT w m a)]) (m a)]{Project the result.}
@defproc[(exec-writer-t  [k (WriterT w m a)]) (m w)]{Project the log.}
@defproc[(tell           [inner-pure (-> a (m a))] [w w]) (WriterT w m Unit)]{WriterT-specific @racket[tell-w].}
@defproc[(lift-writer-t  [inner-mempty w] [ma (m a)]) (WriterT w m a)]{Lift an inner action.}

@subsection{ExceptT}

@defproc[(run-except-t   [k (ExceptT e m a)]) (m (Result e a))]{Unwrap.}
@defproc[(throw-error    [inner-pure (-> a (m a))] [e e]) (ExceptT e m a)]{ExceptT-specific @racket[throw-e].}
@defproc[(catch-error    [inner-pure (-> a (m a))] [k (ExceptT e m a)] [h (-> e (ExceptT e m a))]) (ExceptT e m a)]{ExceptT-specific @racket[catch-e].}
@defproc[(lift-except-t  [ma (m a)]) (ExceptT e m a)]{Lift an inner action.}

@subsection{Identity}

@defproc[(run-identity [k (Identity a)]) a]{Unwrap the identity monad.}

@section[#:tag "optics-helpers"]{Optics primitives}

@subsection{Lenses}

@defproc[(view [l (Lens s a)] [s s])              a]{Read the focused value.}
@defproc[(set  [l (Lens s a)] [v a] [s s])        s]{Replace the focused value.}
@defproc[(over [l (Lens s a)] [f (-> a a)] [s s]) s]{Modify the focused value.}
@defproc[(lens-compose [outer (Lens s a)] [inner (Lens a b)]) (Lens s b)]{Compose two lenses.}

@subsection{Prisms}

@defproc[(preview [p (Prism s a)] [s s]) (Maybe a)]{Try to extract.}
@defproc[(review  [p (Prism s a)] [a a]) s]{Build from a value.}

@subsection{Traversals}

@defproc[(to-list-of [t (Traversal s a)] [s s]) (List a)]{Collect all focused values.}
@defproc[(over-of    [t (Traversal s a)] [f (-> a a)] [s s]) s]{Modify all focused values.}
@defthing[list-traversal (Traversal (List a) a)]{Walks every element of a list.}
@defproc[(lens-as-traversal [l (Lens s a)]) (Traversal s a)]{Treats a lens as a single-element traversal.}

@section[#:tag "inspection"]{Compiler inspection}

These accessors return the most recent elaboration's optimisation log;
they are exposed primarily for tests that need to assert which call
sites were monomorphized or inlined.

@defproc[(rackton-monomorphized-sites) list?]{

Returns the list of @racket[(method . impl)] pairs that the elaborator
resolved at compile time during the most recent elaboration.}

@defproc[(rackton-inlined-sites) list?]{

Returns the list of call sites whose impl bodies the codegen
substituted in place during the most recent elaboration.}

@section[#:tag "system"]{System interface}

@defproc[(random-integer    [lo Integer] [hi Integer]) (IO Integer)]{Uniformly random integer in the half-open range from @racket[lo] (inclusive) to @racket[hi] (exclusive).}
@defthing[random-float      (IO Float)]{Uniformly random @racket[Float] in the half-open range from @racket[0.0] (inclusive) to @racket[1.0] (exclusive).}
@defthing[current-time-seconds (IO Integer)]{Unix epoch seconds.}
@defproc[(list-directory    [path String]) (IO (List String))]{Directory listing.}
@defthing[argv              (IO (List String))]{Command-line arguments.}
@defproc[(getenv            [name String]) (IO (Maybe String))]{

Reads an environment variable.  @racket[None] if unset.}

@defproc[(delete-file       [path String]) (IO Unit)]{Removes a file.}
@defproc[(make-directory    [path String]) (IO Unit)]{Creates a directory.}
