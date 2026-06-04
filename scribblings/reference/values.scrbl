#lang scribble/manual
@require[scribble/manual
         (for-label rackton rackton/data/result)]

@title[#:tag "values"]{Built-in values}
This chapter documents the value bindings Rackton ships, grouped by
role: combinators, numeric helpers, strings, characters and bytes,
lists, pairs, immutable maps and sets, optics, IO and effects, and the
several monad-specific accessors that complement the polymorphic
@secref["classes"] entries.

Many of these groups have been moved out of the auto-prelude into
importable modules (see @secref["stdlib"]); the prelude itself stays
small.  Sections below whose bindings now live in a module open with a
@racket[require] note — without that import, the bindings aren't in
scope.  The unmarked sections (combinators, numeric helpers, strings,
characters, core lists, pairs, core IO) are always available.

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

In any @racket[(Monoid a)], folds @racket[xs] with @racket[mappend] and
@racket[mempty].}

@section[#:tag "strings"]{Strings}

In addition to the @racket[Eq] / @racket[Ord] / @racket[Show] /
@racket[Semigroup] / @racket[Monoid] instances, the prelude ships:

@defproc[(string-length [s String]) Integer]{

The length of @racket[s] in characters.}

@defproc[(string-append [a String] [b String]) String]{

Concatenation.  See also @racket[mappend] (the @racket[Semigroup] method)
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

Concatenates two lists.  See also @racket[mappend] (the @racket[Semigroup]
instance for @racket[List]) which is the polymorphic equivalent.}

@defproc[(filter [p (-> a Boolean)] [xs (List a)]) (List a)]{

Retains only the elements satisfying @racket[p].}

@para{@bold{Module} — @racket[reverse], @racket[append], and
@racket[filter] are in the prelude; the remaining combinators below
(@racket[sort], @racket[zip], @racket[take], @racket[drop],
@racket[find], @racket[concat-map], @racket[group-by]) live in
@racketmodname[rackton/data/list]:
@racket[(require rackton/data/list)].}

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

@subsection{More Data.List combinators}

Also in @racketmodname[rackton/data/list].  The partial accessors are
total, returning @racket[Maybe]; @racket[empty?] and @racket[fold-left]
avoid colliding with @racket[racket/base]'s @racket[null?] / @racket[foldl]
(which stay available for @racket[(racket …)] escapes).

@deftogether[(
  @defproc[(head [xs (List a)]) (Maybe a)]
  @defproc[(tail [xs (List a)]) (Maybe (List a))]
  @defproc[(last [xs (List a)]) (Maybe a)]
  @defproc[(init [xs (List a)]) (Maybe (List a))])]{
Safe first / rest / final / all-but-last; @racket[None] on the empty list.}

@defproc[(empty? [xs (List a)]) Boolean]{@racket[#t] iff @racket[xs] is @racket[Nil].}

@deftogether[(
  @defproc[(elem     [x a] [xs (List a)]) Boolean]
  @defproc[(not-elem [x a] [xs (List a)]) Boolean])]{
Membership / non-membership.  Require @racket[(Eq a)].}

@defproc[(lookup [k k] [xs (List (Pair k v))]) (Maybe v)]{
First value paired with @racket[k] in an association list.  Requires @racket[(Eq k)].}

@deftogether[(
  @defproc[(elem-index [x a] [xs (List a)]) (Maybe Integer)]
  @defproc[(find-index [p (-> a Boolean)] [xs (List a)]) (Maybe Integer)])]{
Index of the first matching element.  @racket[elem-index] requires @racket[(Eq a)].}

@defproc[(concat [xss (List (List a))]) (List a)]{Flatten one level.}

@defproc[(intersperse [sep a] [xs (List a)]) (List a)]{Insert @racket[sep] between elements.}

@defproc[(intercalate [sep (List a)] [xss (List (List a))]) (List a)]{
@racket[concat] of @racket[xss] with @racket[sep] between members.}

@defproc[(replicate [n Integer] [x a]) (List a)]{@racket[n] copies of @racket[x].}

@defproc[(range [lo Integer] [hi Integer]) (List Integer)]{
The inclusive integer range @racket[lo]..@racket[hi] (empty if @racket[lo] > @racket[hi]).}

@deftogether[(
  @defproc[(take-while [p (-> a Boolean)] [xs (List a)]) (List a)]
  @defproc[(drop-while [p (-> a Boolean)] [xs (List a)]) (List a)])]{
Longest satisfying prefix / its complement.}

@deftogether[(
  @defproc[(span      [p (-> a Boolean)] [xs (List a)]) (Pair (List a) (List a))]
  @defproc[(break     [p (-> a Boolean)] [xs (List a)]) (Pair (List a) (List a))]
  @defproc[(partition [p (-> a Boolean)] [xs (List a)]) (Pair (List a) (List a))])]{
@racket[span] splits at the first failure; @racket[break] at the first
success; @racket[partition] separates matches from non-matches (order
preserved).}

@defproc[(fold-left [f (-> b (-> a b))] [z b] [xs (List a)]) b]{
Left fold (Haskell's @tt{foldl}); the prelude's @racket[foldr] is the right fold.}

@deftogether[(
  @defproc[(all?     [p (-> a Boolean)] [xs (List a)]) Boolean]
  @defproc[(any?     [p (-> a Boolean)] [xs (List a)]) Boolean]
  @defproc[(and-list [xs (List Boolean)]) Boolean]
  @defproc[(or-list  [xs (List Boolean)]) Boolean])]{
Universal / existential quantification and Boolean-list conjunction / disjunction.}

@deftogether[(
  @defproc[(maximum [xs (List a)]) (Maybe a)]
  @defproc[(minimum [xs (List a)]) (Maybe a)])]{
Largest / smallest element, or @racket[None] when empty.  Require @racket[(Ord a)].}

@defproc[(zip-with [f (-> a (-> b c))] [as (List a)] [bs (List b)]) (List c)]{
@racket[zip] generalised with a combining function; truncates to the shorter list.}

@defproc[(unzip [xs (List (Pair a b))]) (Pair (List a) (List b))]{Inverse of @racket[zip].}

@defproc[(nub [xs (List a)]) (List a)]{
Remove duplicates, keeping first occurrences.  Requires @racket[(Eq a)].}

@defproc[(nub-by [eq? (-> a (-> a Boolean))] [xs (List a)]) (List a)]{
@racket[nub] with a caller-supplied equality.}

@deftogether[(
  @defproc[(scanl [f (-> b (-> a b))] [z b] [xs (List a)]) (List b)]
  @defproc[(scanr [f (-> a (-> b b))] [z b] [xs (List a)]) (List b)])]{
Left / right folds that return all intermediate accumulators.}

@defproc[(group [xs (List a)]) (List (List a))]{
Runs of consecutive equal elements.  Requires @racket[(Eq a)].}

@deftogether[(
  @defproc[(inits [xs (List a)]) (List (List a))]
  @defproc[(tails [xs (List a)]) (List (List a))])]{
All prefixes (shortest first) / all suffixes (longest first).}

@deftogether[(
  @defproc[(prefix? [ps (List a)] [xs (List a)]) Boolean]
  @defproc[(suffix? [ss (List a)] [xs (List a)]) Boolean]
  @defproc[(infix?  [ns (List a)] [xs (List a)]) Boolean])]{
Sublist tests (Haskell @tt{isPrefixOf} / @tt{isSuffixOf} / @tt{isInfixOf}).
Require @racket[(Eq a)].}

@defproc[(strip-prefix [ps (List a)] [xs (List a)]) (Maybe (List a))]{
Drop @racket[ps] from the front of @racket[xs], or @racket[None] if it
isn't a prefix.  Requires @racket[(Eq a)].}

@defproc[(transpose [xss (List (List a))]) (List (List a))]{
Transpose rows and columns (ragged-aware).}

@deftogether[(
  @defproc[(delete          [x a] [xs (List a)]) (List a)]
  @defproc[(list-difference [xs (List a)] [ys (List a)]) (List a)]
  @defproc[(union           [xs (List a)] [ys (List a)]) (List a)]
  @defproc[(intersect       [xs (List a)] [ys (List a)]) (List a)])]{
Remove the first occurrence / list difference / union / intersection,
by element equality.  Require @racket[(Eq a)].}

@defproc[(insert [x a] [xs (List a)]) (List a)]{
Insert @racket[x] into a sorted list before the first greater element.
Requires @racket[(Ord a)].}

@deftogether[(
  @defproc[(sort-by [lt? (-> a (-> a Boolean))] [xs (List a)]) (List a)]
  @defproc[(sort-on [key (-> a b)] [xs (List a)]) (List a)])]{
Stable merge sort by a strict-less-than comparator / by an @racket[(Ord b)] key.}

@deftogether[(
  @defproc[(foldl1 [f (-> a (-> a a))] [xs (List a)]) (Maybe a)]
  @defproc[(foldr1 [f (-> a (-> a a))] [xs (List a)]) (Maybe a)])]{
Seedless left / right folds; @racket[None] on the empty list.}

@deftogether[(
  @defproc[(iterate-n [n Integer] [f (-> a a)] [x a]) (List a)]
  @defproc[(cycle-n   [n Integer] [xs (List a)]) (List a)])]{
Bounded @tt{iterate} (@racket[n] applications of @racket[f] from
@racket[x]) / @racket[n] copies of @racket[xs].}

@defproc[(unfoldr [f (-> b (Maybe (Pair a b)))] [seed b]) (List a)]{
Dual of a fold: build a list from a seed until @racket[f] yields @racket[None].}

@deftogether[(
  @defproc[(subsequences [xs (List a)]) (List (List a))]
  @defproc[(permutations [xs (List a)]) (List (List a))])]{
All subsequences (the power set) / all orderings.}

@defproc[(map-accum-l [f (-> s (-> a (Pair s b)))] [s s] [xs (List a)]) (Pair s (List b))]{
Left-to-right map threading an accumulator (Haskell @tt{mapAccumL}).}

@section[#:tag "pairs"]{Pairs}

@defproc[(fst  [p (Pair a b)]) a]{First projection.}
@defproc[(snd  [p (Pair a b)]) b]{Second projection.}
@defproc[(swap [p (Pair a b)]) (Pair b a)]{Swaps the two fields.  In
@racketmodname[rackton/data/tuple]: @racket[(require rackton/data/tuple)].
@racket[fst] / @racket[snd] stay in the prelude.}

@section[#:tag "maps"]{Immutable Map}

@para{@bold{Module} — @racket[(require rackton/data/map)].}

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

@para{@bold{Module} — @racket[(require rackton/data/set)].}

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

@para{@bold{Module} — mutable references, files, and error recovery live
in @racketmodname[rackton/system]: @racket[(require rackton/system)].}

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

@para{@bold{Module} — threads, @racket[MVar]s, and async channels live
in @racketmodname[rackton/control/concurrent]
(@racket[(require rackton/control/concurrent)]); the
@secref["stm-primitives"] live in @racketmodname[rackton/control/stm].}

These primitives provide direct, non-polymorphic concurrency control
for code that is fixed to @racket[IO].  They live alongside — not
under — the @racket[Concurrent] class: a @racket[ThreadId] from
@racket[fork-io] is just a join handle, while a @racket[Future] from
the @racket[Concurrent] class's @racket[fork-c] carries a result
value.  Reach for the @racket[Concurrent] methods when a piece of
code must run polymorphically over any forkable monad; reach for
these primitives when @racket[IO] is the only target.

@subsection{Threads}

@defproc[(fork-io    [k (IO a)]) (IO ThreadId)]{

Spawns an OS-level thread running @racket[k]; returns a
@racket[ThreadId] for joining.  The thread's result value is
discarded; use the @racket[Concurrent] class's @racket[fork-c] /
@racket[await-c] if you need to recover it.}

@defproc[(wait-thread [t ThreadId]) (IO Unit)]{

Blocks until @racket[t] terminates.}

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

@para{@bold{Module} — @racket[(require rackton/control/stm)].}

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

@para{@bold{Modules} — these all live in
@tt{rackton/control/monad}: @racket[State] and @racket[StateT]
in @racketmodname[rackton/control/monad/state], @racket[Env] and
@racket[EnvT] in @racketmodname[rackton/control/monad/reader],
@racket[WriterT] in @racketmodname[rackton/control/monad/writer], and
@racket[ExceptT] in @racketmodname[rackton/control/monad/except].  The
@racket[Identity] runner stays in the prelude.}

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

These accessors require @racket[(Applicative m)] (for
@racket[get-state-t] / @racket[put-state-t] / @racket[modify-state-t])
or @racket[(Monad m)] (for @racket[lift-state-t]); the elaborator
inserts the dictionary at each call site, so user code sees the
signatures shown below.

@defproc[(run-state-t    [k (StateT s m a)]) (-> s (m (Pair s a)))]{Unwrap.}
@defproc[(eval-state-t   [k (StateT s m a)] [s s]) (m a)]{Run; project the result.}
@defproc[(exec-state-t   [k (StateT s m a)] [s s]) (m s)]{Run; project the final state.}
@defthing[get-state-t    (StateT s m s)]{StateT version of @racket[get-st].}
@defproc[(put-state-t    [v s]) (StateT s m Unit)]{StateT version of @racket[put-st].}
@defproc[(modify-state-t [f (-> s s)]) (StateT s m Unit)]{StateT version of @racket[modify-st].}
@defproc[(lift-state-t   [ma (m a)]) (StateT s m a)]{Lift an inner action.}

@subsection{EnvT}

@racket[ask-t] requires @racket[(Applicative m)]; @racket[local-t]
and @racket[lift-env-t] require @racket[(Functor m)].

@defproc[(run-env-t  [k (EnvT r m a)]) (-> r (m a))]{Unwrap.}
@defthing[ask-t      (EnvT r m r)]{EnvT version of @racket[ask-en].}
@defproc[(local-t    [f (-> r r)] [k (EnvT r m a)]) (EnvT r m a)]{EnvT version of @racket[local-en].}
@defproc[(lift-env-t [ma (m a)]) (EnvT r m a)]{Lift an inner action.}

@subsection{WriterT}

@racket[tell] requires @racket[(Applicative m)]; @racket[lift-writer-t]
requires both @racket[(Functor m)] and @racket[(Monoid w)].

@defproc[(run-writer-t   [k (WriterT w m a)]) (m (Pair w a))]{Unwrap.}
@defproc[(eval-writer-t  [k (WriterT w m a)]) (m a)]{Project the result.}
@defproc[(exec-writer-t  [k (WriterT w m a)]) (m w)]{Project the log.}
@defproc[(tell           [w w]) (WriterT w m Unit)]{WriterT version of @racket[tell-w].}
@defproc[(lift-writer-t  [ma (m a)]) (WriterT w m a)]{Lift an inner action.}

@subsection{ExceptT}

@racket[throw-error] requires @racket[(Applicative m)];
@racket[catch-error] requires @racket[(Monad m)]; @racket[lift-except-t]
requires @racket[(Functor m)].

@defproc[(run-except-t   [k (ExceptT e m a)]) (m (Result e a))]{Unwrap.}
@defproc[(throw-error    [e e]) (ExceptT e m a)]{ExceptT version of @racket[throw-e].}
@defproc[(catch-error    [k (ExceptT e m a)] [h (-> e (ExceptT e m a))]) (ExceptT e m a)]{ExceptT version of @racket[catch-e].}
@defproc[(lift-except-t  [ma (m a)]) (ExceptT e m a)]{Lift an inner action.}

@subsection{Identity}

@defproc[(run-identity [k (Identity a)]) a]{Unwrap the identity monad.}

@section[#:tag "optics-helpers"]{Optics primitives}

@para{@bold{Module} — @racket[(require rackton/data/lens)].}

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

@para{@bold{Module} — @racket[(require rackton/system)].}

@defproc[(random-integer    [lo Integer] [hi Integer]) (IO Integer)]{Uniformly random integer in the half-open range from @racket[lo] (inclusive) to @racket[hi] (exclusive).}
@defthing[random-float      (IO Float)]{Uniformly random @racket[Float] in the half-open range from @racket[0.0] (inclusive) to @racket[1.0] (exclusive).}
@defthing[current-time-seconds (IO Integer)]{Unix epoch seconds.}
@defproc[(list-directory    [path String]) (IO (List String))]{Directory listing.}
@defthing[argv              (IO (List String))]{Command-line arguments.}
@defproc[(getenv            [name String]) (IO (Maybe String))]{

Reads an environment variable.  @racket[None] if unset.}

@defproc[(delete-file       [path String]) (IO Unit)]{Removes a file.}
@defproc[(make-directory    [path String]) (IO Unit)]{Creates a directory.}
