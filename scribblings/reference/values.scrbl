#lang scribble/manual
@require[scribble/manual
         (for-label rackton rackton/data/result)]

@title[#:tag "values"]{Built-in values}
This chapter documents the value bindings that the @emph{prelude} ships —
the ones in scope in every Rackton program without an import: generic
combinators, numeric helpers, strings, characters and bytes, the core
list and pair operations, and core IO.

The larger libraries were moved out of the auto-prelude into importable
modules so the prelude itself stays small; each is documented under its
module in @secref["stdlib"].  In particular: the full @tt{Data.List}
combinators live in @racketmodname[rackton/data/list], tuples in
@racketmodname[rackton/data/tuple], maps in @racketmodname[rackton/data/map],
sets in @racketmodname[rackton/data/set], optics in
@racketmodname[rackton/data/lens], mutable references / files / the system
interface in @racketmodname[rackton/system], threads / MVars / channels in
@racketmodname[rackton/control/concurrent], software transactional memory
in @racketmodname[rackton/control/stm], and the monad-transformer accessors
in @tt{rackton/control/monad}.  Without the matching @racket[require], those
bindings aren't in scope.

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
@racket[Fractional] protocol methods.  See @secref["classes"] for
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

Constructs an inexact complex number with the given real and imaginary
parts.  The literal @racket[3.0+4.0i] is equivalent to
@racket[(make-complex 3.0 4.0)].}

@defproc[(make-complex-exact [re Integer] [im Integer]) ComplexExact]{

Constructs an exact complex number from two @racket[Integer]
components.  The literal @racket[3+4i] is equivalent to
@racket[(make-complex-exact 3 4)].}

@defproc[(real-part-exact [z ComplexExact]) Integer]{
Real component of an exact complex number.}

@defproc[(imag-part-exact [z ComplexExact]) Integer]{
Imaginary component of an exact complex number.}

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

@section[#:tag "symbols"]{Symbols}

In addition to the @racket[Eq] / @racket[Ord] / @racket[Show] instances,
the prelude converts between @racket[Symbol] and @racket[String]:

@defproc[(symbol->string [s Symbol]) String]{

The name of the symbol @racket[s] as text.}

@defproc[(string->symbol [s String]) Symbol]{

Interns @racket[s], returning the symbol with that name.  Inverse of
@racket[symbol->string].}

@section[#:tag "lists"]{Lists}

The @racket[List] type's @racket[Functor] / @racket[Monad] /
@racket[Foldable] / @racket[Traversable] / @racket[Semigroup] /
@racket[Monoid] instances cover most needs.  These additional
operations are specialised to lists and ship in the prelude:

@defproc[(reverse [xs (List a)]) (List a)]{Reverses a list.}

@defproc[(append [xs (List a)] [ys (List a)]) (List a)]{

Concatenates two lists.  See also @racket[mappend] (the @racket[Semigroup]
instance for @racket[List]) which is the polymorphic equivalent.}

@defproc[(filter [p (-> a Boolean)] [xs (List a)]) (List a)]{

Retains only the elements satisfying @racket[p].}

@para{The remaining @tt{Data.List} combinators — @racket[sort],
@racket[zip], @racket[take], @racket[drop], @racket[find],
@racket[concat-map], @racket[group-by], the safe accessors
(@racket[head] / @racket[tail] / @racket[last] / @racket[init]), folds,
scans, set-like operations, and the rest — live in
@racketmodname[rackton/data/list] and are documented under
@secref["stdlib-data"].}

@section[#:tag "pairs"]{Pairs}

@defproc[(fst [p (Pair a b)]) a]{First projection.}
@defproc[(snd [p (Pair a b)]) b]{Second projection.}

@para{@racket[swap] and the rest of the tuple operations live in
@racketmodname[rackton/data/tuple]; see @secref["stdlib-data"].}

@section[#:tag "arrays"]{Arrays}

@para{Fixed-size arrays are built with @racket[array] / @racket[build-array]
and read with @racket[aref] (see @secref["sf-exprs"]); the @racket[Array]
type is described with the other types.  Multidimensional arrays are
nested, and these two operations flatten one level of nesting.}

@deftogether[(
@defproc[(flatten-major [arr (Array n (Array m a))]) (Array (* n m) a)]
@defproc[(flatten-minor [arr (Array n (Array m a))]) (Array (* n m) a)]
)]{

Collapse a nested @racket[(Array n (Array m a))] into a flat
@racket[(Array (* n m) a)].  Both have the same type and differ only in
the order elements are laid out: @racket[flatten-major] is row-major (the
outer index varies slowest, so each inner array is emitted in turn) and
@racket[flatten-minor] is column-major (the outer index varies fastest).}

@deftogether[(
@defproc[(array-map   [f (-> a b)] [arr (Array n a)]) (Array n b)]
@defproc[(array-imap  [f (-> Integer (-> a b))] [arr (Array n a)]) (Array n b)]
@defproc[(array-fold  [f (-> b (-> a b))] [z b] [arr (Array n a)]) b]
@defproc[(array-foldr [f (-> a (-> b b))] [z b] [arr (Array n a)]) b]
)]{

@racket[array-map] applies @racket[f] to every element, preserving the
size; @racket[array-imap] is the indexed variant, where element @racket[i]
of the result is @racket[(f i (aref arr i))].  @racket[array-fold] is a
strict left fold (@racket[f] applied to the accumulator then each element)
and @racket[array-foldr] the corresponding right fold
(@racket[f x0 (f x1 (… (f xⁿ⁻¹ z)))]).  All work at any size, including a
polymorphic @racket[n] — unlike the concrete-size slices
@racket[array-take] / @racket[array-drop] / @racket[array-split-at].}

@defproc[(array-rotate [k Integer] [arr (Array n a)]) (Array n a)]{

Cyclic rotation, preserving the size: result element @racket[i] is input
element @racket[(i + k) mod n].  A positive @racket[k] rotates left
(so @racket[(array-rotate 1 (array 1 2 3))] is @racket[(array 2 3 1)]), a
negative @racket[k] rotates right, and @racket[k] wraps modulo the size.}

@defproc[(array-traverse [f (-> a (f b))] [arr (Array n a)]) (f (Array n b))]{

The @tt{mapM} / @racket[traverse]-style helper: apply an
@racket[Applicative]-effectful @racket[f] to each element and rebuild the
array of the same size inside that applicative (so effects run
left-to-right and, e.g. with @racket[Maybe], short-circuit to
@racket[None] on the first failure).  Requires @racket[(Applicative f)].}

@section[#:tag "maps"]{Immutable Map and Set}

The @racket[Map] and @racket[Set] types and their constructor primitives
are part of the prelude, so the brace/hash-brace literal syntax (below)
needs no import.  The remaining @tt{Data.Map} / @tt{Data.Set} operations
live in @racketmodname[rackton/data/map] and
@racketmodname[rackton/data/set] and are documented under
@secref["stdlib-data"].

@deftogether[(
@defthing[empty-map (Map k v)]
@defproc[(map-insert [k k] [v v] [m (Map k v)]) (Map k v)]
)]{The empty map, and @racket[m] with @racket[v] stored at key @racket[k]
(replacing any existing value).  Keys compare by structural equality, so
no @racket[(Eq k)] constraint is needed.}

@deftogether[(
@defthing[empty-set (Set a)]
@defproc[(set-insert [x a] [s (Set a)]) (Set a)]
)]{The empty set, and @racket[s] with @racket[x] added.}

@subsection[#:tag "container-literals"]{Bracket and brace literals}

Reader-level literal forms build lists, pairs, maps, and sets directly,
distinguished by their bracket shape:

@itemlist[
@item{@litchar|{[v ...]}| — a @racket[List] literal: each @racket[v] is
evaluated and consed onto the next, so @litchar|{[a b c]}| is exactly
@racket[(list a b c)] and @litchar|{[]}| is @racket[Nil].  Bracket list
literals may also appear as @racket[match] patterns (@litchar|{[a b c]}|,
or @litchar|{[x ...]}| to bind the rest of the list).}
@item{@litchar|{[a . b]}| — a @racket[Pair] literal: the dotted square
bracket evaluates both positions, so @litchar|{[a . b]}| is @racket[(Pair a b)]
(and works as a @racket[match] pattern).  Because s-expression dotted
notation only stays a pair when the tail reads as an atom, the tail of a
@litchar|{[a . b]}| / @litchar|{'(a . b)}| literal must be atomic
(@litchar|{[x . (f y)]}| reads as the @emph{list} @litchar|{[x f y]}|); for
a computed second component, use @racket[(Pair a b)] directly.}
@item{@litchar|{{k v ...}}| — a @racket[Map] literal of alternating keys
and values, so @litchar|{{1 "a" 2 "b"}}| builds the two-entry map.  A
duplicate key keeps the last value (last write wins); an odd number of
forms is a compile-time error.  @litchar|{{}}| is @racket[empty-map].}
@item{@litchar|{#{m ...}}| — a @racket[Set] literal, so @litchar|{#{1 2 3}}|
builds the three-element set and duplicate members collapse.  @litchar|{#{}}|
is @racket[empty-set].}
]

Under quotation the same shapes build data: @litchar|{'(a b c)}| is a
@racket[List] of symbols and @litchar|{'(a . b)}| is @racket[(Pair 'a 'b)];
a head unquote escapes, as in @litchar|{`(,x . tag)}|.  The brace shapes
quote the same way: @litchar|{'{A 1 B 2}}| is a @racket[(Map Symbol Integer)]
(keys and values quoted) and @litchar|{'#{A B C}}| is a
@racket[(Set Symbol)], while under quasiquote a @litchar|{,}| escape
evaluates its position — @litchar|{`{,k 1 ,j 2}}| keys the map by the
@emph{values} of @racket[k] and @racket[j], and @litchar|{`#{,x ,y}}|
builds a set of the values of @racket[x] and @racket[y].  As with the
unquoted forms, a quoted map or set is not a @racket[match] pattern.

Ordinary parentheses keep every meaning they had: @racket[(list ...)],
@racket[(Pair a b)], @racket[(array ...)], applications, and special forms
are unchanged.

@section[#:tag "io"]{IO}

@defproc[(pure-io [x a]) (IO a)]{

Lifts @racket[x] into the @racket[IO] monad — performs no side effects
when run.  Equivalent to @racket[pure] specialised to @racket[IO].}

@defproc[(run-io [k (IO a)]) a]{

Executes the action and returns its result.  This is the bridge from
the typed Rackton world to the surrounding Racket runtime; it is
typically called once, at the top level of a program.}

@subsection[#:tag "ref-standard-streams"]{Standard streams}

@defproc[(print    [s String]) (IO Unit)]{Writes @racket[s] without a trailing newline.}
@defproc[(println  [s String]) (IO Unit)]{Writes @racket[s] followed by a newline.}
@defthing[read-line (IO String)]{Reads one line from standard input.}

@para{Mutable references (@racket[Ref]), file IO, and @racket[IO] error
recovery (@racket[try] / @racket[raise-io]) live in
@racketmodname[rackton/system]; the rest of the system interface
(@racket[getenv], @racket[argv], random numbers, the clock, directory
operations) is there too.  All are documented under
@secref["stdlib-system"].}

@section[#:tag "values-concurrency"]{Concurrency and STM}

@para{Threads, @racket[MVar]s, and async channels live in
@racketmodname[rackton/control/concurrent]; @racket[TVar] and the
@racket[STM] monad live in @racketmodname[rackton/control/stm].  Both
are documented under @secref["stdlib-control"]; the
@secref["concurrency" #:doc '(lib "rackton/scribblings/guide/rackton-guide.scrbl")]
chapter of the Guide walks through them.}

@section[#:tag "monad-helpers"]{Monad-specific helpers}

@para{The @racket[State], @racket[Env], @racket[Writer], and
@racket[Except] families each ship a small set of non-method accessors and
runners (@racket[run-state], @racket[eval-state], @racket[get-state],
@racket[ask], @racket[tell], @racket[throw-error], and the transformer
variants).  These live in @tt{rackton/control/monad} —
@racketmodname[rackton/control/monad/state],
@racketmodname[rackton/control/monad/reader],
@racketmodname[rackton/control/monad/writer], and
@racketmodname[rackton/control/monad/except] — and are documented under
@secref["stdlib-control"].}

@subsection{Identity}

The @racket[Identity] runner stays in the prelude.

@defproc[(run-identity [k (Identity a)]) a]{Unwrap the identity monad.}

@section[#:tag "optics-helpers"]{Optics primitives}

@para{Lenses, prisms, and traversals (@racket[view], @racket[set],
@racket[over], @racket[preview], @racket[review], @racket[to-list-of], …)
live in @racketmodname[rackton/data/lens] and are documented under
@secref["stdlib-data"]; the
@secref["optics" #:doc '(lib "rackton/scribblings/guide/rackton-guide.scrbl")]
chapter of the Guide introduces them.}

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
