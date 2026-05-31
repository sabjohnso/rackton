#lang scribble/manual
@require[scribble/manual]

@title[#:tag "stdlib"]{Standard library modules}

The auto-prelude is deliberately small — roughly the size of Haskell's
@tt{Prelude}: type classes, the core ADTs, the numeric tower, core
@secref["io"], and basic list/combinator helpers.  Everything else
lives in importable modules under @tt{rackton/}, grouped into families
that mirror Haskell's @tt{base} layout.  Import a module with the
@racket[require] surface form inside a Rackton block:

@codeblock|{
#lang rackton
(require rackton/data/map)
(require rackton/control/monad/state)
}|

Type-class @emph{instances} always escape a module regardless of its
@secref["provide-specs"] form (instance coherence is a global
property), so importing a module makes both its bindings and its
instances available.

@section[#:tag "stdlib-data"]{@tt{rackton/data} — containers and structures}

@defmodule[rackton/data/maybe #:no-declare]
@racket[Maybe] eliminators and helpers beyond the @secref["classes"]
methods: @racket[maybe], @racket[from-maybe], @racket[from-just],
@racket[is-just]/@racket[is-nothing], @racket[map-maybe],
@racket[cat-maybes], @racket[maybe->list], @racket[list->maybe].

@defmodule[rackton/data/either #:no-declare]
Data.Either over @racket[Result] (@racket[Err] = Left, @racket[Ok] =
Right): the @racket[either] eliminator, @racket[is-ok]/@racket[is-err],
@racket[from-ok]/@racket[from-err], @racket[oks]/@racket[errs],
@racket[partition-results], and @racket[Maybe] interop.

@defmodule[rackton/data/bool #:no-declare]
@racket[bool] (the @racket[Boolean] eliminator) and @racket[otherwise].

@defmodule[rackton/data/function #:no-declare]
@racket[on] and @racket[apply-to] (id / const / flip / compose are in
the prelude).

@defmodule[rackton/data/ord #:no-declare]
@racket[clamp], @racket[min-by], @racket[max-by] (min / max /
comparisons are prelude Ord methods).

@defmodule[rackton/data/functor #:no-declare]
@racket[const-map] (@tt{<$}), @racket[fmap-flipped] (@tt{<&>}), and
@racket[const-replace-flipped] (@tt{$>}).

@defmodule[rackton/data/foldable #:no-declare]
Generic folds over any @racket[Foldable]: @racket[fold-map] (foldMap),
@racket[fold], @racket[any-of], @racket[all-of], @racket[elem-of].

@defmodule[rackton/data/traversable #:no-declare]
@racket[sequence-a] (sequenceA) and @racket[for-t] (@racket[traverse]
is the prelude class method).

@defmodule[rackton/data/char #:no-declare]
Data.Char predicates (@racket[digit?], @racket[upper?], @racket[lower?],
@racket[alpha?], @racket[alpha-num?], @racket[hex-digit?],
@racket[space?], @racket[control?], @racket[punctuation?]) and
conversions (@racket[ord], @racket[chr], @racket[to-upper],
@racket[to-lower], @racket[digit->int], @racket[int->digit]).

@defmodule[rackton/data/list #:no-declare]
The extended list combinators (@racket[sort], @racket[zip],
@racket[take], @racket[drop], @racket[find], @racket[concat-map],
@racket[group-by], …).  The core list operations (@racket[reverse],
@racket[append], @racket[filter]) stay in the prelude.

@defmodule[rackton/data/list/nonempty #:no-declare]
The @racket[NonEmpty] type (total @racket[ne-head]/@racket[ne-tail])
with @racket[nonempty], @racket[ne-to-list]/@racket[ne-from-list],
@racket[ne-cons], @racket[ne-map], @racket[ne-length].

@defmodule[rackton/data/tuple #:no-declare]
@racket[Pair] helpers: @racket[swap], @racket[curry], @racket[uncurry].

@defmodule[rackton/data/map #:no-declare]
The immutable @racket[Map] type and its operations
(@racket[map-insert]/@racket[map-lookup]/…, @racket[map-member?],
@racket[map-singleton], @racket[map-from-list]/@racket[map-to-list],
@racket[map-adjust], @racket[map-insert-with],
@racket[map-union]/@racket[map-union-with], @racket[map-difference],
@racket[map-intersection-with], @racket[map-map]/@racket[map-map-with-key],
@racket[map-filter]/@racket[map-filter-with-key],
@racket[map-find-with-default]).

@defmodule[rackton/data/set #:no-declare]
The immutable @racket[Set] type and its operations
(@racket[set-insert]/@racket[set-member?]/…, @racket[set-singleton],
@racket[set-from-list],
@racket[set-union]/@racket[set-intersection]/@racket[set-difference],
@racket[set-subset?]/@racket[set-disjoint?],
@racket[set-map]/@racket[set-filter]/@racket[set-foldr]).

@defmodule[rackton/data/monoid #:no-declare]
The @racket[Sum] / @racket[Product] (numeric), @racket[All] /
@racket[Any] (Boolean), @racket[Endo] (functions under composition, via
@racket[app-endo]), and @racket[Dual] (a Semigroup with its arguments
flipped, via @racket[get-dual]) @racket[Monoid] newtypes.

@defmodule[rackton/data/semigroup #:no-declare]
The @racket[Min] / @racket[Max] / @racket[First] / @racket[Last]
selection @racket[Semigroup] newtypes.

@defmodule[rackton/data/bits #:no-declare]
Bitwise operations over @racket[Integer]:
@racket[bit-and]/@racket[bit-or]/@racket[bit-xor]/@racket[bit-not],
@racket[bit-shift-left]/@racket[bit-shift-right],
@racket[bit-test]/@racket[bit-set]/@racket[bit-clear],
@racket[bit-count].  @racket[bit-]-prefixed so racket/base's
@racket[bitwise-and] stays reachable inside @racket[(racket …)] escapes.

@defmodule[rackton/data/ratio #:no-declare]
Derived @racket[Rational] ops: @racket[ratio], @racket[recip],
@racket[to-float], and @racket[approx-rational] (the simplest
@racket[Rational] within a given tolerance of a @racket[Float], i.e.
Numeric's @tt{approxRational}).  (The type and
@racket[make-rational]/@racket[numerator]/@racket[denominator] are in
the prelude.)

@defmodule[rackton/data/complex #:no-declare]
Derived @racket[Complex] ops: @racket[conjugate], @racket[phase],
@racket[mk-polar], @racket[cis], @racket[polar] (the type and
@racket[make-complex] / @racket[real-part] / @racket[imag-part] /
@racket[magnitude] are in the prelude).

@defmodule[rackton/data/lens #:no-declare]
The optics primitives: @racket[Lens], @racket[Prism],
@racket[Traversal] and their combinators.

@section[#:tag "stdlib-control"]{@tt{rackton/control} — applicative, monad, transformers}

@defmodule[rackton/control/applicative #:no-declare]
@racket[lift-a3] (pure / fapply / liftA2 / when / unless are in the
prelude).

@defmodule[rackton/control/monad #:no-declare]
Control.Monad combinators over any @racket[(Monad m)]: @racket[map-m]
(mapM), @racket[for-m], @racket[sequence-m], @racket[fold-m] (foldM),
@racket[replicate-m], @racket[filter-m].  (The @racket[Monad] class and
@racket[join]/@racket[when]/@racket[unless]/@racket[void] are in the
prelude.)

@defmodule[rackton/control/stm #:no-declare]
Software transactional memory (@racket[STM], @racket[TVar]).

@defmodule[rackton/control/concurrent #:no-declare]
Threads, @racket[MVar]s, and async channels.

@defmodule[rackton/control/monad/state #:no-declare]
The @racket[State] monad and the @racket[StateT] transformer.

@defmodule[rackton/control/monad/reader #:no-declare]
The @racket[Env] (Reader) monad and the @racket[EnvT] transformer.

@defmodule[rackton/control/monad/writer #:no-declare]
The @racket[WriterT] transformer.

@defmodule[rackton/control/monad/except #:no-declare]
The @racket[ExceptT] transformer.

@defmodule[rackton/control/monad/trans #:no-declare]
@racket[MonadTrans] (@racket[lift]) and @racket[MonadIO]
(@racket[lift-io]) instances for the four transformers; re-exports the
whole transformer stack.

The mtl-style classes themselves (@racket[MonadState], @racket[MonadEnv],
@racket[MonadWriter], @racket[MonadError]) and their polymorphic
combinators (@racket[asks], @racket[gets]) stay in the prelude; the
transformer modules supply the instances.  Each transformer module owns
every lifted mtl instance for which it is the @emph{outer} transformer,
so importing it makes its full effect stack available.

@section[#:tag "stdlib-numeric"]{@tt{rackton/numeric} — beyond the tower}

@defmodule[rackton/numeric/integer #:no-declare]
Integral helpers over @racket[Integer]:
@racket[num-even?]/@racket[num-odd?], @racket[num-signum],
@racket[num-gcd]/@racket[num-lcm], @racket[num-factorial],
@racket[num-int-pow], @racket[num-from-integral].  Exports are
@racket[num-]-prefixed so they don't shadow racket/base's
@racket[gcd]/@racket[even?] inside @racket[(racket …)] escapes.

@defmodule[rackton/numeric/real #:no-declare]
Floating/RealFrac extras:
@racket[num-asin]/@racket[num-acos]/@racket[num-atan],
@racket[num-sinh]/@racket[num-cosh]/@racket[num-tanh],
@racket[num-log-base], @racket[num-proper-fraction].

@defmodule[rackton/numeric/natural #:no-declare]
The @racket[Natural] newtype (non-negative @racket[Integer]) with
@racket[Eq]/@racket[Ord]/@racket[Show]/@racket[Num] instances,
@racket[num-to-natural] (checked) and @racket[num-from-natural].

@defmodule[rackton/numeric/show #:no-declare]
Integer radix conversion:
@racket[num-show-hex]/@racket[num-show-oct]/@racket[num-show-bin] and
@racket[num-read-hex]/@racket[num-read-oct]/@racket[num-read-dec] (the
read direction returns @racket[(Maybe Integer)]), and the float
formatters @racket[num-show-f-float] (fixed),
@racket[num-show-e-float] (scientific), @racket[num-show-g-float]
(general) — precision is a @racket[(Maybe Integer)] digit count
(@racket[None] = full).

@defmodule[rackton/numeric/conversions #:no-declare]
Coercions across the tower: @racket[num-integer->float],
@racket[num-float->integer], @racket[num-to-rational],
@racket[num-rational->float], and @racket[num-real-to-frac] (any
@racket[Real] to @racket[Float]).

@section[#:tag "stdlib-text"]{@tt{rackton/text} — strings, formatting, parsing}

@defmodule[rackton/text/string #:no-declare]
String operations: @racket[null-string?], @racket[reverse-string],
@racket[to-upper-string]/@racket[to-lower-string],
@racket[strip]/@racket[strip-start]/@racket[strip-end],
@racket[split-keep], @racket[lines]/@racket[words],
@racket[unlines]/@racket[unwords], the affix predicates
@racket[is-prefix?]/@racket[is-suffix?]/@racket[is-infix?],
@racket[take-string]/@racket[drop-string],
@racket[pad-left]/@racket[pad-right], @racket[repeat-string],
@racket[replace], and the substring splitters @racket[split-on] (keeps
empties) / @racket[break-on] / @racket[index-of].

@defmodule[rackton/text/printf #:no-declare]
Type-safe string formatting (the @tt{formatting}/functional-unparsing
technique, not a runtime format string): the @racket[Format] type, the
directives @racket[fmt-lit] / @racket[fmt-int] / @racket[fmt-flt] /
@racket[fmt-str] / @racket[fmt-show], composition with @racket[fmt-cat],
and @racket[sprintf] to run a format.  Argument types and arity are
checked at compile time.

@defmodule[rackton/text/read #:no-declare]
Parse Strings to typed values: @racket[read-int], @racket[read-float],
@racket[read-bool] (each @racket[(Maybe a)]; @racket[read-bool] accepts
@racket["True"] / @racket["False"]).

@defmodule[rackton/text/show #:no-declare]
ShowS difference-list helpers: @racket[show-string], @racket[show-char],
@racket[shows], @racket[show-paren], and @racket[run-shows] (the
@racket[Show] class and @racket[show] are in the prelude).

@defmodule[rackton/text/bytes #:no-declare]
Derived @racket[Bytes] ops: @racket[bytes-empty], @racket[bytes-null?],
@racket[bytes-take], @racket[bytes-drop], @racket[bytes-split],
@racket[bytes-concat] (the type and
@racket[bytes-length]/@racket[bytes-append]/@racket[bytes->list] etc.
are in the prelude).

@section[#:tag "stdlib-system"]{@tt{rackton/system} — the outside world}

@defmodule[rackton/system #:no-declare]
Umbrella re-exporting the whole System.* family in one import; the
individual modules follow.

@defmodule[rackton/system/ref #:no-declare]
Mutable references in @racket[IO] (Data.IORef): the @racket[Ref] type
with @racket[make-ref]/@racket[read-ref]/@racket[write-ref].

@defmodule[rackton/system/file #:no-declare]
Whole-file I/O: @racket[read-file] / @racket[write-file] /
@racket[append-file].

@defmodule[rackton/system/directory #:no-declare]
@racket[file-exists?] / @racket[delete-file] / @racket[make-directory] /
@racket[list-directory] / @racket[does-directory-exist?] /
@racket[get-current-directory] / @racket[rename-file] / @racket[copy-file]
/ @racket[create-directory-if-missing].

@defmodule[rackton/system/exception #:no-declare]
Exceptions in @racket[IO]: @racket[try] / @racket[raise-io].

@defmodule[rackton/system/random #:no-declare]
@racket[random-integer] / @racket[random-float] / the inclusive
@racket[random-r-integer] / @racket[random-r-float], and a pure
splittable SplitMix64 @racket[StdGen]: @racket[mk-std-gen] /
@racket[next-word] / @racket[random-r] / @racket[split].

@defmodule[rackton/system/time #:no-declare]
@racket[current-time-seconds] / @racket[get-current-time-millis] /
@racket[get-cpu-time-millis].

@defmodule[rackton/system/environment #:no-declare]
@racket[getenv] / @racket[argv] / @racket[get-prog-name] /
@racket[set-env].

@defmodule[rackton/system/exit #:no-declare]
@racket[exit-success] / @racket[exit-failure] / @racket[exit-with] over
the @racket[ExitCode] type.

@defmodule[rackton/system/io #:no-declare]
The @racket[Handle] type, @racket[open-file] over @racket[IOMode],
@racket[h-put-str] / @racket[h-put-str-ln] / @racket[h-get-contents] /
@racket[h-get-line] / @racket[h-close] / @racket[h-flush], the
@racket[stdin] / @racket[stdout] / @racket[stderr] handles,
@racket[get-contents], and @racket[with-file] (an exception-safe
open/run/close bracket).

@section[#:tag "stdlib-foreign"]{@tt{rackton/foreign} — raw memory (unsafe)}

@defmodule[rackton/foreign/ptr #:no-declare]
The Foreign.Ptr / Foreign.Marshal core: the opaque @racket[Ptr] type
(and @racket[CString] = @racket[(Ptr Char)]), raw allocation
(@racket[malloc-bytes] / @racket[free-ptr]), @racket[null-ptr] /
@racket[ptr-null?], byte-offset arithmetic (@racket[ptr-plus]) and the
size constants @racket[size-of-int] / @racket[size-of-double] /
@racket[size-of-ptr], type-specific peek/poke
(@racket[peek-int]/@racket[poke-int],
@racket[peek-double]/@racket[poke-double],
@racket[peek-byte]/@racket[poke-byte]), and C strings
(@racket[string->c-string] / @racket[c-string->string]).

This module is @bold{unsafe}: it does no bounds checking and requires
manual @racket[free-ptr] — a stray offset, a double free, or a
use-after-free corrupts memory or crashes the process, exactly as
Haskell's @tt{Foreign} does.  It is @emph{not} part of @tt{batteries};
require it explicitly, and only when you must touch raw memory.  There
is no @tt{Storable} class, so reads and writes are the type-specific
@racket[peek-int] / @racket[poke-int] / … rather than one polymorphic
pair.

@defmodule[rackton/foreign/c #:no-declare]
A curated set of @tt{libm} functions the prelude's Floating class
doesn't cover, bound through the @racket[foreign] form: @racket[c-cbrt],
@racket[c-hypot], @racket[c-expm1], @racket[c-log1p], @racket[c-tgamma]
(gamma), @racket[c-lgamma], @racket[c-erf], @racket[c-erfc].  It also
documents the general recipe for binding your own C function — a small
Racket @racket[get-ffi-obj] shim imported via @racket[foreign] — which
the inline @racket[foreign-c] form (see @secref["syntax-forms"]) is
sugar over.

@section[#:tag "batteries"]{The @tt{batteries} umbrella}

@defmodule[rackton/batteries #:no-declare]

For convenience, @racketmodname[rackton/batteries] re-exports the whole
standard library — every family above — in one import:

@codeblock|{
#lang rackton
(require rackton/batteries)
}|

Prefer the specific module imports in library code (they make
dependencies explicit and keep compile times down); @tt{batteries} is
handy for scripts and exploration.
