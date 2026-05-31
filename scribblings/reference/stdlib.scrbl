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

@section[#:tag "stdlib-families"]{The families}

@itemlist[
@item{@racketmodname[rackton/data/maybe] — @racket[Maybe] eliminators
  and helpers beyond the @secref["classes"] methods: @racket[maybe],
  @racket[from-maybe], @racket[from-just], @racket[is-just]/
  @racket[is-nothing], @racket[map-maybe], @racket[cat-maybes],
  @racket[maybe->list], @racket[list->maybe].}
@item{@racketmodname[rackton/data/either] — Data.Either over
  @racket[Result] (@racket[Err] = Left, @racket[Ok] = Right): the
  @racket[either] eliminator, @racket[is-ok]/@racket[is-err],
  @racket[from-ok]/@racket[from-err], @racket[oks]/@racket[errs],
  @racket[partition-results], and @racket[Maybe] interop.}
@item{@racketmodname[rackton/data/bool] — @racket[bool] (the
  @racket[Boolean] eliminator) and @racket[otherwise].}
@item{@racketmodname[rackton/data/function] — @racket[on] and
  @racket[apply-to] (id / const / flip / compose are in the prelude).}
@item{@racketmodname[rackton/data/ord] — @racket[clamp],
  @racket[min-by], @racket[max-by] (min / max / comparisons are
  prelude Ord methods).}
@item{@racketmodname[rackton/data/functor] — @racket[const-map]
  (@tt{<$}) and @racket[fmap-flipped] (@tt{<&>}).}
@item{@racketmodname[rackton/data/foldable] — generic folds over any
  @racket[Foldable]: @racket[fold-map] (foldMap), @racket[fold],
  @racket[any-of], @racket[all-of], @racket[elem-of].}
@item{@racketmodname[rackton/data/traversable] — @racket[sequence-a]
  (sequenceA) and @racket[for-t] (@racket[traverse] is the prelude
  class method).}
@item{@racketmodname[rackton/data/list/nonempty] — the @racket[NonEmpty]
  type (total @racket[ne-head]/@racket[ne-tail]) with @racket[nonempty],
  @racket[ne-to-list]/@racket[ne-from-list], @racket[ne-cons],
  @racket[ne-map], @racket[ne-length].}
@item{@racketmodname[rackton/data/ratio] — derived @racket[Rational]
  ops: @racket[ratio], @racket[recip], @racket[to-float], and
  @racket[approx-rational] (the simplest @racket[Rational] within a
  given tolerance of a @racket[Float], i.e. Numeric's
  @tt{approxRational}).  (The type and
  @racket[make-rational]/@racket[numerator]/@racket[denominator] are in
  the prelude.)}
@item{@racketmodname[rackton/data/complex] — derived @racket[Complex]
  ops: @racket[conjugate], @racket[phase], @racket[mk-polar],
  @racket[cis], @racket[polar] (the type and @racket[make-complex] /
  @racket[real-part] / @racket[imag-part] / @racket[magnitude] are in
  the prelude).}
@item{@racketmodname[rackton/control/applicative] — @racket[lift-a3]
  (pure / fapply / liftA2 / when / unless are in the prelude).}
@item{@racketmodname[rackton/data/char] — Data.Char predicates
  (@racket[digit?], @racket[upper?], @racket[lower?], @racket[alpha?],
  @racket[alpha-num?], @racket[hex-digit?], @racket[space?],
  @racket[control?], @racket[punctuation?]) and conversions
  (@racket[ord], @racket[chr], @racket[to-upper], @racket[to-lower],
  @racket[digit->int], @racket[int->digit]).}
@item{@racketmodname[rackton/data/monoid] — the @racket[Sum] /
  @racket[Product] (numeric), @racket[All] / @racket[Any] (Boolean),
  @racket[Endo] (functions under composition, via @racket[app-endo]),
  and @racket[Dual] (a Semigroup with its arguments flipped, via
  @racket[get-dual]) @racket[Monoid] newtypes.}
@item{@racketmodname[rackton/data/semigroup] — the @racket[Min] /
  @racket[Max] / @racket[First] / @racket[Last] selection
  @racket[Semigroup] newtypes.}
@item{@racketmodname[rackton/data/bits] — bitwise operations over
  @racket[Integer]: @racket[bit-and]/@racket[bit-or]/@racket[bit-xor]/
  @racket[bit-not], @racket[bit-shift-left]/@racket[bit-shift-right],
  @racket[bit-test]/@racket[bit-set]/@racket[bit-clear],
  @racket[bit-count].  @racket[bit-]-prefixed so racket/base's
  @racket[bitwise-and] stays reachable inside @racket[(racket …)]
  escapes.}
@item{@racketmodname[rackton/data/list] — the extended list combinators
  (@racket[sort], @racket[zip], @racket[take], @racket[drop],
  @racket[find], @racket[concat-map], @racket[group-by], …).  The core
  list operations (@racket[reverse], @racket[append], @racket[filter])
  stay in the prelude.}
@item{@racketmodname[rackton/data/tuple] — @racket[Pair] helpers such as
  @racket[swap].}
@item{@racketmodname[rackton/data/map] — the immutable @racket[Map] type
  and its operations (@racket[map-insert]/@racket[map-lookup]/…,
  @racket[map-member?], @racket[map-singleton], @racket[map-from-list]/
  @racket[map-to-list], @racket[map-adjust], @racket[map-insert-with],
  @racket[map-union]/@racket[map-union-with], @racket[map-difference],
  @racket[map-intersection-with], @racket[map-map]/@racket[map-map-with-key],
  @racket[map-filter]/@racket[map-filter-with-key],
  @racket[map-find-with-default]).}
@item{@racketmodname[rackton/data/set] — the immutable @racket[Set] type
  and its operations (@racket[set-insert]/@racket[set-member?]/…,
  @racket[set-singleton], @racket[set-from-list],
  @racket[set-union]/@racket[set-intersection]/@racket[set-difference],
  @racket[set-subset?]/@racket[set-disjoint?],
  @racket[set-map]/@racket[set-filter]/@racket[set-foldr]).}
@item{@racketmodname[rackton/data/lens] — the optics primitives:
  @racket[Lens], @racket[Prism], @racket[Traversal] and their
  combinators.}
@item{@racketmodname[rackton/control/monad] — Control.Monad
  combinators over any @racket[(Monad m)]: @racket[map-m] (mapM),
  @racket[for-m], @racket[sequence-m], @racket[fold-m] (foldM),
  @racket[replicate-m], @racket[filter-m].  (The @racket[Monad] class
  and @racket[join]/@racket[when]/@racket[unless]/@racket[void] are in
  the prelude.)}
@item{@racketmodname[rackton/control/stm] — software transactional
  memory (@racket[STM], @racket[TVar]).}
@item{@racketmodname[rackton/control/concurrent] — threads,
  @racket[MVar]s, and async channels.}
@item{@racketmodname[rackton/control/monad/state] — the @racket[State]
  monad and the @racket[StateT] transformer.}
@item{@racketmodname[rackton/control/monad/reader] — the @racket[Env]
  (Reader) monad and the @racket[EnvT] transformer.}
@item{@racketmodname[rackton/control/monad/writer] — the @racket[WriterT]
  transformer.}
@item{@racketmodname[rackton/control/monad/except] — the @racket[ExceptT]
  transformer.}
@item{@racketmodname[rackton/control/monad/trans] — @racket[MonadTrans]
  (@racket[lift]) and @racket[MonadIO] (@racket[lift-io]) instances for
  the four transformers; re-exports the whole transformer stack.}
@item{@racketmodname[rackton/system] — umbrella re-exporting the whole
  System.* family in one import.  The family is split into:
  @racketmodname[rackton/system/ref] (mutable @racket[Ref]s),
  @racketmodname[rackton/system/file] (@racket[read-file] /
  @racket[write-file]),
  @racketmodname[rackton/system/directory] (@racket[file-exists?] /
  @racket[delete-file] / @racket[make-directory] /
  @racket[list-directory]),
  @racketmodname[rackton/system/exception] (@racket[try] /
  @racket[raise-io]),
  @racketmodname[rackton/system/random] (@racket[random-integer] /
  @racket[random-float]),
  @racketmodname[rackton/system/time] (@racket[current-time-seconds]),
  @racketmodname[rackton/system/environment] (@racket[getenv] /
  @racket[argv]), @racketmodname[rackton/system/exit]
  (@racket[exit-success] / @racket[exit-failure] / @racket[exit-with]
  over the @racket[ExitCode] type), and @racketmodname[rackton/system/io]
  (the @racket[Handle] type, @racket[open-file] over @racket[IOMode],
  @racket[h-put-str] / @racket[h-put-str-ln] / @racket[h-get-contents] /
  @racket[h-get-line] / @racket[h-close] / @racket[h-flush], the
  @racket[stdin] / @racket[stdout] / @racket[stderr] handles, and
  @racket[get-contents]).}
@item{@racketmodname[rackton/text/string] — String operations:
  @racket[null-string?], @racket[reverse-string],
  @racket[to-upper-string]/@racket[to-lower-string],
  @racket[strip]/@racket[strip-start]/@racket[strip-end],
  @racket[split-keep], @racket[lines]/@racket[words],
  @racket[unlines]/@racket[unwords].}
@item{@racketmodname[rackton/text/printf] — type-safe string formatting
  (the @tt{formatting}/functional-unparsing technique, not a runtime
  format string): the @racket[Format] type, the directives
  @racket[fmt-lit] / @racket[fmt-int] / @racket[fmt-flt] /
  @racket[fmt-str] / @racket[fmt-show], composition with
  @racket[fmt-cat], and @racket[sprintf] to run a format.  Argument
  types and arity are checked at compile time.}
@item{@racketmodname[rackton/text/read] — parse Strings to typed
  values: @racket[read-int], @racket[read-float], @racket[read-bool]
  (each @racket[(Maybe a)]; @racket[read-bool] accepts @racket["True"] /
  @racket["False"]).}
@item{@racketmodname[rackton/text/show] — ShowS difference-list
  helpers: @racket[show-string], @racket[show-char], @racket[shows],
  @racket[show-paren], and @racket[run-shows] (the @racket[Show] class
  and @racket[show] are in the prelude).}
@item{@racketmodname[rackton/text/bytes] — derived @racket[Bytes] ops:
  @racket[bytes-empty], @racket[bytes-null?], @racket[bytes-take],
  @racket[bytes-drop], @racket[bytes-split], @racket[bytes-concat] (the
  type and @racket[bytes-length]/@racket[bytes-append]/@racket[bytes->list]
  etc. are in the prelude).}
@item{@racketmodname[rackton/numeric/integer] — Integral helpers over
  @racket[Integer]: @racket[num-even?]/@racket[num-odd?],
  @racket[num-signum], @racket[num-gcd]/@racket[num-lcm],
  @racket[num-factorial], @racket[num-int-pow],
  @racket[num-from-integral].  Exports are @racket[num-]-prefixed so
  they don't shadow racket/base's @racket[gcd]/@racket[even?] inside
  @racket[(racket …)] escapes.}
@item{@racketmodname[rackton/numeric/real] — Floating/RealFrac extras:
  @racket[num-asin]/@racket[num-acos]/@racket[num-atan],
  @racket[num-sinh]/@racket[num-cosh]/@racket[num-tanh],
  @racket[num-log-base], @racket[num-proper-fraction].}
@item{@racketmodname[rackton/numeric/natural] — the @racket[Natural]
  newtype (non-negative @racket[Integer]) with @racket[Eq]/@racket[Ord]/
  @racket[Show]/@racket[Num] instances, @racket[num-to-natural]
  (checked) and @racket[num-from-natural].}
@item{@racketmodname[rackton/numeric/show] — integer radix conversion:
  @racket[num-show-hex]/@racket[num-show-oct]/@racket[num-show-bin] and
  @racket[num-read-hex]/@racket[num-read-oct]/@racket[num-read-dec]
  (the read direction returns @racket[(Maybe Integer)]), and the float
  formatters @racket[num-show-f-float] (fixed), @racket[num-show-e-float]
  (scientific), @racket[num-show-g-float] (general) — precision is a
  @racket[(Maybe Integer)] digit count (@racket[None] = full).}
@item{@racketmodname[rackton/numeric/conversions] — coercions across
  the tower: @racket[num-integer->float], @racket[num-float->integer],
  @racket[num-to-rational], @racket[num-rational->float], and
  @racket[num-real-to-frac] (any @racket[Real] to @racket[Float]).}
]

The mtl-style classes themselves (@racket[MonadState], @racket[MonadEnv],
@racket[MonadWriter], @racket[MonadError]) and their polymorphic
combinators (@racket[asks], @racket[gets]) stay in the prelude; the
transformer modules supply the instances.  Each transformer module owns
every lifted mtl instance for which it is the @emph{outer} transformer,
so importing it makes its full effect stack available.

@section[#:tag "batteries"]{The @tt{batteries} umbrella}

@defmodule[rackton/batteries]

For convenience, @racketmodname[rackton/batteries] re-exports the whole
standard library — every family above — in one import:

@codeblock|{
#lang rackton
(require rackton/batteries)
}|

Prefer the specific module imports in library code (they make
dependencies explicit and keep compile times down); @tt{batteries} is
handy for scripts and exploration.
