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
  ops: @racket[ratio], @racket[recip], @racket[to-float] (the type and
  @racket[make-rational]/@racket[numerator]/@racket[denominator] are in
  the prelude).}
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
  @racket[Product] (numeric) and @racket[All] / @racket[Any] (Boolean)
  @racket[Monoid] newtypes.}
@item{@racketmodname[rackton/data/semigroup] — the @racket[Min] /
  @racket[Max] / @racket[First] / @racket[Last] selection
  @racket[Semigroup] newtypes.}
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
@item{@racketmodname[rackton/system] — the system interface: mutable
  references (@racket[Ref]), files, @racket[try] / @racket[raise-io],
  and process/environment access.}
@item{@racketmodname[rackton/text/string] — String operations:
  @racket[null-string?], @racket[reverse-string],
  @racket[to-upper-string]/@racket[to-lower-string],
  @racket[strip]/@racket[strip-start]/@racket[strip-end],
  @racket[split-keep], @racket[lines]/@racket[words],
  @racket[unlines]/@racket[unwords].}
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
