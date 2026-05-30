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
  and helpers beyond the @secref["classes"] methods.}
@item{@racketmodname[rackton/data/monoid] — the @racket[Sum] and
  @racket[Product] @racket[Monoid] newtypes over numbers.}
@item{@racketmodname[rackton/data/list] — the extended list combinators
  (@racket[sort], @racket[zip], @racket[take], @racket[drop],
  @racket[find], @racket[concat-map], @racket[group-by], …).  The core
  list operations (@racket[reverse], @racket[append], @racket[filter])
  stay in the prelude.}
@item{@racketmodname[rackton/data/tuple] — @racket[Pair] helpers such as
  @racket[swap].}
@item{@racketmodname[rackton/data/map] — the immutable @racket[Map] type
  and its operations.}
@item{@racketmodname[rackton/data/set] — the immutable @racket[Set] type
  and its operations.}
@item{@racketmodname[rackton/data/lens] — the optics primitives:
  @racket[Lens], @racket[Prism], @racket[Traversal] and their
  combinators.}
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
@item{@racketmodname[rackton/system] — the system interface: mutable
  references (@racket[Ref]), files, @racket[try] / @racket[raise-io],
  and process/environment access.}
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
