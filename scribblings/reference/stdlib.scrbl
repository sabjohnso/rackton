#lang scribble/manual
@require[scribble/manual
         (for-label rackton
                    rackton/data/map
                    rackton/control/monad/state
                    rackton/batteries)
         "../rackton-eval.rkt"]
@(define ev (make-rackton-eval))
@title[#:tag "stdlib" #:style 'toc]{Standard library modules}

The auto-prelude is deliberately small — roughly the size of Haskell's
@tt{Prelude}: type classes, the core ADTs, the numeric tower, core
@secref["io"], and basic list/combinator helpers.  Everything else
lives in importable modules under @tt{rackton/}, grouped into families
that mirror Haskell's @tt{base} layout.  Import a module with the
@racket[require] surface form inside a Rackton block:

@rackton-example[#:eval ev]{
#lang rackton
(require rackton/data/map)
(require rackton/control/monad/state)
}

Type-class @emph{instances} always escape a module regardless of its
@secref["provide-specs"] form (instance coherence is a global
property), so importing a module makes both its bindings and its
instances available.

The families, each documenting the modules beneath it:

@local-table-of-contents[#:style 'immediate-only]

@include-section["stdlib-data.scrbl"]
@include-section["stdlib-control.scrbl"]
@include-section["stdlib-numeric.scrbl"]
@include-section["stdlib-system.scrbl"]
@include-section["stdlib-text.scrbl"]
@include-section["stdlib-unit.scrbl"]

@section[#:tag "stdlib-foreign"]{@tt{rackton/foreign} — raw memory (unsafe)}

The @tt{foreign} family is Rackton's unsafe interface to raw memory and
C, after Haskell's @tt{Foreign.*}.  It is @emph{not} part of
@tt{batteries}; import a module explicitly and only when you must touch
raw memory.  @racketmodname[rackton/foreign/ptr] is the pointer and
marshalling core, and @racketmodname[rackton/foreign/c] binds a handful
of @tt{libm} functions while documenting the recipe for binding your
own.

@local-table-of-contents[]

@subsection{rackton/foreign/ptr}
@defmodule[rackton/foreign/ptr]
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

@subsection{rackton/foreign/c}
@defmodule[rackton/foreign/c]
A curated set of @tt{libm} functions the prelude's Floating class
doesn't cover, bound through the @racket[foreign] form: @racket[c-cbrt],
@racket[c-hypot], @racket[c-expm1], @racket[c-log1p], @racket[c-tgamma]
(gamma), @racket[c-lgamma], @racket[c-erf], @racket[c-erfc].  It also
documents the general recipe for binding your own C function — a small
Racket @racket[get-ffi-obj] shim imported via @racket[foreign] — which
the inline @racket[foreign-c] form (see @secref["syntax-forms"]) is
sugar over.

@section[#:tag "batteries"]{The @tt{batteries} umbrella}

@defmodule[rackton/batteries]

For convenience, @racketmodname[rackton/batteries] re-exports the whole
standard library — every family above — in one import:

@rackton-example[#:eval ev #:mode 'display]{
#lang rackton
(require rackton/batteries)
}

Prefer the specific module imports in library code (they make
dependencies explicit and keep compile times down); @tt{batteries} is
handy for scripts and exploration.
