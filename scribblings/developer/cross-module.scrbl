#lang scribble/manual
@require[scribble/manual
         (for-label rackton)]

@title[#:tag "cross-module"]{Cross-module type information}

A Rackton file's typing environment is propagated to importers via a
@italic{sidecar submodule}.  This chapter describes how the sidecar
is constructed, what it contains, and how an importing module
recovers the information.

@section{The sidecar submodule}

Every @hash-lang[] @racketmodfont{rackton} (and every
@racket[(module @#,racketidfont{name} rackton …)]) emits a
@racketmodfont{rackton-schemes} submodule alongside the user's code.
This submodule contains five tables, serialised as plain
s-expressions:

@itemlist[
@item{@racket[bindings] — name → scheme (the value environment's
      @racket[vars] table, filtered to provided bindings).}
@item{@racket[data-ctors] — constructor-name → scheme (provided ADT
      constructors).}
@item{@racket[tcons] — type-constructor name → kind (provided
      types).}
@item{@racket[classes] — class name → class declaration (provided
      classes).  The declaration carries each method's signature
      @italic{and} its default body: the defaults are serialised as
      stx-free surface AST (the node set @racket[remap-ast-stx]
      relocates), so an instance written in an importing module falls
      back on the protocol's defaults exactly like one in the protocol's
      own module.  The importer re-anchors every handle with
      @racket[freshen-ast], so the placeholder syntax a decoded node
      carries is always replaced; a default that names a binding private
      to the protocol's module is the one thing that will not resolve
      across the boundary.  The @racket[#:derive] cross-class bodies
      (@racket[super-derives]) are still not carried.}
@item{@racket[instances] — full instance table (always — instances
      ignore @racket[provide]).}
@item{@racket[promoted] — DataKinds-promoted constructor → kind, for the
      promoted constructors of exported data types (gated like
      @racket[data-ctors]).  Promotion is computed once, in the defining
      module's @racket[promote-data]; transporting the result lets an
      importer's kind checker enforce a promoted index (e.g. reject
      @racket[(Mem TInt g)] when @racket[Mem]'s first parameter has kind
      @racket[Stack] but @racket[TInt] has kind @racket[Ty]) rather than
      treat it as a fresh, anything-goes kind.}]

@section{The codec}

@filepath{private/scheme-codec.rkt} implements the serialisation and
deserialisation.  Two requirements:

@itemlist[
@item{@bold{Round-trip exactly.}  An importing module must observe
      the same schemes the exporting module wrote.  Any encoding loss
      would let imported and local code disagree about types — a
      coherence violation.}
@item{@bold{Self-contained.}  The serialised form must not embed
      Racket syntax objects, since those don't survive serialisation.
      Source locations are dropped; identifiers become bare symbols.}]

The codec is tested by a round-trip property in
@filepath{tests/cross-module-test.rkt}: serialise an environment,
deserialise, compare equal.  Any new type or scheme construct must be
added to the codec or the property test breaks.

@section{Import flow}

When the elaborator encounters @racket[(require "file.rkt")]:

@itemlist[#:style 'ordered
@item{The import is performed at the Racket level normally — runtime
      bindings flow as usual.}
@item{The elaborator does @racket[(dynamic-require '(submod "file.rkt"
      rackton-schemes) …)] to load the sidecar.  If the submodule
      doesn't exist, the importee is a plain Racket module; its
      bindings remain invisible to the type checker.}
@item{The decoded tables are folded into the current typing
      environment.  Classes and instances merge with overlap checking
      (see @secref["class-entailment"]).}]

This happens at @italic{module elaboration time} of the importer, not
at runtime.  Type errors in the importer's use of imported bindings
are reported with the offending form's source location, but the
imported scheme's origin (which module it came from) is preserved in
the error message.

@section{Instance coherence across modules}

Because instances always escape regardless of @racket[provide], an
importer can never miss an instance from an imported module.  This is
what makes coherence work: a polymorphic function compiled in module
@racket[A] using a method @racket[==] will dispatch through the same
runtime table regardless of which module instantiates it, because
both @racket[A] and the instantiator see the same registered
instances.

The overlap check at import time prevents pathological cases — if
module @racket[A] and module @racket[B] each register an
@racket[(Eq Foo)] instance and a third module imports both, the third
module's elaboration rejects the import with an overlap error.

@section{What about plain Racket modules?}

A plain Racket module without a @racketmodfont{rackton-schemes}
submodule can still be @racket[require]d from a Rackton module.  Its
exported bindings are usable at runtime, but the type checker has no
schemes for them and will reject any direct use.  Wrap them in a
@racket[(racket τ (vars) …)] escape (see
@secref["racket-interop" #:doc '(lib "rackton/scribblings/guide/rackton-guide.scrbl")])
to provide a manual type assertion.
