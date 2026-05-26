#lang scribble/manual
@require[scribble/manual
         (for-label rackton)]

@title[#:tag "codegen"]{Code generation}

@filepath{private/codegen.rkt} lowers the typed-core AST to Racket
syntax.  By the time codegen runs, the inferer has resolved every
class-method call and recorded the resolutions in shared parameters;
the codegen's job is to read those resolutions and emit appropriate
Racket.

@section{Erasure}

Type information is fully erased.  The generated Racket has no
runtime type tags beyond what Racket structs already provide.  An
inferred @racket[Integer] is a Racket exact integer; an inferred
@racket[(Maybe a)] is a @racket[None] or @racket[Some] struct
instance; an inferred function type compiles to a Racket procedure.

This means a Rackton value cannot be inspected for its Rackton type
at runtime — there are no @racket[isInteger?] or similar predicates.
The type checker has already done that work.

@section{ADT lowering}

A @racket[define-data] form expands to a series of
@racketidfont{define-data-ctor} calls in @filepath{private/adt.rkt}.
@racketidfont{define-data-ctor} is a macro that:

@itemlist[#:style 'ordered
@item{Declares a Racket @racket[struct] with the appropriate
      arity.}
@item{Binds the constructor name to either the struct value (for
      nullary constructors) or a curried function that applies the
      struct constructor (for n-ary).}
@item{Registers a @racket[dispatch-tag] for the struct so the class
      dispatchers can index off it.}]

This makes constructors first-class: @racket[Cons] is an ordinary
Racket procedure, not a macro, so it can be passed to higher-order
functions and stored in data structures.

@section{Class method lowering}

@filepath{private/dict.rkt} provides @racketidfont{define-class-method},
which sets up a per-method dispatch table and a generic function:

@itemlist[
@item{The dispatch table maps a struct tag (or a primitive type
      symbol) to the per-instance implementation.}
@item{The generic function takes the user-facing arguments; it pulls
      the dispatch argument by position (declared at class
      definition), reads the struct tag, indexes into the dispatch
      table, and calls the implementation.}
@item{If the dispatch argument's tag isn't in the table, the generic
      raises a clear error naming the class and the value's tag.}]

@racketidfont{register-instance-method!} registers a new entry in a
dispatch table.  Codegen emits one of these for every method of
every @racket[define-instance], using the impl body from the user
source.

@section{Direct calls for resolved sites}

When the inferer has resolved a class-method call at compile time
(because the dispatch argument's type was concrete), it records the
resolution in @racket[current-method-resolutions].  The codegen,
when lowering the call, checks the table — if the call is resolved,
it emits a direct call to the per-instance impl name (e.g.
@racketidfont{$pure:Maybe}, @racketidfont{$<*>:WriterT}) instead of going through
the dispatcher.

This is the @italic{monomorphization} pass.  See
@secref["monomorphization-and-inlining"] for details.

@section{Needs-dict instances}

Some instances need extra "dictionary" arguments at the call site.
A @racket[(MonadError e (StateT s m))] instance, for example, needs
the inner @racket[m]'s @racket[pure] to lift values.  At a call site
where the inferer resolved to such an instance, codegen consults
@racket[current-method-dict-resolutions] for the list of inner-class
impls and inserts them as leading arguments before the user's
arguments.

The receiving impl is defined to accept exactly those leading
arguments — see e.g.\ @racketidfont{$catch-e:StateT} in
@filepath{private/prelude-runtime.rkt}.  The order of dict arguments
follows a fixed convention: own return-typed methods sorted
alphabetically, then the closure over superclass return-typed
methods, then per-extra-constraint methods.  See
@racket[build-dict-skolems] in @filepath{private/infer.rkt} for the
precise rule.

@section{Records and update}

A @racket[define-struct] form compiles to a Racket @racket[struct]
with auto-generated accessors.  The struct's field information goes
into the env's @racket[struct-fields] table, keyed by struct name.

The @racket[e:update] AST node compiles by:

@itemlist[#:style 'ordered
@item{Looking up the struct's field list in @racket[struct-fields].}
@item{Generating one @racket[(struct-copy …)] expression that
      replaces the named fields and copies the rest.}]

@section{Host-language escape}

The @racket[e:escape] node — produced by surface @racket[(racket τ
(vars) body)] — compiles to a verbatim splice of @racket[body],
unmodified.  The asserted type @racket[τ] is recorded by the inferer
but plays no role in code generation; it's purely a typing
assertion.

@section{Pattern matching}

@filepath{private/match.rkt} compiles a Rackton @racket[match] into
a Racket @racket[match] (from @racket[racket/match]).  Each Rackton
pattern translates structurally:

@itemlist[
@item{@racket[p:wild] → @racket[_]}
@item{@racket[p:var] → the variable name (Racket @racket[match]
      binds it)}
@item{@racket[p:lit] → the literal value}
@item{@racket[p:ctor] → a Racket struct-pattern matching the
      constructor's underlying struct}]

Guard clauses use Racket @racket[match]'s @racket[(=>)] mechanism.
Exhaustiveness is the inferer's responsibility, checked before this
module runs; codegen assumes the patterns are total.

@section{Summary: what each node compiles to}

@tabular[#:sep @hspace[2]
         (list
          (list @bold{AST node}         @bold{Compiled to})
          (list @racket[e:literal]      "the literal value")
          (list @racket[e:var]          "the Racket identifier")
          (list @racket[e:lam]          "(lambda ...) — curried")
          (list @racket[e:app]          "Racket application")
          (list @racket[e:let]          "(let ...)")
          (list @racket[e:letrec]       "(letrec ...)")
          (list @racket[e:if]           "(if ...)")
          (list @racket[e:ann]          "the underlying expression — type erased")
          (list @racket[e:match]        "(match ...) — see private/match.rkt")
          (list @racket[e:update]       "(struct-copy ...)")
          (list @racket[e:escape]       "verbatim splice of body")
          (list @racket[e:handle]       "continuation-prompt + abort"))]
