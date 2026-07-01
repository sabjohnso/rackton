#lang scribble/manual
@require[scribble/manual
         (for-label rackton)]

@title[#:tag "representation-tiers"]{Runtime-representation tiers}

Type information is fully erased before a Rackton program runs
(@secref["codegen"]), so every Rackton value is ultimately an ordinary
Racket value: a number, a procedure, a vector, or a @racket[struct]
instance.  @italic{How} those Racket values are built — whether a struct
is transparent, prefab, or opaque, whether a vector is mutable or
immutable — is not incidental.  It decides three properties that matter
beyond a single evaluation:

@itemlist[
@item{@bold{Cross-namespace identity} — whether a value built in one
      module instantiation (for example the REPL's evaluation namespace)
      is recognised by the predicates and @racket[match] patterns of
      another.}
@item{@bold{Reload stability} — whether a value survives a
      @racket[dynamic-rerequire] of the module that defined its type.}
@item{@bold{Serializability} — whether a value may be written with
      @racket[s-exp->fasl] or sent across a @racket[place] boundary,
      the prerequisite for future parallel processing.}]

Because each representation is defined in exactly one module, the global
property "which of these a value has" is easy to violate one module at a
time.  This section fixes it as a policy: every runtime value belongs to
one of three tiers, and each representation module is the sole authority
for its value's tier.

@section{The three tiers}

@subsection{Tier O — Opaque}

An opaque Racket struct whose fields are unreadable without the
controlling inspector, dispatched by an explicit declared
@racketidfont{runtime-tag} rather than by struct introspection (see
@racket[tcon-info] and the @racketidfont{runtime-tag} branch of
@racketidfont{tags-for-instance-head} in @filepath{private/codegen.rkt}).

This is the one tier where "you cannot read or forge this value" holds at
the Racket level, so it is where the runtime enforcement of sealed and
abstract types lives.  A Tier-O value MUST NOT be prefab — prefab is fully
transparent and fabricable from its key, which would defeat the sealing.
Consequently Tier-O values are deliberately @italic{not} serializable; a
parallel algorithm that must transmit one has to expose a Tier-V
projection of it explicitly.

@subsection{Tier V — Value}

A pure data value: either a prefab @racket[struct] with immutable fields,
or a bare immutable vector, every field or element of which is itself
Tier V or a serializable primitive.  This is the default tier for
ordinary data.  A Tier-V value is recognised across module
instantiations, survives @racket[dynamic-rerequire], and is eligible for
@racket[fasl] and @racket[place] transmission @italic{when its contents
also are} (see @secref["eligibility"]).

The members, after the representation pass that established this policy,
are algebraic data types, tuples and @racket[Pair], @racket[Map] and
@racket[Set], @racket[Array], @racket[Bitstring], and the serializable
primitives.

The rules: the struct wrapper is prefab (or, for tuples, a bare immutable
vector); no Tier-V value is built over a mutable store; and structural
@racket[equal?] is the equality — both prefab structs and immutable
vectors provide it, so this does not change when a representation moves
into the tier.

@subsection{Tier R — Reference}

A value with observable identity, a mutable interior, a deferred effect,
or a live host resource: procedures and closures; the IO thunk
@racketidfont{$io}; the concurrency cells @racketidfont{$mvar},
@racketidfont{$tvar}, @racketidfont{$stm}, @racketidfont{$future}; port
and socket handles such as @racketidfont{tcp-conn}; and foreign pointers.

Tier-R values are non-serializable, correctly and by nature.  The purpose
of naming the tier is so the serialization and @racket[place] boundary
@italic{rejects} them with a clear diagnostic rather than corrupting or
silently transmitting them.  A Tier-R struct MAY still be prefab — but
only to obtain cross-namespace type identity (see @secref["two-roles"]),
never to imply eligibility.

@section[#:tag "eligibility"]{Eligibility is recursive}

The authority for "may this value cross a @racket[place] boundary" is
Racket's own @racket[place-message-allowed?]; @racket[fasl] has the same
shape.  A value is @deftech{eligible} exactly when it is a serializable
primitive (exact and inexact real and complex numbers, booleans,
characters, interned symbols, immutable strings and byte strings); or an
immutable vector, hash, list, or box of eligible values; or a prefab
struct all of whose fields are eligible.

The consequence is a distinction the tiers alone do not capture.  A
constructor's representation being Tier V is @italic{necessary but not
sufficient} for a value of that type to be eligible: eligibility is the
recursive closure of the rule over a value's actual contents.  An ADT
value whose field holds a closure, or a @racket[Map] whose values are IO
thunks, is a Tier-R value for eligibility purposes even though it
pattern-matches, dispatches, and prints exactly like any other value of
its type.  Making the wrapper prefab is what makes the type
@italic{capable} of eligibility; whether a given value @italic{is}
eligible depends on its leaves.

Whether eligibility should be enforced statically (a @racket[Sendable]
constraint checked by inference) or dynamically (checked at the boundary)
is deferred; this policy fixes only the representations on which either
mechanism will rest.

@section[#:tag "two-roles"]{Prefab serves two independent roles}

@racketidfont{#:prefab} is used for two distinct reasons, and the tier
model keeps them apart:

@itemlist[#:style 'ordered
@item{@bold{Cross-namespace type identity.}  A representation that
      codegen references through a path which can produce a second
      generative instantiation needs one global identity, so that a value
      built in one instance is recognised in another.  The REPL is the
      canonical case: it evaluates at a top level that separately
      @racket[namespace-require]s some runtime helpers, so a generative
      struct would be a distinct type there.  This is why
      @racketidfont{rkt-array}, @racketidfont{bitstring}, and
      @racketidfont{tcp-conn} were prefab from the start.}
@item{@bold{Serialization and @racket[place] eligibility.}  A consequence
      of a prefab struct all of whose fields are eligible
      (@secref["eligibility"]).}]

Role 1 applies to Tier-R members too: @racketidfont{tcp-conn} is prefab
for cross-namespace recognition yet remains Tier R because it holds live
ports.  Prefab-ness and eligibility are separate properties; a
representation may be prefab for role 1 and still be non-eligible.

@section[#:tag "prefab-soundness"]{Why prefab is sound for ADTs}

A transparent @racket[struct] is @italic{generative}: each definition
produces a fresh type, so two same-named constructors stay distinct.
Prefab instead gives one global identity keyed by name and shape.  Moving
algebraic data types to prefab is sound because Rackton has already
discarded generativity for constructors at both layers that could observe
it:

@itemlist[
@item{The type environment stores constructors in one flat map keyed by
      bare name — @racket[env-extend-data] is a @racket[hash-set] on the
      env's @racket[data-ctors] field.  Constructor names are therefore a
      global namespace; two constructors of the same name cannot denote
      two live types.}
@item{Runtime dispatch keys on the same bare
      @racketidfont{$ctor:}@racket[_name] symbol —
      @racketidfont{tags-for-instance-head} in
      @filepath{private/codegen.rkt} produces it, and
      @racketidfont{dispatch-tag} in @filepath{private/dict.rkt} recovers
      it from a value's @racket[struct-type-info].}]

The only place generativity still did any work was @racket[match] and the
struct predicate, and type soundness makes that unobservable: a
well-typed program never routes a value of one type into a pattern for
another, so two representations coinciding at runtime is invisible.  This
is the same property that type erasure already relies on.  Prefab's
global-by-name identity therefore @italic{matches} Rackton's model rather
than weakening it.

This is a standing precondition, not a one-time observation: if a future
change introduces per-type constructor namespacing, the prefab choice for
ADTs must be revisited.

@section{Per-representation status}

@tabular[#:sep @hspace[2]
         (list
          (list @bold{Representation}          @bold{Tier} @bold{Racket form})
          (list @elem{ADT ctors @racketidfont{$ctor:N}}   "V" "prefab struct, immutable fields")
          (list @elem{Tuples / @racket[Pair]}             "V" "bare immutable vector")
          (list @elem{@racket[Map] / @racket[Set]}        "V" "prefab wrapper over an immutable hash")
          (list @elem{@racket[Array]}                     "V" "prefab wrapper, immutable backing vector")
          (list @elem{@racket[Bitstring]}                 "V" "prefab (natural, natural)")
          (list @elem{Serializable primitives}            "V" "host immutable values")
          (list @elem{Abstract / sealed}                  "O" "opaque struct + runtime-tag")
          (list @elem{@racketidfont{$io}}                 "R" "transparent thunk")
          (list @elem{@racketidfont{$mvar} @racketidfont{$tvar} @racketidfont{$stm} @racketidfont{$future}} "R" "transparent cells")
          (list @elem{@racketidfont{tcp-conn} / ports}    "R" "prefab over ports (role 1 only)")
          (list @elem{Procedures / closures}              "R" "host procedures"))]

The @racketidfont{$ctor:} struct is defined by
@racketidfont{define-data-ctor} in @filepath{private/adt.rkt}; the tuple,
@racket[Map]/@racket[Set] representations in
@filepath{private/prelude-runtime.rkt}; @racket[Array] in
@filepath{private/array-runtime.rkt} (whose single
@racketidfont{array-of} constructor freezes the backing vector, since a
prefab struct cannot carry a coercing @racket[#:guard]); and
@racket[Bitstring] in @filepath{private/bitstring-runtime.rkt}.

@section{What the tiers enable}

@bold{Reload.}  Because a prefab type's identity is by key rather than
generative, an ADT value built before a @racket[dynamic-rerequire] of its
defining module still satisfies the reinstantiated type's predicate and
@racket[match] pattern, and still dispatches.  Under the previous
transparent representation the value would fail its own predicate after a
reload while continuing to dispatch — a silent, partial failure.  Making
ADTs prefab is what makes an interactive edit-and-reload workflow correct.

@bold{Serialization and parallelism.}  With every pure-data
representation prefab and immutable, an entire value graph is eligible so
long as its leaves are.  A @racket[Map] whose values are an @racket[Array]
and an ADT wrapping a tuple round-trips through @racket[fasl] under
structural @racket[equal?] and is accepted by
@racket[place-message-allowed?]; the same graph with a closure at any leaf
is correctly rejected.  This is the representation groundwork on which
@racket[place]- or @racket[future]-based parallel processing can later be
built.

The tier changes are behaviour-preserving within the language:
name-based dispatch, structural @racket[equal?], @racket[struct->vector]
display, and @racket[match] are identical for prefab and transparent
structs and for mutable and immutable vectors, which is why moving a
representation between mutable and immutable, or transparent and prefab,
requires no change to any consumer.
