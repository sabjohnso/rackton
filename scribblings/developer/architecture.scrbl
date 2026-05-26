#lang scribble/manual
@require[scribble/manual
         (for-label rackton)]

@title[#:tag "architecture"]{Architecture}

This chapter is the entry point to the implementation.  It describes
the compilation pipeline, names the modules that make up each stage,
and points to the chapters that go deeper into each.

@section{The pipeline}

The macro @racket[rackton] in @filepath{private/elaborate.rkt} glues
everything together.  Stages, in order:

@itemlist[#:style 'ordered
@item{@bold{Parse.}  @filepath{private/surface.rkt} converts each
      input form into the typed-core source AST.  Lexical rule: a
      lowercase initial letter means @italic{type variable} in type
      position and @italic{pattern variable} in pattern position.
      See @secref["surface-ast"].}

@item{@bold{Infer.}  @filepath{private/infer.rkt} runs Algorithm W
      with let-generalization, skolemization for declared signatures,
      GADT @racket[#:returns] refinement, type-class constraint
      collection, and constraint reduction.  Type AST and substitutions
      live in @filepath{private/types.rkt}; the unifier in
      @filepath{private/unify.rkt}; class entailment / instance search
      / overlap checks in @filepath{private/entail.rkt}; the
      environment (@racket[vars], @racket[data-ctors], @racket[tcons],
      @racket[classes], @racket[instance-table],
      @racket[method-owners], @racket[aliases], @racket[struct-fields],
      @racket[effects]) in @filepath{private/env.rkt}.  See
      @secref["inference"].}

@item{@bold{Codegen.}  @filepath{private/codegen.rkt} lowers the
      typed-core AST to Racket syntax.  Type information is fully
      erased; ADT constructors become struct calls (see
      @filepath{private/adt.rkt}); class methods become
      first-arg-tag-dispatched generics (see
      @filepath{private/dict.rkt}); records use @racket[e:update] which
      consults the env's @racket[struct-fields] table.  See
      @secref["codegen"].}

@item{@bold{Splice.}  @racket[rackton] returns @racket[(begin compiled
      …)].  @racket[rackton/main] additionally emits the
      @racketmodfont{rackton-schemes} submodule for cross-file type
      information.  See @secref["cross-module"].}]

@section{The two entry macros}

Two macros share @racket[rackton-elaborate]:

@itemlist[
@item{@racket[rackton] — embedded form, no sidecar.  Used by
      @racket[(rackton …)] inside an otherwise-Racket module.
      Multiple invocations per Racket module are allowed.}
@item{@racket[rackton/main] — module-top form, emits sidecar.
      Used by @hash-lang[] @racketmodfont{rackton} (via
      @filepath{lang/reader.rkt}) and by
      @racket[(module @#,racketidfont{name} rackton …)] (via the
      custom @racketidfont{#%module-begin} in @filepath{main.rkt}).  At most
      one per Racket module.}]

Both bind into the same shared inference parameters declared in
@filepath{infer.rkt}, allowing user code to inspect the optimisation
log via @racket[rackton-monomorphized-sites] and
@racket[rackton-inlined-sites] after each elaborate.

@section{Module graph}

@verbatim|{
elaborate.rkt
   |
   ├── surface.rkt        (parse → typed-core AST)
   ├── infer.rkt          (Algorithm W; class entailment driver)
   |     |
   |     ├── types.rkt    (types, schemes, substitutions)
   |     ├── unify.rkt    (Robinson unification)
   |     ├── env.rkt      (typing environment record)
   |     └── entail.rkt   (instance search, overlap, coherence)
   ├── codegen.rkt        (typed-core → Racket syntax)
   |     |
   |     ├── adt.rkt      (define-data-ctor, dispatch-tag)
   |     ├── dict.rkt     (define-class-method, dispatch tables)
   |     └── match.rkt    (pattern-match compilation)
   ├── scheme-codec.rkt   (serialise/deserialise the rackton-schemes
   |                      sidecar)
   └── prelude.rkt        (the bootstrap prelude — itself Rackton
                           source; runs at module load to produce
                           prelude-env)
}|

@filepath{prelude-runtime.rkt} is the corresponding @italic{runtime}
side: dispatch tables, builtin instance registrations, and the Racket
primitives the prelude's class methods bind to.  See
@secref["prelude-internals"].

@section{REPL}

@filepath{private/repl.rkt} reuses the same pipeline one form at a
time, carrying inference parameters in a @racket[rackton-repl-state]
struct between calls.  @filepath{repl.rkt} at the project root is the
user-facing launcher.  See @secref["repl-internals"].

@section{Tests as specification}

Test files are named after the feature they cover (e.g.
@filepath{gadts-test.rkt}, @filepath{stm-test.rkt},
@filepath{sealed-abstract-types-test.rkt}).  The language grew
incrementally — each completed feature has its own test file that
pins behaviour, so do not delete or fold existing feature tests when
adding new ones.  Cross-cutting tests like @filepath{end-to-end-test.rkt},
@filepath{typecheck-error-test.rkt}, and @filepath{hkt-test.rkt}
exercise the pipeline as a whole.

When adding a new feature, see @secref["adding-features"] for the
workflow.
