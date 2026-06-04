#lang scribble/manual
@require[scribble/manual
         (for-label rackton)]

@title[#:tag "prelude-internals"]{Prelude internals}

The prelude lives in two files that must be kept in sync:

@itemlist[
@item{@filepath{private/prelude.rkt} — the @italic{compile-time}
      side.  Itself a Rackton program (a quoted list of class /
      instance / data / def forms) that is parsed and inferred at
      module load to produce @racket[prelude-env], the starting
      typing environment of every user program.}
@item{@filepath{private/prelude-runtime.rkt} — the @italic{runtime}
      side.  Dispatch tables, builtin instance registrations, and
      the Racket primitives the prelude's class methods bind to.}]

@section{The bootstrap dance}

@filepath{private/prelude.rkt} is loaded once.  Its top-level
expression is the long quoted list @racket[prelude-source-forms],
which is fed to the same @racket[surface] parser and @racket[infer]
machinery used for user code.  The result is @racket[prelude-env]:
a typing environment seeded with every prelude class, instance, ADT,
and binding.

User code reaches the prelude by starting its elaboration with
@racket[prelude-env] as the initial environment.  Every user
elaboration thus inherits the prelude unchanged.

The runtime side does the corresponding work at Racket load time:
@filepath{private/prelude-runtime.rkt} defines every prelude name as a
Racket binding, sets up the dispatch tables (@racketidfont{$dispatch:+},
@racketidfont{$dispatch:==}, etc.), and registers each builtin instance via
@racketidfont{register-instance-method!}.  When user code references
@racket[+] or @racket[==], the elaborator emits a call to the runtime
binding of the same name; that binding is the dispatcher.

@section{Per-instance impl names}

Several class methods are resolved entirely at compile time and have
per-instance @italic{names} as Racket bindings.  The naming
convention is @racketidfont{$}@racket[_method]@racketidfont{:}@racket[_Tcon]:

@itemlist[
@item{@racketidfont{$pure:Maybe}, @racketidfont{$pure:List}, @racketidfont{$pure:Either},
      @racketidfont{$pure:IO}, @racketidfont{$pure:State}, @racketidfont{$pure:Env} —
      the per-instance @racket[pure] impls.}
@item{@racketidfont{$flatmap:ExceptT}, @racketidfont{$fapply:ExceptT}, @racketidfont{$liftA2:ExceptT}
      — needs-dict impls for ExceptT.}
@item{@racketidfont{$mempty:String}, @racketidfont{$mempty:List}, @racketidfont{$mempty:Sum},
      @racketidfont{$mempty:Product} — return-typed @racket[mempty] impls.}
@item{@racketidfont{$get-st:State}, @racketidfont{$put-st:State},
      @racketidfont{$modify-st:State} and analogues for StateT, EnvT,
      WriterT, ExceptT — MonadState method impls.}
@item{Similar families for @racketidfont{$ask-en}, @racketidfont{$local-en},
      @racketidfont{$tell-w}, @racketidfont{$listen}, @racketidfont{$censor},
      @racketidfont{$throw-e}, @racketidfont{$catch-e}.}]

These names are exported from @filepath{prelude-runtime.rkt} but are
not part of the public language.  The elaborator emits direct calls
to them at call sites where the type-class dispatch is fully resolved
at compile time.

@section{Adding a new prelude binding}

The two halves must stay in sync.  When adding a new name:

@itemlist[#:style 'ordered
@item{In @filepath{private/prelude.rkt}, add the @racket[:] signature
      and the @racket[(define …)] body to
      @racket[prelude-source-forms].  The body may use the
      @racket[racket] escape to call the Racket-level
      implementation.}
@item{In @filepath{private/prelude-runtime.rkt}, add the matching
      Racket definition.  If it's a class-method instance, register
      it against the appropriate @racketidfont{$dispatch:…} table via
      @racketidfont{register-instance-method!}.}
@item{Add the name to the @racket[(provide …)] list in
      @filepath{private/prelude-runtime.rkt} unless it's
      @racket[$]-prefixed (those are documented as internal and the
      coverage test excludes them anyway).}
@item{Add a test in the appropriate feature-named test file in
      @filepath{tests/}.}]

@section{The except-in list}

@filepath{lang/runtime.rkt}'s @racket[except-in] from
@racketmodname[racket/base] excludes exactly the names the prelude
shadows.  When adding a new prelude name that collides with
@racketmodname[racket/base], add it to that @racket[except-in] list
or @racket[(racket …)] escape blocks will break with
"identifier already required".

The corresponding @racket[except-in] in @filepath{main.rkt} must be
kept in sync — the lists should be identical.

@section{Why this two-file split?}

A single file would be simpler but would force the parser to handle
its own prelude — a chicken-and-egg problem.  Splitting into a
@italic{quoted-Rackton-source} file for the typing environment and a
@italic{plain-Racket} file for the runtime bindings means each file is
ordinary code in its own language, and the elaborator only needs to
process user code, never bootstrap itself.

The tradeoff is the synchronisation burden documented above.  In
practice this is a small price for the architectural simplicity.
