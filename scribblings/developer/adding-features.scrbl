#lang scribble/manual
@require[scribble/manual
         (for-label rackton)]

@title[#:tag "adding-features"]{Adding a new feature}

This chapter is the recipe.  It assumes you've read
@secref["architecture"] and the chapter for the subsystem you're
about to touch.

@section{TDD first}

Following the project's CLAUDE.md, every feature follows
red-green-refactor:

@itemlist[#:style 'ordered
@item{@bold{Red.}  Write a feature-named test file in
      @filepath{tests/} (e.g., @filepath{my-feature-test.rkt}) that
      exercises the feature you intend to add.  The file should fail
      with a clear error explaining what's missing.}
@item{@bold{Green.}  Implement the minimum changes across
      @filepath{surface.rkt} → @filepath{infer.rkt} →
      @filepath{codegen.rkt} (and possibly @filepath{prelude.rkt} +
      @filepath{prelude-runtime.rkt}) to make the test pass.}
@item{@bold{Refactor.}  Clean up with confidence, keeping the test
      green.}]

@section{The seven-step checklist}

For a feature that adds a new surface form:

@itemlist[#:style 'ordered
@item{@bold{AST node.}  Add a @racket[struct] in
      @filepath{private/surface.rkt} alongside related forms (e.g.\ a
      new expression form goes near the other @racket[e:…] structs).
      Don't forget the @racket[stx] field for sourcemap-aware
      errors.}
@item{@bold{Parser.}  Extend @racket[parse-expr],
      @racket[parse-top], or @racket[parse-pattern] with a new
      @racket[syntax-parse] clause.  Add the form's head keyword to
      the @racket[#:datum-literals] list at the top of the
      enclosing parser.}
@item{@bold{Inference rule.}  Extend @filepath{private/infer.rkt}
      with a new case in @racket[infer-expr] /
      @racket[infer-program-step] / @racket[infer-pattern].  Decide
      what constraints the rule generates, what bindings it
      introduces, and whether it consults the env for context.}
@item{@bold{Codegen rule.}  Extend @filepath{private/codegen.rkt}
      with the lowering for the new node.}
@item{@bold{Prelude (maybe).}  If the feature introduces a new
      built-in type, class, or value binding, update both
      @filepath{private/prelude.rkt} (the typing side) and
      @filepath{private/prelude-runtime.rkt} (the runtime side).  See
      @secref["prelude-internals"].}
@item{@bold{Test file.}  Save your red-step test as
      @filepath{tests/{feature-name}-test.rkt}.  Add at least one
      property test (with @racket[rackcheck-lib]) for any algebraic
      law, and at least one example test (with @racket[rackunit-lib])
      for each user-visible behaviour.}
@item{@bold{Documentation.}  Add a reference entry to the appropriate
      file under @filepath{scribblings/reference/}.  If the feature
      is user-facing, add a section to the appropriate guide
      chapter.  If the feature has interesting implementation choices,
      add notes to the appropriate developer chapter.}]

@section{Per-module test files}

The project convention is one test file per module under
@filepath{private/} (named @filepath{foo-test.rkt}) plus per-feature
test files in @filepath{tests/}.  Cross-cutting end-to-end tests live
in @filepath{tests/end-to-end-test.rkt} and friends.

@bold{Do not delete or fold existing feature tests when adding a new
feature.}  Each test file pins behaviour established during a
specific development phase; collapsing them loses that history.

@section{The doc-coverage test}

@filepath{tests/doc-coverage-test.rkt} asserts that every public
export, every surface form, every provide spec, and every REPL
command appears as a @racket[@@def…] entry somewhere in the
reference.  If you add a new prelude binding, surface form, or REPL
command, this test will fail until the corresponding documentation
exists.  Treat the test failure as a red-step demand for a doc entry,
not a nuisance.

@section{Running the test suite}

@commandline{raco test -p rackton}                @italic{          # full suite}
@commandline{raco test tests/my-feature-test.rkt} @italic{# one file}
@commandline{raco setup --check-pkg-deps rackton} @italic{# compile + dep check}

CI (@filepath{.github/workflows/ci.yml}) runs the matrix Racket-stable
crossed with BC and CS, plus a @racket[current] row that's allowed to
fail.  Local @racket[raco test] suffices for development; CI catches
version regressions.

@section{Style notes}

@itemlist[
@item{Source files are @hash-lang[] @racketmodfont{racket/base} with
      a top-of-file purpose comment.  The comment should describe
      the module's tenets and public API.}
@item{Per-module test files (@filepath{foo-test.rkt}) sit next to the
      module they test in @filepath{private/}.}
@item{Property tests use @racket[rackcheck-lib]; example tests use
      @racket[rackunit-lib].  Property tests are preferred for
      capturing invariants (round-trips, identities, commutativity).}
@item{Prefer straightforward code over clever code.  Three clear
      lines beat one opaque line.  See the @italic{Readability First}
      section of the user's CLAUDE.md.}]
