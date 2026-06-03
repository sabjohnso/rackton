#lang scribble/manual
@require[scribble/manual
         (for-label rackton rackton/unit/check rackton/unit/gen rackton/unit/laws rackton/unit/lazy rackton/unit/prng rackton/unit/property rackton/unit/tree)]

@title[#:tag "stdlib-unit"]{@tt{rackton/unit} — the property-testing framework}

@section{rackton/unit/check}
@defmodule[rackton/unit/check #:no-declare]
@declare-exporting[rackton/unit/check]

Assertion / check combinators. A check is a pure @racket[Assertion] value
rather than a thrown exception, so a runner can aggregate results without
aborting on the first failure. Assertions form a @racket[Semigroup] where the
first failure wins, and @racket[pass] is the identity element (exported as a
plain value so it resolves across module boundaries).

@defidform[#:kind "type" CheckResult]{The outcome of a single check.
  @deftogether[(@defidform[#:kind "constructor" CheckPass]
                @defidform[#:kind "constructor" CheckFail])]{
    @racket[CheckPass : (CheckResult)] / @racket[CheckFail : (-> String CheckResult)]
    — a passing result, or a failure carrying a message.}}

@defidform[#:kind "type & constructor" Assertion]{A check result wrapped as a value; forms a
  @racket[Semigroup] where the first failure wins.
  
    @racket[Assertion : (-> CheckResult Assertion)] — wrap a @racket[CheckResult].}

@defproc[(assertion-result [a Assertion]) CheckResult]{Unwrap an assertion to its underlying @racket[CheckResult].}

@defthing[pass Assertion]{The passing assertion; the identity element for combining assertions.}

@defproc[(fail [msg String]) Assertion]{Build a failing assertion carrying @racket[msg].}

@defproc[(check-true [b Boolean]) Assertion]{Pass when @racket[b] is true, otherwise fail.}

@defproc[(check-false [b Boolean]) Assertion]{Pass when @racket[b] is false, otherwise fail.}

@defproc[(check-equal? [actual a] [expected a]) Assertion]{Pass when @racket[actual] equals @racket[expected]; requires @racket[Eq] and @racket[Show] on @racket[a].}

@defproc[(check-not-equal? [actual a] [unexpected a]) Assertion]{Pass when @racket[actual] differs from @racket[unexpected]; requires @racket[Eq] and @racket[Show] on @racket[a].}

@defproc[(all-checks [xs (List Assertion)]) Assertion]{Combine a list of assertions, keeping the first failure.}


@section{rackton/unit/gen}
@defmodule[rackton/unit/gen #:no-declare]
@declare-exporting[rackton/unit/gen]

Generators with integrated, Hedgehog-style shrinking. A @racket[Tree] pairs a
generated value with a lazy stream of progressively smaller shrink candidates,
and a @racket[Gen] maps a size and a seed to such a tree, so every generator
shrinks for free with no separate shrink method.

@defidform[#:kind "type & constructor" Tree]{A generated value together with a lazy stream of
  recursively-smaller shrink candidates.
  
    @racket[Tree : (-> a (-> (Lazy (Stream (Tree a))) (Tree a)))] — a value paired
    with the lazy stream of its shrink trees.}

@defidform[#:kind "type & constructor" Gen]{A generator: a function from a size and a
  @racket[Seed] to a shrink @racket[Tree].
  
    @racket[Gen : (-> (-> Integer (-> Seed (Tree a))) (Gen a))] — wraps the
    size-and-seed-to-tree function.}

@defproc[(run-gen [g (Gen a)]) (-> Integer (-> Seed (Tree a)))]{Unwraps a
  generator to its underlying size-and-seed function.}

@defproc[(gen-tree [g (Gen a)] [size Integer] [seed Seed]) (Tree a)]{Runs a
  generator at a given size and starting seed, producing its shrink tree; the
  public way to execute a generator.}

@defproc[(tree-value [t (Tree a)]) a]{Returns the value at the root of a tree.}

@defproc[(tree-children [t (Tree a)]) (Stream (Tree a))]{Forces and returns the
  stream of shrink subtrees of a tree.}

@defproc[(tree-map [f (-> a b)] [t (Tree a)]) (Tree b)]{Maps a function over a
  value and all of its shrinks.}

@defproc[(tree-bind [f (-> a (Tree b))] [t (Tree a)]) (Tree b)]{Monadic bind on
  trees, interleaving the outer tree's re-bound shrinks with the inner tree's
  shrinks so that shrinking composes through @racket[flatmap].}

@defproc[(constant [x a]) (Gen a)]{A generator that always yields @racket[x] with
  no shrinks; the cross-module-safe equivalent of @racket[pure] for @racket[Gen].}

@defproc[(int-range [lo Integer] [hi Integer]) (Gen Integer)]{A uniform integer in
  @racket[(lo hi)], shrinking toward @racket[lo].}

@defthing[bool (Gen Boolean)]{A boolean generator; @racket[#t] shrinks to
  @racket[#f], and @racket[#f] is minimal.}

@defthing[gen-integer (Gen Integer)]{A default integer generator over a moderate
  range.}

@defthing[gen-boolean (Gen Boolean)]{A default boolean generator.}

@defproc[(gen-pair [ga (Gen a)] [gb (Gen b)]) (Gen (Pair a b))]{A pair generator,
  built from two component generators.}

@defproc[(replicate-gen [n Integer] [g (Gen a)]) (Gen (List a))]{A generator of
  exactly @racket[n] elements drawn from @racket[g].}

@defproc[(gen-list [g (Gen a)]) (Gen (List a))]{A generator of lists of 0 to 8
  elements from @racket[g], shrinking both length and each element.}

@defproc[(element-of [xs (List a)]) (Gen a)]{Picks a uniformly-random element of a
  non-empty list, shrinking toward the first element.}

@defthing[gen-string (Gen String)]{A small selection of sample strings, shrinking
  toward the empty string.}


@section{rackton/unit/laws}
@defmodule[rackton/unit/laws #:no-declare]
@declare-exporting[rackton/unit/laws]

Algebraic-law bundles: each takes a generator (and, for the
higher-kinded structures, explicit equality/render/point operations)
and returns a @racket[Test] group of named properties capturing the
laws of a structure. This module re-exports the full
@racket[tree.rkt] tree/runner/generator/check surface, so a consumer
requires only this module (or @racket[rackton/unit]); those re-exported
names are documented under their own modules.

@defproc[(eq-laws [gen (Gen a)]) Test]{
  Bundles the @racket[Eq] laws — reflexivity and symmetry of
  @racket[==] — over values drawn from @racket[gen]. Requires
  @racket[(Eq a)] and @racket[(Show a)].}

@defproc[(ord-laws [gen (Gen a)]) Test]{
  Bundles the @racket[Ord] laws — reflexivity and totality of
  @racket[<=] — over values drawn from @racket[gen]. Requires
  @racket[(Ord a)] and @racket[(Show a)].}

@defproc[(semigroup-laws [gen (Gen a)]) Test]{
  Bundles the @racket[Semigroup] law — associativity of
  @racket[mappend] — over values drawn from @racket[gen]. Requires
  @racket[(Eq a)], @racket[(Show a)], and @racket[(Semigroup a)].}

@defproc[(monoid-laws [gen (Gen a)] [identity a]) Test]{
  Bundles the @racket[Monoid] laws — @racket[identity] is a left and
  right unit for @racket[mappend] — over values drawn from
  @racket[gen]; the identity element is supplied explicitly since the
  return-typed @racket[mempty] does not resolve across module
  boundaries. Requires @racket[(Eq a)], @racket[(Show a)], and
  @racket[(Semigroup a)].}

@defproc[(functor-laws [eqf (-> (f Integer) (-> (f Integer) Boolean))]
                       [render (-> (f Integer) String)]
                       [gen (Gen (f Integer))]) Test]{
  Bundles the @racket[Functor] laws — @racket[fmap id == id] and
  @tt{fmap (g . f) == fmap g . fmap f} — using the explicit
  @racket[eqf] to compare two @racket[(f Integer)] and @racket[render]
  for counterexamples. Requires @racket[(Functor f)].}

@defproc[(applicative-laws [eqf (-> (f Integer) (-> (f Integer) Boolean))]
                           [render (-> (f Integer) String)]
                           [point (-> Integer (f Integer))]
                           [point-fn (-> (-> Integer Integer) (f (-> Integer Integer)))]
                           [gen (Gen (f Integer))]) Test]{
  Bundles the checkable @racket[Applicative] laws — identity
  (@racket[pure id <*> v == v]) and homomorphism
  (@racket[pure f <*> pure x == pure (f x)]) — with @racket[point] and
  @racket[point-fn] supplying @racket[pure] monomorphically at the
  value and function types. Requires @racket[(Applicative f)].}

@defproc[(monad-laws [eqf (-> (m Integer) (-> (m Integer) Boolean))]
                     [render (-> (m Integer) String)]
                     [point (-> Integer (m Integer))]
                     [gen (Gen (m Integer))]) Test]{
  Bundles all three @racket[Monad] laws — left identity, right
  identity, and associativity — with @racket[point] supplying
  @racket[return] monomorphically at the element type. Requires
  @racket[(Monad m)].}

@defproc[(traversable-laws [eqm (-> (Maybe (t Integer)) (-> (Maybe (t Integer)) Boolean))]
                           [render (-> (t Integer) String)]
                           [gen (Gen (t Integer))]) Test]{
  Bundles the @racket[Traversable] identity law specialised to the
  @racket[Maybe] applicative (@racket[traverse Some t == Some t]), with
  @racket[eqm] comparing two @racket[(Maybe (t Integer))]. Requires
  @racket[(Traversable t)].}


@section{rackton/unit/lazy}
@defmodule[rackton/unit/lazy #:no-declare]
@declare-exporting[rackton/unit/lazy]

Laziness primitives for the unit framework's integrated shrinking, where a
generated value carries its unbounded tree of shrink candidates behind a
thunk. The canonical @racket[Lazy]/@racket[Stream] types and combinators are
re-exported from @racket[rackton/data/lazy]; this module adds two back-compat
aliases that the unit framework grew up with.

@defproc[(force-lazy [l (Lazy a)]) a]{Forces a @racket[Lazy] value, an alias for @racket[force].}

@defproc[(delay-lazy [x a]) (Lazy a)]{Defers an already-evaluated value, equivalent to @racket[(delay x)].}


@section{rackton/unit/prng}
@defmodule[rackton/unit/prng #:no-declare]
@declare-exporting[rackton/unit/prng]

A pure, seeded, splittable pseudo-random generator for reproducible
property testing: generation takes no IO and a failing case replays from
a printable starting @racket[Integer] seed. The state is a 64-bit LCG
(the SplitMix/PCG multiplier and odd increment), adequate for test-case
generation rather than statistical rigor.

@defidform[#:kind "type & constructor" Seed]{A 64-bit LCG state kept in @tt{[0, 2^64)}.
  
    @racket[Seed : (-> Integer Seed)] — wraps the raw 64-bit state integer.}

@defproc[(seed-from [n Integer]) Seed]{Builds a seed from any printable @racket[Integer], the user-visible handle.}

@defproc[(next-seed [s Seed]) Seed]{Advances the LCG one step.}

@defproc[(seed-value [s Seed]) Integer]{Extracts a well-mixed value from a seed using arithmetic-only avalanche.}

@defproc[(split-seed [s Seed]) (Pair Seed Seed)]{Derives two independent sub-seeds whose streams diverge immediately.}

@defproc[(seed-int-range [s Seed] [lo Integer] [hi Integer]) Integer]{Produces a value in the inclusive range @tt{[lo, hi]} (assumes @racket[hi >= lo]).}


@section{rackton/unit/property}
@defmodule[rackton/unit/property #:no-declare]
@declare-exporting[rackton/unit/property]

Property-based testing with integrated shrinking. A @racket[Property] is an
opaque seeded computation that runs N cases and reports an outcome; on the first
failing case the shrink loop descends into the smallest still-failing child of
the shrink tree to yield a minimal counterexample. Generation is pure and
seeded, so the reported start seed replays the failure exactly. This module also
re-exports the generator interface from @racket[rackton/unit/gen] (including
@racket[Gen], @racket[Tree], @racket[run-gen], @racket[gen-integer], and
friends) so consumers can require it alone.

@defidform[#:kind "type & constructor" Property]{An opaque computation that quantifies over a
hidden element type, parameterized by a test count and a start seed.
  
    @racket[Property : (-> (-> Integer (-> Integer PropOutcome)) Property)] —
    wraps a function from number of tests and start seed to a @racket[PropOutcome].}

@defidform[#:kind "type" PropOutcome]{The result of running a property.
  @deftogether[(@defidform[#:kind "constructor" PropPassed]
                @defidform[#:kind "constructor" PropFailed])]{
    @racket[PropPassed : (-> Integer PropOutcome)] carries the number of cases
    that passed; @racket[PropFailed : (-> String (-> Integer PropOutcome))]
    carries the shown minimal counterexample and the start seed.}}

@defproc[(for-all-gen [render (-> a String)] [g (Gen a)] [pred (-> a Boolean)]) Property]{
Builds a property from a generator, a predicate, and an explicit renderer for
counterexamples; the renderer is passed as a first-class value rather than via a
@racket[Show] constraint so it threads into the captured closure.}

@defproc[(for-all [g (Gen a)] [pred (-> a Boolean)]) Property]{
Convenience over @racket[for-all-gen] that renders counterexamples with the
@racket[Show] instance for @racket[a].}

@defproc[(run-property [num-tests Integer] [start-seed Integer] [prop Property]) PropOutcome]{
Runs @racket[prop] for @racket[num-tests] cases starting from @racket[start-seed]
and reports the outcome.}


@section{rackton/unit/tree}
@defmodule[rackton/unit/tree #:no-declare]
@declare-exporting[rackton/unit/tree]

The BDD test tree and its @racket[IO] runner. @racket[describe], @racket[context],
@racket[it], and @racket[it-prop] build an immutable @racket[Test] value (a functional
core: nothing runs until the runner consumes it), while @racket[run-tests] is the
imperative shell that walks the tree in @racket[IO], prints an indented ok/FAIL report,
and returns a @racket[Summary] of counts. This module also re-exports the
@racket[rackton/unit/property], @racket[rackton/unit/gen], and @racket[rackton/unit/check]
surfaces so a consumer requires only this one module.

@defidform[#:kind "type" Outcome]{
  What a single test leaf carries.
  @deftogether[(@defidform[#:kind "constructor" Unit-test]
                @defidform[#:kind "constructor" Prop-test])]{
    @racket[Unit-test : (-> Assertion Outcome)] wraps an already-evaluated assertion;
    @racket[Prop-test : (-> Property Outcome)] wraps a property the runner executes
    inside @racket[try].}}

@defidform[#:kind "type" Test]{
  An immutable tree of named leaves and groups.
  @deftogether[(@defidform[#:kind "constructor" TLeaf]
                @defidform[#:kind "constructor" TGroup])]{
    @racket[TLeaf : (-> String (-> Outcome Test))] is one named test;
    @racket[TGroup : (-> String (-> (List Test) Test))] is a named group of child tests.}}

@defidform[#:kind "type & constructor" Summary]{
  A pair of pass/fail counts produced by the runner.
  
    @racket[Summary : (-> Integer (-> Integer Summary))] holds the passed count and the
    failed count, in that order.}

@defproc[(it [name String] [a Assertion]) Test]{Build a unit-test leaf from an
already-evaluated assertion.}

@defproc[(it-prop [name String] [p Property]) Test]{Build a property-test leaf the runner
executes inside @racket[try].}

@defproc[(group-of [name String] [kids (List Test)]) Test]{The desugaring target for the
variadic @racket[describe] / @racket[context] surface forms; groups child tests under a name.}

@defproc[(run-tests [t Test]) (IO Summary)]{Walk the tree in @racket[IO], printing an
indented ok/FAIL line per leaf, and return the totals.}

@defproc[(run-tests-quiet [t Test]) (IO Summary)]{Run the same tree but print only failing
leaves, tagged with their group path, so a passing suite is silent.}

@defproc[(run-suite [name String] [tests (List Test)]) (IO Unit)]{Run @racket[tests]
quietly under a top-level group @racket[name], then @racket[panic] if any leaf failed (so
@tt{raco test} reports a non-zero result).}

@defproc[(summary-passed [s Summary]) Integer]{The number of passing leaves in a summary.}

@defproc[(summary-failed [s Summary]) Integer]{The number of failing leaves in a summary.}


