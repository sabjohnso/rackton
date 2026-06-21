#lang scribble/manual
@require[scribble/manual
         (for-label rackton
                    rackton/unit
                    rackton/unit/check
                    rackton/unit/gen
                    rackton/unit/laws
                    rackton/unit/property
                    rackton/unit/tree)
         "../rackton-eval.rkt"]
@(define ev (make-rackton-eval))

@title[#:tag "testing"]{Testing with @tt{rackton/unit}}

@tt{rackton/unit} is a testing framework written in Rackton itself.  It
provides three layers that share one runner:

@itemlist[
@item{@bold{Checks} — @racketidfont{check-equal?}, @racketidfont{check-true},
      and friends, in the spirit of rackunit.}
@item{@bold{A BDD test tree} — @racketidfont{describe} / @racketidfont{it}
      / @racketidfont{context}, building an immutable @racketidfont{Test}
      value that the runner consumes.}
@item{@bold{Property-based testing with integrated shrinking} —
      @racketidfont{for-all} over reproducible, seeded generators, plus
      bundles of @bold{algebraic laws} (Eq, Ord, Semigroup, Monoid).}]

Bring it all into scope with a single import:

@rackton-example[#:eval ev #:mode 'display]{
#lang rackton
(require rackton/unit)
}

A consumer should @racketidfont{require} only @tt{rackton/unit} (not the
individual submodules); the framework funnels its protocol instances
through a single import path so module-level instance coherence is
satisfied.

@section{Checks and the test tree}

A check is a @italic{value} (an @racketidfont{Assertion}), not a thrown
exception, so the runner aggregates results without aborting.  @racketidfont{it}
names a single assertion; @racketidfont{describe} / @racketidfont{context}
are variadic surface forms that group their child tests directly — no
list wrapper.  @racketidfont{run-tests} walks the tree in @racket[IO],
prints an indented report, and returns a @racketidfont{Summary} of
pass/fail counts.

@rackton-example[#:eval ev]{
#lang rackton
(require rackton/unit)

(: suite Test)
(define suite
  (describe "arithmetic"
    (it "one plus one" (check-equal? (+ 1 1) 2))
    (it "doubling"     (check-equal? (* 2 21) 42))))

(define _ (run-io (run-tests suite)))
}

Several checks can be combined in one @racketidfont{it} with @racketidfont{all-checks}
(a list of assertions, where the first failure wins).

@section{Generators and properties}

A generator @racketidfont{(Gen a)} is a pure, seeded function from a size
and a seed to a value @italic{together with its tree of shrinks}.  Because
generation is pure, a failing property replays exactly from the seed the
runner prints; because shrinking is @italic{integrated}, every generator
shrinks for free — there is no separate shrink method to write.

Generators compose with @racket[fmap], @racket[flatmap], and @racket[do]:

@rackton-example[#:eval ev #:mode 'defs]{
(require rackton/unit)

;; a generator of even integers in [0, 200]
(: gen-even (Gen Integer))
(define gen-even (fmap (lambda (n) (* n 2)) (int-range 0 100)))

;; a generator of (Pair Integer Integer)
(define gen-point (gen-pair (int-range 0 9) (int-range 0 9)))
}

@racketidfont{for-all} builds a property from a generator and a predicate
(it uses @racket[show] to render counterexamples):

@rackton-example[#:eval ev #:mode 'defs]{
(require rackton/unit)

(: addition-commutes Test)
(define addition-commutes
  (it-prop "addition commutes"
    (for-all (int-range -1000 1000)
             (lambda (x) (== (+ x 0) x)))))
}

When a property fails, the runner shrinks the counterexample to a
minimal value and prints the replay seed:

@codeblock|{
  FAIL - too small: counterexample=5 seed=42
}|

Built-in generators include @racketidfont{int-range}, @racketidfont{bool},
@racketidfont{element-of}, @racketidfont{gen-pair}, @racketidfont{gen-list},
@racketidfont{replicate-gen}, @racketidfont{constant}, @racketidfont{gen-integer},
@racketidfont{gen-boolean}, and @racketidfont{gen-string}.  Use
@racketidfont{for-all-gen} when you want to supply your own renderer
instead of the @racket[Show] instance.

A property whose body @racket[panic]s is contained by the runner and
reported as a failure rather than aborting the whole run.

@section{Algebraic laws}

The law bundles turn a generator into a group of properties capturing the
laws of a structure.  Each is parameterised by a generator (and, for
@racketidfont{monoid-laws}, the identity element, supplied explicitly):

@rackton-example[#:eval ev]{
#lang rackton
(require rackton/unit)

(: number-laws Test)
(define number-laws
  (describe "Integer"
    (eq-laws  (int-range -50 50))
    (ord-laws (int-range -50 50))))

(: string-monoid Test)
(define string-monoid
  (monoid-laws gen-string ""))   ;; "" is the identity for string-append

(define _ (run-io (run-tests (describe "laws" number-laws string-monoid))))
}

@racketidfont{eq-laws} checks reflexivity and symmetry; @racketidfont{ord-laws}
checks reflexivity and totality of @racket[<=]; @racketidfont{semigroup-laws}
checks associativity of @racket[mappend]; @racketidfont{monoid-laws} additionally
checks that the supplied identity is a left and right unit.

@section[#:tag "protocol-laws-bundles"]{Running a protocol's own laws}

The bundles above restate each equation by hand.  When you write the laws
once, in a protocol's @racket[#:laws] clause (see
@secref["type-classes"]), and the module also
imports @racket[rackton/unit], Rackton generates a runnable bundle for
you automatically: a function @racketidfont{@racketvarfont{Protocol}-laws}
that turns generators into a @racket[Test] group, one property per law.
You never restate the equation — it comes from the declaration — and each
failing case names the law and labels every binder by its source name.

@rackton-example[#:eval ev]{
#lang rackton
(require rackton/unit)

;; the laws are written once, on the protocol
(protocol (Combine a)
  (: combine (-> a (-> a a)))
  #:laws
    ([associativity ((Eq a) =>
       (All ([x : a] [y : a] [z : a])
         (== (combine (combine x y) z)
             (combine x (combine y z)))))]))

(data MaxI (MkMax Integer))
(instance (Eq MaxI)
  (define (== p q) (match p [(MkMax a) (match q [(MkMax b) (== a b)])])))
(instance (Show MaxI)
  (define (show p) (match p [(MkMax a) (integer->string a)])))
;; max is associative
(instance (Combine MaxI)
  (define (combine p q)
    (match p [(MkMax a) (match q [(MkMax b) (MkMax (if (< a b) b a))])])))

(define gen-max (fmap (lambda (n) (MkMax n)) (int-range 0 100)))

;; Combine-laws is generated from the #:laws clause above
(define _ (run-io (run-tests (Combine-laws gen-max))))
}

The generated bundle's type is inferred — here
@racket[((Eq a) (Show a) (Combine a) => (-> (Gen a) Test))].  It takes one
generator per distinct binder type across the laws (so a first-order
protocol like @racket[Combine] takes a single @racket[(Gen a)]; a
higher-kinded one takes, e.g., @racket[(Gen (f a))]).  Because the bundle
is an ordinary value, you can @racket[provide] it and run it from another
module.

Generation is gated on the @racket[(require rackton/unit)] import — a
protocol declared without it keeps its laws as compile-time documentation
only.  It is also @emph{best-effort per law}: a law is skipped (and the
rest of the protocol's laws still generate) when it quantifies over a
function — there is no function generator — or when its body uses a
return-typed method such as @racket[pure] or @racket[mempty], which cannot
be dispatched without a value.  So a @racket[Functor]-shaped protocol gets
a runnable @racket[identity] law while its @racket[composition] law (which
quantifies over functions) is skipped, and a protocol whose every law is
unrunnable generates no bundle.

@section{Reproducibility and seeds}

Generation never performs @racket[IO]; it threads a pure, splittable
seed.  @racketidfont{run-property} takes the number of cases and a starting
@racket[Integer] seed, so re-running with the seed the runner reported
reproduces a failure deterministically.

@section{Notes and limitations}

@itemlist[
@item{There is no type-directed @racketidfont{Arbitrary} protocol in this
      version: a protocol member returning @racketidfont{(Gen a)} would be
      return-typed, and return-typed methods are resolved per instance at
      compile time and do not cross module boundaries.  Use the explicit
      generators instead.}
@item{For the same reason, build constant generators with
      @racketidfont{constant} rather than @racket[pure] when writing
      generators outside the library.}
@item{A generated @racketidfont{@racketvarfont{Protocol}-laws} bundle
      covers a higher-kinded protocol's first-order laws — a
      @racket[Functor] @racket[identity] law, say — but skips any law that
      quantifies over a function (@racket[Functor] @racket[composition]) or
      uses a return-typed method, since neither can be run.  For those,
      reach for the explicit bundles @racketidfont{functor-laws},
      @racketidfont{applicative-laws}, @racketidfont{monad-laws}, and
      @racketidfont{traversable-laws} — which substitute fixed
      representative functions and take an explicit container @racket[Eq] —
      or write the property directly with @racketidfont{for-all-gen}.}
@item{The pseudo-random generator is adequate for test-case generation,
      not statistical rigor.}]
