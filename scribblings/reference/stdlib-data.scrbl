#lang scribble/manual
@require[scribble/manual
         (for-label rackton rackton/data/bits rackton/data/bool rackton/data/char rackton/data/complex rackton/data/either rackton/data/result rackton/data/foldable rackton/data/function rackton/data/functor rackton/data/lazy rackton/data/arrow-lazy rackton/data/lens rackton/data/list rackton/data/list/nonempty rackton/data/map rackton/data/maybe rackton/data/monoid rackton/data/ord rackton/data/ratio rackton/data/semigroup rackton/data/set rackton/data/traversable rackton/data/tuple)
         "../rackton-eval.rkt"]
@(define ev (make-rackton-eval))

@title[#:tag "stdlib-data" #:style 'toc]{@tt{rackton/data} — containers and structures}

The @tt{data} family collects Rackton's container types and the
algebraic structures defined over them, after Haskell's @tt{Data.*}
hierarchy.  It spans the concrete collections
(@racketmodname[rackton/data/list], @racketmodname[rackton/data/map],
@racketmodname[rackton/data/set], @racketmodname[rackton/data/tuple],
and @racketmodname[rackton/data/list/nonempty]); the small sum types for
optional and fallible values (@racketmodname[rackton/data/maybe],
@racketmodname[rackton/data/either], @racketmodname[rackton/data/result]);
the classes that abstract over containers
(@racketmodname[rackton/data/semigroup],
@racketmodname[rackton/data/monoid],
@racketmodname[rackton/data/functor],
@racketmodname[rackton/data/foldable],
@racketmodname[rackton/data/traversable],
@racketmodname[rackton/data/ord]); scalar and combinator helpers (bits,
booleans, characters, complex numbers, rationals, functions, and the two
@racketmodname[rackton/data/lazy] evaluation modules); and the
@racketmodname[rackton/data/lens] optics core.  None of these are in the
auto-prelude — import the specific module you need.

@local-table-of-contents[]

@section{rackton/data/bits}
@defmodule[rackton/data/bits]

Bitwise operations over the prelude's @racket[Integer], in the spirit of
Haskell's @tt{Data.Bits}. Because Rackton has a single integral type, these
are plain @racket[bit-]-prefixed functions rather than a @racket[Bits] class;
integers are two's-complement of unbounded width, so @racket[bit-not] of a
non-negative value is negative and @racket[bit-count] is defined for
non-negative inputs.

@deftogether[(
  @defproc[(bit-and [a Integer] [b Integer]) Integer]
  @defproc[(bit-or [a Integer] [b Integer]) Integer]
  @defproc[(bit-xor [a Integer] [b Integer]) Integer]
)]{
  Bitwise conjunction, inclusive disjunction, and exclusive disjunction of two
  integers.
}

@defproc[(bit-not [a Integer]) Integer]{
  Bitwise complement (Haskell @tt{complement}).
}

@deftogether[(
  @defproc[(bit-shift-left [a Integer] [n Integer]) Integer]
  @defproc[(bit-shift-right [a Integer] [n Integer]) Integer]
)]{
  Shift @racket[a] by @racket[n] non-negative positions; @racket[bit-shift-left]
  fills with zeros and @racket[bit-shift-right] is the arithmetic
  (sign-extending) shift.
}

@deftogether[(
  @defproc[(bit-test [a Integer] [i Integer]) Boolean]
  @defproc[(bit-set [a Integer] [i Integer]) Integer]
  @defproc[(bit-clear [a Integer] [i Integer]) Integer]
)]{
  Test, set, and clear the bit at position @racket[i], 0-indexed from the
  least-significant bit.
}

@defproc[(bit-count [a Integer]) Integer]{
  Number of set bits in a non-negative integer (Haskell @tt{popCount}).
}


@section{rackton/data/bool}
@defmodule[rackton/data/bool]

Boolean utilities (@racket[Data.Bool]). The @tt{not}, @tt{and}, and @tt{or} operators live in the prelude; this module adds a case-analysis combinator and a guard alias.

@defproc[(bool [f a] [t a] [cond Boolean]) a]{Returns @racket[t] when @racket[cond] is @racket[#t] and @racket[f] otherwise (Haskell's @racket[bool], with false-then-true argument order).}

@defthing[otherwise Boolean]{Always @racket[#t]; reads well as the final guard alternative.}


@section{rackton/data/char}
@defmodule[rackton/data/char]

Data.Char-style predicates and conversions over the prelude's @racket[Char]
type. The prelude already ships @tt{char-upcase}, @tt{char-downcase},
@tt{char-alphabetic?}, @tt{char-numeric?}, @tt{char-whitespace?},
@tt{char->integer}, and @tt{integer->char}; this module adds the rest,
using @racket[(racket …)] escapes for the @tt{racket/base} char predicates the
prelude does not surface.

@deftogether[(@defproc[(ord [c Char]) Integer]
              @defproc[(chr [n Integer]) Char])]{
  @racket[ord] returns a character's code point. @racket[chr] is the total
  inverse, panicking on a code point that is out of range (like Haskell's
  @tt{chr}).}

@deftogether[(@defproc[(to-upper [c Char]) Char]
              @defproc[(to-lower [c Char]) Char])]{
  Upcase / downcase a character, aliasing the prelude's @tt{char-upcase}
  and @tt{char-downcase}.}

@defproc[(digit? [c Char]) Boolean]{
  Tests whether @racket[c] is an ASCII decimal digit (@racket[#\0]–@racket[#\9]).}

@defproc[(hex-digit? [c Char]) Boolean]{
  Tests whether @racket[c] is an ASCII hexadecimal digit (@racket[0]–@racket[9],
  @racket[a]–@racket[f], case-insensitive).}

@deftogether[(@defproc[(upper? [c Char]) Boolean]
              @defproc[(lower? [c Char]) Boolean])]{
  Test whether @racket[c] is an upper-case / lower-case letter.}

@deftogether[(@defproc[(alpha? [c Char]) Boolean]
              @defproc[(alpha-num? [c Char]) Boolean])]{
  @racket[alpha?] tests whether @racket[c] is alphabetic (aliasing the prelude's
  Unicode predicate); @racket[alpha-num?] tests whether it is alphabetic or
  numeric.}

@defproc[(space? [c Char]) Boolean]{
  Tests whether @racket[c] is whitespace.}

@defproc[(control? [c Char]) Boolean]{
  Tests whether @racket[c] is an ISO control character.}

@defproc[(punctuation? [c Char]) Boolean]{
  Tests whether @racket[c] is a punctuation character.}

@deftogether[(@defproc[(digit->int [c Char]) Integer]
              @defproc[(int->digit [n Integer]) Char])]{
  Convert between an ASCII digit character and its numeric value.}


@section{rackton/data/complex}
@defmodule[rackton/data/complex]

Derived operations on the prelude's @racket[Complex] type. The prelude
ships @racket[Complex] with @racket[make-complex], @tt{real-part},
@tt{imag-part}, and @tt{magnitude}; this module adds the
remaining operations, naming them @racket[mk-polar] / @racket[phase]
(rather than @tt{make-polar} / @tt{angle}) so those @tt{racket/base}
names stay usable inside @racket[(racket …)] escapes.

@defproc[(conjugate [z Complex]) Complex]{
  Complex conjugate: negate the imaginary part.}

@defproc[(phase [z Complex]) Float]{
  Phase angle in radians (Haskell's @tt{phase}).}

@defproc[(mk-polar [r Float] [theta Float]) Complex]{
  Build a complex number from polar coordinates.}

@defproc[(cis [theta Float]) Complex]{
  Unit complex at the given angle: @racket[cos θ + i sin θ].}

@defproc[(polar [z Complex]) (Pair Float Float)]{
  The @tt{(magnitude, phase)} pair.}

The exact counterpart @racket[ComplexExact] (the prelude's
@racket[make-complex-exact] / @racket[real-part-exact] /
@racket[imag-part-exact]) gets its derived operations here too.

@defproc[(conjugate-exact [z ComplexExact]) ComplexExact]{
  Exact conjugate: negate the imaginary part.}

@defproc[(complex-exact-norm [z ComplexExact]) Integer]{
  The Gaussian norm @tt{re² + im²}, an exact non-negative
  @racket[Integer].  Unlike @tt{magnitude} it takes no square root, so
  it stays exact.}

@defproc[(complex-exact->complex [z ComplexExact]) Complex]{
  Widen an exact complex number to the inexact @racket[Complex] type.}


@section{rackton/data/either}
@defmodule[rackton/data/either]

Data.Either over the prelude's @racket[Either] type (@racket[Left] /
@racket[Right]). The
@racket[Functor]/@racket[Applicative]/@racket[Monad]/@racket[Bifunctor]
instances for @racket[Either] live in the prelude; this module provides the
non-class eliminators, predicates, and collectors.

@defproc[(either [f (-> a c)] [g (-> b c)] [r (Either a b)]) c]{
  Eliminator: applies @racket[f] to a @racket[Left] payload and @racket[g] to
  a @racket[Right] payload.}

@deftogether[(@defproc[(is-left [r (Either a b)]) Boolean]
              @defproc[(is-right [r (Either a b)]) Boolean])]{
  Test whether @racket[r] is a @racket[Left] or a @racket[Right], respectively.}

@deftogether[(@defproc[(from-left [default a] [r (Either a b)]) a]
              @defproc[(from-right [default b] [r (Either a b)]) b])]{
  Extract the @racket[Left] (resp. @racket[Right]) payload, falling back to
  @racket[default] when @racket[r] is the other variant.}

@deftogether[(@defproc[(lefts [rs (List (Either a b))]) (List a)]
              @defproc[(rights [rs (List (Either a b))]) (List b)])]{
  Collect all @racket[Left] payloads (Haskell @tt{lefts}) or all @racket[Right]
  payloads (Haskell @tt{rights}), respectively, preserving order.}

@defproc[(partition-eithers [rs (List (Either a b))]) (Pair (List a) (List b))]{
  Split @racket[rs] into a @racket[Pair] of the @racket[Left] payloads and the
  @racket[Right] payloads (Haskell @tt{partitionEithers}), preserving order.}

@defproc[(right->maybe [r (Either a b)]) (Maybe b)]{
  Convert @racket[Right] to @racket[Some] and @racket[Left] to @racket[None].}

@defproc[(maybe->either [left a] [m (Maybe b)]) (Either a b)]{
  Convert @racket[Some] to @racket[Right] and @racket[None] to
  @racket[(Left left)].}

@section{rackton/data/result}
@defmodule[rackton/data/result]

A result/success-flavored coproduct.  @racket[Result] is isomorphic to the
prelude's @racket[Either] (@racket[Err] ↔ @racket[Left], @racket[Ok] ↔
@racket[Right]) but a @emph{distinct nominal type}, for code where the
@tt{Ok}/@tt{Err} naming reads better than @tt{Left}/@tt{Right}.  Unlike the
helpers in @racketmodname[rackton/data/either], this module also defines
@racket[Result]'s own class instances; @racket[result->either] /
@racket[either->result] bridge to the prelude coproduct.

@deftogether[(
@defform[#:kind "type" #:id Result #:literals (data Err Ok)
         (data (Result e a)
           (Err e)
           (Ok a))]
@defthing[#:kind "constructor" Err (-> e (Result e a))]
@defthing[#:kind "constructor" Ok (-> a (Result e a))])]{

Tagged-union for fallible computations.  @racket[(Result e a)] is either an
@racket[Err] of type @racket[e] or an @racket[Ok] of type @racket[a].
Instances: @racket[Functor], @racket[Applicative], @racket[Monad]
(over the @racket[a]; @racket[e] is fixed per chain), @racket[Bifunctor],
@racket[Eq], @racket[Show].}

@defproc[(result [f (-> e c)] [g (-> a c)] [r (Result e a)]) c]{
  Eliminator: applies @racket[f] to an @racket[Err] payload and @racket[g] to
  an @racket[Ok] payload.}

@deftogether[(@defproc[(is-ok [r (Result e a)]) Boolean]
              @defproc[(is-err [r (Result e a)]) Boolean])]{
  Test whether @racket[r] is an @racket[Ok] or an @racket[Err], respectively.}

@deftogether[(@defproc[(from-ok [default a] [r (Result e a)]) a]
              @defproc[(from-err [default e] [r (Result e a)]) e])]{
  Extract the @racket[Ok] (resp. @racket[Err]) payload, falling back to
  @racket[default] when @racket[r] is the other variant.}

@deftogether[(@defproc[(oks [rs (List (Result e a))]) (List a)]
              @defproc[(errs [rs (List (Result e a))]) (List e)])]{
  Collect all @racket[Ok] payloads or all @racket[Err] payloads, respectively,
  preserving order.}

@defproc[(partition-results [rs (List (Result e a))]) (Pair (List e) (List a))]{
  Split @racket[rs] into a @racket[Pair] of the @racket[Err] payloads and the
  @racket[Ok] payloads, preserving order.}

@defproc[(ok->maybe [r (Result e a)]) (Maybe a)]{
  Convert @racket[Ok] to @racket[Some] and @racket[Err] to @racket[None].}

@defproc[(maybe->result [e e] [m (Maybe a)]) (Result e a)]{
  Convert @racket[Some] to @racket[Ok] and @racket[None] to @racket[(Err e)].}

@deftogether[(@defproc[(result->either [r (Result e a)]) (Either e a)]
              @defproc[(either->result [x (Either e a)]) (Result e a)])]{
  Bridge to and from the prelude @racket[Either]: @racket[Err] ↔ @racket[Left]
  and @racket[Ok] ↔ @racket[Right].}


@section{rackton/data/foldable}
@defmodule[rackton/data/foldable]

Generic folds over any @racket[Foldable] container (the prelude's instances are @racket[List] and @racket[Maybe]). These are the derived combinators built on the prelude's @racket[foldr] member.

@defproc[(fold-map [f (-> a b)] [t (t a)]) b]{
  Maps each element into a @racket[Monoid] and combines the results (Haskell's @tt{foldMap}); requires @racket[(Monoid b)] and @racket[(Foldable t)].}

@defproc[(fold [t (t m)]) m]{
  Combines a foldable of @racket[Monoid] values into one (Haskell's @tt{fold} / @tt{mconcat}); requires @racket[(Monoid m)] and @racket[(Foldable t)].}

@defproc[(any-of [p (-> a Boolean)] [t (t a)]) Boolean]{
  Returns true when @racket[p] holds for some element of the foldable; requires @racket[(Foldable t)].}

@defproc[(all-of [p (-> a Boolean)] [t (t a)]) Boolean]{
  Returns true when @racket[p] holds for every element of the foldable; requires @racket[(Foldable t)].}

@defproc[(elem-of [x a] [t (t a)]) Boolean]{
  Tests membership of @racket[x] in any foldable using @racket[==]; requires @racket[(Eq a)] and @racket[(Foldable t)].}


@section{rackton/data/function}
@defmodule[rackton/data/function]

Function combinators in the style of Haskell's @tt{Data.Function}. The
combinators @racket[id], @racket[const], @racket[flip], and
@racket[compose] live in the prelude; this module adds the rest.

@defproc[(on [g (-> b (-> b c))] [f (-> a b)] [x a] [y a]) c]{
  Combines two values under a common projection: @racket[(on g f x y)] is
  @racket[(g (f x) (f y))], e.g. @racket[(sort-by (on < key) …)].}

@defproc[(apply-to [x a] [f (-> a b)]) b]{
  Reverse application (Haskell's @tt{&}): @racket[(apply-to x f)] is
  @racket[(f x)].}


@section{rackton/data/functor}
@defmodule[rackton/data/functor]

Data.Functor helpers built on the prelude's @racket[Functor] class. The
core @racket[fmap] method and @tt{void} live in the prelude; this
module adds the @racket[<$], @racket[<&>], and @racket[$>] combinators.

@defproc[(const-map [a a] [fa (f b)]) (f a)]{
  Replaces every value in @racket[fa] with @racket[a] (Haskell @tt{<$}).}

@defproc[(fmap-flipped [fa (f a)] [f (-> a b)]) (f b)]{
  Equal to @racket[(fmap f fa)] with the arguments flipped (Haskell @tt{<&>}).}

@defproc[(const-replace-flipped [fa (f a)] [b b]) (f b)]{
  Replaces every value in @racket[fa] with @racket[b], with the arguments
  flipped relative to @racket[const-map] (Haskell @tt{$>}).}


@section{rackton/data/lazy}
@defmodule[rackton/data/lazy]

First-class laziness for a strict language: @racket[Lazy] is an opaque,
memoizing deferred computation built with the @racket[delay] form and run
with @racket[force] (call-by-need, evaluated at most once and cached).
@racket[Stream] is a lazy cons-list whose tail is deferred, so producers may
be infinite while consumers force only the prefix they need.

@defidform[#:kind "type" Lazy]{An opaque, memoizing deferred computation of
type @racket[(Lazy a)]; construct one with the @racket[delay] form and run it
with @racket[force].}

@defproc[(make-lazy [thunk (-> Unit a)]) (Lazy a)]{The target of the
@racket[delay] form; user code normally writes @racket[(delay e)] rather than
calling this directly. Provided via @tt{rackton/private/lazy-runtime}.}

@defproc[(force [lz (Lazy a)]) a]{Force a @racket[Lazy], computing it the
first time and caching the result thereafter. Provided via
@tt{rackton/private/lazy-runtime}.}

@deftogether[(
@defform[#:kind "type" #:link-target? #f #:id Stream #:literals (data SNil SCons Lazy)
         (data (Stream a)
           SNil
           (SCons a (Lazy (Stream a))))]
@defidform[#:kind "type" Stream]
@defthing[#:kind "constructor" SNil (Stream a)]
@defthing[#:kind "constructor" SCons (-> a (-> (Lazy (Stream a)) (Stream a)))])]{
A lazy cons-list of type
@racket[(Stream a)]: the tail of @racket[SCons] is a @racket[Lazy], so a
producer can be infinite while a consumer forces only the prefix it needs.
@racket[SNil] is the empty stream; @racket[SCons] prepends an element with a
deferred tail.}

@defproc[(stream-head [s (Stream a)]) (Maybe a)]{The first element, if any.}

@defproc[(stream-tail [s (Stream a)]) (Stream a)]{Drop the first element,
forcing one tail; the empty stream stays empty.}

@defproc[(stream-take [n Integer] [s (Stream a)]) (List a)]{The first
@racket[n] elements as a strict @racket[List], forcing only the tails needed.}

@defproc[(stream-map [f (-> a b)] [s (Stream a)]) (Stream b)]{Map a function
over a stream, keeping the tail deferred.}

@defproc[(stream-filter [p (-> a Boolean)] [s (Stream a)]) (Stream a)]{Keep
only elements satisfying @racket[p], lazily.}

@defproc[(stream-append [xs (Stream a)] [ys (Stream a)]) (Stream a)]{Concatenate
two streams lazily.}

@defproc[(stream-repeat [x a]) (Stream a)]{An infinite stream of a single
repeated value.}

@defproc[(stream-iterate [f (-> a a)] [x a]) (Stream a)]{The infinite stream
@racket[x], @racket[(f x)], @racket[(f (f x))], ….}

@defproc[(stream-from [n Integer]) (Stream Integer)]{The infinite stream
@racket[n], @racket[n+1], @racket[n+2], ….}

@defproc[(list->stream [xs (List a)]) (Stream a)]{A finite stream built from a
@racket[List].}


@section{rackton/data/arrow-lazy}
@defmodule[rackton/data/arrow-lazy]

A lazy-function arrow whose @racket[ArrowLoop] can tie a value-recursion
knot — the first arrow over which @racket[proc] @racket[rec] is runnable.
The strict @racket[(->)] arrow has no @racket[ArrowLoop] (feeding an
output back forces it before it is produced).  @racket[LFun] is a
function on @emph{lazy} values paired with the lazy-component product
@racket[LPair], so @racket[arrow-loop] threads the feedback unforced.

The instances supplied are @racket[Prod] for @racket[LPair],
@racket[Coprod] for @racket[LEither], and @racket[Category],
@racket[Arrow], @racket[ArrowChoice], @racket[ArrowApply], and
@racket[ArrowLoop] for @racket[LFun].

@deftogether[(
@defform[#:kind "type & constructor" #:link-target? #f #:id LFun #:literals (data Lazy ->)
         (data (LFun a b)
           (LFun (-> (Lazy a) (Lazy b))))]
@defthing[#:kind "type & constructor" LFun (-> (-> (Lazy a) (Lazy b)) (LFun a b))])]{A lazy-function arrow:
@racket[(LFun (-> (Lazy a) (Lazy b)))] wraps a function on lazy values.}

@deftogether[(
@defform[#:kind "type & constructor" #:link-target? #f #:id LPair #:literals (data Lazy)
         (data (LPair a b)
           (LPair (Lazy a) (Lazy b)))]
@defthing[#:kind "type & constructor" LPair (-> (Lazy a) (-> (Lazy b) (LPair a b)))])]{The lazy product:
@racket[(LPair (Lazy a) (Lazy b))] — both components are deferred, so
projecting one never forces the other (the feedback channel of a loop).}

@deftogether[(
@defform[#:kind "type" #:id LEither #:literals (data LLeft LRight Lazy)
         (data (LEither a b)
           (LLeft (Lazy a))
           (LRight (Lazy b)))]
@defthing[#:kind "constructor" LLeft (-> (Lazy a) (LEither a b))]
@defthing[#:kind "constructor" LRight (-> (Lazy b) (LEither a b))])]{The lazy
coproduct, dual to @racket[LPair].  @racket[LLeft] and @racket[LRight] are the
left and right injections.}

@defproc[(run-lfun [lf (LFun a b)] [x a]) b]{Run a lazy arrow on a strict
input and force the result.}

@defproc[(lcons [x a]) (LFun (Stream a) (Stream a))]{The arrow that
prepends @racket[x] to a stream, dropping its (lazy) input into the new
@racket[SCons]'s deferred tail.  Because it never forces its argument it
can sit in a @racket[proc] @racket[rec] feedback path and stay
productive, unlike an @racket[arr]-lifted function — which forces.  A
runnable example, the self-referential infinite stream of @racket[1]s:

@rackton-example[#:eval ev #:mode 'defs #:context? #t]{
(require rackton/data/arrow-lazy
         rackton/data/lazy)

(: ones (Stream Integer))
(define ones
  (run-lfun
   (proc (_)
     (rec [s <- (feed (lcons 1) s)])
     (feed (arr (lambda (z) z)) s))
   0))
}

@rackton-example[#:eval ev #:mode 'value #:context? #t]{
(require rackton/data/lazy)
(stream-head ones)
}

@rackton-example[#:eval ev #:mode 'value #:context? #t]{
(require rackton/data/lazy)
(stream-take 5 ones)
}}


@section{rackton/data/lens}
@defmodule[rackton/data/lens]

Composable optics for functional access and update: @racket[Lens] (a
getter/setter pair focusing on exactly one part), @racket[Prism]
(focusing on a single sum-type constructor), and @racket[Traversal]
(focusing on zero-or-more parts). Required separately from the prelude;
any module using @racket[#:deriving Lens]/@racket[#:deriving Prism] must
require this module too.

@deftogether[(
@defform[#:kind "type & constructor" #:link-target? #f #:id Lens #:literals (data ->)
         (data (Lens s a)
           (Lens (-> s a) (-> s (-> a s))))]
@defthing[#:kind "type & constructor" Lens (-> (-> s a) (-> s (-> a s)) (Lens s a))])]{
  An optic packing a function to extract an @racket[a] from an @racket[s]
  and a function to inject a new @racket[a] back into an existing
  @racket[s] (a getter and a setter).}

@deftogether[(
@defproc[(view [l (Lens s a)] [s s]) a]
@defproc[(set [l (Lens s a)] [v a] [s s]) s]
@defproc[(over [l (Lens s a)] [f (-> a a)] [s s]) s]
)]{
  @racket[view] extracts the focused @racket[a] from @racket[s];
  @racket[set] replaces it with @racket[v]; @racket[over] applies
  @racket[f] to it, in each case returning the (possibly updated)
  @racket[s].}

@defproc[(lens-compose [outer (Lens s a)] [inner (Lens a b)]) (Lens s b)]{
  Composes two lenses, focusing through @racket[outer] then @racket[inner].}

@deftogether[(
@defform[#:kind "type & constructor" #:link-target? #f #:id Prism #:literals (data Maybe ->)
         (data (Prism s a)
           (Prism (-> s (Maybe a)) (-> a s)))]
@defthing[#:kind "type & constructor" Prism (-> (-> s (Maybe a)) (-> a s) (Prism s a))])]{
  An optic focusing on a single sum-type constructor, built from a partial
  extractor and a constructor.}

@deftogether[(
@defproc[(preview [p (Prism s a)] [s s]) (Maybe a)]
@defproc[(review [p (Prism s a)] [a a]) s]
)]{
  @racket[preview] returns @racket[Some] when the focused constructor
  matches and @racket[None] otherwise; @racket[review] always succeeds,
  building the target constructor from @racket[a].}

@deftogether[(
@defform[#:kind "type & constructor" #:link-target? #f #:id Traversal #:literals (data List ->)
         (data (Traversal s a)
           (Traversal (-> s (List a))
                      (-> (-> a a) (-> s s))))]
@defthing[#:kind "type & constructor" Traversal (-> (-> s (List a)) (-> (-> a a) (-> s s)) (Traversal s a))])]{
  An optic focusing on zero-or-more sub-parts, built from a gatherer and a
  modifier.}

@deftogether[(
@defproc[(to-list-of [t (Traversal s a)] [s s]) (List a)]
@defproc[(over-of [t (Traversal s a)] [f (-> a a)] [s s]) s]
)]{
  @racket[to-list-of] gathers all focused parts into a list;
  @racket[over-of] applies @racket[f] to every focused part.}

@defthing[list-traversal (Traversal (List a) a)]{
  A built-in traversal that focuses on every element of a @racket[List].}

@defproc[(lens-as-traversal [l (Lens s a)]) (Traversal s a)]{
  Promotes a lens to a traversal with a single focus.}

@deftogether[(
@defform[#:kind "type & constructor" #:link-target? #f #:id Tuple3 #:literals (data)
         (data (Tuple3 a b c)
           (Tuple3 a b c))]
@defthing[#:kind "type & constructor" Tuple3 (-> a (-> b (-> c (Tuple3 a b c))))]
@defform[#:kind "type & constructor" #:link-target? #f #:id Tuple4 #:literals (data)
         (data (Tuple4 a b c d)
           (Tuple4 a b c d))]
@defthing[#:kind "type & constructor" Tuple4 (-> a (-> b (-> c (-> d (Tuple4 a b c d)))))]
@defform[#:kind "type & constructor" #:link-target? #f #:id Tuple5 #:literals (data)
         (data (Tuple5 a b c d e)
           (Tuple5 a b c d e))]
@defthing[#:kind "type & constructor" Tuple5 (-> a (-> b (-> c (-> d (-> e (Tuple5 a b c d e))))))]
@defform[#:kind "type & constructor" #:link-target? #f #:id Tuple6 #:literals (data)
         (data (Tuple6 a b c d e f)
           (Tuple6 a b c d e f))]
@defthing[#:kind "type & constructor" Tuple6 (-> a (-> b (-> c (-> d (-> e (-> f (Tuple6 a b c d e f)))))))]
@defform[#:kind "type & constructor" #:link-target? #f #:id Tuple7 #:literals (data)
         (data (Tuple7 a b c d e f g)
           (Tuple7 a b c d e f g))]
@defthing[#:kind "type & constructor" Tuple7 (-> a (-> b (-> c (-> d (-> e (-> f (-> g (Tuple7 a b c d e f g))))))))]
)]{
  Flat tuple focus types (arity 3 through 7) used as the focus of a
  multi-field @racket[#:deriving Prism]; arity 2 reuses the prelude's
  @racket[Pair]. Each derives @racket[Eq], @racket[Ord], and
  @racket[Show].}


@section{rackton/data/list}
@defmodule[rackton/data/list]

List utilities moved out of the auto-prelude (the core ops stay in the prelude).
These less-core helpers, kebab-case and curried like the prelude's list ops, give
Data.List parity; partial accessors return @racket[Maybe] since Rackton prefers
total functions.

@defproc[(zip [as (List a)] [bs (List b)]) (List (Pair a b))]{Pair up corresponding elements, stopping at the shorter list.}

@defproc[(take [n Integer] [xs (List a)]) (List a)]{The first @racket[n] elements of @racket[xs].}

@defproc[(drop [n Integer] [xs (List a)]) (List a)]{Drop the first @racket[n] elements of @racket[xs].}

@defproc[(find [p (-> a Boolean)] [xs (List a)]) (Maybe a)]{The first element satisfying @racket[p], or @racket[None].}

@defproc[(concat-map [f (-> a (List b))] [xs (List a)]) (List b)]{Map @racket[f] over @racket[xs] and concatenate the results.}

@defproc[(split-at [n Integer] [xs (List a)]) (Pair (List a) (List a))]{Split @racket[xs] into its first @racket[n] elements and the rest.}

@defproc[(merge-lists [xs (List a)] [ys (List a)]) (List a)]{Stably merge two @racket[Ord]-sorted lists into one sorted list.}

@defproc[(sort [xs (List a)]) (List a)]{Stable O(n log n) merge sort over an @racket[Ord] instance.}

@defproc[(head [xs (List a)]) (Maybe a)]{The first element, or @racket[None] on an empty list.}

@defproc[(tail [xs (List a)]) (Maybe (List a))]{All but the first element, or @racket[None] on an empty list.}

@defproc[(last [xs (List a)]) (Maybe a)]{The final element, or @racket[None] on an empty list.}

@defproc[(init [xs (List a)]) (Maybe (List a))]{All but the last element, or @racket[None] on an empty list.}

@defproc[(empty? [xs (List a)]) Boolean]{True when @racket[xs] is @racket[Nil].}

@defproc[(elem [x a] [xs (List a)]) Boolean]{True when @racket[x] occurs in @racket[xs] (by @racket[Eq]).}

@defproc[(not-elem [x a] [xs (List a)]) Boolean]{True when @racket[x] does not occur in @racket[xs] (by @racket[Eq]).}

@defproc[(lookup [k k] [xs (List (Pair k v))]) (Maybe v)]{Look up the first value keyed by @racket[k] in an association list.}

@defproc[(elem-index [x a] [xs (List a)]) (Maybe Integer)]{The index of the first element equal to @racket[x] (by @racket[Eq]).}

@defproc[(find-index [p (-> a Boolean)] [xs (List a)]) (Maybe Integer)]{The index of the first element satisfying @racket[p].}

@defproc[(concat [xss (List (List a))]) (List a)]{Concatenate a list of lists.}

@defproc[(intersperse [sep a] [xs (List a)]) (List a)]{Insert @racket[sep] between adjacent elements of @racket[xs].}

@defproc[(intercalate [sep (List a)] [xss (List (List a))]) (List a)]{Insert @racket[sep] between the lists and concatenate.}

@defproc[(replicate [n Integer] [x a]) (List a)]{A list of @racket[n] copies of @racket[x].}

@defproc[(range [lo Integer] [hi Integer]) (List Integer)]{The inclusive integer range @racket[lo] through @racket[hi].}

@defproc[(take-while [p (-> a Boolean)] [xs (List a)]) (List a)]{The longest leading run of elements satisfying @racket[p].}

@defproc[(drop-while [p (-> a Boolean)] [xs (List a)]) (List a)]{Drop the longest leading run of elements satisfying @racket[p].}

@defproc[(span [p (-> a Boolean)] [xs (List a)]) (Pair (List a) (List a))]{Split @racket[xs] at the first element failing @racket[p].}

@defproc[(break [p (-> a Boolean)] [xs (List a)]) (Pair (List a) (List a))]{Split @racket[xs] at the first element satisfying @racket[p].}

@defproc[(partition [p (-> a Boolean)] [xs (List a)]) (Pair (List a) (List a))]{Partition @racket[xs] into those satisfying @racket[p] and the rest.}

@defproc[(fold-left [f (-> b (-> a b))] [z b] [xs (List a)]) b]{Left fold of @racket[f] over @racket[xs] starting from @racket[z].}

@defproc[(all? [p (-> a Boolean)] [xs (List a)]) Boolean]{True when every element of @racket[xs] satisfies @racket[p].}

@defproc[(any? [p (-> a Boolean)] [xs (List a)]) Boolean]{True when some element of @racket[xs] satisfies @racket[p].}

@defproc[(and-list [xs (List Boolean)]) Boolean]{True when every element is true.}

@defproc[(or-list [xs (List Boolean)]) Boolean]{True when some element is true.}

@defproc[(maximum [xs (List a)]) (Maybe a)]{The greatest element by @racket[Ord], or @racket[None] on empty.}

@defproc[(minimum [xs (List a)]) (Maybe a)]{The least element by @racket[Ord], or @racket[None] on empty.}

@defproc[(zip-with [f (-> a (-> b c))] [as (List a)] [bs (List b)]) (List c)]{Combine corresponding elements with @racket[f], stopping at the shorter list.}

@defproc[(unzip [xs (List (Pair a b))]) (Pair (List a) (List b))]{Split a list of pairs into a pair of lists.}

@defproc[(nub [xs (List a)]) (List a)]{Remove duplicate elements (by @racket[Eq]), keeping the first occurrence.}

@defproc[(nub-by [eq? (-> a (-> a Boolean))] [xs (List a)]) (List a)]{Remove duplicates under a supplied equality predicate.}

@defproc[(scanl [f (-> b (-> a b))] [z b] [xs (List a)]) (List b)]{Left scan: the list of successive left-fold accumulators.}

@defproc[(scanr [f (-> a (-> b b))] [z b] [xs (List a)]) (List b)]{Right scan: the list of successive right-fold accumulators.}

@defproc[(group [xs (List a)]) (List (List a))]{Group consecutive equal elements (by @racket[Eq]) into runs.}

@defproc[(inits [xs (List a)]) (List (List a))]{All prefixes of @racket[xs], shortest first.}

@defproc[(tails [xs (List a)]) (List (List a))]{All suffixes of @racket[xs], longest first.}

@defproc[(prefix? [ps (List a)] [xs (List a)]) Boolean]{True when @racket[ps] is a prefix of @racket[xs] (by @racket[Eq]).}

@defproc[(suffix? [ss (List a)] [xs (List a)]) Boolean]{True when @racket[ss] is a suffix of @racket[xs] (by @racket[Eq]).}

@defproc[(infix? [ns (List a)] [xs (List a)]) Boolean]{True when @racket[ns] occurs contiguously within @racket[xs] (by @racket[Eq]).}

@defproc[(strip-prefix [ps (List a)] [xs (List a)]) (Maybe (List a))]{Drop prefix @racket[ps] from @racket[xs], or @racket[None] if absent.}

@defproc[(transpose [xss (List (List a))]) (List (List a))]{Transpose rows and columns of a list of lists.}

@defproc[(delete [x a] [xs (List a)]) (List a)]{Remove the first occurrence of @racket[x] (by @racket[Eq]).}

@defproc[(insert [x a] [xs (List a)]) (List a)]{Insert @racket[x] into a sorted list before the first strictly-greater element.}

@defproc[(list-difference [xs (List a)] [ys (List a)]) (List a)]{Remove from @racket[xs] one occurrence of each element of @racket[ys].}

@defproc[(union [xs (List a)] [ys (List a)]) (List a)]{The list union: @racket[xs] followed by the new, deduplicated elements of @racket[ys].}

@defproc[(intersect [xs (List a)] [ys (List a)]) (List a)]{The elements of @racket[xs] that also occur in @racket[ys].}

@defproc[(merge-by [lt? (-> a (-> a Boolean))] [xs (List a)] [ys (List a)]) (List a)]{Stably merge two lists ordered by the strict less-than @racket[lt?].}

@defproc[(sort-by [lt? (-> a (-> a Boolean))] [xs (List a)]) (List a)]{Merge sort using the strict less-than comparator @racket[lt?].}

@defproc[(sort-on [key (-> a b)] [xs (List a)]) (List a)]{Sort @racket[xs] by an @racket[Ord] key projection.}

@defproc[(foldl1 [f (-> a (-> a a))] [xs (List a)]) (Maybe a)]{Left fold with the first element as seed, or @racket[None] on empty.}

@defproc[(foldr1 [f (-> a (-> a a))] [xs (List a)]) (Maybe a)]{Right fold with the last element as seed, or @racket[None] on empty.}

@defproc[(iterate-n [n Integer] [f (-> a a)] [x a]) (List a)]{The list @racket[x], @racket[(f x)], @racket[(f (f x))], … of length @racket[n].}

@defproc[(cycle-n [n Integer] [xs (List a)]) (List a)]{@racket[n] copies of @racket[xs] concatenated.}

@defproc[(unfoldr [f (-> b (Maybe (Pair a b)))] [seed b]) (List a)]{Build a list by repeatedly unfolding @racket[seed] until @racket[f] yields @racket[None].}

@defproc[(subsequences [xs (List a)]) (List (List a))]{All subsequences of @racket[xs].}

@defproc[(selections [xs (List a)]) (List (Pair a (List a)))]{Each element paired with the list of the others, order preserved.}

@defproc[(permutations [xs (List a)]) (List (List a))]{All permutations of @racket[xs].}

@defproc[(map-accum-l [f (-> s (-> a (Pair s b)))] [s s] [xs (List a)]) (Pair s (List b))]{Left-to-right stateful map threading an accumulator through @racket[f].}


@section{rackton/data/list/nonempty}
@defmodule[rackton/data/list/nonempty]

A list guaranteed to have at least one element, so @racket[ne-head] and
@racket[ne-tail] are total operations.

@deftogether[(
@defform[#:kind "type & constructor" #:link-target? #f #:id NonEmpty #:literals (data List)
         (data (NonEmpty a)
           (NonEmpty a (List a)))]
@defthing[#:kind "type & constructor" NonEmpty (-> a (-> (List a) (NonEmpty a)))])]{A non-empty list of @racket[a]: a head element
  together with a possibly-empty tail.}

@defproc[(nonempty [h a] [t (List a)]) (NonEmpty a)]{Constructs a @racket[NonEmpty]
  from a head and a (possibly empty) tail.}

@defproc[(ne-head [ne (NonEmpty a)]) a]{Returns the head element.}

@defproc[(ne-tail [ne (NonEmpty a)]) (List a)]{Returns the tail @racket[List].}

@defproc[(ne-to-list [ne (NonEmpty a)]) (List a)]{Converts to an ordinary
  @racket[List].}

@defproc[(ne-from-list [xs (List a)]) (Maybe (NonEmpty a))]{Returns @racket[Some]
  non-empty list, or @racket[None] when @racket[xs] is empty.}

@defproc[(ne-cons [x a] [ne (NonEmpty a)]) (NonEmpty a)]{Prepends an element.}

@defproc[(ne-map [f (-> a b)] [ne (NonEmpty a)]) (NonEmpty b)]{Maps @racket[f] over
  every element.}

@defproc[(ne-length [ne (NonEmpty a)]) Integer]{Returns the number of elements
  (always at least one).}


@section{rackton/data/map}
@defmodule[rackton/data/map]

Immutable key/value maps (Data.Map parity), moved out of the auto-prelude.
The runtime is Racket immutable hashes reached via @racket[foreign]; keys
compare by structural equality, so no @racket[(Eq k)] constraint is needed.

@defidform[#:kind "type" Map]{The immutable map type @racket[(Map k v)] from
keys of type @racket[k] to values of type @racket[v]. Opaque; build values
with @racket[empty-map], @racket[map-insert], @racket[map-singleton], or
@racket[map-from-list].}

@defthing[empty-map (Map k v)]{The empty map.}

@deftogether[(
@defproc[(map-insert [k k] [v v] [m (Map k v)]) (Map k v)]
@defproc[(map-lookup [k k] [m (Map k v)]) (Maybe v)]
@defproc[(map-delete [k k] [m (Map k v)]) (Map k v)]
)]{Insert @racket[v] at key @racket[k] (replacing any existing value), look up
the value at @racket[k] as a @racket[Maybe], and remove key @racket[k].}

@deftogether[(
@defproc[(map-keys [m (Map k v)]) (List k)]
@defproc[(map-values [m (Map k v)]) (List v)]
@defproc[(map-size [m (Map k v)]) Integer]
)]{The list of keys, the list of values, and the number of entries.}

@defproc[(map-fold [f (-> k (-> v (-> b b)))] [z b] [m (Map k v)]) b]{Fold
@racket[f] over every key/value pair, starting from accumulator @racket[z].}

@defproc[(group-by [key (-> a k)] [xs (List a)]) (Map k (List a))]{Bucket a
list into a map, grouping elements by their @racket[key].}

@deftogether[(
@defproc[(map-member? [k k] [m (Map k v)]) Boolean]
@defproc[(map-empty? [m (Map k v)]) Boolean]
)]{Whether key @racket[k] is present, and whether the map has no entries.}

@defproc[(map-singleton [k k] [v v]) (Map k v)]{The map containing only the
single binding @racket[k] to @racket[v].}

@deftogether[(
@defproc[(map-elems [m (Map k v)]) (List v)]
@defproc[(map-to-list [m (Map k v)]) (List (Pair k v))]
@defproc[(map-from-list [ps (List (Pair k v))]) (Map k v)]
)]{The list of values, the list of key/value pairs, and a map built from a
list of key/value pairs.}

@defproc[(map-find-with-default [d v] [k k] [m (Map k v)]) v]{The value at
@racket[k], or the default @racket[d] when @racket[k] is absent.}

@defproc[(map-adjust [f (-> v v)] [k k] [m (Map k v)]) (Map k v)]{Apply
@racket[f] to the value at @racket[k], if present; otherwise return @racket[m]
unchanged.}

@defproc[(map-insert-with [f (-> v (-> v v))] [k k] [v v] [m (Map k v)]) (Map k v)]{Insert
@racket[v] at @racket[k], or @racket[(f v old)] when @racket[k] already holds
@racket[old].}

@deftogether[(
@defproc[(map-union [m1 (Map k v)] [m2 (Map k v)]) (Map k v)]
@defproc[(map-union-with [f (-> v (-> v v))] [m1 (Map k v)] [m2 (Map k v)]) (Map k v)]
)]{Left-biased union (values from @racket[m1] win on shared keys), and a union
that combines colliding values with @racket[f].}

@defproc[(map-difference [m1 (Map k v)] [m2 (Map k w)]) (Map k v)]{The map
@racket[m1] with every key also present in @racket[m2] removed.}

@defproc[(map-intersection-with [f (-> v (-> w x))] [m1 (Map k v)] [m2 (Map k w)]) (Map k x)]{The
map over keys present in both, combining their values with @racket[f].}

@deftogether[(
@defproc[(map-map [f (-> v w)] [m (Map k v)]) (Map k w)]
@defproc[(map-map-with-key [f (-> k (-> v w))] [m (Map k v)]) (Map k w)]
)]{Transform every value with @racket[f], optionally also given its key.}

@deftogether[(
@defproc[(map-filter [p (-> v Boolean)] [m (Map k v)]) (Map k v)]
@defproc[(map-filter-with-key [p (-> k (-> v Boolean))] [m (Map k v)]) (Map k v)]
)]{Keep only the entries whose value (or key/value pair) satisfies @racket[p].}


@section{rackton/data/maybe}
@defmodule[rackton/data/maybe]

Additive helpers over the prelude's @racket[Maybe] type, in the spirit of Haskell's @tt{Data.Maybe}: eliminators, predicates, defaulting, and conversions to and from lists.

@defproc[(maybe [d b] [f (-> a b)] [m (Maybe a)]) b]{
Eliminator for @racket[Maybe]: returns @racket[f] applied to the @racket[Some] payload, or the default @racket[d] when @racket[m] is @racket[None].}

@deftogether[(@defproc[(is-just [m (Maybe a)]) Boolean]
              @defproc[(is-nothing [m (Maybe a)]) Boolean])]{
@racket[is-just] is true when @racket[m] is a @racket[Some]; @racket[is-nothing] is true when @racket[m] is @racket[None].}

@defproc[(from-maybe [d a] [m (Maybe a)]) a]{
Returns the @racket[Some] payload, or the default @racket[d] when @racket[m] is @racket[None].}

@defproc[(from-just [m (Maybe a)]) a]{
Returns the @racket[Some] payload, or panics on @racket[None] (Haskell's @tt{fromJust}); prefer @racket[from-maybe] or @racket[maybe] when a total result is possible.}

@defproc[(map-maybe [f (-> a (Maybe b))] [xs (List a)]) (List b)]{
Maps @racket[f] over @racket[xs] and keeps only the @racket[Some] results (Haskell's @tt{mapMaybe}).}

@defproc[(cat-maybes [ms (List (Maybe a))]) (List a)]{
Drops the @racket[None]s from a list of @racket[Maybe]s (Haskell's @tt{catMaybes}).}

@deftogether[(@defproc[(maybe->list [m (Maybe a)]) (List a)]
              @defproc[(list->maybe [xs (List a)]) (Maybe a)])]{
@racket[maybe->list] turns @racket[None] into the empty list and @racket[Some x] into a singleton; @racket[list->maybe] turns the empty list into @racket[None] and a non-empty list into @racket[Some] of its head.}


@section{rackton/data/monoid}
@defmodule[rackton/data/monoid]

Data.Monoid: the numeric monoid newtypes over @racket[Integer] (@racket[Sum]
additive, @racket[Product] multiplicative), the Boolean monoids (@racket[All]
conjunction, @racket[Any] disjunction), the composition monoid (@racket[Endo],
endomorphisms under @tt{.}), and the order-flipping wrapper
(@racket[Dual]). Importing this module brings the @racket[Semigroup] and
@racket[Monoid] instances for these types into scope.

@deftogether[(
@defform[#:kind "type & constructor" #:link-target? #f #:id Sum #:literals (newtype Integer)
         (newtype Sum
           (Sum Integer))]
@defthing[#:kind "type & constructor" Sum (-> Integer Sum)])]{
  The additive monoid over @racket[Integer], whose
  @racket[mappend] adds and whose @racket[mempty] is @racket[(Sum 0)].}

@defproc[(get-sum [s Sum]) Integer]{Unwraps a @racket[Sum] to its integer.}

@deftogether[(
@defform[#:kind "type & constructor" #:link-target? #f #:id Product #:literals (newtype Integer)
         (newtype Product
           (Product Integer))]
@defthing[#:kind "type & constructor" Product (-> Integer Product)])]{
  The multiplicative monoid over @racket[Integer],
  whose @racket[mappend] multiplies and whose @racket[mempty] is
  @racket[(Product 1)].}

@defproc[(get-product [p Product]) Integer]{Unwraps a @racket[Product] to its integer.}

@deftogether[(
@defform[#:kind "type" #:id All #:literals (newtype MkAll Boolean)
         (newtype All
           (MkAll Boolean))]
@defthing[#:kind "constructor" MkAll (-> Boolean All)])]{
  The conjunction monoid over @racket[Boolean], whose
  @racket[mappend] is logical @tt{and} and whose @racket[mempty] is
  @racket[(MkAll #t)].}

@defproc[(get-all [a All]) Boolean]{Unwraps an @racket[All] to its boolean.}

@deftogether[(
@defform[#:kind "type & constructor" #:link-target? #f #:id Any #:literals (newtype Boolean)
         (newtype Any
           (Any Boolean))]
@defthing[#:kind "type & constructor" Any (-> Boolean Any)])]{
  The disjunction monoid over @racket[Boolean], whose
  @racket[mappend] is logical @tt{or} and whose @racket[mempty] is
  @racket[(Any #f)].}

@defproc[(get-any [a Any]) Boolean]{Unwraps an @racket[Any] to its boolean.}

@deftogether[(
@defform[#:kind "type & constructor" #:link-target? #f #:id Endo #:literals (newtype ->)
         (newtype (Endo a)
           (Endo (-> a a)))]
@defthing[#:kind "type & constructor" Endo (-> (-> a a) (Endo a))])]{
  The composition monoid of endomorphisms
  @racket[(-> a a)], whose @racket[mappend] composes (left after right) and whose
  @racket[mempty] is the identity function.}

@defproc[(app-endo [e (Endo a)]) (-> a a)]{Unwraps an @racket[Endo] to its function.}

@deftogether[(
@defform[#:kind "type & constructor" #:link-target? #f #:id Dual #:literals (newtype)
         (newtype (Dual a)
           (Dual a))]
@defthing[#:kind "type & constructor" Dual (-> a (Dual a))])]{
  The order-flipping wrapper: @racket[mappend] applies
  the inner type's @racket[mappend] with its arguments flipped, and @racket[mempty]
  lifts the inner monoid's identity.}

@defproc[(get-dual [d (Dual a)]) a]{Unwraps a @racket[Dual] to its inner value.}


@section{rackton/data/ord}
@defmodule[rackton/data/ord]

Ordering helpers built on the prelude's @racket[Ord] class (where @racket[min], @racket[max], and the comparison operators live). Provides range clamping and key-projected min/max.

@defproc[(clamp [lo a] [hi a] [x a]) a]{Confines @racket[x] to the inclusive range @tt{[lo, hi]}, for any @racket[(Ord a)].}

@defproc[(min-by [key (-> a b)] [x a] [y a]) a]{Returns the argument with the smaller projected key (ties favour @racket[x]), for any @racket[(Ord b)].}

@defproc[(max-by [key (-> a b)] [x a] [y a]) a]{Returns the argument with the larger projected key (ties favour @racket[x]), for any @racket[(Ord b)].}


@section{rackton/data/ratio}
@defmodule[rackton/data/ratio]

Derived operations on the prelude's @racket[Rational] type, which the
runtime keeps in lowest terms via @racket[make-rational],
@tt{numerator}, and @tt{denominator}. Includes a port of GHC's
@tt{approxRational} algorithm for finding the simplest rational within a
given tolerance.

@defproc[(ratio [n Integer] [d Integer]) Rational]{
Builds the rational @racket[n]/@racket[d]; an alias for
@racket[make-rational] (Haskell's @tt{%}, named rather than infix).}

@defproc[(recip [r Rational]) Rational]{
The multiplicative inverse, @racket[d]/@racket[n] for @racket[n]/@racket[d].}

@defproc[(to-float [r Rational]) Float]{
Converts @racket[r] to the nearest @racket[Float].}

@defproc[(approx-rational [x Float] [eps Float]) Rational]{
The simplest @racket[Rational] within @racket[eps] of @racket[x] — the one
with the smallest denominator, then smallest numerator.}

@defproc[(approx-simplest [x Rational] [y Rational]) Rational]{
The simplest @racket[Rational] in the closed interval @tt{[x, y]}.}

@defproc[(approx-simplest-prime [n Integer] [d Integer] [n2 Integer] [d2 Integer]) Rational]{
Assuming @racket[0 < n/d < n2/d2], the simplest @racket[Rational] strictly
between them, found via the continued-fraction quotients of the endpoints.}


@section{rackton/data/semigroup}
@defmodule[rackton/data/semigroup]

Data.Semigroup selection newtypes whose @racket[mappend] keeps the smaller, larger,
first, or last operand. The @racket[mappend] operation itself and the
@racket[Semigroup] / @racket[Monoid] classes live in the prelude. @racket[Min] and
@racket[Max] carry only @racket[Semigroup] (no @racket[Monoid], since that would need
a bounded identity Rackton's numeric types don't provide).

@deftogether[(
@defform[#:kind "type & constructor" #:link-target? #f #:id Min #:literals (newtype)
         (newtype (Min a)
           (Min a))]
@defthing[#:kind "type & constructor" Min (-> a (Min a))])]{
  Selection newtype whose @racket[Semigroup] instance keeps the smaller operand.}

@deftogether[(
@defform[#:kind "type & constructor" #:link-target? #f #:id Max #:literals (newtype)
         (newtype (Max a)
           (Max a))]
@defthing[#:kind "type & constructor" Max (-> a (Max a))])]{
  Selection newtype whose @racket[Semigroup] instance keeps the larger operand.}

@deftogether[(
@defform[#:kind "type & constructor" #:link-target? #f #:id First #:literals (newtype)
         (newtype (First a)
           (First a))]
@defthing[#:kind "type & constructor" First (-> a (First a))])]{
  Selection newtype whose @racket[Semigroup] instance keeps the first operand.}

@deftogether[(
@defform[#:kind "type & constructor" #:link-target? #f #:id Last #:literals (newtype)
         (newtype (Last a)
           (Last a))]
@defthing[#:kind "type & constructor" Last (-> a (Last a))])]{
  Selection newtype whose @racket[Semigroup] instance keeps the last operand.}

@defproc[(get-min [m (Min a)]) a]{Unwrap the value inside a @racket[Min].}
@defproc[(get-max [m (Max a)]) a]{Unwrap the value inside a @racket[Max].}
@defproc[(get-first [m (First a)]) a]{Unwrap the value inside a @racket[First].}
@defproc[(get-last [m (Last a)]) a]{Unwrap the value inside a @racket[Last].}


@section{rackton/data/set}
@defmodule[rackton/data/set]

Immutable sets backed by Racket immutable hashes, reached through
@racket[foreign] primitives. Elements compare by the runtime's
structural equality, so no @racket[(Eq a)] or @racket[(Ord a)]
constraint is required.

@defidform[#:kind "type" Set]{The immutable set type, opaque over its
element type @racket[a].}

@defthing[empty-set (Set a)]{The empty set.}

@defproc[(set-insert [x a] [s (Set a)]) (Set a)]{Returns @racket[s] with
@racket[x] added.}

@defproc[(set-member? [x a] [s (Set a)]) Boolean]{Reports whether
@racket[x] is an element of @racket[s].}

@defproc[(set-delete [x a] [s (Set a)]) (Set a)]{Returns @racket[s] with
@racket[x] removed.}

@defproc[(set-size [s (Set a)]) Integer]{Returns the number of elements
in @racket[s].}

@defproc[(set-to-list [s (Set a)]) (List a)]{Returns the elements of
@racket[s] as a list.}

@defproc[(set-empty? [s (Set a)]) Boolean]{Reports whether @racket[s] has
no elements.}

@defproc[(set-singleton [x a]) (Set a)]{Returns the one-element set
containing @racket[x].}

@defproc[(set-from-list [xs (List a)]) (Set a)]{Builds a set from the
elements of @racket[xs].}

@defproc[(set-union [s1 (Set a)] [s2 (Set a)]) (Set a)]{Returns the union
of @racket[s1] and @racket[s2].}

@defproc[(set-intersection [s1 (Set a)] [s2 (Set a)]) (Set a)]{Returns the
intersection of @racket[s1] and @racket[s2].}

@defproc[(set-difference [s1 (Set a)] [s2 (Set a)]) (Set a)]{Returns the
elements of @racket[s1] that are not in @racket[s2].}

@defproc[(set-subset? [s1 (Set a)] [s2 (Set a)]) Boolean]{Reports whether
every element of @racket[s1] is in @racket[s2].}

@defproc[(set-disjoint? [s1 (Set a)] [s2 (Set a)]) Boolean]{Reports
whether @racket[s1] and @racket[s2] share no elements.}

@defproc[(set-map [f (-> a b)] [s (Set a)]) (Set b)]{Applies @racket[f]
to every element of @racket[s], collecting the results into a new set.}

@defproc[(set-filter [p (-> a Boolean)] [s (Set a)]) (Set a)]{Returns the
elements of @racket[s] satisfying @racket[p].}

@defproc[(set-foldr [f (-> a (-> b b))] [z b] [s (Set a)]) b]{Right-folds
@racket[f] over the elements of @racket[s] starting from @racket[z].}


@section{rackton/data/traversable}
@defmodule[rackton/data/traversable]

Derived forms of @racket[Data.Traversable], built on the prelude's
@racket[Traversable] class method @racket[traverse].

@defproc[(sequence-a [t (t (f a))]) (f (t a))]{
  Turns a structure of actions into an action of a structure (Haskell
  @tt{sequenceA}), for any @racket[Applicative] @racket[f] and
  @racket[Traversable] @racket[t].}

@defproc[(for-t [t (t a)] [f (-> a (f b))]) (f (t b))]{
  @racket[traverse] with its arguments flipped (Haskell @tt{for}).}


@section{rackton/data/tuple}
@defmodule[rackton/data/tuple]

Data.Tuple utilities. @racket[fst] and @racket[snd] remain in the prelude; this module provides @racket[swap] along with @racket[curry] and @racket[uncurry], which convert between a @racket[Pair]-taking function and its two-argument form.

@defproc[(swap [p (Pair a b)]) (Pair b a)]{Exchanges the two components of a @racket[Pair].}

@defproc[(curry [f (-> (Pair a b) c)] [a a] [b b]) c]{Turns a function on a @racket[Pair] into a two-argument function.}

@defproc[(uncurry [f (-> a (-> b c))] [p (Pair a b)]) c]{Turns a two-argument function into one taking a @racket[Pair].}


