#lang scribble/manual
@require[scribble/manual
         (for-label rackton rackton/numeric/conversions rackton/numeric/integer rackton/numeric/natural rackton/numeric/real rackton/numeric/show)]

@title[#:tag "stdlib-numeric"]{@tt{rackton/numeric} — numeric-tower extras}

@section{rackton/numeric/conversions}
@defmodule[rackton/numeric/conversions #:no-declare]
@declare-exporting[rackton/numeric/conversions]

Conversions across the numeric tower. Wraps the prelude's primitive coercions
behind a uniform @racket[num-]-prefixed interface, adds a @racket[Rational] to
@racket[Float] bridge, and provides the polymorphic @racket[num-real-to-frac]
(any @racket[Real] to @racket[Float], the way Haskell uses @tt{realToFrac} most).

@defproc[(num-integer->float [n Integer]) Float]{
  Coerces an @racket[Integer] to a @racket[Float].}

@defproc[(num-float->integer [x Float]) Integer]{
  Coerces a @racket[Float] to an @racket[Integer], truncating toward zero.}

@defproc[(num-to-rational [x a]) Rational]{
  Converts any @racket[Real] @racket[x] to a @racket[Rational].}

@defproc[(num-rational->float [r Rational]) Float]{
  Converts a @racket[Rational] to a @racket[Float] via @tt{exact->inexact}.}

@defproc[(num-real-to-frac [x a]) Float]{
  Converts any @racket[Real] @racket[x] to a @racket[Float], routing through the
  @racket[Rational] bridge.}


@section{rackton/numeric/integer}
@defmodule[rackton/numeric/integer #:no-declare]
@declare-exporting[rackton/numeric/integer]

Integral helper combinators over the prelude's @racket[Integer], derived
from the prelude's @racket[Integral] and @racket[Num] classes. Names are
prefixed @racket[num-] so they don't shadow @tt{racket/base}'s
@tt{gcd}, @tt{lcm}, @tt{even?}, and @tt{odd?} inside @racket[(racket …)]
escapes.

@deftogether[(@defproc[(num-even? [n Integer]) Boolean]
              @defproc[(num-odd? [n Integer]) Boolean])]{
  Parity predicates: @racket[num-even?] is true when @racket[n] is
  divisible by two, and @racket[num-odd?] is its negation.}

@defproc[(num-signum [n Integer]) Integer]{
  Returns @racket[-1], @racket[0], or @racket[1] according to the sign of
  @racket[n].}

@defproc[(num-gcd [a Integer] [b Integer]) Integer]{
  The greatest common divisor of @racket[a] and @racket[b] by Euclid's
  algorithm, always non-negative (matching Haskell @tt{gcd}).}

@defproc[(num-lcm [a Integer] [b Integer]) Integer]{
  The least common multiple of @racket[a] and @racket[b], pinned to
  @racket[0] when either argument is zero.}

@defproc[(num-factorial [n Integer]) Integer]{
  The factorial @racket[n]@tt{!}.}

@defproc[(num-int-pow [b Integer] [e Integer]) Integer]{
  Integer exponentiation @racket[b]@tt{^}@racket[e] for @racket[e] @tt{>=} 0.}

@defproc[(num-from-integral [n Integer]) Float]{
  Converts an @racket[Integer] to a @racket[Float]; the concrete
  @racket[Integer]-to-@racket[Float] instance of Haskell @tt{fromIntegral}.}


@section{rackton/numeric/natural}
@defmodule[rackton/numeric/natural #:no-declare]
@declare-exporting[rackton/numeric/natural]

Numeric.Natural: a @racket[newtype] over the prelude's @racket[Integer] constrained to non-negative values, carrying the @racket[Eq], @racket[Ord], @racket[Show], and @racket[Num] instances Haskell's @tt{Natural} has. Construction is checked and the partial @racket[Num] operations (subtraction below zero, negating a positive) @racket[panic] rather than wrap around.

@defidform[#:kind "type & constructor" Natural]{The natural-number type, a checked wrapper over @racket[Integer] that never holds a negative value.
  @racket[Natural : (-> Integer Natural)] — wraps an @racket[Integer]; construction is unchecked at this level, so prefer @racket[num-to-natural].}

@defproc[(num-to-natural [n Integer]) (Maybe Natural)]{Builds a @racket[Natural] from an @racket[Integer], returning @racket[None] when @racket[n] is negative.}

@defproc[(num-from-natural [x Natural]) Integer]{Projects a @racket[Natural] back to its underlying @racket[Integer].}


@section{rackton/numeric/real}
@defmodule[rackton/numeric/real #:no-declare]
@declare-exporting[rackton/numeric/real]

Derived @racket[Floating] and @racket[RealFrac] operations the prelude does not
already ship: inverse trigonometric functions (via host @tt{asin}/@tt{acos}/@tt{atan}
escapes), hyperbolic functions derived from @racket[exp], change-of-base logarithms,
and @racket[num-proper-fraction].

@deftogether[(
  @defproc[(num-asin [x Float]) Float]
  @defproc[(num-acos [x Float]) Float]
  @defproc[(num-atan [x Float]) Float]
)]{
  Inverse sine, cosine, and tangent, computed through @racket[(racket …)] escapes to
  @tt{asin}, @tt{acos}, and @tt{atan} from @tt{racket/base}.
}

@deftogether[(
  @defproc[(num-sinh [x Float]) Float]
  @defproc[(num-cosh [x Float]) Float]
  @defproc[(num-tanh [x Float]) Float]
)]{
  Hyperbolic sine, cosine, and tangent, derived from @racket[exp]: @racket[num-sinh]
  is @math{(eˣ − e⁻ˣ)/2}, @racket[num-cosh] is @math{(eˣ + e⁻ˣ)/2}, and
  @racket[num-tanh] is their ratio.
}

@defproc[(num-log-base [b Float] [x Float]) Float]{
  Logarithm of @racket[x] in base @racket[b], computed as @racket[(log x)] divided by
  @racket[(log b)].
}

@defproc[(num-proper-fraction [x Float]) (Pair Integer Float)]{
  Splits @racket[x] into a @racket[Pair] of its integer part (truncated toward zero)
  and the fractional remainder @math{x − n} (which has the same sign as @racket[x]).
}


@section{rackton/numeric/show}
@defmodule[rackton/numeric/show #:no-declare]
@declare-exporting[rackton/numeric/show]

Radix conversion and float formatting for numbers, in the spirit of Haskell's
@tt{Numeric} module (@tt{showHex} / @tt{showOct} / @tt{readHex} / @tt{readDec},
plus binary). The integer @racket[show] direction wraps the host's
@tt{number->string}, the @racket[read] direction wraps @tt{string->number} and
returns @racket[None] when the string is not a valid integer in the base, and the
float formatters come in as @racket[foreign] runtime primitives backed by
@tt{racket/format}'s @tt{~r}.

@deftogether[(@defproc[(num-show-hex [n Integer]) String]
              @defproc[(num-show-oct [n Integer]) String]
              @defproc[(num-show-bin [n Integer]) String])]{
  Render integer @racket[n] as a string in base 16, base 8, or base 2,
  respectively.}

@deftogether[(@defproc[(num-read-hex [s String]) (Maybe Integer)]
              @defproc[(num-read-oct [s String]) (Maybe Integer)]
              @defproc[(num-read-dec [s String]) (Maybe Integer)])]{
  Parse string @racket[s] as an exact integer in base 16, base 8, or base 10,
  respectively, returning @racket[None] unless the whole string parses to an
  exact integer in that base.}

@deftogether[(@defproc[(show-f-float [prec Integer] [x Float]) String]
              @defproc[(show-e-float [prec Integer] [x Float]) String]
              @defproc[(show-g-float [prec Integer] [x Float]) String])]{
  Foreign runtime primitives (from @tt{rackton/private/prelude-runtime}) that
  format float @racket[x] in fixed-point, scientific, or general notation; the
  @racket[prec] argument is the number of digits after the decimal point, with a
  negative value requesting full precision.}

@defproc[(prec->int [p (Maybe Integer)]) Integer]{
  Convert an optional precision to the plain @racket[Integer] the float primitives
  expect, mapping @racket[None] to @racket[-1] (full precision) and @racket[(Some n)]
  to @racket[n].}

@deftogether[(@defproc[(num-show-f-float [p (Maybe Integer)] [x Float]) String]
              @defproc[(num-show-e-float [p (Maybe Integer)] [x Float]) String]
              @defproc[(num-show-g-float [p (Maybe Integer)] [x Float]) String])]{
  Format float @racket[x] with an optional precision @racket[p]: fixed-point
  (e.g. @racket[(num-show-f-float (Some 2) 3.14159)] is @racket["3.14"]),
  scientific (e.g. @racket[(num-show-e-float (Some 3) 245.7)] is
  @racket["2.457e2"]), and general (fixed inside @tt{[0.1, 1e7)}, scientific
  outside), respectively.}


