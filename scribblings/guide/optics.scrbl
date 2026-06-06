#lang scribble/manual
@require[scribble/manual
         (for-label rackton
                    rackton/data/lens)
         "../rackton-eval.rkt"]
@(define ev (make-rackton-eval))

@title[#:tag "optics"]{Optics: lenses, prisms, traversals}

Rackton ships first-class functional optics for working with nested
data: @racket[Lens] focuses one position, @racket[Prism] focuses one
branch of a sum, and @racket[Traversal] focuses zero or more
positions.

@section{Lenses}

A @racket[(Lens s a)] is a getter/setter pair for an @racket[a]
inside an @racket[s].  Add @racket[#:deriving Lens] to a
@racket[struct] to synthesise one lens per field, named
@racketidfont{T}@racketidfont{-}@racket[_field]@racketidfont{-lens}:

@rackton-example[#:eval ev #:mode 'value]{
(require rackton/data/lens)

(struct Point [x : Integer] [y : Integer]
  #:deriving Lens Show)

(define p (Point 3 4))

(Tuple3
  (view Point-x-lens p)
  (set  Point-x-lens 99 p)
  (over Point-x-lens (lambda (n) (+ n 1)) p))
}

Without @racket[#:deriving Lens] no lenses are generated — you can
still write them by hand with @racket[Lens] (see below).

Compose lenses with @racket[lens-compose] to drill into nested
structure:

@rackton-example[#:eval ev #:mode 'value]{
(require rackton/data/lens)

(struct Address [city : String] [zip : Integer] #:deriving Lens Show)
(struct Person  [name : String] [addr : Address] #:deriving Lens Show)

(define alice (Person "Alice" (Address "NYC" 10001)))

(define addr-city (lens-compose Person-addr-lens Address-city-lens))
(Pair
  (view addr-city alice)
  (over addr-city (lambda (_) "LA") alice))
}

You can also construct a lens directly with @racket[Lens]:

@rackton-example[#:eval ev #:mode 'defs]{
(require rackton/data/lens)

(: first-of-pair (Lens (Pair a b) a))
(define first-of-pair
  (Lens fst
          (lambda (p) (lambda (a) (Pair a (snd p))))))
}

@section{Prisms}

A @racket[(Prism s a)] is a pattern: either it extracts an @racket[a]
from an @racket[s] or it doesn't.  Add @racket[#:deriving Prism] to a
@racket[data] to synthesise one prism per constructor.

@rackton-example[#:eval ev #:mode 'defs]{
(require rackton/data/lens)

(data Opt
  Absent
  (Present Integer)
  #:deriving Prism)
}

@racket[#:deriving Prism] generates a prism for each constructor,
named @racketidfont{T}@racketidfont{-}@racket[_Ctor]@racketidfont{-prism}:

@rackton-example[#:eval ev #:mode 'value]{
(require rackton/data/lens)

(data Opt
  Absent
  (Present Integer)
  #:deriving Prism Show)

(Tuple3
  (preview Opt-Present-prism (Present 7))
  (preview Opt-Present-prism Absent)
  (review  Opt-Present-prism 42))
}

@racket[preview] tries to focus; @racket[review] builds a value at the
focused position.

The focus type follows the constructor's payload: a nullary
constructor focuses @racket[Unit], a single-field constructor focuses
the field's type, and a multi-field constructor focuses the flat
@emph{tuple} of its fields — @racket[(C a b)] gives @racket[(Prism s
(Pair a b))] and @racket[(C a b c)] gives @racket[(Prism s (Tuple3 a b
c))].  @racket[Pair] is the 2-tuple; @racket[Tuple3] through
@racket[Tuple7] (defined in @racketmodname[rackton/data/lens], so no
extra import) cover arities 3–7.  A constructor with more than seven
fields is a compile error.

@rackton-example[#:eval ev #:mode 'value]{
(require rackton/data/lens)

(data Shape
  (Circle Integer)
  (Rect   Integer Integer)
  (Tri    Integer Integer Integer)
  #:deriving Prism Show)

(Tuple4
  (preview Shape-Rect-prism (Rect 3 4))
  (review  Shape-Rect-prism (Pair 7 8))
  (preview Shape-Tri-prism  (Tri 1 2 3))
  (review  Shape-Tri-prism  (Tuple3 4 5 6)))
}

(Prism deriving is unavailable on @racket[struct] — a
single-constructor record has nothing to discriminate; use
@racket[Lens] instead.)

@section{Traversals}

A @racket[(Traversal s a)] visits zero or more @racket[a]s inside an
@racket[s].  Use @racket[to-list-of] to collect them and
@racket[over-of] to modify them all:

@rackton-example[#:eval ev #:mode 'value]{
(require rackton/data/lens)

(define xs (Cons 1 (Cons 2 (Cons 3 Nil))))

(Pair
  (to-list-of list-traversal xs)
  (over-of    list-traversal (lambda (n) (* n 2)) xs))
}

Convert a lens to a single-element traversal with
@racket[lens-as-traversal]:

@rackton-example[#:eval ev #:mode 'value]{
(require rackton/data/lens)

(struct Point [x : Integer] [y : Integer]
  #:deriving Lens Show)

(define point-x-traversal (lens-as-traversal Point-x-lens))
(to-list-of point-x-traversal (Point 3 4))
}

@section{Which optic do you want?}

@tabular[#:sep @hspace[2]
         (list
          (list @bold{Shape}                  @bold{Use})
          (list "single position, always there"   @racket[Lens])
          (list "one branch of a sum"             @racket[Prism])
          (list "many positions, possibly zero"   @racket[Traversal]))]

All three compose with each other through their respective
combinators (in the general case via a unified profunctor encoding,
which Rackton's implementation hides behind the concrete operations).
