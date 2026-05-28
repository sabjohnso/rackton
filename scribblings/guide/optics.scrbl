#lang scribble/manual
@require[scribble/manual
         (for-label rackton)]

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

@codeblock|{
(struct Point [x : Integer] [y : Integer]
  #:deriving Lens)

(define p (Point 3 4))

(view Point-x-lens p)        ;; ⇒ 3
(set  Point-x-lens 99 p)     ;; ⇒ (Point 99 4)
(over Point-x-lens (lambda (n) (+ n 1)) p)   ;; ⇒ (Point 4 4)
}|

Without @racket[#:deriving Lens] no lenses are generated — you can
still write them by hand with @racket[MkLens] (see below).

Compose lenses with @racket[lens-compose] to drill into nested
structure:

@codeblock|{
(struct Address [city : String] [zip : Integer] #:deriving Lens)
(struct Person  [name : String] [addr : Address] #:deriving Lens)

(define alice (Person "Alice" (Address "NYC" 10001)))

(define addr-city (lens-compose Person-addr-lens Address-city-lens))
(view addr-city alice)                       ;; ⇒ "NYC"
(over addr-city (lambda (_) "LA") alice)
;; ⇒ Person { name="Alice", addr=Address { city="LA", zip=10001 } }
}|

You can also construct a lens directly with @racket[MkLens]:

@codeblock|{
(: first-of-pair (Lens (Pair a b) a))
(define first-of-pair
  (MkLens fst
          (lambda (p) (lambda (a) (MkPair a (snd p))))))
}|

@section{Prisms}

A @racket[(Prism s a)] is a pattern: either it extracts an @racket[a]
from an @racket[s] or it doesn't.  Add @racket[#:deriving Prism] to a
@racket[data] to synthesise one prism per constructor.

@codeblock|{
(data Opt
  Absent
  (Present Integer)
  #:deriving Prism)
}|

@racket[#:deriving Prism] generates a prism for each constructor,
named @racketidfont{T}@racketidfont{-}@racket[_Ctor]@racketidfont{-prism}:

@codeblock|{
(preview Opt-Present-prism (Present 7))   ;; ⇒ (Some 7)
(preview Opt-Present-prism Absent)         ;; ⇒ None

(review  Opt-Present-prism 42)             ;; ⇒ (Present 42)
}|

@racket[preview] tries to focus; @racket[review] builds a value at the
focused position.  (Prism deriving is unavailable on
@racket[struct] — a single-constructor record has nothing to
discriminate; use @racket[Lens] instead.)

@section{Traversals}

A @racket[(Traversal s a)] visits zero or more @racket[a]s inside an
@racket[s].  Use @racket[to-list-of] to collect them and
@racket[over-of] to modify them all:

@codeblock|{
(define xs (Cons 1 (Cons 2 (Cons 3 Nil))))

(to-list-of list-traversal xs)         ;; ⇒ (1 2 3) as a List
(over-of    list-traversal (lambda (n) (* n 2)) xs)
;; ⇒ Cons 2 (Cons 4 (Cons 6 Nil))
}|

Convert a lens to a single-element traversal with
@racket[lens-as-traversal]:

@codeblock|{
(define point-x-traversal (lens-as-traversal Point-x-lens))
(to-list-of point-x-traversal (Point 3 4))   ;; ⇒ (Cons 3 Nil)
}|

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
