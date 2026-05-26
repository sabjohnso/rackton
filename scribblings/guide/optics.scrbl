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
inside an @racket[s].

@codeblock|{
(define-struct Point [x : Integer] [y : Integer]
  #:deriving)
}|

When you derive nothing explicitly, @racket[define-struct] still
generates per-field lenses named @racketidfont{Name}@racketidfont{._}@racket[_field]
(note the underscore):

@codeblock|{
(define p (Point 3 4))

(view  Point._x p)        ;; ⇒ 3
(set   Point._x 99 p)     ;; ⇒ (Point 99 4)
(over  Point._x (lambda (n) (+ n 1)) p)   ;; ⇒ (Point 4 4)
}|

Compose lenses with @racket[lens-compose] to drill into nested
structure:

@codeblock|{
(define-struct Address [city : String] [zip : Integer] #:deriving)
(define-struct Person  [name : String] [addr : Address] #:deriving)

(define alice (Person "Alice" (Address "NYC" 10001)))

(define addr-city (lens-compose Person._addr Address._city))
(view addr-city alice)              ;; ⇒ "NYC"
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
from an @racket[s] or it doesn't.

@codeblock|{
;; Derived for free for every ADT constructor
(define-data (Result e a) (Err e) (Ok a) #:deriving Prism)
}|

@racket[#:deriving Prism] generates a prism for each constructor,
named @racketidfont{Name}@racketidfont{._}@racket[_Ctor]:

@codeblock|{
(preview Result._Ok  (Ok 7))   ;; ⇒ (Some 7)
(preview Result._Ok  (Err "bad"))   ;; ⇒ None

(review  Result._Ok  42)        ;; ⇒ (Ok 42)
}|

@racket[preview] tries to focus; @racket[review] builds a value at the
focused position.

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
(define point-x-traversal (lens-as-traversal Point._x))
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
