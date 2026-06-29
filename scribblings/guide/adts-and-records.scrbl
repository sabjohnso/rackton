#lang scribble/manual
@require[scribble/manual
         (for-label rackton)
         "../rackton-eval.rkt"]
@(define ev (make-rackton-eval))

@title[#:tag "adts-and-records"]{Algebraic data types and records}

Rackton offers three closely-related ways to declare your own data:

@itemlist[
@item{@racket[data] — algebraic data types (sums of products).}
@item{@racket[newtype] — zero-overhead single-field wrappers.}
@item{@racket[struct] — records with named typed fields.}]

@section{Sums of products with @racket[data]}

@rackton-example[#:eval ev #:mode 'defs #:context? #t]{
(data (Tree a)
  Leaf
  (Node (Tree a) a (Tree a)))
}

This declares a type @racket[(Tree a)] with two constructors:
@racket[Leaf : (Tree a)] and @racket[Node : (-> (Tree a) (-> a (-> (Tree a) (Tree a))))].
Use them as expressions and as patterns:

@rackton-example[#:eval ev #:mode 'defs #:context? #t]{
(: tree-sum (-> (Tree Integer) Integer))
(define (tree-sum t)
  (match t
    [(Leaf)        0]
    [(Node l x r)  (+ x (+ (tree-sum l) (tree-sum r)))]))
}

@section{Recursion and parameterisation}

The right-hand side of a constructor may reference the type being
defined, so trees, lists, and graphs all fall out naturally.  The
type parameters @racket[a ...] are universally quantified — the same
@racket[(Tree Integer)] machinery handles @racket[(Tree String)]
without further work.

@section{Named fields and keyword construction}

A constructor's fields may be @emph{named}, written
@racket[[field : type]] in place of a bare field type.  A named
constructor can then be built with keyword arguments
@racket[(Ctor :field value …)] in addition to the positional form
@racket[(Ctor value …)] — both build the very same value:

@rackton-example[#:eval ev #:mode 'value #:context? #t]{
(data (Crate a)
  (Packed [value : a])
  Hollow)

(match (Packed :value 3)
  [(Packed v) v]
  [Hollow     0])
}

Two rules keep naming consistent: within one @racket[data] declaration
either every constructor that has fields names them or none does
(nullary constructors like @racket[Hollow] are exempt), and a single
constructor's fields are either all named or all positional.

Keyword arguments must appear in the constructor's @emph{declared field
order} — the labels document and check the call, they do not reorder it.
A @racket[struct]'s fields are always named, so a @racket[struct] value
may be built positionally or with keywords interchangeably:

@rackton-example[#:eval ev #:mode 'value #:context? #t]{
(struct Vec2
  [x : Float]
  [y : Float])

(Vec2-y (Vec2 :x 3.0 :y 4.0))
}

Listing the labels out of order, omitting one, mixing keyword and
positional arguments in a single call, or applying keywords to a
positional constructor are all rejected at compile time.

@section{Newtypes}

@rackton-example[#:eval ev #:mode 'defs]{
(newtype Distance (MkDistance Float))
}

A newtype is a single-constructor type whose constructor takes exactly
one field.  Runtime representation is a single struct tag plus the
wrapped value; semantically it's a different type from the field's
type, so the type checker won't accidentally treat a
@racket[Distance] as a @racket[Float].

@section{Records with @racket[struct]}

When you want named fields and accessors:

@rackton-example[#:eval ev #:mode 'value #:context? #t]{
(struct Point
  [x : Integer]
  [y : Integer])

(define p (Point 3 4))
(Point-x p)
}

The constructor is the struct name; accessors are
@racket[Point-x] / @racket[Point-y].  Parameterised structs add type
parameters on the head:

@rackton-example[#:eval ev #:mode 'defs]{
(struct (Tagged a)
  [v   : a]
  [tag : String])
}

@section{Functional record update}

The @racket[update] form returns a fresh struct with selected fields
replaced:

@rackton-example[#:eval ev #:mode 'value #:context? #t]{
(define p1 (Point 3 4))
(define p2 (update p1 [x 99]))
(Point-x p2)
}

Untouched fields are copied verbatim.  This is the only way to
"modify" a record value — structs are immutable.

@section{Deriving common instances}

Listing @racket[:deriving] at the end of a @racket[data] or
@racket[struct] synthesises the named protocol instances:

@rackton-example[#:eval ev #:mode 'defs]{
(data (Tree a)
  Leaf
  (Node (Tree a) a (Tree a))
  :deriving Eq Show Functor)
}

Available protocols for deriving include @racket[Eq], @racket[Ord]
(which auto-derives @racket[Eq] as well), @racket[Show],
@racket[Functor], @racket[Foldable], @racket[Traversable],
@racket[Bifunctor], @racket[Semigroup], @racket[Monoid], plus the
optics families: per-field @racket[Lens] on @racket[struct] and
per-constructor @racket[Prism] on @racket[data].  Each derived
instance picks up the appropriate context (so @racket[:deriving Eq]
on @racket[(Tree a)] yields the qualified instance
@racket[((Eq a) => (Eq (Tree a)))]).

@section{Abstract types}

Adding @racket[:abstract] before the constructors hides them from
importing modules even when listed in a @racket[(provide …)] form.
Inside the defining module the constructors work as usual; outside,
only the type constructor is visible.  This is how @racket[IO] and
@racket[Ref] keep their constructors hidden.
