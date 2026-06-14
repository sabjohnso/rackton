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

Listing @racket[#:deriving] at the end of a @racket[data] or
@racket[struct] synthesises the named protocol instances:

@rackton-example[#:eval ev #:mode 'defs]{
(data (Tree a)
  Leaf
  (Node (Tree a) a (Tree a))
  #:deriving Eq Show Functor)
}

Available protocols for deriving include @racket[Eq], @racket[Ord]
(which auto-derives @racket[Eq] as well), @racket[Show],
@racket[Functor], @racket[Foldable], @racket[Traversable],
@racket[Bifunctor], @racket[Semigroup], @racket[Monoid], plus the
optics families: per-field @racket[Lens] on @racket[struct] and
per-constructor @racket[Prism] on @racket[data].  Each derived
instance picks up the appropriate context (so @racket[#:deriving Eq]
on @racket[(Tree a)] yields the qualified instance
@racket[((Eq a) => (Eq (Tree a)))]).

@section{Abstract types}

Adding @racket[#:abstract] before the constructors hides them from
importing modules even when listed in a @racket[(provide …)] form.
Inside the defining module the constructors work as usual; outside,
only the type constructor is visible.  This is how @racket[IO] and
@racket[Ref] keep their constructors hidden.
