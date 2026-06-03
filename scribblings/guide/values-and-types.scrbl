#lang scribble/manual
@require[scribble/manual
         (for-label rackton)]

@title[#:tag "values-and-types"]{Values and types}

@section{Lexical conventions}

The first character of an identifier decides whether the identifier
introduces a fresh binding or refers to an existing one:

@itemlist[
@item{An identifier beginning with a @bold{lowercase letter}
      @italic{introduces a fresh binding} in type and pattern
      positions: a @italic{type variable} in a type, and a
      @italic{pattern variable} in a pattern.  In expression position
      it is an ordinary @italic{reference} to a value binding
      (function parameter, @racket[let]-bound name, top-level
      @racket[define], or prelude function).}
@item{Every other identifier — uppercase initial, or operator-shaped
      names like @racket[->], @racket[+], @racket[==], @racket[>=] —
      is always a @italic{reference} to an already-bound name, never
      a fresh binding.  What it refers to depends on position: a
      @italic{type constructor} or @italic{class name} in a type, a
      @italic{data constructor} as a pattern head, and a
      @italic{value}, @italic{function}, @italic{class method}, or
      @italic{data constructor} in an expression.}
@item{The underscore @racket[_] is the wildcard pattern.}]

These rules are consistent across the language.  In particular, you
cannot define a top-level value named @racket[a] or @racket[xs] in a
type signature's parameter position — the parser would treat it as a
type variable.  But you @italic{can} bind a lowercase value at the
module level with @racket[(define xs …)] and reference it from
expression position.

@section{Primitive types}

The prelude ships these primitive types, each with the obvious
equality and display instances:

@tabular[#:sep @hspace[2]
         (list
          (list @racket[Integer]  "arbitrary-precision integer (42)")
          (list @racket[Float]    "inexact real (3.14)")
          (list @racket[Rational] "exact non-integer rational")
          (list @racket[Complex]  "complex number")
          (list @racket[Boolean]  "#t or #f")
          (list @racket[String]   "immutable string (\"hello\")")
          (list @racket[Char]     "Unicode code point (#\\A)")
          (list @racket[Bytes]    "immutable byte string (#\"hi\")"))]

A numeric literal with a fractional part or exponent
(@racket[3.14], @racket[1e10]) is @racket[Float]; bare integer literals
are @racket[Integer].

@section{Type signatures and inference}

A top-level definition without a signature is inferred:

@codeblock|{
(define (compose f g)
  (lambda (x) (f (g x))))
;; inferred :: (∀ a b c) (-> (-> b c) (-> (-> a b) (-> a c)))
}|

Add a signature and Rackton skolem-checks the body against it:

@codeblock|{
(: compose (-> (-> b c) (-> (-> a b) (-> a c))))
(define (compose f g)
  (lambda (x) (f (g x))))
}|

A type signature isn't required for mutual recursion: top-level
forms in a Rackton module are order-invariant.  Every name is
visible to every other form regardless of where it appears in the
file:

@codeblock|{
(define (even? n) (if (= n 0) #t (odd?  (- n 1))))
(define (odd?  n) (if (= n 0) #f (even? (- n 1))))
}|

The same rule applies to data types, classes, instances, and
references to a value defined later in the file:

@codeblock|{
;; Forward reference between defs:
(: r Integer)
(define r (g 3))
(define (g x) (* x 2))

;; Mutually recursive data types:
(data Tree Leaf (Br Forest Forest))
(data Forest Empty (Cns Tree Forest))

;; A class used before its declaration:
(define (greet x) (pretty x))
(protocol (Pretty a)
  (: pretty (-> a String)))
}|

Inference uses strongly-connected-component analysis on the def
graph: independent defs are generalized individually (so
@racket[id] used at two element types stays polymorphic), while
mutually recursive bindings share a monomorphic shape during
inference and generalize together at the SCC boundary.

The @racket[:] form still pre-registers the name in the typing
environment.  Most code doesn't need it; it remains useful for
documentation, to constrain the inferred scheme, or to declare a
binding that would otherwise be ambiguous (see the @racket[pure]
case in @secref["return-typed-methods"]).

@section{Function types}

Functions are unary; multi-argument functions are
curried.  @racket[(-> a b c)] means @racket[(-> a (-> b c))]:

@codeblock|{
(: add (-> Integer (-> Integer Integer)))
(define (add x y) (+ x y))

((add 3) 4)   ;; ⇒ 7
(add 3 4)     ;; ⇒ 7 — partial application is implicit
}|

@racket[->] is variadic in source position but always parses
right-associatively.

@section{Explicit polymorphism}

When you want to be specific about quantification, use @racket[All]:

@codeblock|{
(: const (All (a b) (-> a (-> b a))))
(define (const x) (lambda (_y) x))
}|

This is exactly equivalent to leaving the type variables implicit;
explicit @racket[All] is mandatory only when you want rank-N
polymorphism (see @secref["polymorphism"]).

@section{Type ascription}

@racket[(ann expr type)] asserts a type:

@codeblock|{
(ann Nil (List Integer))   ;; Nil at type (List Integer)
(ann (pure 3) (Maybe Integer))   ;; pure resolved at the Maybe instance
}|

This is useful for disambiguating return-typed methods (@racket[pure],
@racket[mempty]) and for forcing the type checker to commit to a
specific instantiation.

@section{The host-language escape}

@racket[(racket τ (var ...) body ...)] drops out of Rackton and runs
@racket[body] as Racket, asserting that its value has type
@racket[τ]:

@codeblock|{
(: greet (-> String String))
(define (greet name)
  (racket String (name)
    (string-append "hello " name)))
}|

The named Rackton bindings @racket[var ...] are spliced into
@racket[body] unmodified.  No type-checking is performed on
@racket[body]; the type assertion is taken on faith.  See
@secref["racket-interop"] for the full story.
