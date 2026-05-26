#lang scribble/manual
@require[scribble/manual
         (for-label rackton)]

@title[#:tag "values-and-types"]{Values and types}

@section{Lexical conventions}

Rackton's lexical rules are simple but rigid:

@itemlist[
@item{An identifier beginning with a @bold{lowercase letter} is a
      @italic{type variable} in type position and a @italic{pattern
      variable} in pattern position.}
@item{Every other identifier — including operator-shaped names like
      @racket[->] and @racket[<*>] — is a @italic{type constructor} or
      @italic{data constructor}, depending on context.}
@item{The underscore @racket[_] is the wildcard pattern.}]

These rules are consistent across the language.  In particular, you
cannot define a top-level value named @racket[a] or @racket[xs] — the
parser would treat them as variables.

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

The @racket[:] form pre-registers the name in the typing environment,
so a signature can also appear @italic{before} mutually recursive
definitions:

@codeblock|{
(: even? (-> Integer Boolean))
(: odd?  (-> Integer Boolean))
(define (even? n) (if (= n 0) #t (odd?  (- n 1))))
(define (odd?  n) (if (= n 0) #f (even? (- n 1))))
}|

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
