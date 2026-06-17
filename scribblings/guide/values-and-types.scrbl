#lang scribble/manual
@require[scribble/manual
         (for-label rackton)
         "../rackton-eval.rkt"]
@(define ev (make-rackton-eval))

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
      @italic{type constructor} or @italic{protocol name} in a type, a
      @italic{data constructor} as a pattern head, and a
      @italic{value}, @italic{function}, @italic{protocol method}, or
      @italic{data constructor} in an expression.}
@item{The underscore @racket[_] is the wildcard pattern.}]

These rules are consistent across the language.  In particular, you
cannot define a top-level value named @racket[a] or @racket[xs] in a
type signature's parameter position — the parser would treat it as a
type variable.  But you @italic{can} bind a lowercase value at the
module level with @racket[(define xs …)] and reference it from
expression position.

@section[#:tag "guide-primitive-types"]{Primitive types}

The prelude ships these primitive types, each with the obvious
equality and display instances:

@tabular[#:sep @hspace[2]
         (list
          (list @racket[Integer]  "arbitrary-precision integer (42)")
          (list @racket[Float]    "inexact real (3.14)")
          (list @racket[Rational] "exact non-integer rational")
          (list @racket[Complex]  "inexact complex number (3.0+4.0i)")
          (list @racket[ComplexExact] "exact complex number (3+4i)")
          (list @racket[Boolean]  "#t or #f")
          (list @racket[String]   "immutable string (\"hello\")")
          (list @racket[Char]     "Unicode code point (#\\A)")
          (list @racket[Bytes]    "immutable byte string (#\"hi\")")
          (list @racket[Symbol]   "quoted identifier ('foo)")
          (list @racket[Unit]     "the one-value type (Unit)"))]

A numeric literal with a fractional part or exponent
(@racket[3.14], @racket[1e10]) is @racket[Float]; bare integer literals
are @racket[Integer].

For each type's full set of instances, its literal syntax, and the
operations that build and consume it, see
@secref["ref-primitive-types" #:doc '(lib "rackton/scribblings/reference/rackton-reference.scrbl")].

@section[#:tag "guide-tuples"]{Tuples}

A @deftech{tuple} is a heterogeneous, fixed-arity product.  Build one
with @racket[tuple] and it takes the type @racket[(Tuple τ ...)], where
each @racket[τ] is the type of the corresponding element.  There is no
arity limit.

@rackton-example[#:eval ev #:mode 'defs]{
(: triple (Tuple Integer String Boolean))
(define triple (tuple 1 "a" #t))
}

Read an element with @racket[tref].  The index is a literal and is
checked against the tuple's arity @italic{at compile time}, so each
@racket[tref] recovers the precise element type and an out-of-bounds
index is a type error rather than a runtime fault:

@rackton-example[#:eval ev #:mode 'value]{
(tref (tuple 1 "a" #t) 1)
}

The two-element tuple is exactly @racket[Pair]: @racket[(tuple a b)] and
@racket[(Pair a b)] build the same value and share the type
@racket[(Pair a b)] ≡ @racket[(Tuple a b)], so @racket[fst], @racket[snd],
and @racket[tref] are interchangeable on it.

@rackton-example[#:eval ev #:mode 'value]{
(== (tuple 1 2) (Pair 1 2))
}

A tuple also destructures with a @racket[(tuple p ...)] pattern, and
gains @racket[Eq], @racket[Ord], and @racket[Show] whenever every element
type does:

@rackton-example[#:eval ev #:mode 'value]{
(match (tuple 3 4)
  [(tuple a b) (show (tuple b a))])
}

@section[#:tag "guide-arrays"]{Arrays}

An @deftech{array} is a homogeneous, fixed-size sequence whose length is
carried in its type: @racket[(Array n a)] holds exactly @racket[n]
elements of type @racket[a].  Build one by listing its elements with
@racket[array], or from its indices with @racket[build-array]:

@rackton-example[#:eval ev #:mode 'defs]{
(: tens (Array 3 Integer))
(define tens (array 10 20 30))

(: squares (Array 4 Integer))
(define squares (build-array 4 (lambda (i) (* i i))))
}

Read an element with @racket[aref].  The index is a literal, and when
the size is known it is bounds-checked @italic{at compile time}, so an
out-of-bounds read is a type error rather than a runtime fault:

@rackton-example[#:eval ev #:mode 'value]{
(aref (array 10 20 30) 2)
}

Multidimensional arrays are simply nested — @racket[(Array n (Array m
a))] is an @racket[n]×@racket[m] grid — so a 2-D read is a nested
@racket[aref], and the size arithmetic is tracked in the type.
@racket[flatten-major] and @racket[flatten-minor] collapse one level of
nesting into a flat @racket[(Array (* n m) a)], differing only in element
order (row-major vs column-major):

@rackton-example[#:eval ev #:mode 'value]{
(aref (flatten-major (array (array 1 2 3)
                            (array 4 5 6)))
      3)
}

The element layout is hidden behind the array operations; only the size
and element type are observable.

@section[#:tag "quoted-list-literals"]{Quoted list literals}

A quoted identifier is a @racket[Symbol] (@racket['foo]).  Quoting a
parenthesized form generalizes this: @racket[quote] builds a
@racket[List] literal from the quoted data, and inference gives it the
obvious element type.  A list of integer literals is a
@racket[(List Integer)]:

@rackton-example[#:eval ev #:mode 'value]{
'(1 2 3)        ;; :: (List Integer)
}

A list of bare identifiers is a @racket[(List Symbol)]:

@rackton-example[#:eval ev #:mode 'value]{
'(a b c)        ;; :: (List Symbol)
}

Nesting works the same way — a quoted sublist is itself a @racket[List]:

@rackton-example[#:eval ev #:mode 'value]{
'((a b) (c d))  ;; :: (List (List Symbol))
}

Like every @racket[List], a quoted list is @bold{homogeneous}: all its
elements must share one type.  A quoted list whose elements disagree is a
type error, reported as the ordinary unification failure it is.  Here the
leading symbol @racket[Pair] fixes the element type to @racket[Symbol], so
the @racket[String] @racket["abc"] does not fit:

@rackton-example[#:eval ev #:mode 'display]{
'(Pair "abc" 1 2 3)   ;; type error: expected Symbol, got String
}

@subsection{Quasiquotation}

Backquote (@racket[quasiquote]) is @racket[quote] with two escapes.  An
@racket[unquote] (@litchar{,}) drops back into an ordinary Rackton
expression and contributes its value as one element — so a quasiquoted
list can hold computed values, not just literals:

@rackton-example[#:eval ev #:mode 'value]{
`(,(+ 1 2) ,(* 2 3))   ;; :: (List Integer)
}

An @racket[unquote-splicing] (@litchar[",@"]) instead splices a
list-valued expression into the surrounding list:

@rackton-example[#:eval ev #:mode 'value]|{
`(1 2 ,@(list 3 4) 5)   ;; :: (List Integer)
}|

Escapes are honored only inside a quasiquote.  Writing @litchar{,} or
@litchar[",@"] anywhere else — including inside a plain @racket[quote] — is a
compile error, matching Racket.  (That is exactly what lets the REPL claim
a leading comma for its commands; see @secref["quickstart"].)

Quotation and the @racket[list] form work in @racket[match] patterns too —
including a @racket[(list a b ...)] head/tail binder; see
@secref["list-patterns"].

@section{Type signatures and inference}

A top-level definition without a signature is inferred:

@rackton-example[#:eval ev #:mode 'defs]{
(define (compose f g)
  (lambda (x) (f (g x))))
;; inferred :: (∀ a b c) (-> (-> b c) (-> (-> a b) (-> a c)))
}

Add a signature and Rackton skolem-checks the body against it:

@rackton-example[#:eval ev #:mode 'defs]{
(: compose (-> (-> b c) (-> (-> a b) (-> a c))))
(define (compose f g)
  (lambda (x) (f (g x))))
}

A type signature isn't required for mutual recursion: top-level
forms in a Rackton module are order-invariant.  Every name is
visible to every other form regardless of where it appears in the
file:

@rackton-example[#:eval ev #:mode 'defs]{
(define (even? n) (if (= n 0) #t (odd?  (- n 1))))
(define (odd?  n) (if (= n 0) #f (even? (- n 1))))
}

The same rule applies to data types, protocols, instances, and
references to a value defined later in the file:

@rackton-example[#:eval ev #:mode 'defs]{
;; Forward reference between defs:
(: r Integer)
(define r (g 3))
(define (g x) (* x 2))

;; Mutually recursive data types:
(data Tree Leaf (Br Forest Forest))
(data Forest Empty (Cns Tree Forest))

;; A protocol used before its declaration:
(define (greet x) (pretty x))
(protocol (Pretty a)
  (: pretty (-> a String)))
}

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

@rackton-example[#:eval ev #:mode 'value]{
(: add (-> Integer (-> Integer Integer)))
(define (add x y) (+ x y))

((add 3) 4)
}

Partial application is implicit, so @racket[(add 3 4)] gives the same
result:

@rackton-example[#:eval ev #:mode 'value]{
(: add (-> Integer (-> Integer Integer)))
(define (add x y) (+ x y))

(add 3 4)
}

Currying is honored even when a function produces its result as a
returned lambda: a surplus argument flows into that lambda, so
@racket[(fma 3 4 5)] agrees with @racket[((fma 3 4) 5)].

@rackton-example[#:eval ev #:mode 'value]{
(: fma (-> Integer (-> Integer (-> Integer Integer))))
(define (fma a b) (lambda (c) (+ (* a b) c)))

(fma 3 4 5)
}

@racket[->] is variadic in source position but always parses
right-associatively.

@section{Explicit polymorphism}

When you want to be specific about quantification, use @racket[All]:

@rackton-example[#:eval ev #:mode 'defs]{
(: const (All (a b) (-> a (-> b a))))
(define (const x) (lambda (_y) x))
}

This is exactly equivalent to leaving the type variables implicit;
explicit @racket[All] is mandatory only when you want rank-N
polymorphism (see @secref["polymorphism"]).

@section{Type ascription}

@racket[(ann expr type)] asserts a type:

@rackton-example[#:eval ev #:mode 'value]{
(ann Nil (List Integer))   ;; Nil at type (List Integer)
}

@rackton-example[#:eval ev #:mode 'value]{
(ann (pure 3) (Maybe Integer))   ;; pure resolved at the Maybe instance
}

This is useful for disambiguating return-typed methods (@racket[pure],
@racket[mempty]) and for forcing the type checker to commit to a
specific instantiation.

@section{The host-language escape}

@racket[(racket τ (var ...) body ...)] drops out of Rackton and runs
@racket[body] as Racket, asserting that its value has type
@racket[τ]:

@rackton-example[#:eval ev #:mode 'defs]{
(: greet (-> String String))
(define (greet name)
  (racket String (name)
    (string-append "hello " name)))
}

The named Rackton bindings @racket[var ...] are spliced into
@racket[body] unmodified.  No type-checking is performed on
@racket[body]; the type assertion is taken on faith.  See
@secref["racket-interop"] for the full story.
