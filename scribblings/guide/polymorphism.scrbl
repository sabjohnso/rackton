#lang scribble/manual
@require[scribble/manual
         (for-label rackton)
         "../rackton-eval.rkt"]
@(define ev (make-rackton-eval))

@title[#:tag "polymorphism"]{Polymorphism in depth}

Rackton's type system uses Algorithm W with let-polymorphism: every
@racket[let] / @racket[letrec] / @racket[define] binding is generalised
independently against the surrounding environment.  This chapter
covers the corners: polymorphic recursion, rank-N polymorphism, and
when generalisation does (and doesn't) fire.

@section{Let-polymorphism, briefly}

@rackton-example[#:eval ev #:mode 'value]{
(define (id x) x)             ;; inferred :: (All (a) (-> a a))

(let ([f id])
  (Pair (f 1) (f "hi")))    ;; OK — f re-instantiates at each use
}

Each binding's right-hand side is type-checked, the resulting type is
generalised by abstracting over all free type variables that don't
appear in the surrounding environment, and the generalised scheme is
what's added to the environment.

Function-parameter bindings are NOT generalised — they have plain
monomorphic types:

@rackton-example[#:eval ev #:mode 'display]{
(lambda (f)
  (Pair (f 1) (f "hi")))    ;; TYPE ERROR: f used at two types
}

This is the @italic{value restriction} as it appears in HM with
let-polymorphism.  If you need polymorphic parameters, see rank-N
below.

@section{Polymorphic recursion}

A function with a declared polymorphic scheme can call itself at a
different instantiation than the enclosing call:

@rackton-example[#:eval ev #:mode 'display]{
(: depth (-> (Tree a) Integer))
(define (depth t)
  (match t
    [(Leaf)        0]
    [(Node l _ r)  (+ 1 (max (depth l) (depth r)))]))
}

Inside the body, the recursive @racket[depth] calls use the same
@racket[a] as the enclosing call — that's monomorphic recursion.  But
when the body uses a different @racket[a]:

@rackton-example[#:eval ev #:mode 'defs]{
(: nested-length (-> (List (List a)) Integer))
(define (nested-length xs)
  (match xs
    [(Nil)         0]
    [(Cons h t)
     (+ (length h)              ;; h :: List a
        (nested-length t))]))   ;; t :: List (List a) — same a
}

Without an explicit @racket[:] declaration, recursive calls are
constrained to the same instantiation — Rackton can't infer a more
general scheme.  Declare the signature first, and recursive calls may
re-instantiate freely.

@section{Rank-N polymorphism}

A function parameter can itself be polymorphic if you write the
quantifier inside the parameter's type:

@rackton-example[#:eval ev #:mode 'defs]{
(: apply-twice (-> (All (a) (-> a a)) (Pair Integer String)))
(define (apply-twice f)
  (Pair (f 1) (f "hello")))
}

Inside @racket[apply-twice], @racket[f] is universally quantified over
its argument — the call sites can instantiate it differently.  This
is rank-2.  Higher ranks work the same way by nesting quantifiers
deeper.

The cost of rank-N is that type inference becomes undecidable, so
Rackton requires the explicit @racket[All] inside the parameter type.
It will not synthesise it for you.

@section{Existential types}

Existentials appear on @italic{constructor} signatures, not function
signatures, using a per-constructor @racket[#:forall] / @racket[#:where]
clause:

@rackton-example[#:eval ev #:mode 'display]{
(data Anything
  (Wrap #:forall (a) a (-> a String)))
}

@racket[Wrap] takes a value of any type and a printer for that type;
the type variable @racket[a] is hidden from the outside.  Add
@racket[#:where] to require the existential to satisfy one or more
class constraints — those constraints become hypotheses available
inside any clause that matches the constructor:

@rackton-example[#:eval ev #:mode 'defs]{
(data ExistsShow
  (PackShow #:forall (a) #:where (Show a) a))
}

Pattern matching on an existential constructor introduces a fresh
skolem inside the clause:

@rackton-example[#:eval ev #:mode 'display]{
(: describe (-> Anything String))
(define (describe x)
  (match x [(Wrap v print) (print v)]))
}

Inside the clause, @racket[v] and @racket[print] share the same fresh
@racket[a]; outside, that @racket[a] cannot escape.  See
@secref["advanced-types"] for more.
