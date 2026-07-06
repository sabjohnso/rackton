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

@rackton-example[#:eval ev #:mode 'error]{
(define (bad f)
  (Pair (f 1) (f "hi")))
}

This is the @italic{value restriction} as it appears in HM with
let-polymorphism.  If you need polymorphic parameters, see rank-N
below.

@section{Polymorphic recursion}

A function with a declared polymorphic scheme can call itself at a
different instantiation than the enclosing call:

@rackton-example[#:eval ev #:mode 'defs]{
(data (Tree a) Leaf (Node (Tree a) a (Tree a)))

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
signatures, using a per-constructor @racket[:forall] / @racket[:where]
clause:

@rackton-example[#:eval ev #:mode 'defs #:context? #t]{
(data Anything
  (Wrap :forall (a) a (-> a String)))
}

@racket[Wrap] takes a value of any type and a printer for that type;
the type variable @racket[a] is hidden from the outside.  Add
@racket[:where] to require the existential to satisfy one or more
protocol constraints — those constraints become hypotheses available
inside any clause that matches the constructor:

@rackton-example[#:eval ev #:mode 'defs]{
(data ExistsShow
  (PackShow :forall (a) :where (Show a) a))
}

Pattern matching on an existential constructor introduces a fresh
skolem inside the clause:

@rackton-example[#:eval ev #:mode 'defs #:context? #t]{
(: describe (-> Anything String))
(define (describe x)
  (match x [(Wrap v print) (print v)]))
}

Inside the clause, @racket[v] and @racket[print] share the same fresh
@racket[a]; outside, that @racket[a] cannot escape.  See
@secref["advanced-types"] for more.

@section{First-class existential types}

The constructor form above wraps the existential in a named datatype.
You can also write an existential @italic{inline}, anywhere a type is
expected, with @racket[Exists] — the dual of @racket[All].  Constraints
use the same infix @racket[=>]:

@rackton-example[#:eval ev #:mode 'defs #:context? #t]{
(: a-showable (Exists (a) ((Show a) => a)))
(define a-showable (ann 42 (Exists (a) ((Show a) => a))))
}

A value is @italic{packed} into an existential by annotating it with the
existential type (the @racket[ann] form).  The witness type — here
@racket[Integer] — is hidden, and its @racket[Show] constraint is
discharged at the pack site.  Because the witness is hidden, values of
different types share one element type and sit in one list:

@rackton-example[#:eval ev #:mode 'defs #:context? #t]{
(: showables (List (Exists (a) ((Show a) => a))))
(define showables
  (Cons (ann 42 (Exists (a) ((Show a) => a)))
        (Cons (ann "hi" (Exists (a) ((Show a) => a)))
              Nil)))
}

A packed value is @italic{unpacked} with @racket[open].  @racket[(open e
(a x) body)] binds the hidden type as a fresh rigid @racket[a] and the
witness value as @racket[x], with the packed constraints in scope — so a
method like @racket[show] resolves and dispatches on the runtime value:

@rackton-example[#:eval ev #:mode 'defs #:context? #t]{
(: render (-> (Exists (a) ((Show a) => a)) String))
(define (render e) (open e (a x) (show x)))
}

@rackton-example[#:eval ev #:mode 'value #:context? #t]{
(render a-showable)
}

The hidden type may not @italic{escape} the @racket[open]: the body's
result type cannot mention @racket[a].  Returning the witness itself is
a compile error, because its type is exactly the hidden @racket[a].

Like rank-N @racket[All], @racket[Exists] is never inferred: you pack
with an explicit @racket[ann] and bound the hidden type's scope with
@racket[open].
