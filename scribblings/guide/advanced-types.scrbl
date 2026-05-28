#lang scribble/manual
@require[scribble/manual
         (for-label rackton)]

@title[#:tag "advanced-types"]{Advanced types}

This chapter covers four type features that go beyond ordinary ADTs:
GADTs, existentials, type aliases, and sealed abstract types, plus
associated type families.

@section[#:tag "existentials"]{Existential types}

An existential parameter on a constructor lets the constructor hide a
type from the outside world.  Use a per-constructor @racket[#:forall]
clause to introduce the hidden variable:

@codeblock|{
(data Anything
  (Wrap #:forall (a) a (-> a String)))

(define many
  (Cons (Wrap 42 show)
        (Cons (Wrap "hi" id)
              Nil)))
}|

Each @racket[Wrap] inside @racket[many] has a different @racket[a],
but they all coexist in @racket[(List Anything)].  Add a
@racket[#:where] clause to require the existential to satisfy class
constraints — those become hypotheses inside any clause that matches
the constructor:

@codeblock|{
(data ExistsShow
  (PackShow #:forall (a) #:where (Show a) a))
}|

Pattern matching introduces a fresh skolem for each clause; the
skolem cannot escape its clause:

@codeblock|{
(: describe-each (-> (List Anything) (List String)))
(define (describe-each xs)
  (match xs
    [(Nil) Nil]
    [(Cons (Wrap v print) rest)
     (Cons (print v)               (code:comment "OK: print and v share a skolem")
           (describe-each rest))]))
}|

@section{Generalised ADTs (GADTs)}

A constructor can refine its result type by giving its full type
signature after a @racket[:].  The signature is an arrow whose final
type is the result and whose leading types are the fields:

@codeblock|{
(data (Term a)
  (Lit     : (-> a (Term a)))
  (IntLit  : (-> Integer (Term Integer)))
  (BoolLit : (-> Boolean (Term Boolean)))
  (Plus    : (-> (Term Integer) (Term Integer) (Term Integer)))
  (If      : (-> (Term Boolean) (Term a) (Term a) (Term a))))
}|

How much a constructor refines the parameter @racket[a] depends on
whether its result type is @emph{concrete} or a @emph{bare variable}:

@itemlist[

@item{@racket[IntLit], @racket[BoolLit], and @racket[Plus] have
concrete result indices (@racket[(Term Integer)], @racket[(Term
Boolean)]).  Matching one refines @racket[a] to that concrete type.}

@item{@racket[(Lit : (-> a (Term a)))] has a bare-variable index equal
to its field type, so it is an ordinary polymorphic constructor — type
@racket[(-> a (Term a))] for every @racket[a].  Matching it refines
nothing: the stored value already has type @racket[a], so you just get
it back.}

@item{@racket[If] is likewise parametric in @racket[a] — its two
branches and its result all share the same @racket[a] — so it refines
nothing either.}

]

Matching on a GADT constructor refines the scrutinee's type parameter
within the clause:

@codeblock|{
(: eval-term (-> (Term a) a))
(define (eval-term t)
  (match t
    [(Lit     x)        x]      (code:comment "a stays polymorphic")
    [(IntLit  n)        n]      (code:comment "here a == Integer")
    [(BoolLit b)        b]      (code:comment "here a == Boolean")
    [(Plus x y)         (+ (eval-term x) (eval-term y))]
    [(If c then else)   (if (eval-term c) (eval-term then) (eval-term else))]))
}|

The refinement is local to the clause, so different clauses can return
different types — exactly what makes a typed interpreter possible.

The same concrete-vs-variable rule governs @emph{field-less}
constructors, which use a non-arrow signature whose lone type is the
result.  A concrete result refines, while a bare-variable result is
universally quantified and refines nothing — exactly like @racket[Nil]
has type @racket[(List a)] for every @racket[a]:

@codeblock|{
(data (Tagged a)
  (IntTag  : (Tagged Integer))   (code:comment "concrete — refines a ~ Integer")
  (BoolTag : (Tagged Boolean))   (code:comment "concrete — refines a ~ Boolean")
  (AnyTag  : (Tagged a)))        (code:comment "polymorphic — no refinement")

(: width (-> (Tagged a) Integer))
(define (width t)
  (match t
    [(IntTag)  64]
    [(BoolTag) 1]
    [(AnyTag)  0]))   (code:comment "a is unconstrained here")
}|

@section{Type aliases}

@codeblock|{
(define-alias Name           String)
(define-alias (Endo a)       (-> a a))
(define-alias (Pair3 a b c)  (Pair a (Pair b c)))
}|

Aliases expand inline during type resolution; they introduce no
runtime cost.  Recursive aliases are rejected with a clear error.

@section{Sealed abstract types}

Adding @racket[#:abstract] to a @racket[data] hides the
constructors from the type checker in any module that doesn't define
the type:

@codeblock|{
;; counter.rkt
#lang rackton
(provide (data-out Counter) make-counter increment count)

(data Counter #:abstract
  (Counter Integer))

(: make-counter (-> Counter))
(define (make-counter) (Counter 0))

(: increment (-> Counter Counter))
(define (increment c) (match c [(Counter n) (Counter (+ n 1))]))

(: count (-> Counter Integer))
(define (count c) (match c [(Counter n) n]))
}|

Inside @filepath{counter.rkt} the constructor works normally.  In any
importing module, @racket[Counter] is a type but the @racket[Counter]
constructor is invisible — you must go through @racket[make-counter],
@racket[increment], @racket[count].  This is how you build a true
encapsulation boundary.

@section{Associated type families}

A class may declare an associated type via a @racket[#:type] clause
inside its body:

@codeblock|{
(protocol (Container c)
  (#:type Elem)
  (: empty? (-> c Boolean))
  (: head   (-> c (Maybe (Elem c)))))
}|

Each instance supplies a concrete type for the family with
@racket[(#:type (Family = T))]:

@codeblock|{
(instance (Container (List a))
  (#:type (Elem = a))
  (define (empty? xs) (match xs [(Nil) #t] [(Cons _ _) #f]))
  (define (head   xs) (match xs [(Nil) None] [(Cons h _) (Some h)])))
}|

Calls to @racket[head] on a @racket[(List Integer)] resolve
@racket[(Elem (List Integer))] to @racket[Integer], and the result type
becomes @racket[(Maybe Integer)] with no further declaration needed.
Associated types are how Rackton lets a class abstract over a type
relationship that's more flexible than a fundep.
