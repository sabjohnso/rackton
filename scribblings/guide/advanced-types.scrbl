#lang scribble/manual
@require[scribble/manual
         (for-label rackton)]

@title[#:tag "advanced-types"]{Advanced types}

This chapter covers four type features that go beyond ordinary ADTs:
GADTs, existentials, type aliases, and sealed abstract types, plus
associated type families.

@section[#:tag "existentials"]{Existential types}

An existential parameter on a constructor lets the constructor hide a
type from the outside world:

@codeblock|{
(define-data Anything
  #:exists (a)
  (Wrap a (-> a String)))

(define many
  (Cons (Wrap 42 show)
        (Cons (Wrap "hi" id)
              Nil)))
}|

Each @racket[Wrap] inside @racket[many] has a different @racket[a],
but they all coexist in @racket[(List Anything)].  Pattern matching
introduces a fresh skolem for each clause; the skolem cannot escape
its clause:

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

A constructor can refine its result type with @racket[#:returns]:

@codeblock|{
(define-data (Term a)
  (IntLit  Integer    #:returns (Term Integer))
  (BoolLit Boolean    #:returns (Term Boolean))
  (Plus    (Term Integer) (Term Integer)  #:returns (Term Integer))
  (If      (Term Boolean) (Term a) (Term a) #:returns (Term a)))
}|

Matching on a GADT constructor refines the scrutinee's type parameter
within the clause:

@codeblock|{
(: eval-term (-> (Term a) a))
(define (eval-term t)
  (match t
    [(IntLit  n)        n]      (code:comment "here a == Integer")
    [(BoolLit b)        b]      (code:comment "here a == Boolean")
    [(Plus x y)         (+ (eval-term x) (eval-term y))]
    [(If c then else)   (if (eval-term c) (eval-term then) (eval-term else))]))
}|

The refinement is local to the clause, so different clauses can return
different types — exactly what makes a typed interpreter possible.

@section{Type aliases}

@codeblock|{
(define-alias Name           String)
(define-alias (Endo a)       (-> a a))
(define-alias (Pair3 a b c)  (Pair a (Pair b c)))
}|

Aliases expand inline during type resolution; they introduce no
runtime cost.  Recursive aliases are rejected with a clear error.

@section{Sealed abstract types}

Adding @racket[#:abstract] to a @racket[define-data] hides the
constructors from the type checker in any module that doesn't define
the type:

@codeblock|{
;; counter.rkt
#lang rackton
(provide (data-out Counter) make-counter increment count)

(define-data Counter #:abstract
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

A class may declare an associated type:

@codeblock|{
(define-class (Container c)
  (define-associated-type (Elem c))
  (: empty? (-> c Boolean))
  (: head   (-> c (Maybe (Elem c)))))
}|

Each instance supplies a concrete type for the family:

@codeblock|{
(define-instance (Container (List a))
  (define-associated-type (Elem (List a)) a)
  (define (empty? xs) (match xs [(Nil) #t] [(Cons _ _) #f]))
  (define (head   xs) (match xs [(Nil) None] [(Cons h _) (Some h)])))
}|

Calls to @racket[head] on a @racket[(List Integer)] resolve
@racket[(Elem (List Integer))] to @racket[Integer], and the result type
becomes @racket[(Maybe Integer)] with no further declaration needed.
Associated types are how Rackton lets a class abstract over a type
relationship that's more flexible than a fundep.
