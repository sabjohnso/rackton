#lang scribble/manual
@require[scribble/manual
         (for-label rackton)]

@title[#:tag "classes"]{Type classes}

Type classes let you overload an operation across many types while
keeping each call site fully resolved at type-check time.  Rackton's
classes are modelled on Haskell's: a class declares one or more
method signatures, instances provide implementations, and constraints
flow through type signatures so a polymorphic function can demand the
operations it needs.

@section{Declaring a class}

@codeblock|{
(protocol (Eq a)
  (: ==  (-> a (-> a Boolean)))
  (: /= (-> a (-> a Boolean)))
  (define (/= x y) (if (== x y) #f #t)))
}|

A class declaration introduces zero or more @italic{method signatures}
(@racket[(: name type)]) and zero or more @italic{default
implementations} (@racket[(define …)]).  Each method is added to the
value environment with the qualified scheme @racket[(All (a) ((Eq a) => τ))],
so any polymorphic use of @racket[==] automatically carries the
class constraint.

@section{Superclasses}

A class can demand its parameters already satisfy another class:

@codeblock|{
(protocol ((Eq a) => (Ord a))
  (: < (-> a (-> a Boolean))))
}|

Superclass closure is followed during entailment: any program with an
@racket[Ord] constraint on @racket[a] automatically discharges
@racket[Eq] constraints on the same type.

@section{Declaring an instance}

@codeblock|{
(instance (Eq Integer)
  (define (== x y) (= x y)))

(instance ((Eq a) => (Eq (Maybe a)))
  (define (== x y)
    (match x
      [(None)   (match y [(None) #t] [(Some _) #f])]
      [(Some u) (match y [(None) #f] [(Some v) (== u v)])])))
}|

The head of an instance can carry a context (@racket[((Eq a) => …)])
that becomes a hypothesis available to the body and required at use
sites.  Omitted methods fall back to the class's default.

@section{Constrained polymorphic functions}

A function that uses a class method picks up its constraint
automatically:

@codeblock|{
(: contains? ((Eq a) => (-> a (-> (Maybe a) Boolean))))
(define (contains? target m)
  (match m
    [(None)   #f]
    [(Some x) (== x target)]))
}|

The inferred scheme is
@racket[(All (a) ((Eq a) => (-> a (-> (Maybe a) Boolean))))].  Rackton
discharges class constraints by dispatching at runtime on the type
tag of the value argument, so the constraint is fully erased from the
calling convention — no explicit dictionary is threaded through user
code.

@section{Default methods}

A default method body inside a class declaration is used by any
instance that omits it:

@codeblock|{
(protocol (Eq a)
  (: == (-> a (-> a Boolean)))
  (: /= (-> a (-> a Boolean)))
  (define (/= x y) (if (== x y) #f #t)))   ;; default for /=

(instance (Eq Integer)
  (define (== x y) (= x y)))
;; /= for Integer falls back to the class default.
}|

Defaults can call other methods of the same class — Rackton resolves
each call to the eventual instance's body.

@section{Coherence}

Rackton enforces @italic{module-level} coherence: an instance is
visible everywhere its defining module is loaded, regardless of any
@racket[provide] form.  This is the Haskell tradition.  It guarantees
that two different parts of a program can never disagree about which
instance to use for a given class/type pair.

@section{See also}

@itemlist[
@item{@secref["higher-kinded"] — kinds, multi-parameter classes, and
      functional dependencies.}
@item{@secref["do-and-monads"] — monads and @racket[do]-notation.}
@item{Reference: @secref["classes" #:doc '(lib "rackton/scribblings/reference/rackton-reference.scrbl")]
      for the full list of prelude classes and methods.}]
