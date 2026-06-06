#lang scribble/manual
@require[scribble/manual
         (for-label rackton)
         "../rackton-eval.rkt"]
@(define ev (make-rackton-eval))

@title[#:tag "type-classes"]{Type classes}

Type classes let you overload an operation across many types while
keeping each call site fully resolved at type-check time.  Rackton's
classes are modelled on Haskell's: a class declares one or more
method signatures, instances provide implementations, and constraints
flow through type signatures so a polymorphic function can demand the
operations it needs.

@section{Declaring a class}

@rackton-example[#:eval ev #:mode 'defs #:context? #t]{
(protocol (Eq a)
  (: ==  (-> a (-> a Boolean)))
  (: /= (-> a (-> a Boolean)))
  (define (/= x y) (if (== x y) #f #t)))
}

A class declaration introduces zero or more @italic{method signatures}
(@racket[(: name type)]) and zero or more @italic{default
implementations} (@racket[(define …)]).  Each method is added to the
value environment with the qualified scheme @racket[(All (a) ((Eq a) => τ))],
so any polymorphic use of @racket[==] automatically carries the
class constraint.

@section{Superclasses}

A class can demand its parameters already satisfy another class.  The
requirement is written as a
@tech[#:doc '(lib "rackton/scribblings/reference/rackton-reference.scrbl")]{bound}
on the parameter, after @racket[=>]:

@rackton-example[#:eval ev #:mode 'defs]{
(protocol (Ord [a => Eq])
  (: < (-> a (-> a Boolean))))
}

A bound @racket[[a => Eq]] reads ``@racket[a] is an @racket[Eq]'' and
desugars to the superclass constraint @racket[(Eq a)].  Several classes
may be stacked on one parameter (@racket[[a => Num Ord]]), and a
multi-parameter class bounds each parameter in turn
(@racket[(MonadWriter [w => Monoid] [m => Monad])]).  When the bound's
class needs more arguments, supply them and let the parameter fill the
last slot: @racket[[b => (Convert a)]] desugars to @racket[(Convert a b)].
A superclass that genuinely relates several parameters at once is
written instead as a trailing @racket[(#:requires (C …))] clause in the
body.

The older head-prefix form, which listed superclasses before the class
head as @racket[(protocol ((Eq a) => (Ord a)) …)], has been retired and
is now a syntax error.  Restate each superclass as a parameter bound or
a @racket[#:requires] clause.  (This is distinct from an @italic{instance}
context, @racket[((Eq a) => (Eq (Maybe a)))], which is unchanged — see
below.)

Superclass closure is followed during entailment: any program with an
@racket[Ord] constraint on @racket[a] automatically discharges
@racket[Eq] constraints on the same type.

@section{Declaring an instance}

@rackton-example[#:eval ev #:mode 'defs #:context? #t]{
(instance (Eq Integer)
  (define (== x y) (= x y)))

(instance ((Eq a) => (Eq (Maybe a)))
  (define (== x y)
    (match x
      [(None)   (match y [(None) #t] [(Some _) #f])]
      [(Some u) (match y [(None) #f] [(Some v) (== u v)])])))
}

The head of an instance can carry a context (@racket[((Eq a) => …)])
that becomes a hypothesis available to the body and required at use
sites.  Omitted methods fall back to the class's default.

@section{Constrained polymorphic functions}

A function that uses a class method picks up its constraint
automatically:

@rackton-example[#:eval ev #:mode 'defs #:context? #t]{
(: contains? ((Eq a) => (-> a (-> (Maybe a) Boolean))))
(define (contains? target m)
  (match m
    [(None)   #f]
    [(Some x) (== x target)]))
}

The inferred scheme is
@racket[(All (a) ((Eq a) => (-> a (-> (Maybe a) Boolean))))].  Rackton
discharges class constraints by dispatching at runtime on the type
tag of the value argument, so the constraint is fully erased from the
calling convention — no explicit dictionary is threaded through user
code.

@section{Default methods}

A default method body inside a class declaration is used by any
instance that omits it:

@rackton-example[#:eval ev #:mode 'defs]{
(protocol (Eq a)
  (: == (-> a (-> a Boolean)))
  (: /= (-> a (-> a Boolean)))
  (define (/= x y) (if (== x y) #f #t)))   ;; default for /=

(instance (Eq Integer)
  (define (== x y) (= x y)))
;; /= for Integer falls back to the class default.
}

Defaults can call other methods of the same class — Rackton resolves
each call to the eventual instance's body.

@section{Cyclic defaults}

Some classes arrange their defaults in a cycle so an instance can pick
whichever method is most natural and the others derive.  The prelude's
@racket[Monad] is a 2-cycle:

@rackton-example[#:eval ev #:mode 'defs]{
(protocol (Monad [m => Applicative])
  (: flatmap (-> (-> a (m b)) (-> (m a) (m b))))
  (: join    (-> (m (m a)) (m a)))
  (define (flatmap f ma) (join (fmap f ma)))
  (define (join mma)     (flatmap (lambda (m) m) mma)))
}

An instance must define at least one of @racket[flatmap] or
@racket[join]; the other is derived.  Defining neither would loop at
runtime, so Rackton rejects such an instance at compile time:

@rackton-example[#:eval ev #:mode 'display]{
(instance (Monad MyType))   ;; → instance is incomplete:
                            ;;   methods (flatmap join) form a
                            ;;   cyclic default chain
                            ;;   (flatmap → join → flatmap); at
                            ;;   least one must be defined directly
                            ;;   to break the cycle.
}

@racket[Applicative] is a 3-cycle (@racket[fapply] ← @racket[product]
← @racket[liftA2] ← @racket[fapply]); defining any single member
suffices.

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
