#lang scribble/manual
@require[scribble/manual
         (for-label rackton)
         "../rackton-eval.rkt"]
@(define ev (make-rackton-eval))

@title[#:tag "higher-kinded"]{Higher-kinded and multi-parameter protocols}

This chapter covers three orthogonal extensions to single-parameter
protocols: higher-kinded type parameters, multi-parameter protocols, and
functional dependencies.

@section[#:tag "guide-kinds"]{Kinds}

Kinds classify type-level expressions just as types classify values.
The base kind is @racket[*]; arrow kinds @racket[(-> k1 k2)] describe
type constructors.

@itemlist[
@item{@racket[Integer]   has kind @racket[*].}
@item{@racket[Maybe]     has kind @racket[(-> * *)] — it takes a type
      and yields a type.}
@item{@racket[Pair]      has kind @racket[(-> * (-> * *))] — two
      arguments.}
@item{@racket[(Maybe Integer)] has kind @racket[*].}]

Every type expression is kind-checked, and kinds are @emph{inferred},
so you rarely write a kind annotation.  A protocol parameter's kind
follows from its method signatures — a parameter used as @racket[(f a)]
must have kind @racket[(-> * *)]:

@rackton-example[#:eval ev #:mode 'defs]{
(protocol (Mappable f)
  (: mapp (-> (-> a b) (-> (f a) (f b)))))
}

Here @racket[f] is inferred to have kind @racket[(-> * *)] from
@racket[mapp]'s use of @racket[(f a)] — no annotation needed.  (A data
type's kind is inferred the same way, from how its parameters appear
in its constructors.)  You may still annotate a protocol parameter
explicitly in the head with @racket[::] when you want the kind stated:

@rackton-example[#:eval ev #:mode 'defs]{
(protocol (Functor2 (f :: (-> * *)))
  (: fmap2 (-> (-> a b) (-> (f a) (f b)))))
}

An ill-kinded type is a compile-time error — a constructor applied to
too many arguments, an ordinary type applied at all, or a protocol given
an argument of the wrong kind.  The error is blamed at the exact
offending sub-expression (line and column), even when it is buried
inside a larger type, so the caret lands on the part that is wrong:

@racketblock[
(code:comment "List has kind (-> * *) but is applied to 2 arguments")
(: bad1 (List Integer Integer))
(code:comment "Integer has kind * and cannot be applied")
(: bad2 (-> (Integer Boolean) a))
(code:comment "Functor expects an argument of kind (-> * *), but this one has kind *")
(instance (Functor Integer) (define (fmap f x) x))
(code:comment "blamed on the inner (List …), not the whole signature")
(: bad3 (-> Integer (-> Boolean (List Integer Integer))))
]

@section{Multi-parameter protocols}

A protocol declaration may carry more than one type parameter:

@rackton-example[#:eval ev #:mode 'defs]{
(protocol (Convertible a b)
  (: convert (-> a b)))

(instance (Convertible Integer String)
  (define (convert n) (show n)))

(instance (Convertible Boolean String)
  (define (convert b) (if b "yes" "no")))
}

Runtime dispatch uses the first argument whose type mentions a protocol
parameter — for @racket[convert] that's its single argument.  The
non-dispatching parameters are resolved at compile time only; an
ambiguous call site may need a @racket[(ann e τ)] ascription to pin
the result type.

@section{Functional dependencies}

A multi-parameter protocol may declare that some parameters are uniquely
determined by others:

@rackton-example[#:eval ev #:mode 'defs]{
(protocol (Convert a b)
  (:fundep a -> b)
  (: convert (-> a b)))

(instance (Convert Integer String)
  (define (convert n) (show n)))
}

The @racket[:fundep a -> b] clause says: @racket[a] determines
@racket[b].  Rackton uses this to resolve ambiguity — if a call site
fixes @racket[a] to @racket[Integer], the type checker can conclude
@racket[b] must be @racket[String] without needing an ascription.  In
return, you cannot declare two instances of @racket[Convert] with the
same @racket[a] but different @racket[b]; Rackton rejects them as
inconsistent with the fundep.

@section{Higher-kinded with constraint}

The two combine naturally — @racket[Monad] is a higher-kinded protocol
with a @racket[Functor] superprotocol.  The bound carries both facts at
once: because @racket[Functor]'s parameter has kind @racket[(-> * *)],
the bound @racket[[m => Functor]] makes @racket[m] higher-kinded
without a separate @racket[::] annotation:

@rackton-example[#:eval ev #:mode 'defs]{
(protocol (Monad [m => Functor])
  (: flatmap (-> (-> a (m b)) (-> (m a) (m b)))))
}

Dispatch for higher-kinded protocol methods uses the position of the
first argument whose type mentions a protocol parameter.  For
@racket[fmap], that is the second argument (the container); for
@racket[flatmap], that is also the second argument (the @racket[(m a)]
follows the continuation).  This is computed automatically at protocol
definition.

@section{Built-in instances}

The prelude ships @racket[Functor], @racket[Applicative], and
@racket[Monad] instances for @racket[Maybe], @racket[Either a],
@racket[List], @racket[IO], @racket[State s], @racket[Env r],
@racket[Identity], @racket[STM], and the four transformers
@racket[StateT], @racket[EnvT], @racket[WriterT], @racket[ExceptT].
@racket[List]'s @racket[flatmap] is concatMap.  The transformer
instances are themselves qualified — a @racket[StateT s m] is a
@racket[Monad] only when @racket[m] is.

@section{Arrows}

Arrows (Hughes) generalize plain functions.  The @racket[Category] →
@racket[Arrow] hierarchy abstracts composition (@racket[comp]), lifting a
function (@racket[arr]), and pairing computations (@racket[fanout],
@racket[split]), so the same point-free code runs over ordinary functions
and over richer arrows.  The canonical instance is the function arrow
@racket[(->)], where every combinator collapses to function plumbing:

@rackton-example[#:eval ev #:mode 'defs]{
(: inc-then-double (-> Integer Integer))
(define inc-then-double
  ;; comp is right-to-left: the second arrow (+1) runs first, then (*2).
  (comp (arr (lambda (n) (* n 2)))
        (arr (lambda (n) (+ n 1)))))
}

The @racket[proc] form is the point-free analogue of @racket[do]: each
command feeds a value through an arrow, and bindings stay in scope for
later commands.

@rackton-example[#:eval ev #:mode 'defs]{
(: sum-with-succ (-> Integer Integer))
(define sum-with-succ
  (proc (x)
    [y <- (feed (arr (lambda (n) (+ n 1))) x)]
    (feed (arr (lambda (p) (match p [(Pair a b) (+ a b)]))) (Pair x y))))
}

The method names are deliberately non-infix and distinct from existing
prelude names (@racket[ident]/@racket[comp], not @racket[id]/@racket[compose];
@racket[on-first]/@racket[on-second], not @racket[Bifunctor]'s
@racket[first]/@racket[second]).  The reference covers the full hierarchy
— @racket[ArrowChoice], @racket[ArrowApply], @racket[ArrowLoop] — and the
@racket[proc] command grammar.
