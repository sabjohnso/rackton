#lang scribble/manual
@require[scribble/manual
         (for-label rackton)]

@title[#:tag "higher-kinded"]{Higher-kinded and multi-parameter classes}

This chapter covers three orthogonal extensions to single-parameter
classes: higher-kinded type parameters, multi-parameter classes, and
functional dependencies.

@section{Kinds}

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

Kind annotations on class parameters use @racket[::]:

@codeblock|{
(protocol (Functor (f :: (-> * *)))
  (: fmap (-> (-> a b) (-> (f a) (f b)))))
}|

Without the annotation @racket[f] defaults to kind @racket[*], which
would reject @racket[(f a)] as ill-kinded.

@section{Multi-parameter classes}

A class declaration may carry more than one type parameter:

@codeblock|{
(protocol (Convertible a b)
  (: convert (-> a b)))

(instance (Convertible Integer String)
  (define (convert n) (show n)))

(instance (Convertible Boolean String)
  (define (convert b) (if b "yes" "no")))
}|

Runtime dispatch uses the first argument whose type mentions a class
parameter — for @racket[convert] that's its single argument.  The
non-dispatching parameters are resolved at compile time only; an
ambiguous call site may need a @racket[(ann e τ)] ascription to pin
the result type.

@section{Functional dependencies}

A multi-parameter class may declare that some parameters are uniquely
determined by others:

@codeblock|{
(protocol (Convert a b)
  (#:fundep a -> b)
  (: convert (-> a b)))

(instance (Convert Integer String)
  (define (convert n) (show n)))
}|

The @racket[#:fundep a -> b] clause says: @racket[a] determines
@racket[b].  Rackton uses this to resolve ambiguity — if a call site
fixes @racket[a] to @racket[Integer], the type checker can conclude
@racket[b] must be @racket[String] without needing an ascription.  In
return, you cannot declare two instances of @racket[Convert] with the
same @racket[a] but different @racket[b]; Rackton rejects them as
inconsistent with the fundep.

@section{Higher-kinded with constraint}

The two combine naturally — @racket[Monad] is a higher-kinded class
with a @racket[Functor] superclass.  The bound carries both facts at
once: because @racket[Functor]'s parameter has kind @racket[(-> * *)],
the bound @racket[[m => Functor]] makes @racket[m] higher-kinded
without a separate @racket[::] annotation:

@codeblock|{
(protocol (Monad [m => Functor])
  (: flatmap (-> (-> a (m b)) (-> (m a) (m b)))))
}|

Dispatch for higher-kinded class methods uses the position of the
first argument whose type mentions a class parameter.  For
@racket[fmap], that is the second argument (the container); for
@racket[flatmap], that is also the second argument (the @racket[(m a)]
follows the continuation).  This is computed automatically at class
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

@codeblock|{
(: inc-then-double (-> Integer Integer))
(define inc-then-double
  (comp (arr (lambda (n) (+ n 1)))
         (arr (lambda (n) (* n 2)))))
}|

The @racket[proc] form is the point-free analogue of @racket[do]: each
command feeds a value through an arrow, and bindings stay in scope for
later commands.

@codeblock|{
(: sum-with-succ (-> Integer Integer))
(define sum-with-succ
  (proc (x)
    [y <- (feed (arr (lambda (n) (+ n 1))) x)]
    (feed (arr (lambda (p) (match p [(Pair a b) (+ a b)]))) (Pair x y))))
}|

The method names are deliberately non-infix and distinct from existing
prelude names (@racket[ident]/@racket[comp], not @racket[id]/@racket[compose];
@racket[on-first]/@racket[on-second], not @racket[Bifunctor]'s
@racket[first]/@racket[second]).  The reference covers the full hierarchy
— @racket[ArrowChoice], @racket[ArrowApply], @racket[ArrowLoop] — and the
@racket[proc] command grammar.
