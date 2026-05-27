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
with a @racket[Functor] superclass:

@codeblock|{
(protocol ((Functor m) => (Monad (m :: (-> * *))))
  (: >>= (-> (m a) (-> (-> a (m b)) (m b)))))
}|

Dispatch for higher-kinded class methods uses the position of the
first argument whose type mentions a class parameter.  For
@racket[fmap], that is the second argument (the container); for
@racket[>>=], the first.  This is computed automatically at class
definition.

@section{Built-in instances}

The prelude ships @racket[Functor], @racket[Applicative], and
@racket[Monad] instances for @racket[Maybe], @racket[Result e],
@racket[IO], @racket[State s], @racket[Env r], @racket[Identity],
@racket[STM], and the four transformers @racket[StateT],
@racket[EnvT], @racket[WriterT], @racket[ExceptT].  @racket[List] is
shipped as @racket[Functor] and @racket[Applicative] only.  The
transformer instances are themselves qualified — a @racket[StateT s m]
is a @racket[Monad] only when @racket[m] is.
