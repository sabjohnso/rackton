#lang scribble/manual
@require[scribble/manual
         (for-label rackton)
         "../rackton-eval.rkt"]
@(define ev (make-rackton-eval))

@title[#:tag "guide-macros"]{Macros}

Rackton's macros are Racket's macros.  When you write a macro definition
inside a Rackton program, you are defining an ordinary hygienic Racket
syntax transformer; a front phase runs the Racket expander over your code,
turning each macro use into core Rackton forms before the type checker ever
sees it.  You get the full power of the Racket macro system — and, because
the real expander does the expanding, you get hygiene for free.

@section{Defining and using a macro}

The simplest macros are pattern macros, written with
@racket[define-syntax-rule]: a use is rewritten to a template with the
arguments substituted in.  The expansion is plain Rackton, so it is
type-checked like anything else.

@rackton-example[#:eval ev #:mode 'value]{
(define-syntax-rule (square x) (* x x))
(square 9)
}

Macros may be used wherever an expression is expected, may nest, and may be
built out of one another:

@rackton-example[#:eval ev #:mode 'value]{
(define-syntax-rule (square x) (* x x))
(define-syntax-rule (sum-of-squares a b) (+ (square a) (square b)))
(sum-of-squares 3 4)
}

@section{Hygiene}

You never have to worry about a macro's temporary names clashing with
yours.  A binder the macro introduces is automatically kept separate from
your variables, even when the names coincide:

@rackton-example[#:eval ev #:mode 'value]{
(define-syntax-rule (double-via-tmp n)
  (let ([tmp n]) (+ tmp tmp)))
(let ([tmp 5])
  (double-via-tmp tmp))
}

The inner @racket[tmp] introduced by @racket[double-via-tmp] is distinct
from the @racket[tmp] you bound, so this is @racket[(+ 5 5)] — the macro
cannot accidentally capture your @racket[tmp].  The same protection runs
the other way: a macro that calls a top-level function still calls
@emph{that} function even if you have a local binding of the same name.

@section{Going further}

Beyond pattern macros you can write @emph{procedural} macros with
@racket[define-syntax], which run arbitrary code at compile time, and you
can @racket[provide] a macro so that other modules @racket[require] and use
it.  Both are covered, with the precise grammar and the
@racket[(require (for-syntax racket/base))] you need for procedural
transformers, in the
@secref["macros" #:doc '(lib "rackton/scribblings/reference/rackton-reference.scrbl")]
chapter of the reference.
