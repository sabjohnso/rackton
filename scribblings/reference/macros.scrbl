#lang scribble/manual
@require[scribble/manual
         (for-label rackton)
         "../rackton-eval.rkt"]
@(define ev (make-rackton-eval))

@title[#:tag "macros"]{Macros}

Rackton macros @emph{are} Racket macros.  A macro definition written
inside a @racket[(rackton …)] block, a @racket[(module @#,racketidfont{name}
rackton …)] body, or a @hash-lang[] @racketmodfont{rackton} file introduces
an ordinary, hygienic Racket syntax transformer.  Before type inference
runs, a front phase drives the Racket expander over the block, expanding
every macro use into core Rackton forms; only the expanded program reaches
the parser and the type checker.

Because the genuine expander does the work, the whole Racket macro toolbox
is available — pattern macros, procedural transformers that compute at
compile time, and macros that define other macros — and expansion is
@deftech{hygienic}: a binder a macro introduces never captures a user
binding of the same name, and a macro's reference to a top-level binding is
never captured by a same-named binding at the use site.

@section{Pattern macros}

@defform[(define-syntax-rule (name pattern-arg ...) template)]{
Defines @racket[name] as a pattern macro.  Each use
@racket[(name arg ...)] is rewritten to @racket[template] with each
@racket[pattern-arg] replaced by the corresponding @racket[arg].  The
expansion is an ordinary Rackton expression, type-checked like any other.}

@rackton-example[#:eval ev #:mode 'value]{
(define-syntax-rule (twice x) (+ x x))
(twice 21)
}

Uses may nest, and one macro may be defined in terms of another:

@rackton-example[#:eval ev #:mode 'value]{
(define-syntax-rule (inc x) (+ x 1))
(define-syntax-rule (add3 x) (inc (inc (inc x))))
(add3 0)
}

@section{Hygiene}

A binder a macro introduces stays distinct from same-named user bindings.
Here @racket[add-via-tmp] introduces its own @racket[tmp]; the use site
also binds @racket[tmp] and passes it in.  Hygienic expansion keeps the two
apart, so the result is @racket[(+ 1 100)], not @racket[(+ 1 1)]:

@rackton-example[#:eval ev #:mode 'value]{
(define-syntax-rule (add-via-tmp a b)
  (let ([tmp a]) (+ tmp b)))
(let ([tmp 100])
  (add-via-tmp 1 tmp))
}

The dual property holds for references: a macro that mentions a top-level
binding resolves to the binding visible where the macro was @emph{defined},
even if the use site shadows that name with a local binding.

@section{Procedural macros}

@defform*[[(define-syntax (name stx) body ...+)
           (define-syntax name transformer-expr)]]{
Defines @racket[name] as a procedural macro: @racket[name] is bound to a
compile-time transformer that receives the macro use as a syntax object and
returns its replacement.  The transformer body runs at phase 1, so a module
that writes one needs @racket[(require (for-syntax racket/base))] in scope
to bring @racket[syntax-case], @racket[syntax->datum], @racket[datum->syntax]
and the rest of the compile-time toolbox into the transformer environment.}

A procedural transformer can run arbitrary computation while the program is
being compiled.  This @racket[triple-literal] folds @racket[(* 3 n)] at
compile time and splices the resulting literal:

@rackton-example[#:eval ev #:mode 'value]{
(require (for-syntax racket/base))

(define-syntax (triple-literal stx)
  (syntax-case stx ()
    [(_ n) (datum->syntax stx (* 3 (syntax->datum #'n)))]))

(triple-literal 5)
}

The @racket[15] is folded during compilation and spliced as a literal.

A macro may even expand into a fresh macro definition (a macro-defining
macro); the front phase binds the generated macro before later uses:

@rackton-example[#:eval ev #:mode 'value]{
(define-syntax-rule (define-doubler name op)
  (define-syntax-rule (name x) (op x x)))

(define-doubler dbl +)
(dbl 4)
}

@section{Exporting macros across modules}

A macro is exported with @racket[provide] like any other binding, and a
module that @racket[require]s the library may use it directly — its
definition travels alongside the library's values and types:

@racketmod[#:file "double.rkt"
rackton
(provide double)
(code:blank)
(define-syntax-rule (double x) (+ x x))]

@racketblock[
(require "double.rkt")
(code:blank)
(: forty-two Integer)
(define forty-two (double 21))]

Pattern macros (and procedural macros whose transformer is self-contained)
cross the module boundary this way.
