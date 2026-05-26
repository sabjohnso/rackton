#lang scribble/manual
@require[scribble/manual
         (for-label rackton)]

@title[#:tag "surface-ast"]{The surface AST}

@filepath{private/surface.rkt} converts each user form into the
typed-core source AST.  Every AST node carries the originating
@racket[stx] (syntax object) so the inferer and codegen can produce
sourcemap-aware errors.

@section{Node taxonomy}

The AST has three top-level categories:

@itemlist[
@item{@bold{Expressions} — @racket[e:literal], @racket[e:var],
      @racket[e:lam], @racket[e:app], @racket[e:let], @racket[e:if],
      @racket[e:ann], @racket[e:match], @racket[e:letrec],
      @racket[e:update], @racket[e:escape], @racket[e:handle].}
@item{@bold{Top-level forms} — @racket[top:def], @racket[top:dec]
      (the @racket[:] signature), @racket[top:data], @racket[top:class],
      @racket[top:instance], @racket[top:alias],
      @racket[top:struct-fields], @racket[top:effect],
      @racket[top:require], @racket[top:provide].}
@item{@bold{Patterns} — @racket[p:wild], @racket[p:var],
      @racket[p:lit], @racket[p:ctor].}]

Types and constraints have their own AST (@racket[ty:var], @racket[ty:con],
@racket[ty:app], @racket[ty:forall], @racket[ty:qual], @racket[constraint]),
documented in @secref["type-representation"].  Kinds (@racket[k:star],
@racket[k:arr]) are similar.

@section{Parsing entry points}

Three mutually exclusive parsers cover the three categories:

@itemlist[
@item{@racket[parse-top] — top-level forms.  Dispatches by
      head-symbol.}
@item{@racket[parse-expr] — expressions.  Recognises literals
      directly; dispatches by head-symbol for keyword forms; otherwise
      treats the form as a function application.}
@item{@racket[parse-pattern] — patterns.  Lowercase initial → variable,
      uppercase → constructor, literals → literal patterns.}]

Each is implemented with @racket[syntax-parse] for clarity.  Failure
messages are kept terse; the inferer produces richer error messages
once it has type context.

@section{The lexical rule}

A single rule, applied consistently at parse time, distinguishes
variables from constructors:

@nested[#:style 'inset]{
An identifier whose first character is a lowercase letter is a
@italic{variable} in pattern position and a @italic{type variable} in
type position.  Every other identifier is a @italic{constructor}.}

This is why @racket[->] (an operator-shaped identifier) is a type
constructor — its first character is not a lowercase letter.  The
rule keeps the surface free of declarations like "let a be a
variable" or "let Foo be a constructor"; the casing is the
declaration.

@section{Where to add a new form}

Adding a new surface form starts in
@filepath{private/surface.rkt}.  The typical flow:

@itemlist[#:style 'ordered
@item{Add a struct for the AST node, near related forms.}
@item{Extend the appropriate parser (@racket[parse-expr],
      @racket[parse-top], or @racket[parse-pattern]) with a new
      @racket[syntax-parse] clause.}
@item{Add the form's head symbol to the @racket[#:datum-literals]
      list at the top of @racket[parse-expr] or its sibling.}
@item{Extend @filepath{private/infer.rkt} with the inference rule
      for the new node.}
@item{Extend @filepath{private/codegen.rkt} with the lowering rule.}
@item{Add a feature-named test file in @filepath{tests/}.}]

See @secref["adding-features"] for the full workflow.
