#lang scribble/manual
@require[scribble/manual
         (for-label rackton)
         "../rackton-eval.rkt"]
@(define ev (make-rackton-eval))

@title[#:tag "pattern-matching"]{Pattern matching}

Pattern matching is the primary way to take a value apart in Rackton.

@section{The basics}

@rackton-example[#:eval ev #:mode 'value]{
(match (Some 5)
  [(None)   "empty"]
  [(Some x) (show x)])
}

Each clause is @racket[[pattern body]].  Patterns are tried in order;
the first to match runs its body.  Pattern variables (lowercase
identifiers) bind in the body's scope.

@section{Pattern syntax}

@tabular[#:sep @hspace[2]
         (list
          (list @racket[_]            "wildcard — matches anything, binds nothing")
          (list @racket[name]         "variable — matches anything, binds name (lowercase)")
          (list @racket[42]           "integer literal")
          (list @racket[#t]           "boolean literal")
          (list @racket["hi"]         "string literal")
          (list @racket[#\A]          "character literal")
          (list @racket[None]         "nullary constructor")
          (list @racket[(Some x)]     "n-ary constructor with sub-patterns")
          (list @racket[(Cons h t)]   "Cons of two patterns")
          (list @racket[(list a b c)] "fixed-length list pattern")
          (list @racket['(1 2 3)]     "quoted literal list pattern"))]

Sub-patterns nest arbitrarily:

@rackton-example[#:eval ev #:mode 'value]{
(match (Cons (Some 0) Nil)
  [(Cons (Some 0) rest) (length rest)]
  [_                    -1])
}

@section[#:tag "list-patterns"]{Quoted and @racket[list] patterns}

Quotation and the @racket[list] form work in pattern position too,
mirroring the @secref["quoted-list-literals"] on the expression side.  A @racket[(list …)] pattern matches a list of
exactly that many elements:

@rackton-example[#:eval ev #:mode 'value]{
(match (list 10 20 30)
  [(list a b c) (+ a (+ b c))]
  [_            0])
}

A trailing rest-binder — a variable or @racket[_] followed by
@litchar{...} — binds the @emph{rest} of the list as a single
@racket[(List _)].  So @racket[(list a b ...)] is exactly
@racket[(Cons a b)]: @racket[a] is the head and @racket[b] the tail.

@rackton-example[#:eval ev #:mode 'value]{
(match (list 1 2 3 4)
  [(list first rest ...) rest]
  [(list)                Nil])
}

The pattern before @litchar{...} must be a variable or @racket[_], and
@litchar{...} must be the final element; both together with @racket[(list)]
cover every list, so the match above is exhaustive.

A quoted list is a fixed pattern that matches the literal data, and a
quasiquoted list may carry @litchar{,}-escaped sub-patterns:

@rackton-example[#:eval ev #:mode 'value]{
(match (list 1 99 3)
  [`(1 ,x 3) x]
  [_         0])
}

@section{Exhaustiveness}

A @racket[match] is checked at compile time and rejected if it omits
any constructor of an ADT scrutinee, omits @racket[#t] or @racket[#f]
on a @racket[Boolean] scrutinee, or lacks a catchall on an
unconstrained scrutinee.

@rackton-example[#:eval ev #:mode 'display]{
;; Compile error: missing case (None)
(match m
  [(Some x) x])
}

To opt out, add a wildcard or variable pattern:

@rackton-example[#:eval ev #:mode 'value]{
(match (Some 9)
  [(Some x) x]
  [_        0])
}

@section{Guards}

A clause may add a Boolean guard that runs after the pattern matches.
Guards see the bound pattern variables:

@rackton-example[#:eval ev #:mode 'value]{
(match 7
  [k #:when (> k 0) "positive"]
  [k #:when (< k 0) "negative"]
  [_                "zero"])
}

@section{Destructuring in @racket[let] bindings}

A @racket[let] (or @racket[let*]) binding may use a pattern on its
left-hand side, so a single match needs no @racket[match]:

@rackton-example[#:eval ev #:mode 'value]{
(let ([(Pair x y) (Pair 3 4)])
  (+ x y))
}

The body sees @racket[x] and @racket[y].  A failure to match raises a
panic — use a pattern binding only when the pattern is irrefutable
(a constructor with exactly one inhabitant, like @racket[Pair] or a
@racket[struct] constructor).  A @racket[let] evaluates every right-hand
side in the surrounding scope, so its pattern bindings are independent;
use @racket[let*] when a later binding must see an earlier one.

@section{Matching in a lambda: @racket[case-lambda]}

@racket[case-lambda] (also spelled @racket[case-λ]) is an anonymous
function that matches on @emph{all} of its arguments at once.  Each
clause begins with a parenthesized list of argument patterns, followed
by a body:

@rackton-example[#:eval ev #:mode 'value]{
(let ([f (case-lambda
           [((Some x) (Some y)) (Some (+ x y))]
           [(_ _)               None])])
  ((f (Some 3)) (Some 4)))
}

The number of patterns in the argument list fixes the arity, and every
clause must share it.  The form above is equivalent to a two-argument
@racket[lambda] over @racket[match]:

@rackton-example[#:eval ev #:mode 'display]{
(lambda (a b)
  (match* (a b)
    [((Some x) (Some y)) (Some (+ x y))]
    [(_ _)               None]))
}

Because the first element of a clause is always the argument list, a
single argument that is itself a constructor pattern needs its own
parentheses:

@rackton-example[#:eval ev #:mode 'value]{
(let ([f (case-λ
           [(None)     0]
           [((Some x)) x])])
  (f (Some 5)))
}

Clauses may carry a @racket[#:when] guard, just as in @racket[match]:

@rackton-example[#:eval ev #:mode 'value]{
(let ([f (case-lambda
           [(n) #:when (> n 0)  1]
           [(n) #:when (< n 0) -1]
           [(_)                 0])])
  (f 42))
}
