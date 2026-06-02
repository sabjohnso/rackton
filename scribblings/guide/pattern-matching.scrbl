#lang scribble/manual
@require[scribble/manual
         (for-label rackton)]

@title[#:tag "pattern-matching"]{Pattern matching}

Pattern matching is the primary way to take a value apart in Rackton.

@section{The basics}

@codeblock|{
(match m
  [(None)   "empty"]
  [(Some x) (show x)])
}|

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
          (list @racket[(Cons h t)]   "Cons of two patterns"))]

Sub-patterns nest arbitrarily:

@codeblock|{
(match xs
  [(Cons (Some 0) rest) (length rest)]
  [_                    -1])
}|

@section{Exhaustiveness}

A @racket[match] is checked at compile time and rejected if it omits
any constructor of an ADT scrutinee, omits @racket[#t] or @racket[#f]
on a @racket[Boolean] scrutinee, or lacks a catchall on an
unconstrained scrutinee.

@codeblock|{
;; Compile error: missing case (None)
(match m
  [(Some x) x])
}|

To opt out, add a wildcard or variable pattern:

@codeblock|{
(match m
  [(Some x) x]
  [_        0])
}|

@section{Guards}

A clause may add a Boolean guard that runs after the pattern matches.
Guards see the bound pattern variables:

@codeblock|{
(match n
  [k #:when (> k 0) "positive"]
  [k #:when (< k 0) "negative"]
  [_                "zero"])
}|

@section{Destructuring in @racket[let] bindings}

A @racket[let] (or @racket[where]) binding may use a pattern on its
left-hand side, so a single match needs no @racket[match]:

@codeblock|{
(let ([(MkPair x y) (MkPair 3 4)])
  (+ x y))   ;; ⇒ 7
}|

The body sees @racket[x] and @racket[y].  A failure to match raises a
panic — use a pattern binding only when the pattern is irrefutable
(a constructor with exactly one inhabitant, like @racket[MkPair] or a
@racket[struct] constructor).  A @racket[let] evaluates every right-hand
side in the surrounding scope, so its pattern bindings are independent;
use @racket[where] when a later binding must see an earlier one.
