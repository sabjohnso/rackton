#lang scribble/manual
@require[scribble/manual
         (for-label rackton)]

@title[#:tag "interfaces"]{The three interfaces}

Every Rackton form ultimately reaches the same compilation pipeline:
parse → infer → codegen → splice.  The three @italic{interfaces} differ
only in how the user's source is delivered to that pipeline.

@section{@hash-lang[] @racketmodfont{rackton}}

The canonical interface.  An entire file is Rackton; the file's
reader wraps every form into one elaborator invocation.

@codeblock|{
#lang rackton

(provide fact)

(: fact (-> Integer Integer))
(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))
}|

A @hash-lang[] @racketmodfont{rackton} module also emits a
@racketmodfont{rackton-schemes} sidecar submodule so other Rackton
files importing this one can recover its typing environment.  Run
directly with @exec{racket fact.rkt}, or @racket[(require "fact.rkt")]
from another module.

@section{Embedded @racket[(rackton …)]}

Use this when most of a module is Racket but a few definitions want
type-checked Rackton:

@codeblock|{
#lang racket/base
(require rackton)

(rackton
  (: from-maybe (-> a (-> (Maybe a) a)))
  (define (from-maybe d m)
    (match m [(None) d] [(Some x) x])))

;; from-maybe is now a normal Racket procedure here.
(from-maybe 0 (Some 7))
}|

Multiple @racket[(rackton …)] blocks may coexist in one Racket module.
Each elaborates independently against the prelude.  Bindings from one
block are visible at runtime to a later block, but their typing
information does not propagate — declare a fresh @racket[:] signature
inside each block when you need cross-block type-checking.

@section{The @racket[(module @#,racketidfont{name} rackton …)] form}

The bridge interface.  A submodule whose language is @racket[rackton]
gets the same treatment as a @hash-lang[] @racketmodfont{rackton}
file:

@codeblock|{
#lang racket/base

(module pure rackton
  (provide step)
  (: step (-> Integer Integer))
  (define (step n) (+ n 1)))

(require 'pure)
(step 41)   ;; ⇒ 42
}|

This is convenient when an existing Racket project has a small chunk
that benefits from type-checking but you don't want to split it into
its own file.

@section{Picking an interface}

@itemlist[
@item{Greenfield Rackton code: @hash-lang[] @racketmodfont{rackton}.}
@item{Adding types to a small block in a Racket module: embedded
      @racket[(rackton …)].}
@item{Adding a fully-typed submodule to a Racket module:
      @racket[(module … rackton …)].}]

All three reach the same elaborator and produce the same runtime
representation.
