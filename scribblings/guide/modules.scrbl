#lang scribble/manual
@require[scribble/manual
         (for-label rackton)
         "../rackton-eval.rkt"]
@(define ev (make-rackton-eval))

@title[#:tag "modules"]{Modules: require, provide, multi-file}

Rackton uses Racket's module system unchanged, but layers type
information on top: an importing module sees not just the runtime
bindings but also their schemes, data constructors, type
constructors, protocols, and instances.

@section{provide controls export}

A Rackton module exports nothing by default.  Every binding,
constructor, type, protocol, and protocol method is module-private unless
listed in a @racket[(provide …)] form.

@rackton-example[#:eval ev #:mode 'display]{
#lang rackton

(provide tree-sum
         tree-depth
         (data-out Tree))   (code:comment "exports Tree and all its constructors")

(data (Tree a) Leaf (Node (Tree a) a (Tree a)))

(: tree-sum (-> (Tree Integer) Integer))
(define (tree-sum t) ...)
}

Supported spec forms include bare identifiers, @racket[(all-defined-out)],
@racket[(data-out T)], @racket[(struct-out S)], @racket[(protocol-out C)],
@racket[(rename-out [old new] …)], and @racket[(except-out spec name …)].
See @secref["provide-specs" #:doc '(lib "rackton/scribblings/reference/rackton-reference.scrbl")]
for the full grammar.

A binding excluded from @racket[provide] is invisible to importers
@italic{both} at runtime and to the type checker — its scheme is
omitted from the @racketmodfont{rackton-schemes} sidecar.

@bold{Instances are an exception:} they always escape regardless of
@racket[provide], because instance coherence is a module-level
property in the Haskell tradition.

@section{require imports types too}

Inside a @racket[(rackton …)] block, @racket[(require "path.rkt")]
imports both the runtime bindings and (if the target is a
@hash-lang[] @racketmodfont{rackton} module) its typing schemes.

@rackton-example[#:eval ev #:mode 'display]{
;; lib.rkt
#lang rackton
(provide tree-sum (data-out Tree))
(data (Tree a) Leaf (Node (Tree a) a (Tree a)))
(: tree-sum (-> (Tree Integer) Integer))
(define (tree-sum t) ...)

;; main.rkt
#lang rackton
(require "lib.rkt")
(: result Integer)
(define result (tree-sum (Node Leaf 1 (Node Leaf 2 Leaf))))
}

The importer's type checker reads @filepath{lib.rkt}'s
@racketmodfont{rackton-schemes} sidecar submodule to recover the
schemes.  Plain Racket modules can still be @racket[require]d, but
their bindings are invisible to the type checker — they're runtime
only.

The require sub-forms @racket[only-in], @racket[rename-in],
@racket[prefix-in], and @racket[except-in] work as in Racket and select
or rename the imported schemes the same way they select or rename the
runtime bindings, so a renamed import is known to the type checker under
its new name:

@racketblock[
(require (rename-in "lib.rkt" [tree-sum total]))
(require (only-in   "lib.rkt" tree-sum))
(require (prefix-in t: "lib.rkt"))
(require (except-in "lib.rkt" tree-sum))]

They nest, so @racket[(prefix-in t: (only-in "lib.rkt" tree-sum))]
selects @racket[tree-sum] and then prefixes it to @racket[t:tree-sum].
Instances are unaffected by these sub-forms: module-level coherence
makes them global, so they are always imported.

@section{Qualified imports}

@racket[(qualified-in prefix module)] imports @racket[module]'s
@emph{term-level} names — values and data constructors — each behind a
colon-separated @racket[prefix].  A value @racket[depth] from the module
is then written @racketidfont{st:depth}, and a constructor @racket[Push]
is written @racketidfont{st:Push} in both expressions and patterns.
This disambiguates imports whose names would otherwise collide, letting
two modules that each export, say, @racket[Cons] and @racket[Nil]
coexist under different prefixes.

@rackton-example[#:eval ev #:mode 'display]{
;; stack.rkt
#lang rackton
(provide (data-out Stack) depth)
(data (Stack a) Empty (Push a (Stack a)))
(: depth (-> (Stack a) Integer))
(define (depth s)
  (match s [Empty 0] [(Push _ rest) (+ 1 (depth rest))]))

;; main.rkt
#lang rackton
(require (qualified-in st "stack.rkt"))
(: s (Stack Integer))
(define s (st:Push 1 (st:Push 2 st:Empty)))
(: n Integer)
(define n (st:depth s))
}

Type constructors and protocols keep their @emph{plain} names — the type
above is written @racket[Stack], not @racketidfont{st:Stack} — so an
imported constructor's result type stays consistent with the type you
write in annotations.  A qualified reference uses exactly one colon: a
@emph{leading} colon (as in @racket[:value]) marks a keyword, not a
qualifier, and a name with a trailing or repeated colon is a syntax
error.

@section[#:tag "qualifying-the-prelude"]{Qualifying the prelude}

The prelude is normally implicit — @racket[Cons], @racket[fmap],
@racket[+], and the rest are in scope in every module with no
@racket[require].  When a module defines its @emph{own} binding that
shadows a prelude name — say a non-empty list with its own @racket[Cons]
constructor — the prelude's version is hidden.  The module
@racketmodname[rackton/prelude] makes the whole prelude importable, so a
qualifier reaches the shadowed names:

@rackton-example[#:eval ev]{
#lang rackton
(require (qualified-in p rackton/prelude))

;; this module's own Cons — a Nonempty-List constructor
(data (Nonempty-List a) (Sole a) (Cons a (Nonempty-List a)))

(: ne (Nonempty-List Integer))
(define ne (Cons 1 (Sole 2)))          ;; our Cons

(: pl (List Integer))
(define pl (p:Cons 10 (p:Cons 20 p:Nil)))   ;; the prelude's Cons / Nil

(: total Integer)
(define total (p:length pl))
}

Everything the prelude exports is available under the prefix —
@racketidfont{p:Cons}, @racketidfont{p:Nil}, @racketidfont{p:fmap},
@racketidfont{p:filter}, and so on — with the same types they have
unprefixed.  @racket[prefix-in] works too (@racket[(prefix-in p
rackton/prelude)] gives @racketidfont{pCons}); @racket[qualified-in]'s
colon form is the idiomatic choice.  Type constructors, protocols, and
instances are unaffected, exactly as for any qualified import.

@section{Cross-file protocols and instances}

@hash-lang[] @racketmodfont{rackton} modules also export their protocol
declarations and instance registrations.  An importing module sees
both, so the type checker can discharge constraints against imported
instances without local redeclaration:

@rackton-example[#:eval ev #:mode 'display]{
;; lib.rkt
#lang rackton
(provide (protocol-out Container) (data-out Stack))

(protocol (Container (f :: (-> * *)))
  (: empty? (-> (f a) Boolean)))

(data (Stack a) Empty (Push a (Stack a)))

(instance (Container Stack)
  (define (empty? s) (match s [(Empty) #t] [(Push _ _) #f])))

;; main.rkt
#lang rackton
(require "lib.rkt")
(define result (empty? (Push 1 Empty)))
}

Protocol default-method bodies still bind in the @italic{defining}
module's lexical scope; when an importing module uses a default, the
identifiers in the default body are re-anchored to the instance site
so they resolve via that module's imports.

@section{Multi-block embedded code}

A single Racket module may contain any number of @racket[(rackton …)]
invocations.  Each block elaborates independently against the
prelude:

@rackton-example[#:eval ev #:mode 'display]{
#lang racket/base
(require rackton)

(rackton (define x 1))
(rackton (define y (+ 1 2)))   (code:comment "no x in scope for type checking")

;; both x and y are visible at runtime
(printf "~a ~a\n" x y)
}

Cross-block imports go via Racket's normal binding system at runtime
only.  At elaboration time, each @racket[(rackton …)] block sees only
the prelude — so a reference from a later block to a binding defined
in an earlier block fails with @racket[infer: unbound identifier].
Redeclare the binding's @racket[:] signature at the top of the later
block to make inference see it.
