#lang scribble/manual
@require[scribble/manual
         (for-label rackton)
         "../rackton-eval.rkt"]
@(define ev (make-rackton-eval))

@title[#:tag "modules"]{Modules: require, provide, multi-file}

Rackton uses Racket's module system unchanged, but layers type
information on top: an importing module sees not just the runtime
bindings but also their schemes, data constructors, type
constructors, classes, and instances.

@section{provide controls export}

A Rackton module exports nothing by default.  Every binding,
constructor, type, class, and class method is module-private unless
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

@section{Cross-file classes and instances}

@hash-lang[] @racketmodfont{rackton} modules also export their class
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

Class default-method bodies still bind in the @italic{defining}
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
