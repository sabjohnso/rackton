#lang scribble/manual
@require[scribble/manual
         (for-label rackton)]

@title[#:tag "provide-specs"]{@racket[provide] specs}
A Rackton module exports nothing by default — every binding,
constructor, type, class, and class method is module-private unless
explicitly exported with a @racket[(provide …)] form.  Multiple
@racket[provide] forms in the same module are unioned.

The supported spec forms are listed below.  Each may appear directly
inside a @racket[(provide …)] body, and most may be combined with the
others via @racket[except-out].

A binding excluded from the @racket[provide] list is invisible to
importers both at runtime and to the type checker — its scheme is
omitted from the @racketmodfont{rackton-schemes} sidecar.  Instances
are an exception: they always escape, regardless of the
@racket[provide] list, because instance coherence is a module-level
property in the Haskell tradition.

@defidform[#:kind "provide spec" all-defined-out]{

Exports every locally-defined value binding, data constructor, type
constructor, and class.  Imported and prelude names are not
re-exported.

@racketblock[(provide (all-defined-out))]}

@defform[#:literals (all-from-out)
         (all-from-out module-path ...)]{

Re-exports every name imported from each @racket[module-path] — value
bindings, data constructors, type constructors, and classes — without
listing them individually.  Use it to build a re-export chain or an
umbrella module.  Instances escape regardless of @racket[provide], so
they are carried through too.

@racketblock[
(require "tree.rkt")
(provide (all-from-out "tree.rkt"))]}

@defform[#:literals (data-out)
         (data-out T)]{

Exports the @racket[data] type @racket[T] together with every
constructor of @racket[T].

@racketblock[
(data (Tree a) Leaf (Node (Tree a) a (Tree a)))
(provide (data-out Tree))]}

@defform[#:literals (struct-out)
         (struct-out S)]{

Analogue of Racket's @racket[struct-out] for @racket[struct]:
exports the struct type @racket[S], its constructor, and every field
accessor @racketidfont{S}@racketidfont{-}@racket[_fname].

@racketblock[
(struct Point [x : Integer] [y : Integer])
(provide (struct-out Point))]}

@defform[#:literals (protocol-out)
         (protocol-out C)]{

Exports the protocol @racket[C] together with every method declared by
@racket[C].

@racketblock[
(protocol (Shape a) (: area (-> a Float)))
(provide (protocol-out Shape))]}

@defform[#:literals (rename-out)
         (rename-out [old new] ...)]{

Exports each @racket[old] under the external name @racket[new].

@racketblock[
(define (internal-helper x) x)
(provide (rename-out [internal-helper helper]))]}

@defform[#:literals (except-out)
         (except-out spec name ...)]{

Resolves @racket[spec], then drops each @racket[name] from the
resulting export set.

@racketblock[
(provide (except-out (all-defined-out) helper))]}

@section{Bare name spec}

A @racket[provide] body may also contain a bare identifier, which
exports the named value binding, data constructor, type constructor, or
class.  Data constructors exported this way are exported both as a
runtime value and (when their owning type is not @racket[#:abstract])
as a type-checker entry.

@racketblock[
(define (greet name) name)
(data Color Red Green Blue)
(provide greet Color Red Green Blue)]
