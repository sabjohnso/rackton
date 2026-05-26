#lang scribble/manual

@title{Rackton}
@author{Samuel B. Johnson}

@bold{Rackton} is a Racket adaptation of the
@hyperlink["https://coalton-lang.github.io/"]{Coalton} statically-typed
functional language.  It embeds a Hindley–Milner core — type inference,
let-polymorphism, algebraic data types, type classes, kinds, GADTs,
existentials, rank-N polymorphism, records, type aliases, sealed
abstract types, algebraic effects, software transactional memory, and
optics — inside Racket, available as a @racket[(rackton …)] form, as a
module language for @racket[(module @#,racketidfont{name} rackton …)],
or as a whole-file @hash-lang[] @racketmodfont{rackton} program.

The documentation is split into three documents, each addressing a
different audience.

@itemlist[
@item{@other-doc['(lib "rackton/scribblings/guide/rackton-guide.scrbl")] —
      narrative introduction.  Start here if you are new to Rackton.}
@item{@other-doc['(lib "rackton/scribblings/reference/rackton-reference.scrbl")] —
      exhaustive API reference.  Every prelude binding, every surface
      form, every type class and instance.  Consult this when you need
      precise signatures or behaviour.}
@item{@other-doc['(lib "rackton/scribblings/developer/rackton-developer.scrbl")] —
      implementation guide.  The compilation pipeline, the inference
      algorithm, codegen, cross-module type information, and the
      theoretical foundations.  For maintainers and contributors.}]
