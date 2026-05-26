#lang scribble/manual
@require[scribble/manual
         (for-label rackton)]

@title{Rackton — Developer Guide}
@author{Samuel B. Johnson}

This document is for developers of Rackton itself.  It describes the
compilation pipeline, the inference algorithm, the entailment and
overlap procedures, the cross-module type-information mechanism, the
codegen strategy, and the theoretical foundations that motivate each
choice.

If you are using Rackton rather than working on it, start with
@other-doc['(lib "rackton/scribblings/guide/rackton-guide.scrbl")] and consult
@other-doc['(lib "rackton/scribblings/reference/rackton-reference.scrbl")] for
precise signatures.

@table-of-contents[]

@include-section["architecture.scrbl"]
@include-section["surface-ast.scrbl"]
@include-section["type-representation.scrbl"]
@include-section["inference.scrbl"]
@include-section["unification.scrbl"]
@include-section["class-entailment.scrbl"]
@include-section["codegen.scrbl"]
@include-section["cross-module.scrbl"]
@include-section["prelude-internals.scrbl"]
@include-section["repl-internals.scrbl"]
@include-section["monomorphization-and-inlining.scrbl"]
@include-section["adding-features.scrbl"]
@include-section["bibliography.scrbl"]
