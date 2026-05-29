#lang scribble/manual
@require[scribble/manual
         (for-label rackton)]

@title{The Rackton Guide}
@author{Samuel B. Johnson}

This guide is a narrative introduction to Rackton, intended to be read
linearly.  Early chapters cover the surface syntax and the type system
in their simplest form; later chapters deepen each topic with the
features that build on top.  Every form mentioned in prose is fully
specified in
@other-doc['(lib "rackton/scribblings/reference/rackton-reference.scrbl")] — the
guide links there from each topic so you can drop into precise
signatures when needed.

@table-of-contents[]

@include-section["quickstart.scrbl"]
@include-section["interfaces.scrbl"]
@include-section["values-and-types.scrbl"]
@include-section["pattern-matching.scrbl"]
@include-section["adts-and-records.scrbl"]
@include-section["classes.scrbl"]
@include-section["higher-kinded.scrbl"]
@include-section["polymorphism.scrbl"]
@include-section["advanced-types.scrbl"]
@include-section["do-and-monads.scrbl"]
@include-section["io-and-mutation.scrbl"]
@include-section["effects.scrbl"]
@include-section["concurrency.scrbl"]
@include-section["optics.scrbl"]
@include-section["modules.scrbl"]
@include-section["racket-interop.scrbl"]
@include-section["testing.scrbl"]
@include-section["examples.scrbl"]
