#lang scribble/manual
@require[scribble/manual]

@title{The Rackton Reference}
@author{Samuel B. Johnson}

@defmodule[rackton]

This reference describes every form, type, class, and binding that
Rackton makes available to user programs.  It is organized by topic
rather than by chronology: read the chapter that names the feature you
need.  For a narrative introduction, see
@other-doc['(lib "rackton/scribblings/guide/rackton-guide.scrbl")].

For information about how the language is implemented, see
@other-doc['(lib "rackton/scribblings/developer/rackton-developer.scrbl")].

@table-of-contents[]

@include-section["entry-points.scrbl"]
@include-section["syntax-forms.scrbl"]
@include-section["provide-specs.scrbl"]
@include-section["types.scrbl"]
@include-section["classes.scrbl"]
@include-section["values.scrbl"]
@include-section["repl.scrbl"]
@include-section["diagnostics.scrbl"]
