#lang scribble/manual
@require[scribble/manual]

@title[#:tag "tools"]{Tools}

@section{The LSP server}

@defmodule[rackton/lsp]

Rackton ships a Language Server Protocol server for @hash-lang[]
@racketmodfont{rackton} files:

@commandline{racket -l rackton/lsp}

It speaks LSP over stdio and provides:

@itemlist[
 @item{@bold{diagnostics} — the module is re-analyzed (parse +
       inference, nothing executed) on open and on every change, so
       type errors appear as you edit, with positions.  A form that
       does not parse — the normal mid-edit state — does not blind
       the analysis to the rest of the file.}
 @item{@bold{hover} — the type scheme of the name at point (or its
       kind, for types and classes).}
 @item{@bold{completion} — session names, imported names, prelude
       names, and the surface keywords; while the buffer is broken
       mid-edit, candidates fall back to the last parse's definitions
       plus the prelude.}
 @item{@bold{go-to-definition} — within the file from the analysis;
       for imported names through the required module's
       @racketidfont{rackton-defs} sidecar table.}
 @item{@bold{document symbols} — every top-level definition with its
       kind.}]

For Emacs with eglot:

@verbatim|{
(add-to-list 'eglot-server-programs
             '(rackton-mode . ("racket" "-l" "rackton/lsp")))
}|

(substitute the major mode your @hash-lang[] @racketmodfont{rackton}
buffers use, e.g. @racketidfont{racket-mode}).

Limitations (v1): whole-file analysis stops at the first type error,
so one diagnostic is reported per analysis; columns are computed in
characters (identical to LSP's UTF-16 units for all of the Basic
Multilingual Plane); references, rename, and formatting are not
implemented.

@section{Signature search from the shell}

@defmodule[rackton/search]

The Hoogle-style queries of @litchar{,search} / @litchar{,returns} /
@litchar{,accepts} (see @secref["repl"]) over the installed standard
library, with each match's defining module and line:

@commandline{racket -l rackton/search -- "(-> (List a) Integer)"}
@commandline{racket -l rackton/search -- --returns "(List Integer)"}
@commandline{racket -l rackton/search -- --accepts "Integer"}
@commandline{racket -l rackton/search -- --name "stream"}

Index search runs without a typing environment, so the
constraint-satisfiability filter the REPL applies is off here — a
constrained match is listed even when no instance is in scope.
