#lang scribble/manual
@require[scribble/manual
         (for-label rackton)
         "../rackton-eval.rkt"]
@(define ev (make-rackton-eval))

@title[#:tag "foreign"]{Foreign function interface}

The @secref["racket-interop"] chapter showed the @racket[racket]
escape: an inline trapdoor into Racket.  This chapter covers the
@emph{named, typed} ways to reach outside Rackton — importing a host
binding with @racket[foreign], binding an external C function with
@racket[foreign-c], and the low-level raw-memory core in
@racketmodname[rackton/foreign/ptr].

Every form here shares one rule: @bold{the declared Rackton type is an
unchecked trust boundary}.  The compiler believes it; it does not (and
cannot) verify that the host value actually has that type.  A wrong
type — or, for C, a wrong signature or library — is a bug the type
checker will not catch, and at the C level it can crash the process.
Wrap each import in one place and keep the rest of your code type-safe.

@section{Importing a host binding: @racket[foreign]}

@racket[foreign] gives a Racket binding a Rackton type so typed code
can call it.  Use it for primitives the prelude does not surface — for
example something from @racketmodname[racket/string] or
@racketmodname[racket/set].

@rackton-example[#:eval ev #:mode 'defs]{
(foreign str-replace (-> String (-> String (-> String String)))
         #:from racket/string #:as string-replace)

(: slashify (-> String String))
(define (slashify s) (str-replace s "." "/"))
}

@racket[#:from] names the Racket module (a collection path, or a
string for a relative file).  Without @racket[#:as] the host binding
has the same name as the Rackton one; @racket[#:as string-replace]
binds Racket's @racket[string-replace] to the Rackton name
@racket[str-replace] (handy when the host name collides with a prelude
name).  The type is curried as usual, but calls must still match the
host function's real arity — partially applying a strict host function
raises at runtime.

@section{Binding a C function: @racket[foreign-c]}

@racket[foreign-c] imports an external C function directly, lowering to
@racket[get-ffi-obj] from @racketmodname[ffi/unsafe].  It is sugar for
a hand-written @racketmodname[ffi/unsafe] shim imported via
@racket[foreign].

@rackton-example[#:eval ev #:mode 'defs]{
(foreign-c c-cbrt (-> Float Float)
           #:lib #f #:symbol "cbrt" #:sig (double -> double))

(foreign-c c-getpid (IO Integer)
           #:lib #f #:symbol "getpid" #:sig (-> int))
}

@racket[#:lib] is the shared library — @racket[#f] for the running
process (libc, and whatever it already links, such as libm) or a
string passed to @racket[ffi-lib].  @racket[#:symbol] is the C symbol
name.  @racket[#:sig] gives the C signature as keywords
(@racketidfont{double}, @racketidfont{int}, @racketidfont{string},
@racketidfont{pointer}, @racketidfont{byte}, @racketidfont{void}) with
one @racket[->] splitting argument types from the result.

Whether the binding is a pure function or an @racket[IO] action is read
from the Rackton type: if the result sits in @racket[IO] it is an
@racket[IO] action (a value, when there are no arguments) — so
@racket[c-cbrt] is a pure @racket[(-> Float Float)] while
@racket[c-getpid] is an @racket[(IO Integer)] you run with
@racket[run-io].  Versioned sonames like @tt{libm.so.6} are awkward to
name with a single @racket[#:lib]; for those, prefer a
@racket[get-ffi-obj] shim imported via @racket[foreign] (see
@racketmodname[rackton/foreign/c], which binds a curated set of libm
functions this way).

@section{Raw pointers and @racket[Storable]}

@racketmodname[rackton/foreign/ptr] is the unsafe @tt{Foreign.Ptr} /
@tt{Foreign.Marshal} core: an opaque @racket[(Ptr a)] (the @racket[a]
is a phantom tag), allocation (@racket[malloc-bytes], @racket[free-ptr]),
and reads/writes.  The prelude's @racket[Storable] class gives
polymorphic @racket[peek] / @racket[poke]; its instances (for
@racket[Integer] and @racket[Float]) live in that module.  @racket[peek]
is return-typed, so the element type comes from the expected result —
usually an @racket[ann].

@rackton-example[#:eval ev #:mode 'io #:run "round-trip"]{
(require rackton/foreign/ptr)

(: round-trip (IO Integer))
(define round-trip
  (do [p <- (malloc-bytes size-of-int)]
      [_ <- (poke p 99)]
      [v <- (ann (peek p) (IO Integer))]
      [_ <- (free-ptr p)]
      (println (string-append "round-trip: " (show v)))
      (pure v)))
}

This layer is @bold{thoroughly unsafe}: there is no bounds checking, no
use-after-free protection, and @racket[malloc-bytes] hands back raw
memory you must @racket[free-ptr] yourself.  A wrong @racket[Storable]
element type or an out-of-range pointer corrupts memory.  Treat it as
you would C.

@section{Reference}

Precise signatures are in the reference: @racket[foreign] and
@racket[foreign-c] under
@other-doc['(lib "rackton/scribblings/reference/rackton-reference.scrbl")]'s
syntax forms, and the @racket[Storable] class under its classes
section.
