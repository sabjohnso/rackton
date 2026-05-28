#lang scribble/manual
@require[scribble/manual
         (for-label rackton)]

@title[#:tag "bibliography"]{Bibliography}

The implementation choices in Rackton are informed by a number of
papers and reference implementations.  This chapter collects them
with brief notes on where each idea lands in the codebase.

@section{Type inference}

@itemlist[

@item{Damas, Luis and Robin Milner.
      @italic{Principal type-schemes for functional programs}.
      POPL '82.

      The original Algorithm W.  Rackton's @filepath{private/infer.rkt}
      is a direct descendant.}

@item{Jones, Mark P.  @italic{Typing Haskell in Haskell}.  Haskell
      Workshop '99.

      The clearest expository implementation of Algorithm W with
      let-generalisation, class constraints, and constraint
      reduction.  The structure of Rackton's inferer borrows heavily
      from this paper's "Thih" reference implementation.}

@item{Peyton Jones, Simon, Dimitrios Vytiniotis, Stephanie
      Weirich, and Mark Shields.  @italic{Practical type inference for
      arbitrary-rank types}.  ICFP '07.

      The rank-N polymorphism story: explicit @racket[All] inside
      parameter types as the price for decidable inference.}]

@section{Type classes and instances}

@itemlist[

@item{Wadler, Philip and Stephen Blott.  @italic{How to make ad-hoc
      polymorphism less ad hoc}.  POPL '89.

      Type classes via dictionary passing.  Rackton uses runtime
      tag dispatch instead — see @secref["class-entailment"] for
      the rationale.}

@item{Jones, Mark P.  @italic{Type classes with functional
      dependencies}.  ESOP '00.

      Fundeps as constraints on instance resolution, plus the
      consistency rule (no two instances may disagree about the
      determined parameter).  Rackton's
      @racket[(#:fundep a -> b)] follows this design.}

@item{Sulzmann, Martin, Manuel M. T. Chakravarty, Simon Peyton
      Jones, and Kevin Donnelly.  @italic{System F with type
      equality coercions}.  TLDI '07.

      The theoretical underpinning of GADTs.  Rackton's per-constructor
      @racket[: (-> field … Result)] signature is surface syntax for the
      same idea, though the implementation is more direct than the full
      coercion calculus.}]

@section{Algebraic effects}

@itemlist[

@item{Plotkin, Gordon and Matija Pretnar.  @italic{Handlers of
      algebraic effects}.  ESOP '09.

      The deep-handler semantics Rackton uses.  Each effect compiles
      to a Racket continuation-prompt-tag; @racket[handle] installs
      the prompt and registers per-operation handlers.}]

@section{Monad transformers and MTL}

@itemlist[

@item{Liang, Sheng, Paul Hudak, and Mark Jones.
      @italic{Monad transformers and modular interpreters}.  POPL '95.

      The MTL design: classes like @racket[MonadState] that abstract
      over the carrier monad, with @racket[lift] threading through
      transformer stacks.}

@item{The @racket[mtl] Haskell package.

      The naming conventions and class structure Rackton's
      @racket[MonadState] / @racket[MonadEnv] / @racket[MonadWriter] /
      @racket[MonadError] follow.}]

@section{Software transactional memory}

@itemlist[

@item{Harris, Tim, Simon Marlow, Simon Peyton Jones, and Maurice
      Herlihy.  @italic{Composable memory transactions}.  PPoPP '05.

      The @racket[STM] interface, @racket[retry], and
      @racket[orElse].  Rackton's STM is a simplified port: an
      equal?-hash log of (read, version) and (write, value),
      committed under a global lock.}]

@section{Optics}

@itemlist[

@item{Pickering, Matthew, Jeremy Gibbons, and Nicolas Wu.
      @italic{Profunctor optics: modular data accessors}.  Programming
      '17.

      The unifying profunctor encoding of @racket[Lens] /
      @racket[Prism] / @racket[Traversal].  Rackton presents the
      concrete operations (@racket[view], @racket[set],
      @racket[preview], etc.) without exposing the profunctor
      machinery — pragmatic, less theoretically elegant.}]

@section{Racket-specific}

@itemlist[

@item{Tobin-Hochstadt, Sam and Matthias Felleisen.  @italic{The
      design and implementation of Typed Scheme}.  POPL '08.

      Typed Racket's design.  Rackton sits in a different design
      space — it elaborates ML-style HM rather than gradually typing
      Racket — but Typed Racket is the existence proof that a typed
      sister language can coexist with @racketmodname[racket/base]
      and is the source of many Scribble documentation conventions
      Rackton follows.}

@item{Felleisen, Matthias, Robert Bruce Findler, Matthew Flatt, and
      Shriram Krishnamurthi.  @italic{How to design programs}.  Second
      edition, 2018.

      The style of writing tests as feature specifications, used
      throughout Rackton's @filepath{tests/} directory.}]

@section{Reference implementations consulted}

@itemlist[

@item{@bold{Coalton.}  @url{https://coalton-lang.github.io/}

      Rackton's namesake.  A Haskell-flavoured statically typed
      functional language embedded in Common Lisp.  Many of
      Rackton's design choices (the prelude shape, the @racket[do]
      syntax, the deriving list) follow Coalton's.}

@item{@bold{GHC.}  @url{https://gitlab.haskell.org/ghc/ghc}

      The reference for class entailment, overlap, coherence, and
      the standard MTL.  Rackton implements a small subset of GHC's
      behaviour; the chapters on entailment and codegen note where
      they diverge.}]
