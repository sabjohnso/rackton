#lang scribble/manual
@require[@for-label[rackton
                    (except-in racket/base
                               compose + - * < > <= >=
                               not and or length foldr filter
                               substring string-length string-append
                               abs min max read-line println print
                               reverse append sort file-exists?
                               sqrt
                               random getenv path->string
                               delete-file make-directory directory-list
                               current-seconds
                               modulo quotient
                               number->string string->number
                               char-upcase char-downcase
                               char-alphabetic? char-numeric? char-whitespace?
                               char->integer integer->char
                               string-ref string->list
                               bytes-length bytes-ref bytes-append
                               bytes->list list->bytes make-bytes
                               bytes->string/utf-8 string->bytes/utf-8
                               string
                               void when unless
                               exp log sin cos tan
                               numerator denominator
                               real-part imag-part magnitude)]]

@title{Rackton}
@author{sbj}

@defmodule[rackton]

@bold{Rackton} is a Racket adaptation of the Coalton statically-typed functional
language.  It embeds a small Hindley–Milner core — type inference, let-polymorphism,
algebraic data types, and pattern matching — inside Racket, either as an
@racket[(rackton ...)] form inside an ordinary module or as a whole-file
@hash-lang[] @racketmodfont{rackton} program.

This documentation describes the @bold{Phase 1 + 2 + 3 + 4 + 5 + 6 + 7
+ 8 + 9 + 10 + 11 + 12 + 13 + 14 + 15 + 16 + 17 + 18 + 19 + 20 + 21 + 22 + 23 + 24 + 25 + 26 + 27 + 28 + 29 + 30}
subset.  Phase 30 extends Phase 29's body-rewriting from top-level
free functions to instance method bodies, so user code can define
@emph{lifted instances}: an instance whose body uses a polymorphic
class method bound by the instance's qualifying context.  This is
the prerequisite for Phase 31's mtl-style classes.
Phase 29 closes the long-standing Phase 24 limitation —
user code can now write its own needs-dict function bodies (e.g.
@racket[(define (my-concat xs) (foldr <> mempty xs))]); the
elaborator tracks the skolems introduced by the declared qualifying
context and redirects polymorphic return-typed-method references in
the body to locally-bound dict-arg parameters.
Phase 28 added the @racket[Char] and @racket[Bytes]
primitives — single Unicode codepoints and binary blobs — with
literal syntax (@code{#\A} / @code{#"hello"}), conversions in both
directions to @racket[String], and basic per-element accessors.
Phase 27 extended the elaborator's dict-passing to
positional class-method calls — when the dispatch arg's inferred type
identifies a needs-dict instance, the elaborator routes the call to a
per-instance impl named @tt{$method:TCon} with the resolved dict args
prepended.  That unblocks @racket[ExceptT] (whose @racket[>>=] Err
branch needs inner @racket[pure]) and any future class method on a
needs-dict instance.  Phase 26 added @racket[WriterT w m] — an
accumulating @racket[Monoid] log over an inner monad — and surfaced an
honest limit on the then-current class-method dispatch (@racket[ExceptT]
was deferred and lands now in Phase 27).
Phase 25 added the @racket[State] and @racket[Env] monads
plus their transformers @racket[StateT] and @racket[EnvT] over an
inner monad @racket[m].  The transformer instances required extending
the elaborator's dict-passing once more — at each return-typed-method
resolution it now also walks the matching instance's qualifying
context, so the inner monad's @racket[pure] flows in alongside the
outer.
Phase 24 extended Phase 20's dict-passing to free functions
(not just class methods) so @racket[mconcat] can ship — at every
call site the elaborator inserts the resolved @racket[mempty] impl.
Phase 23 added @racket[define-newtype] and the canonical
@racket[Sum] / @racket[Product] newtype-Monoids over @racket[Integer].
Phase 22 introduced a real CLI example (a small todo
manager under @racket[examples/todo.rkt]) and filled the prelude
gaps it surfaced: @racket[cond], @racket[string-prefix?],
@racket[string-split], @racket[string-join].  Phase 21 added three
surface-ergonomic affordances: pattern
guards in @racket[match] clauses, @racket[match-let] for inline
destructuring, and @racket[where] for sequential local bindings.
Phase 20 added @racket[Traversable] (with @racket[traverse]
for @racket[Maybe] and @racket[List]) using dict-passing for the
inner @racket[pure] call — the elaborator inserts the resolved
@racket[pure]-impl as a leading argument at each call site.  Phase
19 added @racket[Semigroup] (@racket[<>]) and @racket[Monoid]
(@racket[mempty]) — the latter is the second customer for Phase 18's
return-typed dispatch and confirms the mechanism handles 0-arity
class members as well as functions.  Phase 18 added return-typed
class-method dispatch, so @racket[pure] is now a real class method
on @racket[Applicative].  Phase 17 turned
multi-argument functions into auto-currying ones, so partial
application Just Works.  Phase 16 inserted @racket[Applicative]
between @racket[Functor] and @racket[Monad] and added
@racket[Bifunctor] and @racket[Foldable].  Phase 15 added a
system surface: @racket[random-integer], @racket[random-float],
@racket[current-time-seconds], @racket[getenv], @racket[argv],
@racket[list-directory], @racket[make-directory], and
@racket[delete-file] — all in @racket[IO].  Rackton can now write
CLI tools that interact with their environment.

@section{Two surfaces, one elaborator}

Every Rackton form goes through the same pipeline: surface parser → type
checker (Algorithm W with skolemization for declared signatures) → code
generator.  The two surfaces only differ in how the user's source is reached.

@subsection{Embedded form}

@codeblock|{
#lang racket/base
(require rackton)

(rackton
  (define-data (Maybe a) None (Some a))

  (: from-maybe (-> a (-> (Maybe a) a)))
  (define (from-maybe d m)
    (match m
      [(None)   d]
      [(Some x) x])))
}|

@subsection{@hash-lang[] @racketmodfont{rackton}}

@codeblock|{
#lang rackton

(define (fact n)
  (if (= n 0) 1 (* n (fact (- n 1)))))
}|

A @racketmodfont{#lang rackton} file is read into a single
@racket[(rackton ...)] invocation and auto-provides every top-level
definition.

@section{Lexical conventions}

@itemlist[
 @item{Identifiers that start with a lowercase letter are
       @bold{type variables} (in type positions) or @bold{pattern variables}
       (in pattern positions).}
 @item{Every other identifier is a @bold{type constructor} or
       @bold{data constructor} depending on position.  This includes names
       starting with an uppercase letter, but also operator-style names like
       @racket[->].}
 @item{The underscore @racket[_] is the wildcard pattern.}]

@section{Supported surface}

@subsection{Top-level forms}

@itemlist[
 @item{@racket[(: name type)] — declare a polymorphic or monomorphic type
       signature.  Free type variables in @racket[type] are implicitly
       universally quantified, or an explicit @racket[(All (a ...) type)]
       form may be used.}
 @item{@racket[(define name expr)] or @racket[(define (name p ...) body)]
       — bind a value.  Top-level @racket[define] is recursive.  When a
       matching @racket[:] declaration is in scope, the declared type is
       skolem-checked against the body.}
 @item{@racket[(define-data (Name a ...) Ctor-spec ...)] — declare an
       algebraic data type.  Each @racket[Ctor-spec] is either a bare
       constructor name @racket[Foo] (nullary) or
       @racket[(Foo type ...)] (n-ary).}]

@subsection{Expressions}

@racket[lambda] / @racket[λ], application, @racket[let] (parallel,
let-polymorphic), @racket[if], @racket[(ann expr type)] type ascription,
@racket[match] (with @racket[[pattern body]] clauses), integer / boolean /
string literals, and variables.

@subsection{Patterns}

@itemlist[
 @item{@racket[_] — wildcard.}
 @item{lowercase identifier — variable binding.}
 @item{numeric / boolean / string — literal match.}
 @item{@racket[Ctor] — nullary constructor pattern.}
 @item{@racket[(Ctor sub-pat ...)] — n-ary constructor pattern.}]

@section{Built-in identifiers}

Phase 1 ships a small monomorphic prelude:

@itemlist[
 @item{Arithmetic: @racket[+], @racket[-], @racket[*] on @racket[Integer].}
 @item{Comparison: @racket[=], @racket[<], @racket[>], @racket[<=],
       @racket[>=] on @racket[Integer] returning @racket[Boolean].}]

These are monomorphic because Rackton has no @racket[Num] type class yet;
that lands with type-class support in a later phase.

@section{Type errors}

Type errors are raised as @racket[exn:fail:syntax] at compile time, with the
offending form's source location.  Ill-typed Rackton code never reaches the
generated Racket runtime.

@section{Type classes (Phase 2)}

@subsection{Declaring a class}

@codeblock|{
(define-class (Eq a)
  (: eq  (-> a (-> a Boolean)))
  (: neq (-> a (-> a Boolean)))
  (define (neq x y) (if (eq x y) #f #t)))
}|

A class declaration introduces one or more @italic{method signatures}
(@racket[(: name type)]) and zero or more @italic{default implementations}
(@racket[(define …)]).  Each method is added to the value environment with
a qualified scheme @racket[(All (a) ((C a) => τ))], so a polymorphic use of
the method automatically carries the class constraint.

A class can declare superclass constraints in front of its head:

@codeblock|{
(define-class ((Eq a) => (Ord a))
  (: lt (-> a (-> a Boolean)))
  (: gt (-> a (-> a Boolean)))
  (define (gt x y) (lt y x)))
}|

Superclass closure is followed during entailment: any program with an
@racket[Ord] constraint on @racket[a] automatically discharges
@racket[Eq] constraints on the same type.

@subsection{Declaring an instance}

@codeblock|{
(define-instance (Eq Integer)
  (define (eq x y) (= x y)))

(define-instance ((Eq a) => (Eq (Maybe a)))
  (define (eq x y)
    (match x
      [(None)   (match y [(None) #t] [(Some _) #f])]
      [(Some u) (match y [(None) #f] [(Some v) (eq u v)])])))
}|

The head of an instance can include a context (@racket[((Eq a) => …)])
that becomes a hypothesis when type-checking the body — and at runtime,
the recursive method call dispatches on the inner value's tag rather than
requiring an explicit dictionary parameter.

If the instance omits a method, the class's default is used.

@subsection{Constrained polymorphic functions}

A function that uses a class method inherits its constraint:

@codeblock|{
(: contains? ((Eq a) => (-> a (-> (Maybe a) Boolean))))
(define (contains? target m)
  (match m
    [(None)   #f]
    [(Some x) (eq x target)]))
}|

The inferred scheme is
@racket[(All (a) ((Eq a) => (-> a (-> (Maybe a) Boolean))))].  Phase 2
discharges class constraints by dispatching at the runtime call site on
the type tag of the first argument, so the constraint is fully erased
from the runtime calling convention — no explicit dictionary is threaded
through user code.

@section{Host-language escape (Phase 3)}

@codeblock|{
(: greet (-> String String))
(define (greet name)
  (racket String (name)
    (string-append "hello " name)))
}|

@racket[(racket τ (var ...) body)] drops into raw Racket and returns a
value typed as @racket[τ].  The named Rackton bindings are guaranteed to
be in scope inside @racket[body], which is spliced verbatim at codegen
time.  The escape is the only way for Rackton code to reach Racket's
standard library (string-manipulation, I/O, …).

@section{Built-in prelude (Phase 3)}

Every Rackton module — both @racket[(rackton ...)] forms and
@hash-lang[] @racketmodfont{rackton} files — has the following bindings
available without any local declaration:

@itemlist[
 @item{@bold{Classes}: @racket[Eq] (with @racket[==], @racket[/=]),
       @racket[Ord] (with @racket[<], @racket[>], @racket[<=],
       @racket[>=], superclass @racket[Eq]),
       @racket[Num] (with @racket[+], @racket[-], @racket[*]),
       @racket[Show] (with @racket[show]).}
 @item{@bold{Instances}: @racket[Num], @racket[Eq], @racket[Ord],
       @racket[Show] over @racket[Integer]; @racket[Eq], @racket[Show]
       over @racket[Boolean] and @racket[String].}
 @item{@bold{ADTs}: @racket[Maybe] (@racket[None], @racket[Some]),
       @racket[List] (@racket[Nil], @racket[Cons]),
       @racket[Pair] (@racket[MkPair]),
       @racket[Result] (@racket[Ok], @racket[Err]),
       @racket[Unit] (@racket[MkUnit]).}
 @item{@bold{Combinators}: @racket[id], @racket[const].}]

@section{Exhaustive @racket[match] (Phase 3)}

A @racket[match] is checked at compile time and rejected if it omits a
constructor of an ADT scrutinee, omits @racket[#t] or @racket[#f] on a
@racket[Boolean] scrutinee, or lacks a catchall on an unconstrained
scrutinee.  Add a wildcard (@racket[_]) or variable pattern to opt out.

@section{Higher-kinded type classes (Phase 4)}

A class parameter can be declared with an explicit kind to permit
type-constructor abstraction:

@codeblock|{
(define-class (Functor (f :: (-> * *)))
  (: fmap (-> (-> a b) (-> (f a) (f b)))))

(define-class ((Functor m) => (Monad (m :: (-> * *))))
  (: >>= (-> (m a) (-> (-> a (m b)) (m b)))))
}|

Kinds are written as @racket[*] for ordinary types or @racket[(-> k1 k2)]
for type-constructor kinds.  Class parameters without a @racket[::]
annotation default to @racket[*].

Dispatch for higher-kinded class methods uses the position of the first
argument whose type mentions a class parameter.  For @racket[fmap], that
is the second argument (the container); for @racket[>>=], the first.
This is computed automatically at class definition.

@subsection{Built-in higher-kinded instances}

@itemlist[
 @item{@racket[Functor]: @racket[Maybe], @racket[List], @racket[Result e].}
 @item{@racket[Monad]:   @racket[Maybe], @racket[Result e].}]

@section{Multi-file (Phase 4)}

A @hash-lang[] @racketmodfont{rackton} module emits a sidecar
@racketmodfont{rackton-schemes} submodule containing the type schemes
of all its bindings, data constructors, and type constructors as data.
A @racket[(require "file.rkt")] form inside a @racket[(rackton …)] block
loads that submodule at compile time and extends the typing environment
with the imported schemes, so the importing module can type-check uses
of the imported bindings without redeclaring their types.

@codeblock|{
;; lib.rkt
#lang rackton
(define-data (Tree a) Leaf (Node (Tree a) a (Tree a)))
(: tree-sum (-> (Tree Integer) Integer))
(define (tree-sum t)
  (match t [(Leaf) 0]
           [(Node l x r) (+ x (+ (tree-sum l) (tree-sum r)))]))

;; main.rkt
#lang rackton
(require "lib.rkt")
(: result Integer)
(define result (tree-sum (Node Leaf 1 (Node Leaf 2 Leaf))))
}|

Classes and instances do not yet travel across module boundaries; users
must redeclare those if needed.  Plain Racket modules (no
@racketmodfont{rackton-schemes} submodule) can still be required, but
their bindings will be invisible to the type checker.

@section{do-notation (Phase 5)}

Any class that provides @racket[>>=] (i.e. any @racket[Monad]) can be
sequenced with @racket[do]:

@codeblock|{
(do [x <- (Some 3)]
    [y <- (Some 4)]
  (Some (+ x y)))
;; ⇒ (Some 7)
}|

Each @racket[[var <- expr]] clause desugars to a nested @racket[>>=]
call.  The trailing @racket[body] is the final computation; its type
must be a monad of the same shape.  Short-circuiting (e.g.
@racket[None] in a Maybe chain) propagates per the underlying
@racket[>>=] instance.

@section{deriving (Phase 5)}

@racket[#:deriving Eq Show] at the end of a @racket[define-data] form
synthesises the matching instances:

@codeblock|{
(define-data (Tree a)
  Leaf
  (Node (Tree a) a (Tree a))
  #:deriving Eq Show)
}|

Derived @racket[Eq] compares constructor identity then recursively
checks corresponding fields; derived @racket[Show] prints
@racket[Ctor] for nullary constructors and @racket[(Ctor arg1 arg2)]
for n-ary ones, recursing on fields via @racket[show].  For
parameterised types, the derived instance adds a corresponding
context (so deriving @racket[Eq] on @racket[(Tree a)] gives
@racket[(Eq a) => (Eq (Tree a))]).

@section{Cross-file classes and instances (Phase 5)}

A @hash-lang[] @racketmodfont{rackton} module now also exports the
class and instance information it introduces.  An importing module
sees both the class declarations and the registered instances, so the
type checker can discharge constraints against imported instances
without any local redeclaration.

@codeblock|{
;; lib.rkt
#lang rackton
(define-class (Container (f :: (-> * *)))
  (: empty? (-> (f a) Boolean)))
(define-data (Stack a) Empty (Push a (Stack a)))
(define-instance (Container Stack)
  (define (empty? s) (match s [(Empty) #t] [(Push _ _) #f])))

;; main.rkt
#lang rackton
(require "lib.rkt")
(define result (empty? (Push 1 Empty)))
}|

Class default-method bodies still bind in the @italic{defining}
module's lexical scope; when an importing module uses a default, the
identifiers in the default body are re-anchored to the instance site
so they resolve via that module's imports.

@section{Small stdlib (Phase 5)}

@itemlist[
 @item{Boolean: @racket[not], @racket[and], @racket[or].}
 @item{List: @racket[length], @racket[foldr], @racket[filter] (operating
       on the prelude @racket[List] ADT).}]

@section{Records (Phase 6)}

@racket[define-struct] introduces a single-constructor data type with
typed named fields and auto-generated accessors:

@codeblock|{
(define-struct Point
  [x : Integer]
  [y : Integer])

(define p (Point 3 4))
(define px (Point-x p))   ;; 3
(define py (Point-y p))   ;; 4
}|

The parameterised form @racket[(define-struct (Box a) [v : a] [tag : String])]
generates the same accessors with the appropriate polymorphic schemes.

@section{Multi-parameter classes (Phase 6)}

A class declaration may carry more than one type parameter:

@codeblock|{
(define-class (Convertible a b)
  (: convert (-> a b)))

(define-instance (Convertible Integer String)
  (define (convert n) (show n)))

(define-instance (Convertible Boolean String)
  (define (convert b) (if b "yes" "no")))
}|

Runtime dispatch still uses the first argument whose type mentions a
class parameter — for @racket[convert] this is its single argument.
The non-dispatching parameters are resolved at compile time only; an
ambiguous call site may need a @racket[(ann e τ)] ascription to fix
the result type.  This is enough for the common "input determines
output" pattern but does not implement functional dependencies in
general.

@section{Deriving Ord (Phase 6)}

@racket[#:deriving Ord] synthesises a strictly-less-than (@racket[<])
method that:

@itemlist[
 @item{Compares the constructor-declaration-order index first — values
       built with an earlier constructor are less than values built
       with a later one.}
 @item{Falls back to lexicographic comparison on fields when both
       sides share a constructor.}]

Since Ord superclasses Eq, the synthesiser auto-derives Eq as well, so
@racket[#:deriving Ord] is enough to use both.

@section{String and numeric stdlib (Phase 7)}

The prelude now ships:

@itemlist[
 @item{@bold{Strings}: @racket[string-length], @racket[string-append]
       (binary), @racket[substring] (start, end exclusive).}
 @item{@bold{Numerics}: @racket[mod], @racket[div] (integer division),
       @racket[abs], @racket[min], @racket[max],
       @racket[integer->string], @racket[string->integer] (returns a
       @racket[(Maybe Integer)]).}]

All are typed in the prelude's own elaboration step and implemented as
thin wrappers over the corresponding Racket primitives via the
host-language escape.

@section{IO monad (Phase 7)}

@codeblock|{
(: greet (-> String (IO Unit)))
(define (greet name)
  (println (string-append "hello, " name)))

(: greet-both (-> String (-> String (IO Unit))))
(define (greet-both a b)
  (do [_ <- (greet a)]
      [_ <- (greet b)]
    (pure-io MkUnit)))
}|

@racket[IO] is declared with no public constructors, so values can
only be built by primitives.  Sequencing uses @racket[do] /
@racket[>>=] just like any other @racket[Monad].

Primitives shipped:

@itemlist[
 @item{@racket[print]      — @racket[(-> String (IO Unit))]}
 @item{@racket[println]    — @racket[(-> String (IO Unit))]}
 @item{@racket[read-line]  — @racket[(IO String)]}
 @item{@racket[pure-io]    — @racket[(-> a (IO a))]}
 @item{@racket[run-io]     — @racket[(-> (IO a) a)] — executes the action.}]

A typical @hash-lang[] @racketmodfont{rackton} program binds its
top-level @racket[main] to an @racket[IO] action and executes it
explicitly:

@codeblock|{
#lang rackton
(define main (println "Hello, world!"))
(define _    (run-io main))
}|

@section{Mutable refs and file I/O (Phase 8)}

A @racket[(Ref a)] is an opaque mutable cell.  All operations are IO
actions:

@itemlist[
 @item{@racket[make-ref]  — @racket[(-> a (IO (Ref a)))]}
 @item{@racket[read-ref]  — @racket[(-> (Ref a) (IO a))]}
 @item{@racket[write-ref] — @racket[(-> (Ref a) (-> a (IO Unit)))]}]

@codeblock|{
(: counter-up (-> (Ref Integer) (IO Integer)))
(define (counter-up r)
  (do [n <- (read-ref r)]
      [_ <- (write-ref r (+ n 1))]
    (read-ref r)))
}|

File I/O is similarly IO-typed:

@itemlist[
 @item{@racket[read-file]    — @racket[(-> String (IO String))]}
 @item{@racket[write-file]   — @racket[(-> String (-> String (IO Unit)))]}
 @item{@racket[file-exists?] — @racket[(-> String (IO Boolean))]}]

@section{List and pair stdlib growth (Phase 8)}

@itemlist[
 @item{@bold{List}: @racket[reverse], @racket[append] (binary),
       @racket[zip], @racket[take], @racket[drop], @racket[find]
       (returns @racket[(Maybe a)]), @racket[sort] (insertion sort
       requiring @racket[Ord]).}
 @item{@bold{Pair}: @racket[fst], @racket[snd], @racket[swap].}]

@section{Deriving Functor (Phase 8)}

@racket[#:deriving Functor] synthesises an @racket[fmap]
implementation for any ADT whose last type parameter is the one being
mapped over.  For each field of each constructor:

@itemlist[
 @item{a field of exactly the functor parameter is replaced with
       @racket[(f field)];}
 @item{a field whose type is a recursive use of the same data type is
       transformed with @racket[(fmap f field)] — the dispatch lands
       back on the same instance at runtime;}
 @item{other fields pass through unchanged.}]

@codeblock|{
(define-data (Tree a)
  Leaf
  (Node (Tree a) a (Tree a))
  #:deriving Functor)
}|

@section{letrec (Phase 9)}

@codeblock|{
(letrec ([even? (lambda (n) (if (== n 0) #t (odd?  (- n 1))))]
         [odd?  (lambda (n) (if (== n 0) #f (even? (- n 1))))])
  (even? 8))
}|

Bindings are mutually recursive: every right-hand side has all the
names in scope.  Each binding is generalised independently against
the surrounding environment after inference.

@section{Type aliases (Phase 9)}

@codeblock|{
(define-alias Name           String)
(define-alias (Endo a)       (-> a a))
(define-alias (Pair3 a b c)  (Pair a (Pair b c)))
}|

Aliases expand inline during type resolution; they introduce no
runtime cost.  Recursive aliases are rejected with a clear error.

@section{Polymorphic recursion (Phase 9)}

A function with a declared polymorphic scheme can call itself at a
different instantiation than the enclosing call:

@codeblock|{
(: const-int (-> a Integer))
(define (const-int x)
  (if (== 0 0) 99 (const-int 5)))
}|

Without an explicit declaration, recursive calls are still
monomorphic — only the declaration's scheme licenses fresh
instantiations.

@section{panic (Phase 9)}

@racket[panic : (-> String a)] terminates the program with an error
message.  Its return type is universally quantified, so it can appear
in any branch:

@codeblock|{
(: pick-positive (-> Integer Integer))
(define (pick-positive n)
  (if (< n 0) (panic "negative not allowed") n))
}|

@section{Multi-block support (Phase 9)}

A single Racket module may now contain any number of @racket[(rackton
...)] invocations.  Each block elaborates independently against the
prelude.  Cross-file imports (@racket[require]) still see only
@hash-lang[] @racketmodfont{rackton} modules' schemes; multiple
embedded @racket[(rackton …)] blocks are visible only at the runtime
level, not via the typing channel.

@section{"did you mean?" diagnostics (Phase 9)}

An unbound-identifier error now searches the surrounding environment
for a near-match (Levenshtein distance ≤ 2) and suggests it:

@codeblock|{
> (rackton (define n (legnth (Cons 1 Nil))))
;; infer: unbound identifier: legnth (did you mean `length`?)
}|

@section{Immutable Map and Set (Phase 10)}

@racket[(Map k v)] is an opaque immutable mapping; every operation
returns a new map without modifying its input.

@codeblock|{
(define m0 empty-map)
(define m1 (map-insert "alpha" 1 m0))
(define m2 (map-insert "beta"  2 m1))

(map-lookup "alpha" m2)   ;; (Some 1)
(map-lookup "zeta"  m2)   ;; None
(map-size m2)             ;; 2
(map-size m1)             ;; 1  — persistence
}|

API summary:

@itemlist[
 @item{@racket[empty-map]   — @racket[(Map k v)]}
 @item{@racket[map-insert]  — @racket[((Eq k) => (-> k (-> v (-> (Map k v) (Map k v)))))]}
 @item{@racket[map-lookup]  — @racket[((Eq k) => (-> k (-> (Map k v) (Maybe v))))]}
 @item{@racket[map-delete]  — @racket[((Eq k) => (-> k (-> (Map k v) (Map k v))))]}
 @item{@racket[map-keys]    — @racket[(-> (Map k v) (List k))]}
 @item{@racket[map-values]  — @racket[(-> (Map k v) (List v))]}
 @item{@racket[map-size]    — @racket[(-> (Map k v) Integer)]}
 @item{@racket[map-fold]    — @racket[(-> (-> k (-> v (-> b b))) (-> b (-> (Map k v) b)))]}]

@racket[(Set a)] is the analogous immutable set:

@itemlist[
 @item{@racket[empty-set]    — @racket[(Set a)]}
 @item{@racket[set-insert]   — @racket[((Eq a) => (-> a (-> (Set a) (Set a))))]}
 @item{@racket[set-member?]  — @racket[((Eq a) => (-> a (-> (Set a) Boolean)))]}
 @item{@racket[set-delete]   — @racket[((Eq a) => (-> a (-> (Set a) (Set a))))]}
 @item{@racket[set-size]     — @racket[(-> (Set a) Integer)]}
 @item{@racket[set-to-list]  — @racket[(-> (Set a) (List a))]}]

Both are backed by Racket's immutable @racket[hash] (with
@racket[equal?] keys).  Order of @racket[map-keys] and
@racket[set-to-list] is unspecified.

@section{List helpers, ctd. (Phase 10)}

@racket[concat-map] flattens by composition:

@codeblock|{
(concat-map (lambda (n) (Cons n (Cons (* 2 n) Nil)))
            (Cons 1 (Cons 2 Nil)))
;; ⇒ (Cons 1 (Cons 2 (Cons 2 (Cons 4 Nil))))
}|

@racket[group-by] uses @racket[Map] to bucket a list by a key
function:

@codeblock|{
(group-by (lambda (n) (mod n 2))
          (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil))))))
;; ⇒ A (Map Integer (List Integer)) with 0 → [2,4] and 1 → [1,3,5]
}|

@section{Float arithmetic (Phase 11)}

@racket[Float] is a primitive type representing inexact reals.  A
numeric literal with a fractional part or exponent (e.g.
@racket[3.14], @racket[1e10]) is parsed as @racket[Float]; bare
integer literals stay @racket[Integer].

Standard instances are registered: @racket[Num Float] (binding
@racket[+] @racket[-] @racket[*]), @racket[Eq Float], @racket[Ord
Float], and @racket[Show Float].

Real division lives in its own @racket[Fractional] class:

@codeblock|{
(float-div 7.0 2.0)   ;; ⇒ 3.5
}|

@racket[Fractional] superclasses @racket[Num], so any
@racket[Fractional a] is also a @racket[Num a].

Bridges to integers: @racket[(integer->float : (-> Integer Float))]
and @racket[(float->integer : (-> Float Integer))] — the latter
truncates toward zero.

@racket[(sqrt : (-> Float Float))] wraps Racket's @racket[sqrt].

@section{Structured error recovery (Phase 11)}

@racket[panic] (Phase 9) raises an unrecoverable error.
@racket[try : (-> (IO a) (IO (Result String a)))] catches it and
delivers the result as a @racket[(Result String a)]:

@codeblock|{
(: safe-read (-> String (IO (Result String String))))
(define (safe-read path)
  (try (read-file path)))

(match (run-io (safe-read "missing.txt"))
  [(Ok body) (println body)]
  [(Err msg) (println (string-append "failed: " msg))])
}|

@racket[raise-io : (-> String (IO a))] is the typed counterpart that
fails an @racket[IO] action with a message; it pairs naturally with
@racket[try].

@section{Example program (Phase 12)}

@filepath{examples/calc.rkt} is a small expression-language interpreter
written entirely in Rackton.  It exercises ADTs (@racket[Sexpr],
@racket[Expr]), pattern matching with nested constructors,
@racket[(Map String Integer)] as a typing environment, @racket[Result]
for parse errors, @racket[IO] with @racket[read-line] / @racket[println]
for a REPL loop, mutual recursion between
@racket[parse-expr]/@racket[parse-list]/@racket[parse-form]
(declared via @racket[:] signatures before any of the defines), and the
host-language escape to call into Racket's @racket[read] for tokenizing.

Run it with @exec{racket examples/calc.rkt} and type expressions:

@codeblock|{
calc> (+ 1 2)
3
calc> (let x 5 (* x x))
25
calc> (let x 3 (let y 4 (+ x (* y y))))
19
}|

@section{Top-level forward references (Phase 12)}

A @racket[(: name type)] declaration now also pre-registers the name
in the typing environment, so later top-level forms can call mutually
recursive functions whose definitions appear after.  Declare all the
signatures first, then the bodies.

@section{Multi-form escapes (Phase 12)}

@racket[(racket τ (vars) body ...)] now accepts any number of body
forms; they're wrapped in a @racket[begin] automatically.  This lets
the escape host inner @racket[(define ...)] and @racket[(let …)]
forms naturally.

@section{Functional dependencies (Phase 13)}

A multi-parameter class may declare a functional dependency stating
that one (or more) parameters are uniquely determined by others:

@codeblock|{
(define-class (Convert a b)
  (#:fundep a -> b)
  (: convert (-> a b)))

(define-instance (Convert Integer String)
  (define (convert n) (show n)))

(define-instance (Convert Boolean Integer)
  (define (convert b) (if b 1 0)))
}|

Inside any program that uses @racket[convert], the type checker
consults registered instances: if a pred @racket[(Convert Integer ?)]
appears and there is an instance @racket[(Convert Integer String)],
the FD pins the result type to @racket[String].  This lets call
sites omit ascriptions that would otherwise be required.

The dependency is written as @racket[(#:fundep lhs ... -> rhs ...)] —
multiple parameters may appear on each side and a class may carry
multiple @racket[#:fundep] clauses.

@section{Merge sort (Phase 13)}

The prelude's @racket[sort] is now an @italic{O(n log n)} stable
merge sort instead of the previous insertion sort.  Same surface:
@racket[((Ord a) => (-> (List a) (List a)))].

@section{Diagnostics (Phase 14)}

Every type-mismatch error now reports a two-row breakdown:

@codeblock|{
> (rackton (define x (if 1 1 2)))
;; tests/foo.rkt:1:21: infer: type mismatch
;;   expected: Boolean
;;   got:      Integer
;;   in: 1
}|

The blame token (after @racket[in:]) is the most specific syntax
object responsible for the mismatch.  For applications, the bad
argument is highlighted rather than the whole call form:

@codeblock|{
> (rackton (define x (+ 1 "two")))
;; infer: type mismatch
;;   expected: Integer
;;   got:      String
;;   in: "two"
}|

Unbound-identifier diagnostics (Phase 9) extend in Phase 14 to:

@itemlist[
 @item{class-name look-ups (@racket[Eqq] → suggests @racket[Eq])}
 @item{constructor look-ups in patterns (@racket[Nul] → suggests
       @racket[Nil])}]

@section{System surface (Phase 15)}

Everything is IO-typed so randomness, the wall clock, and the
filesystem don't leak into pure code:

@itemlist[
 @item{@racket[random-integer] — @racket[(-> Integer (-> Integer (IO Integer)))] — uniform on [lo, hi).}
 @item{@racket[random-float]   — @racket[(IO Float)] — uniform on [0, 1).}
 @item{@racket[current-time-seconds] — @racket[(IO Integer)] — Unix epoch seconds.}
 @item{@racket[getenv] — @racket[(-> String (IO (Maybe String)))].}
 @item{@racket[argv]   — @racket[(IO (List String))] — command-line args (program name not included).}
 @item{@racket[list-directory] — @racket[(-> String (IO (List String)))].}
 @item{@racket[make-directory] — @racket[(-> String (IO Unit))].}
 @item{@racket[delete-file]    — @racket[(-> String (IO Unit))].}]

@codeblock|{
(define greeting
  (do [name <- (env-or-default "USER" "world")]
    (println (string-append "hello, " name))))

(define _main (run-io greeting))
}|

@section{Applicative, Bifunctor, Foldable (Phase 16)}

The higher-kinded class hierarchy gains @racket[Applicative] between
@racket[Functor] and @racket[Monad].  @racket[Applicative] provides
@racket[<*>] and a default @racket[liftA2]; instances ship for
@racket[Maybe], @racket[(Result e)], @racket[IO], and @racket[List]
(cartesian product).

@codeblock|{
(define maybe-sum (liftA2 (lambda (x y) (+ x y)) (Some 3) (Some 4)))
;; maybe-sum : (Maybe Integer) — evaluates to (Some 7)
}|

@racket[Bifunctor] generalises mapping over a type with two slots —
instances for @racket[Pair] and @racket[Result] make it natural to
transform both sides of a tuple or both arms of a fallible
computation.  @racket[bimap] is the workhorse; @racket[first] and
@racket[second] are derived defaults.

@codeblock|{
(define labelled (bimap (lambda (e) (string-length e))
                        (lambda (v) (* v 10))
                        (Err "boom")))
;; labelled : (Result Integer Integer) — evaluates to (Err 4)
}|

@racket[Foldable] generalises the right-fold over any container with
one type parameter.  The class supplies @racket[foldr] (the primitive)
plus @racket[length] and @racket[to-list] as derived defaults.
Instances ship for @racket[List] and @racket[Maybe]; @racket[sum] is a
top-level convenience for @racket[(Foldable t) => (-> (t Integer) Integer)].

@codeblock|{
(define total (sum (Cons 1 (Cons 2 (Cons 3 Nil)))))
;; total : Integer — evaluates to 6
}|

@racket[pure] is on @racket[Applicative].  See @secref{Return-typed_dispatch_(Phase_18)} for how the elaborator resolves it at each call site.

@section{Partial application (Phase 17)}

Functions of more than one parameter compile to a
@racket[case-lambda] that accepts either the full arity at once
(fast path, no closure created) or any shorter prefix — in which
case it returns a new function that collects the remaining
arguments.  This works uniformly for user-defined functions,
class methods, and the built-in prelude.

@codeblock|{
(: inc (-> Integer Integer))
(define inc (+ 1))               ;; partial application of (+)

(: greet (-> String String))
(define greet (string-append "hello, "))

(: lifted-inc (-> (Maybe Integer) (Maybe Integer)))
(define lifted-inc (fmap (+ 1))) ;; partial application of a class
                                 ;; method routes through dispatch
                                 ;; on the eventual container arg
}|

The implementation lives in @racket[build-curried-lambda] in
@racket[private/codegen.rkt] (for surface @racket[e:lam] forms) and
@racket[define/curried] in @racket[private/dict.rkt] (for the
hand-written prelude functions in @racket[private/prelude-runtime.rkt]).
@racket[define-class-method] now takes both a dispatch position and
the method's total arity so the wrapper knows when to stop collecting
arguments and fire the dispatch.

@section{Return-typed dispatch (Phase 18)}

A class method whose type carries the class parameter only in the
@emph{return} position — the canonical example is
@racket[pure :: a -> f a] — has no value at the call site whose tag
could select an instance.  Phase 18 resolves these at compile time
instead.  At inference, each reference to such a method is recorded
along with the fresh type variables that stand in for the class
parameters; once the enclosing definition's constraints have been
reduced, each recorded site is graduated to a per-instance impl name
of the form @tt{$method:TCon}.  Codegen consults the resolution
table when emitting the @racket[e:var] node and emits the impl name
in place of the original method name.

@codeblock|{
(: maybe-id (Maybe Integer))
(define maybe-id (pure 42))    ;; resolves to $pure:Maybe → (Some 42)

(: io-id (IO Integer))
(define io-id (pure 42))       ;; resolves to $pure:IO

(: ambiguous Integer)
(define ambiguous (pure 5))    ;; rejected — `f` cannot be determined
}|

The mechanism is purely monomorphic specialization at concrete call
sites; it does @bold{not} handle polymorphic instance bodies (e.g. a
@racket[Traversable] instance whose body calls @racket[pure] while
@racket[f] is itself a class parameter).  That requires either
runtime dictionary passing or a deeper specialization pass, which
remains future work.

The plumbing lives in @racket[private/infer.rkt]
(@racket[return-typed-method?], @racket[current-method-uses],
@racket[resolve-method-uses!]) and @racket[private/codegen.rkt]
(the @racket[e:var] consultation of @racket[current-method-resolutions]).
Both phases share their hashtables via a single
@racket[parameterize] in @racket[private/elaborate.rkt].

@section{Semigroup + Monoid (Phase 19)}

@racket[Semigroup] carries the associative @racket[<>] (sappend);
@racket[Monoid] refines it with a left-and-right identity
@racket[mempty].  Instances ship for @racket[String] and
@racket[(List a)].  @racket[mempty] is a return-typed class member
(it has no arrow positions at all), so the elaborator decides which
instance impl to call from the expected type at the use site.

@codeblock|{
(: greeting String)
(define greeting (<> "hello, " "world"))            ;; "hello, world"

(: empty-strs String)
(define empty-strs (ann mempty String))             ;; ""

(: numbers (List Integer))
(define numbers (<> (Cons 1 Nil) (Cons 2 Nil)))     ;; (Cons 1 (Cons 2 Nil))

;; Identity laws hold both sides:
(: round-trip String)
(define round-trip
  (<> (<> (ann mempty String) "x")
      (ann mempty String)))                          ;; "x"
}|

A @racket[mempty] expression with no type context (no @racket[ann],
no declared signature, no flow-based inference) is rejected at
compile time with @tt{ambiguous use of mempty: cannot determine
target type at this call site} — the elaborator surfaces a clear
error rather than letting an unsolved constraint propagate.

@section{Traversable (Phase 20)}

@racket[traverse] walks a container of type @racket[(t a)] calling an
@racket[Applicative]-effectful function on each element and rebuilds
the container inside that applicative.  Phase 16 deferred this class
because its instance bodies call @racket[pure] while @racket[f] is
still polymorphic; Phase 18 unblocked it for concrete call sites by
adding return-typed dispatch, and Phase 20 closes the loop with
@emph{dict-passing}: at each concrete use of @racket[traverse], the
elaborator resolves the @racket[Applicative f] constraint to a
specific @racket[$pure:f] impl and inserts it as the leading
argument to the dispatch wrapper.

@codeblock|{
(: parse-positive (-> Integer (Maybe Integer)))
(define (parse-positive n) (if (> n 0) (Some n) None))

(: all-positive (Maybe (List Integer)))
(define all-positive
  (traverse parse-positive (Cons 1 (Cons 2 (Cons 3 Nil)))))
;; ⇒ (Some (Cons 1 (Cons 2 (Cons 3 Nil))))

(: any-failure (Maybe (List Integer)))
(define any-failure
  (traverse parse-positive (Cons 1 (Cons -2 Nil))))
;; ⇒ None   — short-circuits on the inner None
}|

Mechanically: methods whose qualifying context introduces type
variables that appear in the method body (`traverse`'s @racket[f] is
the canonical case) are flagged at class declaration as "needs-dict"
via @racket[method-dict-requirements].  At each use site the
elaborator records the fresh tvars carrying the dict constraint;
after the enclosing definition's constraints are reduced, the entry
is graduated into @racket[current-method-dict-resolutions] as a list
of impl names (today: just @racket[$pure:f] for @racket[Applicative]
dicts).  @racket[compile-expr] consults that table when emitting an
@racket[e:app] whose head names a needs-dict method and prepends the
resolved impl names to the call's arguments.  The runtime dispatch
wrapper's position and arity are shifted at class-compile time to
match what it actually receives.

A bare @racket[e:var] reference to a needs-dict method (e.g.
@racket[(map traverse xs)]) is @bold{not} supported — there is no
@racket[e:app] for the elaborator to attach the dict insertion to.
Use the method at a call position.

@section{Pattern guards, @racket[match-let], @racket[where] (Phase 21)}

@bold{Pattern guards.}  A @racket[match] clause may carry a
@racket[#:when guard] between its pattern and body; the clause fires
only when the pattern matches @emph{and} the guard expression
evaluates to @racket[#t].  Guarded clauses do not contribute to
exhaustiveness — a guarded wildcard is not a catch-all, since the
guard may fail.

@codeblock|{
(: classify (-> (Maybe Integer) String))
(define (classify m)
  (match m
    [(Some x) #:when (> x 0) "positive"]
    [(Some x) #:when (< x 0) "negative"]
    [(Some _)                "zero"]
    [(None)                  "missing"]))
}|

@bold{@racket[match-let].}  @racket[(match-let ([pat e] ...+) body)]
destructures each rhs against its pattern in turn, binding the
pattern variables for the remainder of the chain.  Patterns are
@emph{irrefutable}: the user is asserting the rhs fits the pattern,
and the synthesized @racket[match] is exempt from exhaustiveness
checks.

@codeblock|{
(: pair-sum Integer)
(define pair-sum
  (match-let ([(MkPair a b) (MkPair 7 35)]
              [(Cons h _)   (Cons 100 Nil)])
    (+ a (+ b h))))   ;; ⇒ 142
}|

@bold{@racket[where].}  @racket[(where ([n e] ...+) body)] introduces
sequential local bindings; each binding sees the ones above it.  This
is the equivalent of @racket[let*] in other Lisps and reads as the
natural Rackton spelling of Haskell's @racket[where] clauses.

@codeblock|{
(: scaled-sum (-> Integer (-> Integer Integer)))
(define (scaled-sum x y)
  (where ([sum     (+ x y)]
          [doubled (* 2 sum)])
    (+ sum doubled)))
}|

@section{Example: a todo CLI (Phase 22)}

@racket[examples/todo.rkt] is a small but complete command-line todo
manager written in @hash-lang[] @racketmodfont{rackton}.  It exists
as a pressure-test of the whole stack: argv parsing, environment
variables, file I/O, do-notation, Semigroup / Monoid, pattern
guards, currying, and the ADT match are all exercised on one program.

@verbatim|{
$ racket examples/todo.rkt add buy milk
added: buy milk
$ racket examples/todo.rkt add walk dog
added: walk dog
$ racket examples/todo.rkt list
1. [ ] buy milk
2. [ ] walk dog
$ racket examples/todo.rkt done 1
marked done: #1
$ racket examples/todo.rkt clear
cleared 1 items
$ racket examples/todo.rkt list
1. [ ] walk dog
}|

State lives at @code{$TODO_FILE} (defaults to @code{./todos.txt}),
one item per line: @code{[ ] task} or @code{[x] task}.  The
demo-driver test in @racket[tests/todo-demo-test.rkt] subprocesses
the example for each scenario, exercising end-to-end behaviour
against a fresh temp file.

Building this example surfaced four prelude gaps which Phase 22 also
filled: a @racket[cond] surface form (desugars to nested @racket[if]),
and three string helpers — @racket[string-prefix?], @racket[string-split],
and @racket[string-join].

@section{Newtypes (Phase 23)}

@racket[define-newtype] is sugar over @racket[define-data] for the
common shape "one constructor, one field" — a nominal wrapper around
an existing type that gives the wrapped form its own type identity
without changing what it stores.

@codeblock|{
(define-newtype Sum     (MkSum     Integer))
(define-newtype Product (MkProduct Integer))
}|

The point is that one underlying type can now carry multiple
distinct typeclass instances.  @racket[Integer] doesn't have a
single canonical @racket[Monoid] (is it addition or multiplication?),
but @racket[Sum] and @racket[Product] each do, and they're both in
the prelude:

@codeblock|{
(<> (MkSum 3) (MkSum 5))           ;; ⇒ (MkSum 8)     — additive
(<> (MkProduct 3) (MkProduct 5))   ;; ⇒ (MkProduct 15) — multiplicative
(ann mempty Sum)                   ;; ⇒ (MkSum 0)
(ann mempty Product)               ;; ⇒ (MkProduct 1)
}|

Folding a list of @racket[Integer] into either one then becomes
a straight @racket[foldr] with an ascribed @racket[mempty]:

@codeblock|{
(foldr (lambda (n acc) (<> (MkSum n) acc))
       (ann mempty Sum)
       (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))))
;; ⇒ (MkSum 10)
}|

@racket[mconcat] is now a real prelude function (see
@secref{Free-function_dict-passing_(Phase_24)} just below).

At runtime a newtype is identical to a single-constructor
@racket[define-data] — the "zero-cost" of a true newtype (eliding the
struct wrap) is documentary, not yet a perf optimization.

@section{Free-function dict-passing (Phase 24)}

Phase 20 introduced dict-passing for class methods like
@racket[traverse], where the elaborator resolves an outer class
constraint at the call site and prepends the resolved impl name to
the arguments.  Phase 24 extends the same machinery to @emph{free
functions} whose own qualifying context constrains a class with
return-typed methods.  The canonical use is @racket[mconcat]:

@codeblock|{
(: mconcat ((Monoid a) => (-> (List a) a)))
}|

At every call site the elaborator looks up the function's
qualifying context, resolves each constraint over a return-typed
class (today: @racket[Monoid]'s @racket[mempty]) against the
inferred type, and inserts the impl name (e.g. @racket[$mempty:Sum])
as a prepended argument.  Runtime side is a single hand-written
function in @racket[private/prelude-runtime.rkt] that accepts the
@racket[mempty]-impl as its leading argument:

@codeblock|{
(define (mconcat mempty-impl xs)
  (foldr (lambda (x acc) (<> x acc)) mempty-impl xs))
}|

The user-facing code reads naturally:

@codeblock|{
(mconcat (Cons "a" (Cons "b" (Cons "c" Nil))))   ;; ⇒ "abc"

(mconcat (Cons (MkSum 3) (Cons (MkSum 5) Nil))) ;; ⇒ (MkSum 8)

(mconcat (ann Nil (List Sum)))                  ;; ⇒ (MkSum 0)
}|

Phase 29 closes this loop: user code can now write needs-dict
function bodies that use polymorphic @racket[mempty] / @racket[pure]
calls.  See @secref{User_needs-dict_bodies_(Phase_29)} below.

@section{State, Env, and their transformers (Phase 25)}

@bold{State.}  @racket[State s a] is the canonical state-passing
monad — internally a function @racket[s -> (Pair s a)].

@codeblock|{
(: tick (State Integer Integer))
(define tick
  (do [n <- get-state]
      [_ <- (put-state (+ n 1))]
    (pure n)))

((run-state (do [a <- tick]
                [b <- tick]
              (pure (Cons a (Cons b Nil)))))
 0)
;; ⇒ (MkPair 2 (Cons 0 (Cons 1 Nil)))
}|

Helpers: @racket[run-state], @racket[eval-state], @racket[exec-state],
@racket[get-state], @racket[put-state], @racket[modify-state].

@bold{Env.}  Named to avoid colliding with Scheme's
@racket[read]/@racket[readtable] vocabulary, @racket[Env r a] is the
canonical Reader monad — a function @racket[r -> a] with
@racket[ask] and @racket[local].

@codeblock|{
(: greet (Env String String))
(define greet
  (do [name <- ask]
    (pure (<> "hello, " name))))

((run-env greet) "world")              ;; ⇒ "hello, world"
((run-env (local (lambda (s) (<> s "!!")) greet)) "world")
                                       ;; ⇒ "hello, world!!"
}|

@bold{Transformers (@racket[StateT s m a] and @racket[EnvT r m a]).}
Each wraps an inner monad @racket[m].  The Functor/Applicative/Monad
instances are declared with @racket[(Monad m) =>] qualifiers; the
methods whose body genuinely needs the inner @racket[pure] (only
@racket[pure] itself and @racket[get-state-t]/@racket[put-state-t]/
@racket[modify-state-t]/@racket[ask-t]) receive the resolved
@racket[$pure:m] impl as a leading argument from the elaborator.
@racket[lift-state-t], @racket[lift-env-t], @racket[local-t], and the
@racket[Functor]/@racket[Monad] methods of either transformer get by
with the inner @racket[fmap] and @racket[>>=], which dispatch on
runtime value tags and need no compile-time dict at all.

@codeblock|{
(: safe-div (-> Integer (-> Integer (StateT Integer Maybe Integer))))
(define (safe-div num den)
  (if (== den 0)
      (lift-state-t None)
      (do [acc <- get-state-t]
          [_   <- (put-state-t (+ acc 1))]
        (pure (div num den)))))

((run-state-t (do [a <- (safe-div 20 4)]
                  [b <- (safe-div 10 a)]
                (pure (+ a b))))
 0)
;; ⇒ (Some (MkPair 2 7))   — state counts successful divisions

((run-state-t (do [_ <- (safe-div 20 4)]
                  [_ <- (safe-div 1 0)]
                (pure 0)))
 0)
;; ⇒ None                  — the inner Monad short-circuits
}|

@bold{Honest limitation.}  The elaborator supports @emph{one level}
of instance-qual dict-passing: an instance such as
@racket[(Monad m) => Monad (StateT s m)] resolves the inner
@racket[$pure:m] correctly when @racket[m] is a concrete tcon.
@emph{Nested} transformers (e.g. @racket[StateT s (StateT s' Maybe)])
would need the dict args to themselves be partial-application forms,
which the resolver doesn't yet emit.  Documented as future work.

@bold{Naming note.}  Haskell calls the environment monad
@racket[Reader].  Rackton names it @racket[Env] because @racket[Reader]
would collide with the Scheme reader vocabulary
(@racket[read]/@racket[readtable]/etc.) — confusing inside a
Racket-hosted language.  @racket[ask] and @racket[local] are kept as
the generic verbs.

@section{WriterT (Phase 26)}

@racket[WriterT w m a] threads an accumulating @racket[Monoid w] log
through an inner monad @racket[m].  Internally it wraps
@racket[(m (Pair w a))]; @racket[tell] appends a single log entry and
@racket[lift-writer-t] hoists an arbitrary @racket[m]-action into the
transformer with an @racket[mempty] starting log.

@codeblock|{
(: logged-greeting (WriterT String IO Integer))
(define logged-greeting
  (do [_ <- (tell "hello, ")]
      [_ <- (tell "world")]
    (pure 42)))

(run-io (run-writer-t logged-greeting))
;; ⇒ (MkPair "hello, world" 42)
}|

The Applicative instance qual context lists @bold{two}
return-typed-bearing constraints: @racket[(Monad m)] (for inner
@racket[pure]) and @racket[(Monoid w)] (for @racket[mempty]).  This
is the first instance in the prelude that exercises Phase 25's
resolver with multiple dict args — the elaborator inserts both
@racket[$pure:m] and @racket[$mempty:w] at every call site of
@racket[pure] on a @racket[WriterT] value.

Class methods @racket[fmap]/@racket[<*>]/@racket[liftA2]/@racket[>>=]
on @racket[WriterT] are rewritten to use only inner
@racket[fmap]/@racket[>>=] (both runtime-dispatched on the inner
value's tag), so the impls receive no dict args.  This is what
allows @racket[WriterT] to ship cleanly while @racket[ExceptT] is
deferred — see below.

@racket[ExceptT] ships in Phase 27 — see the next section.

@section{ExceptT (Phase 27)}

@racket[ExceptT e m a] layers typed exceptions over an inner monad
@racket[m].  Internally it wraps @racket[(m (Result e a))]; the
@racket[Err] branch short-circuits both @racket[>>=] and @racket[<*>],
and @racket[catch-error] lets a handler recover.

@codeblock|{
(: divide-safely (-> Integer (-> Integer (ExceptT String IO Integer))))
(define (divide-safely num den)
  (if (== den 0)
      (throw-error "division by zero")
      (pure (div num den))))

(run-io
 (run-except-t
  (do [a <- (divide-safely 100 5)]
      [b <- (divide-safely a 0)]    ;; throws — rest skipped
      [c <- (divide-safely a 2)]
    (pure (+ b c)))))
;; ⇒ (Err "division by zero")

(run-io
 (run-except-t
  (catch-error (divide-safely 1 0)
               (lambda (_) (pure 999)))))
;; ⇒ (Ok 999)
}|

The runtime body of @racket[>>=] for @racket[ExceptT] genuinely
needs the inner monad's @racket[pure] to lift @racket[(Err e)] back
into @racket[m].  Phase 27 supports this by extending the elaborator's
dict-passing one more notch: at every class-method call on a value of
type @racket[(ExceptT e m a)], the elaborator (1) routes the call
directly to a per-instance impl named @tt{$method:ExceptT} and
(2) prepends the resolved @racket[$pure:m] as a leading dict arg.

@bold{Mechanism, end-to-end.}  Three resolver paths now coexist:

@itemlist[
 @item{@emph{Return-typed dispatch} (Phase 18): a method like
       @racket[pure] resolves to a per-instance impl name from the
       expected return type alone.}
 @item{@emph{Method-qual dict-passing} (Phase 20/24/25): when the
       method's own qualifying context, or a free function's qualifying
       context, names a return-typed-bearing class, dict args are
       inserted at the call site (e.g.
       @racket[traverse] for @racket[Traversable] instances,
       @racket[mconcat] for @racket[Monoid] instances).}
 @item{@emph{Instance-qual dispatch} (Phase 27): a positional
       class-method call on a value whose inferred type identifies a
       needs-dict instance.  The dispatch arg's type is enough to pick
       the matching instance via @racket[match-pred]; the instance's
       qual context yields the dict-args list.}]

@bold{Skipped on polymorphic call sites.}  Phase 27 only fires when
the dispatch arg's class-param tvars resolve to concrete type
constructors.  When they're still polymorphic at resolve time the
elaborator leaves the call alone and the runtime dispatcher handles
it — fine for non-needs-dict instances, raises a clear arity error
at runtime if a polymorphic call actually reaches a needs-dict
instance.  A future phase can add a dispatcher shim that surfaces a
friendlier error.

@section{Char and Bytes (Phase 28)}

Two new primitive types backed by Racket's native @racket[char?] and
@racket[bytes?] values.  Literal syntax passes through the reader
unchanged: @code{#\A} is a @racket[Char], @code{#"hello"} a
@racket[Bytes].

@codeblock|{
(: codes (List Integer))
(define codes
  (fmap char->integer (string->chars "abc")))
;; ⇒ (Cons 97 (Cons 98 (Cons 99 Nil)))

(integer->char 65)              ;; ⇒ (Some #\A)
(integer->char -1)              ;; ⇒ None

(char-upcase #\a)               ;; ⇒ #\A
(char-alphabetic? #\7)          ;; ⇒ #f

(bytes-length #"hi")            ;; ⇒ 2
(bytes-ref #"hi" 0)             ;; ⇒ (Some 104)
(bytes-ref #"hi" 99)            ;; ⇒ None
(bytes-append #"foo" #"-bar")   ;; ⇒ #"foo-bar"

(string->bytes "Aé")            ;; ⇒ #"A\xc3\xa9"
(bytes->string #"\xff\xfe\xfd") ;; ⇒ None  (invalid UTF-8)
}|

Instances: @racket[Eq] / @racket[Ord] / @racket[Show] for
@racket[Char]; @racket[Eq] / @racket[Show] for @racket[Bytes].
Partial operations like @racket[integer->char],
@racket[string-ref], @racket[bytes-ref], and @racket[bytes->string]
return @racket[(Maybe …)] so the caller decides what to do on
failure.  @racket[string->bytes] uses UTF-8.

@section{User needs-dict bodies (Phase 29)}

Phase 24 added the call-site half of dict-passing: a function declared
with a return-typed-bearing qualifying context (like @racket[Monoid a]
or @racket[Applicative f]) had the resolved impl inserted at every
call.  But until Phase 29 the elaborator wouldn't accept the
@emph{body} of such a function if it used the polymorphic class method
itself — @tt{ambiguous use of mempty / pure} would fire because the
class param is still abstract.  Users were limited to declared-only
prelude wrappers.

Phase 29 lifts that.  When a top-level definition declares a needs-dict
qualifying context, the elaborator:

@itemlist[
 @item{pre-allocates a fresh local dict-arg name for each
       return-typed-method that the qual context demands;}
 @item{skolemizes the qual-context tvars with tracking, building a map
       from each skolem @racket[tcon] back to its dict-arg name;}
 @item{makes that map visible to the resolver while inferring the body
       — any return-typed method call whose class param resolves to a
       tracked skolem gets routed to the local dict-arg instead of a
       (nonexistent) per-tcon impl;}
 @item{prepends the dict-arg parameters to the compiled lambda.}]

The result is that user code looks the way it ought to:

@codeblock|{
(: my-concat ((Monoid a) => (-> (List a) a)))
(define (my-concat xs)
  (foldr (lambda (x acc) (<> x acc)) mempty xs))

(my-concat (Cons "a" (Cons "b" (Cons "c" Nil))))   ;; ⇒ "abc"
(my-concat (Cons (MkSum 3) (Cons (MkSum 5) Nil))) ;; ⇒ (MkSum 8)

(: my-pure-pair ((Applicative f) => (-> a (f (Pair a a)))))
(define (my-pure-pair x) (pure (MkPair x x)))

(ann (my-pure-pair 42) (Maybe (Pair Integer Integer)))
;; ⇒ (Some (MkPair 42 42))
}|

The mechanism interacts cleanly with the rest of the dispatch story.
@racket[<>] in the body of @racket[my-concat] is positional-dispatched
at runtime on its value arg, which carries its concrete type tag, so
no compile-time work is needed for it — only the return-typed
@racket[mempty] (where there is no value to dispatch on) takes the
dict-arg route.

@bold{Limits.}  The mechanism is single-skolem-per-constraint: a
multi-parameter dict class (none in the prelude yet) would need
parallel local args.  Mutually recursive needs-dict definitions are
not handled — each def is independently tracked but cross-references
would need a forward-declaration pass to align the dict-arg names.

@section{Lifted instance bodies (Phase 30)}

Phase 29 made user-defined needs-dict @emph{free functions} work.
Phase 30 does the same for instance method bodies — required for
the canonical pattern where a transformer's class instance is
@emph{lifted} from an inner monad's instance:

@codeblock|{
(define-class (HasUnit (m :: (-> * *)))
  (: unit-val (m Integer)))

(define-instance (HasUnit Maybe)
  (define unit-val (Some 1)))

;; Lifted instance — the body's `unit-val` is polymorphic in `m`,
;; bound by the instance's qual context.
(define-instance ((HasUnit m) => (HasUnit (EnvT String m)))
  (define unit-val (lift-env-t unit-val)))

((run-env-t (ann unit-val (EnvT String Maybe Integer))) "ignored")
;; ⇒ (Some 1)
}|

The elaborator skolemizes the qualifying-context tvars at
@racket[handle-instance-form] time and builds a map from each skolem
to a freshly-allocated local dict-arg name — keyed by
@code{(skolem-name . method-name)} so a class with multiple methods
ends up with one dict arg per method.  While inferring the body, the
skolem map is active in @racket[current-dict-skolems]; polymorphic
class-method references whose class param resolves to a tracked
skolem rewrite to the local dict-arg name.  At codegen,
@racket[compile-instance] consults @racket[current-needs-dict-defs]
to prepend those names as leading parameters to the generated
@code{$method:TCon} impl.

The instance is still stored in the env under its @emph{original}
(un-skolemized) head and qual so other code that asks "is there an
@racket[(HasUnit (EnvT String Maybe))] instance?" finds it normally.

Phase 31 builds on this to ship the mtl-style classes
(@racket[MonadState], @racket[MonadEnv], @racket[MonadWriter],
@racket[MonadError]) with full instance matrices including the
lifted cases.

@section{mtl-style classes (Phase 31)}

Phase 31 ships four polymorphic effect classes — @racket[MonadState],
@racket[MonadEnv], @racket[MonadWriter], @racket[MonadError] — each
parameterized by an effect payload and a monad with a functional
dependency from the monad to the payload (@code{m -> s} etc.) so type
inference can recover the payload from the chosen monad.  Code can
ask for "any monad with state" via @code{(MonadState Integer m) => …}
and have the concrete transformer chosen at the call site:

@codeblock|{
(: incr-by ((MonadState Integer m) => (-> Integer (m Unit))))
(define (incr-by n)
  (do [k <- get-st]
    (put-st (+ k n))))

(define s-incr   (incr-by 5))   ;; (State  Integer Unit)
(define s-t-incr (incr-by 3))   ;; (StateT Integer IO Unit)
(define e-incr   (incr-by 7))   ;; (EnvT String (State Integer) Unit)
}|

Each class has both a @emph{base} instance (e.g.
@racket[(MonadState s (State s))]) and @emph{lifted} instances over
the other three transformers (e.g.
@code{(MonadState s m) => (MonadState s (EnvT r m))}), so any pairing
of transformers — including nested stacks like
@code{(StateT s (EnvT r IO) String)} — picks up the right effect
methods.

The dispatch story extends earlier phases in three steps:

@itemlist[
  @item{@bold{Fundep-aware dispatch.}  Per-class return-typed impl
        names drop arg slots that the fundep makes redundant —
        @racket[(MonadState s (State s))] uses @code{$get-st:State},
        not @code{$get-st:Integer-State}.  Both
        @racket[resolve-return-impl] (infer) and
        @racket[compile-instance] (codegen) consult
        @racket[class-info-fundeps] when forming or matching the impl
        name.  @racket[find-dispatch-pos] similarly skips arg
        positions whose only class-params are fundep-determined.}
  @item{@bold{Superclass-closure dict-passing.}  A constraint of class
        @racket[C] now ships dicts for every reachable return-typed
        method — @racket[C]'s own plus those of every transitive
        superclass.  A @code{(MonadEnv String m) =>} function carries
        dicts for @racket[ask-en] (own) and @racket[pure] (Monad via
        Applicative superclass).  @racket[collect-dict-method-args]
        recurses through @racket[class-info-supers], threading outer
        arg-types through each super-pred so inherited methods see
        only their own class's args.}
  @item{@bold{Nested instance-qual dicts.}  Each impl in the
        dict-passing path is wrapped via
        @racket[resolve-impl-with-quals] with the dicts its matching
        instance's qual context demands.  So @racket[(incr-by 3)] at
        @racket[(StateT Integer IO Unit)] emits not just
        @code{$get-st:StateT} but @code{($get-st:StateT $pure:IO)} —
        the call pre-applies the inner-monad dict so the
        dispatcher sees a saturated transformer value.  Dedup
        matches @racket[build-dict-skolems]' @code{(skolem . method)}
        key so multi-constraint signatures like
        @code{((MonadState … m) (MonadEnv … m) =>)} produce the same
        number of dict args on each side.}
]

The runtime impls (@code{$get-st:T}, @code{$ask-en:T},
@code{$tell-w:T}, @code{$throw-e:T}, plus the matching positional
methods) live alongside the existing transformer code in
@code{prelude-runtime.rkt}.  Lifted impls accept the inner-monad's
return-typed dicts as leading curried args in alphabetical method
order followed by superclass-closure dicts — matching what the call
site emits.

@section{MTL polish (Phase 32)}

Phase 32 sands off the rough edges Phase 31 left behind and grows the
combinator surface for everyday MTL code.

@itemlist[
  @item{@bold{Lifted @racket[local-en] now actually rewrites the env}
        rather than passing the inner action through unchanged.  A new
        runtime dispatch table (@code{$dispatch:local-en}) carries the
        @racket[Env] and @racket[EnvT] base impls so the lifted
        StateT / WriterT / ExceptT cases can recurse via
        @racket[local-en] on the unwrapped inner-monad value.}
  @item{@bold{Lifted @racket[catch-e] for StateT / EnvT / WriterT}
        properly invokes the user handler.  The lifted impls hard-wire
        @racket[catch-error] for the inner @racket[ExceptT] (the only
        base @racket[MonadError] instance) rather than chasing a
        dispatcher that can't carry the @racket[ExceptT] qual's
        inner-pure dict.  Lifted @racket[catch-e] through a
        @emph{transformer-of-transformer} stack still needs deeper
        qual-chain resolution and is listed under Not Yet Supported.}
  @item{@bold{@racket[MonadWriter] grows @racket[listen] and
        @racket[censor]} as positional methods.  The base @racket[WriterT]
        impls reach into the inner @code{(Pair w a)} via runtime-
        dispatched @racket[fmap] (so they need no extra dicts);
        lifted instances for the other transformers remain
        host-language stubs and aren't exercised in tests.}
  @item{@bold{Five new derived combinators}: @racket[asks] / @racket[gets]
        wrap a pure transformation around an env / state read;
        @racket[void] drops a Functor's result to @racket[Unit];
        @racket[when] and @racket[unless] are Applicative-conditional
        actions returning @racket[Unit].  All four are needs-dict free
        functions threading the resolved class-method impls
        as leading curried params.}
  @item{@bold{Concrete-class-param dict pruning.}  When a needs-dict
        function's scheme pins one of a class-param slot to a concrete
        type (e.g. @code{(MonadWriter String m) =>}), the body's
        references to that class's methods resolve to a per-type
        global (@code{$mempty:String}) rather than through a dict.
        The call site no longer emits a spurious dict arg for those
        cases — matching @racket[build-dict-skolems]' empty-skolems
        treatment in the body's lambda layout.  Without this fix,
        @racket[(MonadWriter String m) (Monad m) =>] callers would
        pass an unexpected @racket[mempty] arg and crash with an
        arity mismatch.}
]

The fundep-aware inst-dispatch impl name was also generalized: positional
class methods at fundep-bearing classes now drop determined params from
the per-instance impl name (so @code{$local-en:EnvT} matches
@racket[compile-instance]'s emission instead of the previous
@code{$local-en:String-EnvT}).

@section{Runtime resolvers for needs-dict instances (Phase 33)}

Phase 27 / 30 wired up compile-time @emph{inst-dispatch} so a call to
a class method whose target instance is needs-dict
(e.g. @racket[(MonadError e (ExceptT e m))]) gets routed at the call
site with its qual dicts pre-applied.  That only works when the call
site's type is concrete: a polymorphic monad body like
@code{(MonadError e m) =>} reaches the call with @racket[m] still a
tvar, falls through to the runtime dispatcher, and crashes — no
runtime registration ever existed for @racket[(>>= ea f)] on a
@racket[MkExceptT] value.

The same trap bit @racket[do]-notation chains over @racket[ExceptT]:
each desugared @racket[>>=] dispatches by the runtime tag of its
first arg, and that tag is @racket[$ctor:MkExceptT] with no entry
in the @racket[>>=] table.

Phase 33 closes the loop:

@itemlist[
  @item{@bold{@racket[pure-via-witness]} resolves @racket[pure] from
        any monadic value by walking its ctor chain.  For wrapper
        ctors (@racket[MkExceptT], @racket[MkStateT], @racket[MkEnvT])
        it unwraps, recurses on the inner tag, and re-wraps; for
        non-needs-dict bases (@racket[IO], @racket[Maybe], @racket[List],
        @racket[Result]) it looks up in a per-tag @racket[$pure-by-tag]
        table.  @racket[WriterT] is excluded — its
        @racket[pure] also needs the log @racket[Monoid]'s
        @racket[mempty], which isn't recoverable from the runtime tag.}
  @item{@bold{Runtime closures on @racket[$ctor:MkExceptT]} register
        in @racket[$dispatch:>>=] / @racket[$dispatch:<*>] /
        @racket[$dispatch:liftA2] / a new @racket[$dispatch:catch-e]
        table.  Each closure calls the existing needs-dict impl
        (@code{$>>=:ExceptT}, @racket[catch-error], ...) with
        @racket[(pure-via-witness (run-except-t ea))] as the
        inner-pure dict — letting the runtime path do what the
        compile-time inst-dispatch path was doing.}
  @item{@bold{Lifted @racket[catch-e] now recursively dispatches.}
        @racket[$catch-e:StateT] / @racket[$catch-e:EnvT] /
        @racket[$catch-e:WriterT] used to hard-call @racket[catch-error]
        on the unwrapped inner value, which assumed the inner monad
        was always base @racket[ExceptT].  They now call
        @racket[catch-e] (the class method, runtime-dispatched), so
        deeper qual chains like @code{ExceptT e1 (ExceptT e2 IO)}
        resolve correctly through whatever @racket[catch-e] instance
        is registered for the inner value.}
]

@section{Transformer-side runtime registrations (Phase 34)}

Phase 33 wired the @code{$ctor:MkExceptT} side of the runtime
dispatcher.  Phase 34 finishes the same treatment for the rest of the
transformer ctors:

@itemlist[
  @item{@bold{@racket[catch-e]} on @racket[$ctor:MkStateT] /
        @racket[$ctor:MkEnvT] / @racket[$ctor:MkWriterT] —
        delegates to runtime @racket[catch-e] on the inner monadic
        value after unwrapping with @racket[run-state-t] /
        @racket[run-env-t] / @racket[run-writer-t].}
  @item{@bold{@racket[listen] and @racket[censor]} on
        @racket[$ctor:MkWriterT] — both @racket[fmap] over the
        inner @racket[(Pair w a)] via runtime-dispatched
        @racket[fmap], so they need no dict args.}
  @item{@bold{@racket[local-en]} on @racket[$ctor:MkStateT] /
        @racket[$ctor:MkWriterT] / @racket[$ctor:MkExceptT] — same
        recursion-via-inner pattern.}
]

Each registered closure has the same body as Phase 31's curried
@code{$method:T} impl with the unused dict slots dropped — Phase 33's
refactor stopped consuming them, leaving the dicts purely as a
layout match for the compile-time inst-dispatch path.  Polymorphic-
monad bodies (e.g. @code{(MonadWriter String m) => …} run at
@racket[WriterT String IO]) now resolve every call site through one
of these two paths.

No @racket[mempty-via-witness] is needed: nothing in the runtime path
actually consumes @racket[inner-mempty].  Only @racket[pure] (return-
typed, compile-time-resolved) and @racket[lift-writer-t] (Phase 31
compile-time-resolved) do, and both routes already supply the dict.

@section{Deriving for records and Foldable (Phase 35)}

@racket[define-data] has shipped @racket[#:deriving Eq Show Ord Functor]
since Phase 11.  Phase 35 extends the same mechanism to two more
surfaces and one more class.

@itemlist[
  @item{@bold{@racket[define-struct] accepts @racket[#:deriving]}
        as a trailing clause after the field specs.  Records are
        single-ctor data types, so the existing
        @racket[synthesize-eq-instance] / @racket[synthesize-show-instance]
        / @racket[synthesize-ord-instance] /
        @racket[synthesize-functor-instance] helpers work without
        modification.  Constraint propagation (e.g.
        @code{(Eq a) (Eq b) => (Eq (Pair2 a b))}) is the same as
        for @racket[define-data].}
  @item{@bold{@racket[define-newtype] accepts @racket[#:deriving]} —
        already routed through @racket[parse-data-form], so this is
        mostly a documentation + regression-test reach.  Phase 35
        also tightens the rest-args validator so a malformed newtype
        body (multiple ctors) still raises the proper error.}
  @item{@bold{@racket[Foldable] joins the deriving menu.}  Synthesizes
        @racket[foldr] for an ADT whose last type parameter is the
        fold element: walk each ctor's fields right-to-left, calling
        @racket[f] when a field's type @emph{is} the parameter,
        recursing via @racket[foldr] for recursive uses of the same
        data type, and skipping otherwise.  No constraint
        propagation — Foldable doesn't constrain its element type.
        The class's other methods (@racket[length], @racket[to-list],
        @racket[sum]) come from class defaults, so deriving Foldable
        gives all three for free.}
]

The shared @code{synthesize-deriving} helper used by all three
surface forms handles superclass implication: deriving @racket[Ord]
also derives @racket[Eq] (the synthesized @racket[<] calls
@racket[==]), and the deriving menu's error message lists every
supported class.

@section{Concurrency primitives (Phase 36)}

Three opaque types — @racket[ThreadId], @racket[(MVar a)],
@racket[(Chan a)] — plus a small surface of IO-returning primitives,
all thin wrappers over Racket's @racket[thread] / semaphore / async
channel APIs.

@codeblock|{
(define counter-action
  (do [counter <- (new-mvar 0)]
      [t1 <- (fork-io (modify-mvar counter (lambda (n) (+ n 1))))]
      [t2 <- (fork-io (modify-mvar counter (lambda (n) (+ n 1))))]
      [_  <- (wait-thread t1)]
      [_  <- (wait-thread t2)]
    (read-mvar counter)))
}|

@itemlist[
  @item{@bold{@racket[fork-io] / @racket[wait-thread].}  Spawn a
        Racket thread running the given IO action; @racket[wait-thread]
        blocks until the thread terminates.}
  @item{@bold{@racket[MVar a].}  A rendezvous variable backed by a box
        and two semaphores.  @racket[put-mvar] blocks while full,
        @racket[take-mvar] blocks while empty, @racket[read-mvar]
        reads non-destructively (also blocks while empty), and
        @racket[modify-mvar] atomically take-transform-puts while
        holding the filled-permit.}
  @item{@bold{@racket[Chan a].}  An unbounded async channel — sends
        never block, @racket[recv-chan] blocks until a value is
        available.}
]

All operations are total: the only failure modes are deadlocks the
user constructs, which the runtime can't catch.  No polymorphic
@code{(Concurrent m)} class — yet; concurrency is anchored to IO for
now.

@section{Overlapping instances (Phase 37)}

Earlier phases dispatched every positional class method through a
runtime hash table keyed by the value's outer ctor.  That worked
fine when each ctor had at most one instance per class, but quietly
clobbered if two instances shared the same outer ctor — a specific
@racket[(Show (Box Integer))] declared after a generic
@racket[(Show (Box a))] would overwrite the generic in the table,
making @racket[(show (MkBox "hi"))] return the wrong answer.

Phase 37 introduces compile-time specificity-based dispatch for
classes that have overlapping instances:

@codeblock|{
(define-data (Box a) (MkBox a))

(define-instance ((Show a) => (Show (Box a)))
  (define (show b) (match b [(MkBox v) (<> "box(" (<> (show v) ")"))])))

(define-instance (Show (Box Integer))
  (define (show b) "int-box"))

(show (MkBox 99))         ;; → "int-box"
(show (MkBox "hello"))    ;; → "box(\"hello\")"
}|

@itemlist[
  @item{@bold{@racket[find-matching-instance]} picks the unique
        most-specific match among all matching instances.  Instance
        A is @emph{strictly more specific than} B when
        @racket[match-pred] from B's head to A's head succeeds but
        the reverse fails.  Incomparable matches raise an
        ``overlapping instances'' error.}
  @item{@bold{@racket[env-class-has-overlap?]} detects overlap via
        head-unification: any two instances whose heads unify share
        at least one concrete target and so overlap.  Both the
        strictly-more-specific case (Box a vs Box Integer) and the
        incomparable case (P2 Integer b vs P2 a Integer) are caught.}
  @item{@bold{Compile-time impl names for overlap groups.}  When a
        class has overlap, @racket[compile-instance] emits a
        deep-fingerprint impl name (@code{$show:Box_Integer},
        @code{$show:Box_*}) and skips runtime-table registration.
        Call sites with concrete class-arg types resolve at compile
        time via @racket[record-inst-dispatch-use!] (extended here
        to fire for overlap-having classes, not just needs-dict).}
  @item{@bold{Duplicate-instance rejection.}  Two instances with
        α-equivalent heads (e.g. @code{(Eq Integer)} declared twice
        within the same elaboration) raise a compile-time error.
        A class redeclaration via @racket[define-class] clears the
        prior instances for that class so the redeclaration starts
        from a clean slate.}
]

The rule for polymorphic call sites: when the call site's class-arg
is still a tvar after final substitution, no static resolution is
possible and the call falls through to runtime dispatch — which for
overlap-group classes has no entries.  In practice, polymorphic code
over an overlap-group class needs the class constraint in its qual
context so dict resolution can pin the specific impl at the call
site.  Less-common cases may need explicit type annotations.

@section{Bifunctor / Semigroup / Monoid deriving (Phase 38)}

Phase 38 extends the @racket[#:deriving] menu with three more
classes (Traversable deferred — see Not Yet Supported).

@itemlist[
  @item{@bold{@racket[Bifunctor]} for ADTs with at least two type
        parameters.  The penultimate tparam is the first bifunctor
        argument, the last is the second.  For each ctor's fields,
        rebuild via @racket[bimap]: first-tparam fields get @racket[f],
        last-tparam fields get @racket[g], recursive-tcon fields
        recurse via @racket[bimap], anything else passes through.}
  @item{@bold{@racket[Semigroup]} for single-ctor ADTs.  Combine
        fields pairwise via @racket[<>].  The qual context carries
        a @code{(Semigroup ft)} constraint for each field type @code{ft}:
        concrete @code{ft}s (e.g. @racket[String]) get discharged at
        instance-elaboration time; tvar @code{ft}s flow through as
        the instance's actual qual.  Multi-ctor ADTs error — there's
        no canonical way to combine across constructors.}
  @item{@bold{@racket[Monoid]} for single-ctor ADTs.  Synthesizes
        @racket[mempty] as the ctor applied to per-field
        @racket[mempty]s.  Asking for @racket[Monoid] alone
        auto-derives @racket[Semigroup] too (Monoid's superclass).}
]

Two infrastructure fixes shipped alongside:

@itemlist[
  @item{The derivation synthesizers now use a @code{fresh-stx}
        helper to give every @racket[e:var] node a distinct syntax
        identity.  Previously all leaves of a synthesized body
        shared a single @racket[stx], causing
        @racket[current-method-resolutions] (keyed by stx) to alias
        every variable reference to the most-recently-resolved
        class method.  Visible only for class-method-heavy synth
        bodies like Semigroup's @racket[<>] and Monoid's
        @racket[mempty].}
  @item{@racket[instance-qual-return-impls] now skips dict-impl
        passing when the matched instance's @emph{original} qual
        context has no tvar-bearing constraints — concrete-qual
        instances are emitted without a dict-arg lambda, and
        passing dicts to them at the call site would arity-mismatch.}
]

@section{Method-qual dict-threading + user-defined Traversable (Phase 39)}

Phase 30 set up dict-skolems for an instance's qual context
(@code{(MonadState s m) =>} on a lifted instance).  Phase 39 extends
the same mechanism to a method's own qual context: every method
scheme can carry its own @code{=>} clause introducing method-local
quantifiers, and references inside the body to dict-needing methods
on those quantifiers need to flow through dict args.

The canonical case is @racket[traverse]:

@codeblock|{
(define-class (Traversable (t :: (-> * *)))
  (: traverse ((Applicative f) =>
               (-> (-> a (f b)) (-> (t a) (f (t b)))))))
}|

A user-written @racket[(Traversable Tree)] instance body uses
@racket[pure] / @racket[<*>] / @racket[liftA2] on the method-local
@racket[f].  Before Phase 39, these references couldn't be resolved
(no concrete @racket[f] yet; no skolem-dict mapping).  Phase 39 fixes
this with:

@itemlist[
  @item{Skolemize the method-local tvars in the method-qual
        constraints (e.g. @racket[f] in @code{(Applicative f) =>}).
        Apply the skolem-subst to @racket[expected-type] and
        @racket[method-extra-preds] so they're rigid during body
        inference.}
  @item{Build a dict-skolems map for the method-qual constraints
        (same mechanism as @racket[build-dict-skolems] for instance-
        qual), merge with the instance-qual skolems, save the
        combined @racket[dict-arg-names] in
        @racket[current-needs-dict-defs] per method.  Entries are now
        stored as @code{(inst-args . method-args)} so
        @racket[compile-instance] can route differently for the two
        kinds.}
  @item{@racket[compile-instance] for positional class methods now
        picks the dispatch path by which kind of dicts the instance
        carries:
        @itemlist[
          @item{Instance-qual dicts present → named impl
                (@code{$method:Tcon}) + skip runtime-table
                registration.  Compile-time inst-dispatch routes
                the call (Phase 30).}
          @item{Method-qual dicts only → register in the runtime
                dispatch table with method-qual dicts as leading
                lambda params.  The existing class-method wrapper
                already inserts these at call sites via
                @racket[class-info-dictreqs].}
        ]
  }
  @item{@racket[dispatchpos] computation in @racket[handle-class-form]
        now uses @racket[qual-body-deep] to peel @emph{all} qual
        layers before calling @racket[find-dispatch-pos] — without
        this, method-qual @code{=>} clauses hid the method's arrow
        type and any method with a qual context was misclassified as
        @code{'return}-dispatched.}
  @item{Traversable rejoins the @racket[#:deriving] menu (deferred
        in Phase 38).}
]

@section{Numeric tower (Phase 40)}

Phase 40 adds @racket[Rational] and @racket[Complex] types and the
Haskell-style class hierarchy on top of @racket[Num] / @racket[Fractional]:

@itemlist[
  @item{@bold{@racket[Rational]} — exact non-integer rationals backed
        by Racket's exact-rational arithmetic.  Constructed via
        @racket[(make-rational n d)]; @racket[numerator] /
        @racket[denominator] extract.  Instances: @racket[Eq],
        @racket[Ord], @racket[Show], @racket[Num], @racket[Fractional],
        @racket[Real], @racket[RealFrac].}
  @item{@bold{@racket[Complex]} — Racket complex numbers over
        @racket[Float].  Constructed via @racket[(make-complex re im)];
        @racket[real-part] / @racket[imag-part] / @racket[magnitude].
        Instances: @racket[Eq], @racket[Show], @racket[Num],
        @racket[Fractional], @racket[Floating].}
  @item{@bold{@racket[Integral a]} — @code{(Num a) =>}; methods
        @racket[div], @racket[mod], @racket[quot], @racket[rem].
        The previous free-function @racket[div]/@racket[mod] are
        migrated to this class.  Instance: @racket[Integer].}
  @item{@bold{@racket[Real a]} — @code{(Num a) (Ord a) =>}; method
        @racket[to-rational].  Instances: @racket[Integer],
        @racket[Float], @racket[Rational].}
  @item{@bold{@racket[Floating a]} — @code{(Fractional a) =>}; methods
        @racket[pi], @racket[exp], @racket[log], @racket[sqrt],
        @racket[sin], @racket[cos], @racket[tan], @racket[**].  The
        previous free-function @racket[sqrt] is migrated here.
        Instances: @racket[Float], @racket[Complex].}
  @item{@bold{@racket[RealFrac a]} — @code{(Real a) (Fractional a) =>};
        methods @racket[floor-real], @racket[ceiling-real],
        @racket[round-real], @racket[truncate-real] — all producing
        @racket[Integer].  Instances: @racket[Float], @racket[Rational].}
  @item{@bold{@racket[RealFloat a]} — @code{(RealFrac a) (Floating a) =>};
        methods @racket[is-nan?], @racket[is-infinite?],
        @racket[atan2].  Instance: @racket[Float].}
]

@racket[dispatch-tag] gains two new cases: exact non-integer rationals
map to @racket['Rational], non-real numbers map to @racket['Complex].
Existing arithmetic call sites (@racket[+], @racket[-], @racket[*])
work on the new types without modification — the @racket[Num]
instances register through the same dispatch tables.

@section{Not yet supported}

Overlapping instances, kind polymorphism (kind variables), threads /
channels, subprocesses, a fuller numeric tower (rationals, complex,
exact decimals), and bare-var references to dict-needing methods.
These are tracked under later phases.
