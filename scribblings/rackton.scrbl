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
                               number->string string->number)]]

@title{Rackton}
@author{sbj}

@defmodule[rackton]

@bold{Rackton} is a Racket adaptation of the Coalton statically-typed functional
language.  It embeds a small Hindley–Milner core — type inference, let-polymorphism,
algebraic data types, and pattern matching — inside Racket, either as an
@racket[(rackton ...)] form inside an ordinary module or as a whole-file
@hash-lang[] @racketmodfont{rackton} program.

This documentation describes the @bold{Phase 1 + 2 + 3 + 4 + 5 + 6 + 7
+ 8 + 9 + 10 + 11 + 12 + 13 + 14 + 15 + 16 + 17 + 18} subset.  Phase
18 added return-typed class-method dispatch, so @racket[pure] is now
a real class method on @racket[Applicative].  Phase 17 turned
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

@section{Not yet supported}

Overlapping instances, kind polymorphism (kind variables), threads /
channels, subprocesses, a fuller numeric tower (rationals, complex,
exact decimals), and @racket[Traversable] (needs polymorphic-
instance-body specialization beyond Phase 18's concrete-call-site
resolution).  These are tracked under later phases.
