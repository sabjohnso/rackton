#lang scribble/manual
@require[scribble/manual
         (for-label rackton)
         "../rackton-eval.rkt"]
@(define ev (make-rackton-eval))

@title[#:tag "advanced-types"]{Advanced types}

This chapter covers four type features that go beyond ordinary ADTs:
GADTs, existentials, type aliases, and sealed abstract types, plus
associated type families.

@section[#:tag "existentials"]{Existential types}

An existential parameter on a constructor lets the constructor hide a
type from the outside world.  Use a per-constructor @racket[:forall]
clause to introduce the hidden variable:

@rackton-example[#:eval ev #:mode 'defs #:context? #t]{
(data Anything
  (Wrap :forall (a) a (-> a String)))

(define many
  (Cons (Wrap 42 show)
        (Cons (Wrap "hi" id)
              Nil)))
}

Each @racket[Wrap] inside @racket[many] has a different @racket[a],
but they all coexist in @racket[(List Anything)].  Add a
@racket[:where] clause to require the existential to satisfy protocol
constraints — those become hypotheses inside any clause that matches
the constructor:

@rackton-example[#:eval ev #:mode 'defs]{
(data ExistsShow
  (PackShow :forall (a) :where (Show a) a))
}

Pattern matching introduces a fresh skolem for each clause; the
skolem cannot escape its clause:

@rackton-example[#:eval ev #:mode 'defs #:context? #t]{
(: describe-each (-> (List Anything) (List String)))
(define (describe-each xs)
  (match xs
    [(Nil) Nil]
    [(Cons (Wrap v print) rest)
     ;; OK: print and v share a skolem
     (Cons (print v)
           (describe-each rest))]))
}

@section{Generalised ADTs (GADTs)}

A constructor can refine its result type by giving its full type
signature after a @racket[:].  The signature is an arrow whose final
type is the result and whose leading types are the fields:

@rackton-example[#:eval ev #:mode 'defs #:context? #t]{
(data (Term a)
  (Lit     : (-> a (Term a)))
  (IntLit  : (-> Integer (Term Integer)))
  (BoolLit : (-> Boolean (Term Boolean)))
  (Plus    : (-> (Term Integer) (Term Integer) (Term Integer)))
  (If      : (-> (Term Boolean) (Term a) (Term a) (Term a))))
}

How much a constructor refines the parameter @racket[a] depends on
whether its result type is @emph{concrete} or a @emph{bare variable}:

@itemlist[

@item{@racket[IntLit], @racket[BoolLit], and @racket[Plus] have
concrete result indices (@racket[(Term Integer)], @racket[(Term
Boolean)]).  Matching one refines @racket[a] to that concrete type.}

@item{@racket[(Lit : (-> a (Term a)))] has a bare-variable index equal
to its field type, so it is an ordinary polymorphic constructor — type
@racket[(-> a (Term a))] for every @racket[a].  Matching it refines
nothing: the stored value already has type @racket[a], so you just get
it back.}

@item{@racket[If] is likewise parametric in @racket[a] — its two
branches and its result all share the same @racket[a] — so it refines
nothing either.}

]

Matching on a GADT constructor refines the scrutinee's type parameter
within the clause:

@rackton-example[#:eval ev #:mode 'defs #:context? #t]{
(: eval-term (-> (Term a) a))
(define (eval-term t)
  (match t
    [(Lit     x)        x]      ;; a stays polymorphic
    [(IntLit  n)        n]      ;; here a == Integer
    [(BoolLit b)        b]      ;; here a == Boolean
    [(Plus x y)         (+ (eval-term x) (eval-term y))]
    [(If c then else)   (if (eval-term c) (eval-term then) (eval-term else))]))
}

The refinement is local to the clause, so different clauses can return
different types — exactly what makes a typed interpreter possible.

The same concrete-vs-variable rule governs @emph{field-less}
constructors, which use a non-arrow signature whose lone type is the
result.  A concrete result refines, while a bare-variable result is
universally quantified and refines nothing — exactly like @racket[Nil]
has type @racket[(List a)] for every @racket[a]:

@rackton-example[#:eval ev #:mode 'defs]{
(data (Tagged a)
  (IntTag  : (Tagged Integer))   ;; concrete — refines a ~ Integer
  (BoolTag : (Tagged Boolean))   ;; concrete — refines a ~ Boolean
  (AnyTag  : (Tagged a)))        ;; polymorphic — no refinement

(: width (-> (Tagged a) Integer))
(define (width t)
  (match t
    [(IntTag)  64]
    [(BoolTag) 1]
    [(AnyTag)  0]))   ;; a is unconstrained here
}

The refinement reaches the @emph{whole} clause, not just the matched
value: any other binding in scope whose type mentions the same parameter
is refined too.  That is what lets a refinement flow into a second
argument — for example a continuation whose type is indexed by the same
parameter:

@rackton-example[#:eval ev #:mode 'defs #:context? #t]{
(data (Held a) (Hold a))

;; Matching `term` refines a; the in-scope `box : (Held a)` refines
;; with it, so its contents are usable at the refined type.
(: combine (-> (Term a) (Held a) a))
(define (combine term box)
  (match term
    [(IntLit  n) (match box [(Hold x) (+ x n)])]   ;; a == Integer
    [(BoolLit b) (match box [(Hold x) x])]         ;; a == Boolean
    [_           (match box [(Hold x) x])]))
}

@section[#:tag "promoted-data"]{Promoted data (DataKinds)}

A datatype is @emph{promoted} to the kind level: its name becomes a
kind, and each of its constructors becomes a type-level constructor of
that kind.  Promotion adds type-level identities only; the value-level
datatype is unchanged.  We start with a parameter-free datatype here; a
@seclink["polymorphic-data-kinds"]{parameterised one} promotes too.

This lets a type be @emph{indexed} by structured type-level data, with
the index @emph{kind-checked}.  The standard example is a typed stack
machine whose instructions are indexed by the shape of the operand
stack.  First the tags and the stack shape, as ordinary datatypes:

@rackton-example[#:eval ev #:mode 'defs #:context? #t]{
(data Ty TInt TBool)                  ;; promoted to a kind Ty
(data Stack SEmpty (SPush Ty Stack))  ;; promoted to a kind Stack
}

Now @racket[Code] is indexed by two promoted stack shapes (the stack
before and after running it).  Its kind, @racket[(-> Stack Stack *)], is
@emph{inferred} from the constructors' use of the promoted
@racket[SPush] and @racket[TInt] — no kind annotation is written:

@rackton-example[#:eval ev #:mode 'defs #:context? #t]{
(data (Code s t)
  (HALT  : (Code s s))
  (PUSHI : (-> Integer (Code (SPush TInt s) t) (Code s t)))
  (IADD  : (-> (Code (SPush TInt s) t)
               (Code (SPush TInt (SPush TInt s)) t))))
}

Because the indices are kind-checked, an ill-kinded stack shape is a
compile-time error: @racket[(SPush Integer s)] is rejected because
@racket[Integer] has kind @racket[*], not @racket[Ty], and
@racket[(SPush TInt TInt)] is rejected because @racket[SPush]'s tail must
itself have kind @racket[Stack].  A phantom encoding over kind
@racket[*] could not catch either mistake.

A constructor whose name already denotes a type is left value-only, so
promotion never reinterprets an existing type.

@subsection[#:tag "polymorphic-data-kinds"]{Polymorphic data kinds}

Promotion is not limited to parameter-free datatypes.  A
@emph{parameterised} datatype is promoted as well, and its parameters'
kinds are @emph{generalised}: one promoted structure is reusable at any
element kind (kind polymorphism, the kind-level analogue of ordinary
polymorphism).  Promoting @racket[(data (TList a) …)] makes
@racket[(TList k)] a kind for @emph{any} kind @racket[k]: the promoted
@racket[TNil] is an empty @racket[(TList k)] at any @racket[k], and the
promoted @racket[TCons] takes a @racket[k] and a @racket[(TList k)] and
yields a @racket[(TList k)].

@rackton-example[#:eval ev #:mode 'defs #:context? #t]{
(data (TList a) TNil (TCons a (TList a)))   ;; kind (TList k)
(data (Phantom a) MkPhantom)                ;; kind-polymorphic: k -> * for any k
}

The same promoted @racket[TList] now kind-checks at element kind
@racket[Ty] @emph{and} at element kind @racket[Nat] — @racket[(TCons
TInt TNil)] has kind @racket[(TList Ty)], while @racket[(TCons 5 TNil)]
has kind @racket[(TList Nat)]:

@rackton-example[#:eval ev #:mode 'defs #:context? #t]{
(: at-ty  (Phantom (TCons TInt TNil)))
(define at-ty MkPhantom)
(: at-nat (Phantom (TCons 5 TNil)))
(define at-nat MkPhantom)
}

A type indexed by a promoted @racket[(TList Ty)] is the reusable form of
the stack machine's stack shape above — no bespoke @racket[Stack] /
@racket[SPush] datatype is needed.  Ill-kinded indices are still
rejected: @racket[(TCons TInt TInt)] is a kind error because
@racket[TCons]'s tail must itself be a @racket[(TList k)], not a tag.

A phantom or otherwise-unconstrained type parameter generalises the same
way — @racket[Phantom] above is kind-polymorphic in its parameter, so it
applies both to @racket[*]-kinded types and to higher-kinded ones.

@section{Type aliases}

@rackton-example[#:eval ev #:mode 'defs]{
(define-alias Name           String)
(define-alias (Endo a)       (-> a a))
(define-alias (Pair3 a b c)  (Pair a (Pair b c)))
}

Aliases expand inline during type resolution; they introduce no
runtime cost.  Recursive aliases are rejected with a clear error.

@section{Sealed abstract types}

Adding @racket[:abstract] to a @racket[data] hides the
constructors from the type checker in any module that doesn't define
the type:

@rackton-example[#:eval ev]{
#lang rackton
(provide (data-out Counter) make-counter increment count)

(data Counter :abstract
  (Counter Integer))

(: make-counter (-> Counter))
(define (make-counter) (Counter 0))

(: increment (-> Counter Counter))
(define (increment c) (match c [(Counter n) (Counter (+ n 1))]))

(: count (-> Counter Integer))
(define (count c) (match c [(Counter n) n]))
}

Inside @filepath{counter.rkt} the constructor works normally.  In any
importing module, @racket[Counter] is a type but the @racket[Counter]
constructor is invisible — you must go through @racket[make-counter],
@racket[increment], @racket[count].  This is how you build a true
encapsulation boundary.

@section{Associated type families}

A protocol may declare an associated type via a @racket[:type] clause
inside its body:

@rackton-example[#:eval ev #:mode 'defs #:context? #t]{
(protocol (Container c)
  (:type Elem)
  (: empty? (-> c Boolean))
  (: head   (-> c (Maybe (Elem c)))))
}

Each instance supplies a concrete type for the family with
@racket[(:type (Family = T))]:

@rackton-example[#:eval ev #:mode 'defs #:context? #t]{
(instance (Container (List a))
  (:type (Elem = a))
  (define (empty? xs) (match xs [(Nil) #t] [(Cons _ _) #f]))
  (define (head   xs) (match xs [(Nil) None] [(Cons h _) (Some h)])))
}

Calls to @racket[head] on a @racket[(List Integer)] resolve
@racket[(Elem (List Integer))] to @racket[Integer], and the result type
becomes @racket[(Maybe Integer)] with no further declaration needed.
Associated types are how Rackton lets a protocol abstract over a type
relationship that's more flexible than a fundep.

@section[#:tag "standalone-type-families"]{Standalone type families}

A @racket[type-family] is a top-level type-level function — distinct from
the associated families above, which belong to a protocol.  It is reduced
during type checking, so a value whose declared type mentions a family
checks against the family's @emph{result}.

A @bold{closed} family lists ordered equations; the first whose patterns
match the arguments fires.  (A later equation applies only when every
earlier one is @emph{apart} from the arguments, so reduction stays sound
even on partly-unknown types.)

@rackton-example[#:eval ev #:mode 'defs #:context? #t]{
(data PBool PTrue PFalse)
(type-family (If b t e)
  [PTrue  t e = t]
  [PFalse t e = e])
}

Now @racket[(If PTrue Integer String)] reduces to @racket[Integer]
wherever it appears in a type:

@rackton-example[#:eval ev #:mode 'defs #:context? #t]{
(: yes (If PTrue Integer String))
(define yes 5)
}

A @bold{open} family is declared with no equations and extended by
separate @racket[type-instance] forms.  Its equations must be
@emph{coherent} — no two may overlap — so at most one matches any
argument:

@rackton-example[#:eval ev #:mode 'defs #:context? #t]{
(type-family (Elem c))
(type-instance (Elem String)  = Char)
(type-instance (Elem Boolean) = Boolean)
}

A family's @seclink["guide-kinds"]{kind} is @emph{inferred} from its
equations (here @racket[If] takes a @racket[PBool] then two types), and
applications are kind-checked: @racket[(If Integer Integer String)] is a
kind error, because @racket[If]'s first argument must have kind
@racket[PBool], not @racket[*].  A symbolic application such as
@racket[(If b Integer String)] with an unknown @racket[b] stays
unreduced (rigid) until @racket[b] is known.

Standalone families cross module boundaries: a family declared in one
@tt{#lang rackton} module reduces the same way in any module that
imports it.

@subsection[#:tag "type-level-recursion"]{Recursion}

A closed family whose right-hand side mentions the family itself
@emph{recurses}, computing over promoted data.  Appending two promoted
lists, for instance:

@rackton-example[#:eval ev #:mode 'defs #:context? #t]{
(data (Lst a) LNil (LCons a (Lst a)))
(type-family (Append xs ys)
  [LNil         ys = ys]
  [(LCons x zs) ys = (LCons x (Append zs ys))])
}

@racket[(Append (LCons A LNil) ys)] reduces to @racket[(LCons A ys)],
and a deeper list unwinds one @racket[LCons] per step.  Promoted Peano
naturals recurse the same way — @racket[(type-family (Plus a b) [PZ b =
b] [(PS n) b = (PS (Plus n b))])] computes type-level addition.

Reduction is bounded by a @emph{fuel} budget: a family that never reaches
a base case (or whose recursion grows without limit) stops with a
compile-time error — @racketfont{type-level reduction … exceeded its
budget} — rather than hanging the compiler.  Rackton is not a proof
assistant; the budget trades guaranteed termination for a clear failure.

@margin-note{For @emph{linear} arithmetic on the built-in type-level
@racket[Nat] (the sizes in @racket[(Array n a)]), Rackton uses a
dedicated solver, not recursive families.  Use a promoted Peano datatype
when you need to recurse structurally over a natural; use the built-in
@racket[Nat] (with @racket[+]/@racket[*]) when you need to solve linear
size equations.}

@section[#:tag "data-families"]{Data families}

A @racket[data-family] declares a type constructor with @emph{no}
constructors of its own; each @racket[data-instance] then supplies
constructors for a specific index — and different instances may use
entirely different runtime representations.

@rackton-example[#:eval ev #:mode 'defs #:context? #t]{
(data-family (Arr a))
(data-instance (Arr Boolean) (MkBits Integer))   ;; a bitset
(data-instance (Arr Integer) (MkInts String))    ;; a packed string
}

Each instance's constructors build and match @emph{at that index only} —
@racket[MkBits] makes an @racket[(Arr Boolean)], @racket[MkInts] an
@racket[(Arr Integer)]:

@rackton-example[#:eval ev #:mode 'defs #:context? #t]{
(: popcount (-> (Arr Boolean) Integer))
(define (popcount a) (match a [(MkBits n) n]))
}

The family's @seclink["guide-kinds"]{kind} is inferred from its
instances, so a family indexed by a promoted tag rather than by a
@racket[*]-kinded type works too:

@rackton-example[#:eval ev #:mode 'defs #:context? #t]{
(data Shape SBox SDisc)
(data-family (Render s))
(data-instance (Render SBox)  (RBox  Integer Integer))
(data-instance (Render SDisc) (RDisc Integer))
}

Here @racket[Render] has kind @racket[(-> Shape *)], inferred from the
@racket[SBox]/@racket[SDisc] indices.  Instance heads must be
@emph{coherent} (non-overlapping), and a data family — tcon plus instance
constructors — crosses module boundaries like an ordinary type.

A function that is generic in the family's index cannot pattern-match the
constructors (which constructor is present depends on the index, which is
not yet known) — reach them at a concrete index, or through a protocol
method, exactly as in Haskell.

@section[#:tag "constraint-synonyms"]{Constraint synonyms}

A @racket[define-constraint] names a @emph{conjunction} of constraints,
so a recurring context can be written once and reused:

@rackton-example[#:eval ev #:mode 'defs #:context? #t]{
(define-constraint (Stringy a) (Show a) (Eq a))
}

Now @racket[(Stringy a)] in a @racket[=>] context stands for both
@racket[(Show a)] and @racket[(Eq a)].  It works in both directions: a
@racket[(Stringy a) =>] signature @emph{provides} those components to the
body, and a call @emph{demands} them at the argument type.

@rackton-example[#:eval ev #:mode 'defs #:context? #t]{
(: describe ((Stringy a) => (-> a String)))
(define (describe x) (show x))   ;; uses Show, provided by Stringy
}

@racket[(describe 5)] type-checks because @racket[Integer] has both
@racket[Show] and @racket[Eq].  Synonyms are expanded during constraint
solving — they are not abstract — and they cross module boundaries like
any other declaration.

@section[#:tag "constraint-families"]{Constraint families}

A @racket[constraint-family] @emph{computes} a constraint from type
arguments.  Its clauses are matched in order (like a closed
@seclink["standalone-type-families"]{type family}), but each right-hand
side is a list of constraints rather than a type — and a clause may apply
a @emph{parameter} as a constraint head, so a family can be higher-order
over the constraint it imposes.

The canonical example is @racket[All]: ``constraint @racket[c] holds of
every element of the promoted list @racket[xs]''.

@rackton-example[#:eval ev #:mode 'defs #:context? #t]{
(data (TList a) TNil (TCons a (TList a)))
(constraint-family (All c xs)
  [c TNil         = ]                  ;; empty list: no obligation
  [c (TCons x xs) = (c x) (All c xs)]) ;; head element, then the rest
}

Now @racket[(All Show xs)] expands to a @racket[Show] obligation on every
element of @racket[xs].  Used as a function constraint, it is discharged
at the call site once @racket[xs] is a concrete list:

@rackton-example[#:eval ev #:mode 'defs #:context? #t]{
(data (Proxy a) MkProxy)
(: witness ((All Show xs) => (-> (Proxy xs) Integer)))
(define (witness p) 0)

(: pr (Proxy (TCons Integer (TCons String TNil))))
(define pr MkProxy)
}

@racket[(witness pr)] type-checks: @racket[(All Show (TCons Integer (TCons
String TNil)))] reduces to @racket[(Show Integer)] and @racket[(Show
String)], both satisfied.  A list element lacking the instance is a
compile-time error.

Reduction is recursive and bounded by the same @seclink["type-level-recursion"]{fuel
budget} as type families, so a family that never reaches a base case
fails clearly instead of hanging.  Constraint families cross module
boundaries like other declarations.
