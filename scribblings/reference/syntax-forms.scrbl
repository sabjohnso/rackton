#lang scribble/manual
@require[scribble/manual
         (for-label rackton)]

@title[#:tag "syntax-forms"]{Syntax forms}
The forms in this chapter are recognised by the Rackton surface parser
and may appear inside any @racket[(rackton …)] block, inside a
@racket[(module @#,racketidfont{name} rackton …)] body, or inside a
@hash-lang[] @racketmodfont{rackton} file.  Forms are grouped by role:
@seclink["sf-top"]{top-level declarations}, @seclink["sf-exprs"]{expressions},
and @seclink["sf-patterns"]{patterns}.

@section[#:tag "sf-top"]{Top-level declarations}

@defform*[[(define name expr)
           (define (name p ...) body)]
          #:contracts ([name id?])]{

Binds @racket[name] at module scope.  The first form binds
@racket[name] to the value of @racket[expr]; the second is shorthand
for @racket[(define name (lambda (p ...) body))].  Top-level
@racket[define] is recursive: @racket[expr] may refer to
@racket[name].  When a matching @racket[(: name type)] declaration is
in scope, the declared type is skolem-checked against the body.

Each parameter @racket[p] is either a bare identifier (binds a
plain parameter) or a parenthesised pattern (destructures the
argument):

@codeblock|{
(struct Point [x : Float] [y : Float])
(define (distance (Point px py) (Point qx qy))
  (sqrt (+ (sqr (- px qx)) (sqr (- py qy)))))
}|

A single-form definition with a destructuring pattern desugars
to an irrefutable match: if the argument doesn't fit the pattern
the call raises a runtime error.  For dispatch over multiple
constructors, use the multi-clause form below.

Multiple @racket[(define (name p ...) body)] forms for the same
@racket[name] combine into a single function that pattern-matches
across every parameter position in source-order priority — a
Haskell-style equational definition:

@codeblock|{
(data MyList Nada (Mcons Integer MyList))
(define (myhead (Mcons x _)) x)
(define (myhead Nada)        0)
}|

In a multi-clause context a bare uppercase identifier @racket[Nada]
dispatches as a 0-arg constructor pattern (matching the
@racket[match] convention); use @racket[(Nada)] explicitly if you
prefer the parenthesised form.  All clauses must have the same
arity.  Combining a function-form @racket[(define (g x) …)] with a
value-form @racket[(define g 5)] for the same name is rejected at
parse time.}

@defform[#:literals (:)
         (: name type)
         #:contracts ([name id?])]{

Declares a polymorphic or monomorphic type signature for the
matching @racket[(define name …)].  Free type variables in
@racket[type] are implicitly universally quantified, or an explicit
@racket[(All (a ...) type)] form may be used.  The signature may
appear before or after the matching @racket[define] in source
order — top-level forms in a Rackton module are order-invariant
(see @secref["values-and-types" #:doc '(lib "rackton/scribblings/guide/rackton-guide.scrbl")]
for the full ordering story).

@racketblock[
(: fact (-> Integer Integer))
(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))]}

@defform[#:literals (All)
         (All (a ...) type)]{

Universal quantifier for type signatures.  Binds the type variables
@racket[a ...] in @racket[type].  Equivalent to leaving the variables
free in a top-level @racket[:] declaration, except that an explicit
@racket[All] in a parameter position introduces a rank-N quantifier
that is not generalised at the surrounding @racket[define].

@racketblock[
(: apply-twice (All (a) (-> (-> a a) (-> a a))))
(define (apply-twice f) (lambda (x) (f (f x))))]}

@defform[
         (data (Name a ...) ctor-spec ... maybe-deriving)
         #:grammar
         [(ctor-spec       bare-ctor (Ctor type ...))
          (bare-ctor       id)
          (maybe-deriving  code:blank (code:line #:deriving class ...))]]{

Declares an algebraic data type @racket[Name] parameterised by
@racket[a ...], with one or more constructors.  Each constructor is
either a bare identifier (nullary) or a parenthesised form naming the
constructor and its field types.

The optional @racket[#:deriving] clause synthesises instances of the
named classes; see @racket[Eq], @racket[Ord], @racket[Show],
@racket[Functor], @racket[Foldable], @racket[Traversable],
@racket[Bifunctor], @racket[Semigroup], @racket[Monoid], and the
auto-derived lens and prism families.

A type may also be marked @racket[#:abstract] (before the constructors),
which hides its constructors from the type checker in importing
modules even when listed in a @racket[(provide …)] form.

A constructor may carry its own existential quantifier and constraint
context via per-constructor @racket[#:forall] / @racket[#:where]
clauses (see
@seclink["existentials" #:doc '(lib "rackton/scribblings/guide/rackton-guide.scrbl")]{the guide}),
and may give a full type signature after a @racket[:] to assert a
refined return type (GADT-style): @racket[(Ctor : (-> field … Result))],
where the arrow's final type is the result and the leading types are
the fields.  A field-less GADT constructor uses a non-arrow signature
@racket[(Ctor : Result)].

@racketblock[
(data ExistsShow
  (PackShow #:forall (a) #:where (Show a) a))]}

@defform[
         (newtype Name (MkName Type))]{

Declares a single-field, single-constructor wrapper type with no
runtime overhead beyond a struct tag.  Equivalent to
@racket[(data Name (MkName Type))] but documents intent.}

@defform*[
          [(struct Name [field : type] ... maybe-deriving)
           (struct (Name a ...) [field : type] ... maybe-deriving)]
          #:grammar
          ([maybe-deriving  code:blank (code:line #:deriving class ...)])]{

Declares a single-constructor product type with typed named fields and
auto-generated field accessors @racketidfont{Name}@racketidfont{-}@racket[_field].
The parameterised form supports polymorphic structs whose accessors
have the appropriate polymorphic schemes.

@racketblock[
(struct Point [x : Integer] [y : Integer])
(define p (Point 3 4))
(define px (Point-x p))]

@racket[#:deriving] honours the same classes as
@racket[data], including @racket[Foldable] and the auto-derived
field-lens family.}

@defform[
          (protocol class-head method ...)
          #:grammar
          ([class-head    (ClassName param ...)]
           [param         id
                          (id :: kind)
                          (id => bound ...)]
           [bound         ClassName
                          (ClassName type ...)]
           [method        (code:line (: method-name type))
                          (code:line (define (method-name p ...) body))
                          (code:line #:requires constraint ...)
                          (code:line #:fundep var ... -> var ...)
                          (code:line (#:derive SuperClass (define …) ...))]
           [constraint    (ClassName type ...)])]{

Declares a type class.  Each method signature
(@racket[(: name type)]) is added to the value environment with a
qualified scheme @racket[(All (param ...) ((C param ...) => τ))], so a
polymorphic use of the method automatically carries the class
constraint.  Default method bodies (@racket[(define …)]) are used by
instances that omit the corresponding method.

A superclass requirement is written as a @deftech{bound} on the
parameter it constrains: @racket[(id => bound ...)].  A bare
@racket[ClassName] bound on @racket[id] desugars to the constraint
@racket[(ClassName id)]; a partially-applied bound
@racket[(ClassName type ...)] appends @racket[id] as the final
argument, so @racket[(b => (Convert a))] desugars to
@racket[(Convert a b)].  Several bounds may be stacked on one
parameter (@racket[(a => Num Ord)]).  A superclass that relates several
parameters at once — and so cannot be attached to a single
parameter — is written as a trailing @racket[#:requires] clause in the
body, listing the constraints directly.

Class parameters may carry an explicit kind annotation
(@racket[(param :: kind)]); without one the kind defaults to
@racket[*], unless a superclass determines it.  Wherever a parameter
appears directly as an argument of a superclass constraint — whether
as a bound's subject (@racket[[w => Functor]], i.e. @racket[(Functor
w)]) or mentioned inside another parameter's bound (@racket[[g =>
(Pairing f)]], i.e. @racket[(Pairing f g)], which pins @racket[f] as
well) — it inherits the kind of the corresponding parameter of that
superclass.  Kinds are written as @racket[*] for ordinary types or
@racket[(-> k1 k2)] for type-constructor kinds.

A class may declare one or more functional dependencies via
@racket[#:fundep] clauses inside the body.

A @racket[#:derive] clause declares a @deftech{cross-class derivation}:
canonical bodies that fill a @emph{superclass}'s methods in terms of
@emph{this} class's methods.  It is the inter-class analogue of a default
method.  For example, @racket[Monad] derives its @racket[Functor] and
@racket[Applicative] superclasses from @racket[flatmap] and @racket[pure].
The bodies are consumed only when an instance opts in with
@racket[#:derive-superclasses] (see @racket[instance]).  A
@racket[#:derive] table is available within the module that declares the
class; it is not currently serialized across module boundaries, so a
user-defined class's derivations apply only to instances written in the
same file (the built-in monad stack works everywhere).}

@defform*[
          [(instance head method ...)
           (instance (qual ... => head) method ...)
           (instance head #:derive-superclasses method ...)
           (instance (qual ... => head) #:derive-superclasses method ...)]
          #:grammar
          ([head    (ClassName type ...)]
           [qual    (ClassName var ...)]
           [method  (define (name p ...) body)])]{

Provides an implementation of @racket[ClassName] for the given
type(s).  The optional context @racket[(qual ... => head)] introduces
hypothesis constraints that become available to the body and that
must be discharged at each use site.

Omitted methods fall back to the class's default implementations.

A method may be written in @deftech{point-free} form — @racket[(define
method fn)], aliasing an existing function — instead of spelling out
@racket[(define (method arg ...) body)].  This works even when
@racket[fn] is a top-level @racket[define] that appears after the
instance.  (The sole exception is an arity-0 return-typed method such as
@racket[mempty]: alias it to a self-contained value, or define the
aliased binding before the instance.)

With @racket[#:derive-superclasses], the instance bundles only the
irreducible primitives, and the compiler synthesizes the missing
superclass instances from the class's @tech{cross-class derivation}
table.  Writing a @racket[Monad] instance this way, for example, requires
only @racket[pure] and one of @racket[flatmap]/@racket[join] — the
@racket[Functor] and @racket[Applicative] instances are generated
(inheriting the same context).  A superclass that already has an instance
— hand-written or imported — is left untouched.  Note that @racket[pure]
cannot be derived from @racket[Monad] (none of @racket[fmap],
@racket[flatmap], @racket[join] can manufacture an @racket[(f a)] from a
bare @racket[a]), so it must always be supplied; omitting it is a
compile-time error.

Instances always escape regardless of @racket[provide]; coherence is a
module-level property.}

@defform*[
          [(define-alias Name type)
           (define-alias (Name a ...) type)]]{

Declares a type alias.  Aliases expand inline during type resolution;
they introduce no runtime cost.  Recursive aliases are rejected with
a clear error.

@racketblock[
(define-alias Name           String)
(define-alias (Endo a)       (-> a a))
(define-alias (Pair3 a b c)  (Pair a (Pair b c)))]}

@defform[
         (define-effect Name op ...)
         #:grammar
         ([op  (op-name arg-type ... -> result-type)])]{

Declares an algebraic effect.  Each operation @racket[op-name] becomes
a callable that, when invoked under a matching @racket[handle], aborts
to the handler.  Effects are not tracked in types; an operation
invoked outside any handler is a runtime error.}

@defform*[#:literals (=)
          [(code:line #:type Family)
           (code:line #:type (Family = T))]]{

Associated type family declaration and definition.

A @racket[(#:type Family)] clause inside a @racket[protocol] body
declares the family.  Each instance must supply a concrete type with a
matching @racket[(#:type (Family = T))] clause inside its
@racket[instance] body, where @racket[T] is the type that the
family resolves to for that instance head.

@racketblock[
(protocol (Container c)
  (#:type Elem)
  (: empty? (-> c Boolean))
  (: head   (-> c (Maybe (Elem c)))))

(instance (Container (List a))
  (#:type (Elem = a))
  (define (empty? xs) (match xs [(Nil) #t] [(Cons _ _) #f]))
  (define (head   xs) (match xs [(Nil) None] [(Cons h _) (Some h)])))]}

@section[#:tag "sf-exprs"]{Expressions}

@deftogether[(
  @defform[(lambda (p ...) body)]
  @defform[(λ (p ...) body)])]{

Anonymous function with parameters @racket[p ...] and result
@racket[body].  Currying is implicit: @racket[(lambda (x y) e)] has
type @racket[(-> a (-> b c))], not a tupled domain.}

@defform*[[(let (binding ...) body)
           (let loop ([var init] ...) body)]
          #:grammar ([binding [pattern expr]])]{

Parallel binding.  Each @racket[expr] is evaluated in the surrounding
scope; the resulting values bind the variables of each @racket[pattern]
in @racket[body].  A bare-identifier pattern is generalised independently
against the surrounding environment after inference, giving Rackton its
let-polymorphism; any other pattern destructures its value via an
irrefutable match (a failure panics — use only irrefutable patterns,
such as a single-constructor type or a @racket[struct]).  Because every
@racket[expr] is evaluated in the surrounding scope, a binding cannot see
another binding's pattern variables (use @racket[let*] for that).

@racketblock[
(let ([(MkPair a b) (MkPair 3 4)]
      [n            10])
  (+ a (+ b n)))]

The second, @emph{named} form is a loop: @racket[loop] is bound in
@racket[body] to a recursive procedure of the @racket[var ...], invoked
with the @racket[init] seeds.  It desugars to
@racket[(letrec ([loop (lambda (var ...) body)]) (loop init ...))].

@racketblock[
(let loop ([i 0] [acc 0])
  (if (> i 10) acc (loop (+ i 1) (+ acc i))))]}

@defform[(letrec ([var expr] ...) body)]{

Mutually recursive binding: every right-hand side sees all the names
in scope.  Each binding is generalised independently against the
surrounding environment after inference.

@racketblock[
(letrec ([even? (lambda (n) (if (== n 0) #t (odd?  (- n 1))))]
         [odd?  (lambda (n) (if (== n 0) #f (even? (- n 1))))])
  (even? 8))]}

@defform[(let* (binding ...) body)
         #:grammar ([binding [pattern expr]])]{

Sequential binding, as in Lisp/Scheme: each @racket[expr] is evaluated
and type-checked in the scope of all the preceding bindings.  Equivalent
to nested singleton @racket[let]s.  Like @racket[let], a binding may
destructure with a @racket[pattern] (an irrefutable match); unlike
@racket[let], a later @racket[expr] may reference the variables bound
earlier.}

@defform[(if test then else)]{

Three-armed conditional.  @racket[test] must have type @racket[Boolean];
@racket[then] and @racket[else] must have the same type, which becomes
the type of the @racket[if].}

@defform[(cond [test expr] ... [else expr])]{

Multi-way conditional.  Equivalent to nested @racket[if]s with an
@racket[else] catch-all.  Each @racket[test] must have type
@racket[Boolean] and each @racket[expr] must have the same type.}

@defform[(match scrutinee clause ...)
         #:grammar
         [(clause  [pattern body]
                   [pattern (=> guard) body]
                   [pattern #:when guard body])]]{

Pattern-matches @racket[scrutinee] against each @racket[pattern] in
order, evaluating the first matching @racket[body].  Patterns may
guard their match with an arbitrary @racket[Boolean]-typed
@racket[guard] expression.

@racket[match] is checked at compile time and rejected if it omits a
constructor of an ADT scrutinee, omits @racket[#t] or @racket[#f] on a
@racket[Boolean] scrutinee, or lacks a catchall on an unconstrained
scrutinee.  Add a wildcard (@racket[_]) or variable pattern to opt
out.}

@defform[(do clause ... body)
         #:grammar
         [(clause  [var <- expr]
                   [_   <- expr]
                   expr)]]{

Monadic do-notation.  Each @racket[[var <- expr]] clause desugars to a
nested @racket[flatmap] call; a bare-expression clause sequences
without binding.  The trailing @racket[body] is the final computation; its
type is a monad of the same shape as the preceding clauses.

@racketblock[
(do [x <- (Some 3)]
    [y <- (Some 4)]
  (Some (+ x y)))]}

@defform[(let& ([var expr] ...) body)]{

Sequential monadic binding (in the spirit of OCaml's @tt{let*}, with a
different introducer since @racket[let*] already has a meaning in
Scheme).  Each @racket[expr] is a monadic value @racket[(m a)]; @racket[var]
binds the unwrapped @racket[a] and is in scope for the later
@racket[expr]s and @racket[body].  Desugars to a nested @racket[flatmap]
chain — the same family as @racket[do], in binding-list shape.  The
trailing @racket[body] is the final monadic computation.

@racketblock[
(let& ([a (Some 1)]
       [b (Some (+ a 1))])
  (Some (+ a b)))]}

@defform*[[(let% ([var expr] ...) body)
           (let% loop ([var expr] ...) body)]]{

Parallel (independent) monadic binding.  The @racket[expr]s may not
reference one another; they are gathered with @racket[product] and the
result is fed through @racket[flatmap] into the monadic @racket[body].
Requires @racket[(Monad m)].

The named form is a monadic loop: @racket[loop]'s parameters are the
monadic values, recombined via @racket[product] on every iteration, so
the body may recurse with fresh monadic values.

@racketblock[
(let% ([a (Some 10)]
       [b (Some 20)])
  (Some (+ a b)))]}

@defform[(let+ ([var expr] ...) body)]{

Applicative binding (OCaml's @tt{let+} / @tt{and+}).  Like @racket[let%]
the @racket[expr]s are independent and gathered with @racket[product],
but @racket[body] is a @emph{pure} expression mapped in with
@racket[fmap]; the result is wrapped by the functor.  Requires only
@racket[(Applicative f)], and has no named form.

@racketblock[
(let+ ([a (Some 4)]
       [b (Some 5)])
  (+ a b))]}

@defform[(list elem ...)]{

List-literal sugar.  Desugars to a @racket[Cons]/@racket[Nil] chain, so
@racket[(list 1 2 3)] is @racket[(Cons 1 (Cons 2 (Cons 3 Nil)))] and
@racket[(list)] is @racket[Nil].  The result is an ordinary
@racket[(List a)]; all elements must share one type.}

@defform[(ann expr type)]{

Type ascription.  Asserts that @racket[expr] has type @racket[type];
the inferred type must unify with the annotation.  Useful for
disambiguating return-typed methods such as @racket[pure] or
@racket[mempty] at concrete return types.}

@defform[(update record [field value] ...)
         #:contracts ([field id?])]{

Functional record update.  Returns a new struct of the same type as
@racket[record], replacing the named fields with the supplied values.
Untouched fields are copied verbatim.

@racketblock[
(struct Point [x : Integer] [y : Integer])
(define p1 (Point 3 4))
(define p2 (update p1 [x 99]))     (code:comment "Point with x=99, y=4")]}

@defform[(escape expr)]{

Internal form used by some derived-instance bodies.  User code should
prefer @racket[racket] (see below) for host-language interop.}

@defform[(racket type (var ...) body ...)
         #:contracts ([type type?] [var id?])]{

Drops into raw Racket, returning a value typed as @racket[type].  The
named Rackton bindings @racket[var ...] are spliced into @racket[body]
unmodified.  Multiple @racket[body] forms are wrapped in an implicit
@racket[begin].

@racketblock[
(: greet (-> String String))
(define (greet name)
  (racket String (name)
    (string-append "hello " name)))]

The escape is the only way for Rackton code to reach Racket's standard
library beyond what the prelude exposes.  No type-checking is performed
on @racket[body]; the type assertion is taken on faith.}

@defform[(handle expr op-clause ... return-clause)
         #:grammar
         ([op-clause     [op-name (param ...) k-name -> body]]
          [return-clause [return result-var -> body]])]{

Handles algebraic effects raised inside @racket[expr].  Each
@racket[op-clause] names an operation @racket[op-name], binds its
parameters @racket[param ...] and the captured continuation
@racket[k-name], and evaluates @racket[body] in their scope.  The
@racket[return] clause runs on the result of @racket[expr] when it
completes without performing an unhandled operation.

The handler is @italic{deep}: the prompt is re-installed on every
resumption, so a resumed continuation can perform further operations
under the same handler.  See @secref["effects" #:doc '(lib "rackton/scribblings/guide/rackton-guide.scrbl")]
for the full story.}

@section[#:tag "sf-patterns"]{Patterns}

A pattern — used by @racket[match] and by @racket[let] / @racket[let*]
bindings — is one of:

@itemlist[
@item{@racket[_] — wildcard; matches any value, binds nothing.}
@item{a lowercase identifier — variable binding; matches any value
      and binds the given name to it.}
@item{a numeric, boolean, string, character, or bytes literal — matches
      only equal values.}
@item{@racket[Ctor] — nullary constructor pattern; matches only values
      built with @racket[Ctor].}
@item{@racket[(Ctor sub-pat ...)] — n-ary constructor pattern; matches
      only values built with @racket[Ctor] whose corresponding fields
      match the sub-patterns.}]

@section{Module forms}

@defform[(require spec ...)
         #:grammar
         [(spec  module-path
                 (only-in module-path name ...))]]{

Imports bindings from another Rackton or Racket module.  When the
target is a @hash-lang[] @racketmodfont{rackton} module (or any
module that emits a @racketmodfont{rackton-schemes} submodule), the
type checker also recovers its schemes, data constructors, type
constructors, classes, and instances.  Plain Racket modules are
imported at runtime only; their bindings are invisible to the type
checker.}

@defform[(provide spec ...)]{

Declares export specifications for the current module.  See
@secref["provide-specs"] for the supported spec forms.}

@defform*[#:literals (->)
          [(foreign name type #:from module-path)
           (foreign name type #:from module-path #:as racket-id)]]{

Imports a host (Racket) binding and gives it a Rackton @racket[type], so
Rackton code can use a primitive that the prelude does not surface (for
example something from @racketmodname[racket/string] or
@racketmodname[racket/set]).  @racket[module-path] is a Racket module
path (a collection path like @racketmodname[racket/string], or a
@racket[_string] for a relative file).  Without @racket[#:as] the Racket
binding has the same name as @racket[name]; with @racket[#:as
racket-id] the host binding @racket[racket-id] is bound to the Rackton
@racket[name].

The declared @racket[type] is the @emph{trust boundary} — it is
@bold{not} checked against the host binding (FFI-style).  An incorrect
type, or a host value whose runtime representation does not match the
Rackton type (e.g. a Racket list where a Rackton @racket[List] is
claimed), is a bug the type checker cannot catch.  Calls must also match
the host binding's arity (a curried @racket[type] is fine, but partially
applying a strict host function will raise at runtime).

@racketblock[
(foreign string-contains? (-> String (-> String Boolean))
         #:from racket/string)
(foreign str-replace (-> String (-> String (-> String String)))
         #:from racket/string #:as string-replace)]}

@defform[#:literals (->)
         (foreign-c name type #:lib lib #:symbol symbol
                    #:sig (ctype ... -> ctype))]{

Imports an external C function directly, lowering to
@racket[get-ffi-obj] from @racketmodname[ffi/unsafe] (it is sugar for a
hand-written @racket[ffi/unsafe] shim imported via @racket[foreign]).
@racket[lib] is the shared library: @racket[#f] for the running process
(libc, and whatever it already links such as libm), or a @racket[_string]
passed to @racket[ffi-lib].  @racket[symbol] is the C symbol name (a
@racket[_string]).  @racket[#:sig] gives the C signature as type keywords
(@racketidfont{double}, @racketidfont{int}, @racketidfont{string},
@racketidfont{pointer}, @racketidfont{byte}, @racketidfont{void}) with a
single @racket[->] splitting the argument types from the result.

The Rackton @racket[type] is the trust boundary, exactly as for
@racket[foreign].  Whether the binding is a pure function or an
@racket[IO] action is read from @racket[type]: if the result (after the
signature's argument arrows) sits in @racket[IO], the binding is an
@racket[IO] action (a value when there are no arguments); otherwise it is
a pure function.

@racketblock[
(foreign-c c-cbrt (-> Float Float)
           #:lib #f #:symbol "cbrt" #:sig (double -> double))
(foreign-c c-getpid (IO Integer)
           #:lib #f #:symbol "getpid" #:sig (-> int))]

Like @racket[foreign] this is @bold{unsafe}: a wrong signature or library
crashes the process.  Versioned shared-library sonames (e.g.
@tt{libm.so.6}) are awkward to name with a single @racket[#:lib] string;
for those, prefer a @racket[get-ffi-obj] shim with a version list (see
@racketmodname[rackton/foreign/c]) imported via @racket[foreign].}
