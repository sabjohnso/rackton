#lang scribble/manual
@require[scribble/manual
         (for-label rackton)]

@title[#:tag "do-and-monads"]{do-notation and monads}

Rackton ships the full @racket[Functor] / @racket[Applicative] /
@racket[Monad] hierarchy plus @racket[do]-notation for sequencing.
This chapter assumes you've read @secref["classes"] and
@secref["higher-kinded"].

@section{The Monad class}

A @racket[Monad] is a higher-kinded class providing @racket[flatmap]
(the function-first cousin of Haskell's @tt{>>=}):

@codeblock|{
(protocol ((Applicative m) => (Monad (m :: (-> * *))))
  (: flatmap (-> (-> a (m b)) (-> (m a) (m b)))))
}|

Pronounced: take a function from @racket[a] to @racket[(m b)] and an
@racket[(m a)], yield @racket[(m b)].

@section{do-notation}

Writing nested @racket[flatmap] calls by hand quickly becomes
unreadable:

@codeblock|{
(flatmap (lambda (x)
           (flatmap (lambda (y) (Some (+ x y)))
                    (Some 4)))
         (Some 3))
}|

The @racket[do] form is sugar for the same shape:

@codeblock|{
(do [x <- (Some 3)]
    [y <- (Some 4)]
  (Some (+ x y)))
;; ⇒ (Some 7)
}|

Each @racket[[var <- expr]] clause desugars to a nested
@racket[flatmap] call binding @racket[var] to the unwrapped result.
The trailing expression is the final result of the block; its type
must be a monad of the same shape as the preceding clauses.

You can also write a clause without binding:

@codeblock|{
(do (println "hello")
    [name <- read-line]
  (println (string-append "hi, " name)))
}|

A bare expression clause discards its result (via
@racket[(flatmap (lambda (_) _) _)]).

@section{Built-in monads}

@itemlist[
@item{@racket[Maybe]   — short-circuiting on @racket[None].}
@item{@racket[Result e] — short-circuiting on @racket[Err].}
@item{@racket[IO]      — sequential side-effects (see @secref["io-and-mutation"]).}
@item{@racket[State s], @racket[Env r] — pure threaded effects.}
@item{@racket[StateT s m], @racket[EnvT r m], @racket[WriterT w m],
      @racket[ExceptT e m] — lifting any of those effects over an
      inner monad.}
@item{@racket[STM]     — software transactional memory (see @secref["concurrency"]).}
@item{@racket[Identity] — the trivial monad, useful as a base for
      transformer stacks.}]

@racket[List] is a @racket[Functor] and an @racket[Applicative] but
@italic{not} a @racket[Monad] in the prelude.  Use @racket[concat-map]
for non-deterministic sequencing.

@section{Return-typed methods: @racket[pure] and @racket[mempty]}

@racket[pure] (in @racket[Applicative]) has type @racket[(-> a (f a))]:
its single argument fixes @racket[a], but the outer constructor
@racket[f] — the instance — is determined only by the call's expected
return type:

@codeblock|{
(: greet (IO Unit))
(define greet (pure MkUnit))   (code:comment "pure here is (IO Unit)'s pure")

(: many  (Maybe Integer))
(define many  (pure 3))         (code:comment "pure here is Maybe's pure")
}|

When the expected type is ambiguous (e.g., the result of @racket[pure]
is fed to a polymorphic function), use @racket[ann]:

@codeblock|{
((lambda (x) x) (ann (pure 5) (Maybe Integer)))   ;; (Some 5)
}|

@racket[mempty] (in @racket[Monoid]) is similar: its type is the
identity element, ambiguous without context.

@subsection{Polymorphic monadic functions without a signature}

A function whose body produces its result via @racket[pure] (or
@racket[mempty]) over an unconstrained type variable is inferred as
polymorphic in that variable, with the appropriate constraint in its
scheme:

@codeblock|{
(define (madd mx my)
  (do [x <- mx]
      [y <- my]
    (pure (+ x y))))
;; inferred:  madd :: ((Monad m) (Num a) => (-> (m a) (m a) (m a)))
}|

No signature is required.  Internally this routes through the same
dict-passing path as a user-written
@racket[(: madd ((Monad m) (Num a) => …))]: the compiled lambda
acquires leading dict-arg parameters, every recursive call inside the
body re-threads them, and each external call site resolves them to
per-instance impls.  Self-recursive needs-dict functions work without
a signature.

The inferred path only fires when the right-hand side is a lambda.  A
bare value binding such as

@codeblock|{
(define x (pure 5))   ;; rejected: ambiguous use of pure
}|

is still rejected at compile time --- ascribe it
(@racket[(: x (Maybe Integer))]) or wrap it in a function.

@section{MTL-style classes}

The four MTL classes — @racket[MonadState], @racket[MonadEnv],
@racket[MonadWriter], @racket[MonadError] — let you write a single
function body that runs against any monad supporting the effect:

@codeblock|{
(: count-down ((MonadState Integer m) => (m (List Integer))))
(define (count-down)
  (do [n <- get-st]
    (if (<= n 0)
        (pure Nil)
        (do (put-st (- n 1))
            [rest <- (count-down)]
          (pure (Cons n rest))))))
}|

This @racket[count-down] runs against the bare @racket[State Integer]
monad, or against @racket[(StateT Integer IO)], or even
@racket[(EnvT String (StateT Integer Identity))] — the instance is
resolved at each call site.  See @secref["classes" #:doc '(lib "rackton/scribblings/reference/rackton-reference.scrbl")]
for the full method list of each MTL class.
