#lang scribble/manual
@require[scribble/manual
         (for-label rackton)]

@title[#:tag "do-and-monads"]{do-notation and monads}

Rackton ships the full @racket[Functor] / @racket[Applicative] /
@racket[Monad] hierarchy plus @racket[do]-notation for sequencing.
This chapter assumes you've read @secref["classes"] and
@secref["higher-kinded"].

@section{The Monad class}

A @racket[Monad] is a higher-kinded class providing @racket[>>=] (read
"bind"):

@codeblock|{
(define-class ((Applicative m) => (Monad (m :: (-> * *))))
  (: >>= (-> (m a) (-> (-> a (m b)) (m b)))))
}|

Pronounced: take an @racket[(m a)], a function from @racket[a] to
@racket[(m b)], yield @racket[(m b)].

@section{do-notation}

Writing nested @racket[>>=] calls by hand quickly becomes unreadable:

@codeblock|{
(>>= (Some 3)
     (lambda (x)
       (>>= (Some 4)
            (lambda (y)
              (Some (+ x y))))))
}|

The @racket[do] form is sugar for the same shape:

@codeblock|{
(do [x <- (Some 3)]
    [y <- (Some 4)]
  (Some (+ x y)))
;; ⇒ (Some 7)
}|

Each @racket[[var <- expr]] clause desugars to a nested @racket[>>=]
call binding @racket[var] to the unwrapped result.  The trailing
expression is the final result of the block; its type must be a monad
of the same shape as the preceding clauses.

You can also write a clause without binding:

@codeblock|{
(do (println "hello")
    [name <- read-line]
  (println (string-append "hi, " name)))
}|

A bare expression clause discards its result (via
@racket[(>>= _ (lambda (_) _))]).

@section{Built-in monads}

@itemlist[
@item{@racket[Maybe]   — short-circuiting on @racket[None].}
@item{@racket[List]    — non-determinism / cartesian product.}
@item{@racket[Result e] — short-circuiting on @racket[Err].}
@item{@racket[IO]      — sequential side-effects (see @secref["io-and-mutation"]).}
@item{@racket[State s], @racket[Env r] — pure threaded effects.}
@item{@racket[StateT s m], @racket[EnvT r m], @racket[WriterT w m],
      @racket[ExceptT e m] — lifting any of those effects over an
      inner monad.}
@item{@racket[STM]     — software transactional memory (see @secref["concurrency"]).}
@item{@racket[Identity] — the trivial monad, useful as a base for
      transformer stacks.}]

@section{Return-typed methods: @racket[pure] and @racket[mempty]}

@racket[pure] (in @racket[Applicative]) has no value-typed argument —
its result type alone determines which instance to dispatch to:

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
