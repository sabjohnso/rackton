#lang scribble/manual
@require[scribble/manual
         (for-label rackton)
         "../rackton-eval.rkt"]
@(define ev (make-rackton-eval))

@title[#:tag "effects"]{Algebraic effects}

Rackton supports first-class algebraic effects via @racket[define-effect]
and @racket[handle].  An effect declares a set of operations; a handler
interprets those operations, optionally resuming the suspended
computation via a captured continuation.

@section{A first effect}

@rackton-example[#:eval ev #:mode 'value]{
(define-effect Env
  (ask -> Integer))

(: prog-env (-> Unit Integer))
(define (prog-env _)
  (+ (ask) (ask)))

(: run-env (-> Integer (-> Unit Integer) Integer))
(define (run-env val prog)
  (handle (prog Unit)
    [ask () k -> (k val)]
    [return v -> v]))

(run-env 7 prog-env)
}

The clause @racket[[ask () k -> (k val)]] runs on each invocation of
@racket[ask].  The captured continuation @racket[k] is a function
expecting the value @racket[ask] should evaluate to; resuming it by
calling @racket[(k val)] continues the computation with @racket[val]
in place of @racket[ask].

@section{The shape of a handler}

@rackton-example[#:eval ev #:mode 'display]{
(handle expr
  [op-name (param ...) k-name -> body]   (code:comment "for each op")
  ...
  [return result-var -> body])           (code:comment "for normal return")
}

@itemlist[
@item{Each @racket[op-clause] names an operation, binds its parameters,
      and binds the continuation @racket[k-name].  The clause body
      decides whether to resume (call @racket[k-name]), ignore (don't
      call it), or call it multiple times.}
@item{The @racket[return] clause runs on the value @racket[expr]
      produces when no unhandled effect remains.  Use it to massage
      the final result.}]

The prompt is re-installed on every resumption — handlers are
@bold{deep}, so a resumed continuation may perform further operations
under the same handler.

@section{Classic effects}

@subsection{Counter — peek-and-bump}

@rackton-example[#:eval ev #:mode 'display]{
(define-effect Counter
  (peek -> Integer)
  (bump -> Unit))

(: run-counter (-> Integer (-> Unit a) a))
(define (run-counter start prog)
  (letrec
    ([loop (lambda (n)
             (handle (prog Unit)
               [peek () k -> ((loop n) (k n))]
               [bump () k -> ((loop (+ n 1)) (k Unit))]
               [return v -> v]))])
    (loop start)))
}

@subsection{Exception — abort with a fallback}

@rackton-example[#:eval ev #:mode 'display]{
(define-effect Exn
  (raise-e -> Integer))

(: run-exn (-> Integer (-> Unit Integer) Integer))
(define (run-exn fallback prog)
  (handle (prog Unit)
    [raise-e () _ -> fallback]      (code:comment "don't resume; return fallback")
    [return v     -> v]))
}

@section{Limitations and design notes}

@itemlist[
@item{@bold{Effects are not tracked in types.}  An operation invoked
      outside any handler is a runtime error.  Row polymorphism to
      propagate effect signatures through function types is a possible
      future extension; today, programmer discipline is required.}
@item{The program passed to a handler should be a thunk (a function
      taking @racket[Unit]), so operations aren't performed before
      the prompt is installed.}
@item{0-arg operations are typed @racket[(-> Unit T)] internally; call
      sites @racket[(op)] receive an implicit @racket[Unit].}]

@section{Effects vs. monad transformers}

@racket[State], @racket[Env], @racket[Writer], @racket[Except] are
also available as monad transformers (see @secref["do-and-monads"]).
The choice is a matter of style:

@itemlist[
@item{@bold{Monads / transformers} give you static effect tracking,
      composable algebraic structure, and a richer set of operations
      via the MTL classes.  They are the choice for most code.}
@item{@bold{Algebraic effects} give you direct delimited continuations
      (one-shot or multi-shot), more flexible handler composition, and
      a simpler mental model for some patterns (e.g., backtracking
      search).  They are the choice when you specifically need
      first-class continuations.}]
