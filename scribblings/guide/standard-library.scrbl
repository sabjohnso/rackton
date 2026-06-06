#lang scribble/manual
@require[scribble/manual
         (for-label rackton
                    rackton/data/list
                    rackton/data/map
                    rackton/data/monoid
                    rackton/data/semigroup
                    rackton/data/lazy
                    rackton/control/monad
                    rackton/control/monad/state
                    rackton/control/monad/trans
                    rackton/numeric/show
                    rackton/text/string
                    rackton/text/printf
                    rackton/system
                    rackton/batteries)
         "../rackton-eval.rkt"]
@(define ev (make-rackton-eval))

@title[#:tag "stdlib-guide"]{The standard library}

Rackton's auto-imported prelude is deliberately small — the class
hierarchy (@racket[Eq], @racket[Ord], @racket[Functor], @racket[Monad],
…), the core ADTs (@racket[Maybe], @racket[List], @racket[Pair],
@racket[Either]), the numeric tower, and a handful of combinators.
Everything else lives in modules you @racket[require] explicitly, laid
out to mirror Haskell's @tt{base}:
@tt{rackton/data}, @tt{rackton/control}, @tt{rackton/numeric},
@tt{rackton/text}, and @tt{rackton/system}.

This section is a tour.  For the exhaustive module-by-module list, see
@secref["stdlib"
        #:doc '(lib "rackton/scribblings/reference/rackton-reference.scrbl")]
in the reference.

@section{Importing}

Require the specific module you need — that keeps dependencies explicit
and compile times down:

@rackton-example[#:eval ev]{
#lang rackton
(require rackton/data/list)

(: evens (List Integer))
(define evens (filter (lambda (n) (== (mod n 2) 0)) (list 1 2 3 4 5 6)))
}

For scripts and exploration, @racketmodname[rackton/batteries]
re-exports the whole library in one import:

@rackton-example[#:eval ev #:mode 'display]{
#lang rackton
(require rackton/batteries)
}

@section{@tt{data} — containers and structures}

@racketmodname[rackton/data/list] is the Data.List toolkit
(@racket[sort], @racket[take], @racket[zip], @racket[group-by],
@racket[concat-map], …).  @racketmodname[rackton/data/map] and
@racketmodname[rackton/data/set] give immutable @racket[Map] / @racket[Set]
with the usual operations, and @racketmodname[rackton/data/maybe] /
@racketmodname[rackton/data/either] round out the small sum types:

@rackton-example[#:eval ev #:mode 'defs]{
(require rackton/data/map)

;; tally occurrences with map-insert-with
(: counts (Map String Integer))
(define counts
  (foldr (lambda (w m) (map-insert-with + w 1 m))
         empty-map
         (list "a" "b" "a")))
}

The @racket[Monoid] wrappers live in
@racketmodname[rackton/data/monoid] (@racket[Sum], @racket[Product],
@racket[All], @racket[Any], @racket[Endo], @racket[Dual]) and the
@racket[Semigroup] selectors in @racketmodname[rackton/data/semigroup]
(@racket[Min], @racket[Max], @racket[First], @racket[Last]).

@section{@tt{control} — applicative, monad, transformers}

@racketmodname[rackton/control/monad] collects the monad combinators
(@racket[map-m], @racket[sequence-m], @racket[fold-m], @racket[filter-m],
…) that work over any @racket[Monad].  The monad transformers each have a
module — @racketmodname[rackton/control/monad/state],
@racketmodname[rackton/control/monad/reader],
@racketmodname[rackton/control/monad/writer],
@racketmodname[rackton/control/monad/except] — and
@racketmodname[rackton/control/monad/trans] supplies the
@racket[MonadTrans] (@racket[lift]) and @racket[MonadIO]
(@racket[lift-io]) instances that stack them:

@rackton-example[#:eval ev #:mode 'defs]{
(require rackton/control/monad/state)

;; a StateT computation over Maybe
(: bump (StateT Integer Maybe Integer))
(define bump
  (do [n <- get-st]
      [_ <- (put-st (+ n 1))]
      (pure n)))
}

@section{@tt{numeric} — beyond the tower}

The numeric @emph{tower} (the @racket[Num] / @racket[Integral] /
@racket[Floating] / … classes and their instances) is in the prelude;
the derived operations are in @tt{rackton/numeric}.
@racketmodname[rackton/numeric/integer] has @racket[num-gcd] /
@racket[num-lcm] / @racket[num-factorial], @racketmodname[rackton/numeric/real]
the extra trig, @racketmodname[rackton/numeric/show] radix and
float formatting, and @racketmodname[rackton/numeric/natural] the
@racket[Natural] type:

@rackton-example[#:eval ev #:mode 'value]{
(require rackton/numeric/show)

(: hex String) (define hex (num-show-hex 255))
(: pi2 String) (define pi2 (num-show-f-float (Some 2) 3.14159))

(Pair hex pi2)
}

@section{@tt{text} — strings, formatting, parsing}

@racketmodname[rackton/text/string] extends the prelude's String
primitives (@racket[lines], @racket[words], @racket[strip],
@racket[split-on], @racket[replace], @racket[pad-left], …).
@racketmodname[rackton/text/read] parses (@racket[read-int],
@racket[read-float]), @racketmodname[rackton/text/show] gives the
@racket[ShowS] helpers, and @racketmodname[rackton/text/printf] is
@emph{type-safe} formatting: you compose directives, and the argument
types are checked at compile time.

@rackton-example[#:eval ev #:mode 'value]{
(require rackton/text/printf)

;; greeting : (-> String (-> Integer String))   <- inferred
(: greeting (-> String (-> Integer String)))
(define greeting
  (sprintf (fmt-cat (fmt-lit "hello ")
           (fmt-cat fmt-str
            (fmt-cat (fmt-lit ", you are ") fmt-int)))))

((greeting "ada") 36)
}

@section{@tt{system} — the outside world}

@racketmodname[rackton/system] is an umbrella over the @tt{System.*}
family: mutable @racket[Ref]s, file and directory I/O, handles,
exceptions, the environment, exit codes, clocks, and a splittable
random generator.  Everything is in @racket[IO]:

@rackton-example[#:eval ev #:mode 'io #:run "greet"]{
(require rackton/system)

(: greet (IO Unit))
(define greet
  (do [name <- (getenv "USER")]
      (println (match name
                 [(Some u) (mappend "hi " u)]
                 [(None)   "hi stranger"]))))
}

@section{Laziness}

Rackton is strict, but @racketmodname[rackton/data/lazy] adds opt-in
laziness.  The @racket[delay] form defers a computation as a
@racket[Lazy]; @racket[force] runs it, at most once, caching the result
(call-by-need).  @racket[delay] is a form, not a function — it does not
evaluate its argument until forced:

@rackton-example[#:eval ev #:mode 'value]{
(require rackton/data/lazy)

(: slow (Lazy Integer))
(define slow (delay (* 6 7)))   ;; not computed yet

(: answer Integer)
(define answer (force slow))    ;; computes 42 once, then caches

answer
}

A @racket[Stream] is a lazy cons-list whose tail is a @racket[Lazy], so
producers may be infinite while consumers force only a finite prefix:

@rackton-example[#:eval ev #:mode 'value]{
(require rackton/data/lazy)

(: nats (Stream Integer))
(define nats (stream-iterate (lambda (n) (+ n 1)) 0))   ;; 0 1 2 3 …

(: first5 (List Integer))
(define first5 (stream-take 5 nats))                    ;; (0 1 2 3 4)

first5
}

@racket[stream-map], @racket[stream-filter], and @racket[stream-append]
stay lazy, so they compose over infinite streams as long as the final
consumer (e.g. @racket[stream-take]) is finite.

@section{A program across families}

@filepath{examples/word-count.rkt} (see @secref["examples"]) ties
@tt{text}, @tt{data}, @tt{control}, and @tt{system} together in about
forty lines — a good next read once these pieces feel familiar.
