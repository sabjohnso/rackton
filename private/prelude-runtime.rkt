#lang racket/base

;; Rackton — runtime side of the prelude.
;;
;; Defines the ADT structs that ship with the language, the per-method
;; dispatch tables for every prelude class, the generic-method functions,
;; and the built-in instance registrations for `Integer`, `Boolean`, and
;; `String`.  The compile-time side — class-info / instance-info entries
;; in the typing env — lives in env.rkt's `prelude-env`.
;;
;; Built-in Racket operators are imported under `rkt:` prefixes so they
;; don't clash with the Rackton method names we want to expose.

(require (rename-in racket/base
                    [+  rkt:+]  [-  rkt:-]  [*  rkt:*]  [/  rkt:/]
                    [<  rkt:<]  [>  rkt:>]  [=  rkt:=]
                    [<= rkt:<=] [>= rkt:>=]
                    [not rkt:not]
                    [and rkt:and]
                    [or  rkt:or]
                    [length    rkt:length]
                    [substring rkt:substring]
                    [string-length rkt:string-length]
                    [string-append rkt:string-append]
                    [modulo  rkt:modulo]
                    [quotient rkt:quotient]
                    [abs rkt:abs]
                    [min rkt:min]
                    [max rkt:max]
                    [number->string rkt:number->string]
                    [string->number rkt:string->number]
                    [read-line  rkt:read-line]
                    [reverse    rkt:reverse]
                    [append     rkt:append]
                    [sort       rkt:sort]
                    [sqrt       rkt:sqrt]
                    [exact->inexact rkt:exact->inexact]
                    [inexact->exact rkt:inexact->exact]
                    [truncate   rkt:truncate]
                    [random     rkt:random]
                    [current-seconds rkt:current-seconds]
                    [getenv     rkt:getenv]
                    [current-command-line-arguments rkt:argv]
                    [path->string rkt:path->string]
                    [delete-file rkt:delete-file]
                    [make-directory rkt:make-directory]
                    [directory-list rkt:directory-list]
                    [string-ref     rkt:string-ref]
                    [string->list   rkt:string->list]
                    [bytes-length   rkt:bytes-length]
                    [bytes-ref      rkt:bytes-ref]
                    [bytes-append   rkt:bytes-append]
                    [bytes->list    rkt:bytes->list]
                    [bytes->string/utf-8 rkt:bytes->string/utf-8]
                    [string->bytes/utf-8 rkt:string->bytes/utf-8]
                    [make-bytes     rkt:make-bytes]
                    [bytes          rkt:bytes]
                    [string         rkt:string]
                    [char-upcase    rkt:char-upcase]
                    [char-downcase  rkt:char-downcase]
                    [char-alphabetic? rkt:char-alphabetic?]
                    [char-numeric?    rkt:char-numeric?]
                    [char-whitespace? rkt:char-whitespace?]
                    [char->integer  rkt:char->integer]
                    [integer->char  rkt:integer->char]
                    [void           rkt:void]
                    [when           rkt:when]
                    [unless         rkt:unless])
         (only-in racket/base
                  [string=? rkt:string=?])
         (only-in racket/string
                  [string-join   rkt:string-join])
         (only-in racket/base
                  [regexp-split  rkt:regexp-split]
                  [regexp-quote  rkt:regexp-quote])
         racket/format
         racket/match
         racket/file
         (only-in racket/port port->string)
         racket/async-channel
         "adt.rkt"
         "dict.rkt")

(provide
 ;; Monomorphization log accessors — exported so the
 ;; rackton macro's emitted code can call the setter from a user
 ;; module without dragging in codegen.
 set-rackton-monomorphized-log-snapshot!
 rackton-monomorphized-sites
 ;; Same pattern for inlining.
 set-rackton-inlined-log-snapshot!
 rackton-inlined-sites

 ;; ADTs (constructors usable as expressions and as match patterns)
 None Some Nil Cons MkPair Ok Err MkUnit
 ;; State/Env (run-state/get-state/run-env/ask/local + MkState/MkEnv)
 ;; moved to rackton/control/monad/state + reader.
 ;; StateT runtime (MkStateT, run/eval/exec/get/put/modify/lift-state-t,
 ;; and its $pure / $flatmap / mtl impls) moved to
 ;; rackton/control/monad/state — authored there as pure Rackton.
 ;; EnvT runtime moved to rackton/control/monad/reader.
 ;; WriterT runtime moved to rackton/control/monad/writer.
 ;; ExceptT runtime moved to rackton/control/monad/except.
 MkIdentity

 ;; Class methods
 +  -  *
 ==  /=
 <  >  <=  >=
 show
 fmap
 fapply liftA2 product
 flatmap join
 bimap first second
 foldr length to-list sum
 <>
 traverse
 mconcat

 ;; Return-typed dispatch tables — shared so user-defined instances in
 ;; other modules register their pure / mempty / … here and call sites
 ;; look them up by result-type tag (Enabler A).
 $dispatch:pure $dispatch:mempty $dispatch:pi
 $dispatch:get-st $dispatch:put-st $dispatch:modify-st
 $dispatch:ask-en $dispatch:tell-w $dispatch:throw-e
 $dispatch:await-c $dispatch:yield-c $dispatch:lift-io $dispatch:lift

 ;; Return-typed class methods (resolved at compile time per call site;
 ;; the `$pure:TCon` names are what the codegen emits after resolution).
 |$pure:Maybe| |$pure:List| |$pure:Result| |$pure:IO|
 ;; (transformer $pure / $flatmap / mtl impls are regenerated in the
 ;; carved rackton/control/monad/* modules.)
 |$mempty:String| |$mempty:List|
 ;; Runtime dispatchers for the positional class methods
 ;; introduced by mtl polish (local-en already covered above).
 ;; catch-e is exported so carved transformer modules
 ;; (rackton/control/monad/*) can call it polymorphically on the inner
 ;; monad value from their lifted MonadError instances.
 local-en listen censor catch-e
 ;; Mtl-style combinators (derived from class methods).
 asks gets void when unless

 ;; pure-via-witness registry + helper — exposed so carved transformer
 ;; modules can register how to derive their `pure` from a runtime
 ;; witness (needed for needs-dict value-dispatched methods, e.g.
 ;; ExceptT's flatmap/catch-e, and for nesting one transformer in
 ;; another).  inner-pure-from-witness is what codegen emits at those
 ;; registrations' call sites.
 register-pure-witness-deriver! inner-pure-from-witness inner-pure-from-args
 pure-via-witness
 ;; Dispatch tables — exposed so user modules that declare new
 ;; instances (including derived ones) can register against them.
 $dispatch:+  $dispatch:-  $dispatch:*
 $dispatch:abs $dispatch:negate
 $dispatch:== $dispatch:/=
 $dispatch:<  $dispatch:>  $dispatch:<= $dispatch:>=
 $dispatch:min $dispatch:max
 $dispatch:show
 $dispatch:fmap
 $dispatch:fapply
 $dispatch:liftA2
 $dispatch:product
 $dispatch:flatmap
 $dispatch:join
 $dispatch:bimap
 $dispatch:first
 $dispatch:second
 $dispatch:foldr
 $dispatch:length
 $dispatch:to-list
 $dispatch:<>
 $dispatch:traverse
 $dispatch:float-div
 ;; Positional mtl-method tables — exposed so the carved transformer
 ;; modules (rackton/control/monad/*) and user instances can register
 ;; their lifted MonadEnv/MonadError/MonadWriter impls against them.
 $dispatch:local-en $dispatch:catch-e $dispatch:listen $dispatch:censor

 ;; Combinators
 id compose flip const

 ;; Stdlib
 not and or filter

 ;; Strings
 string-length string-append substring
 string-prefix? string-split string-join
 ;; codegen-only helper for derived Show
 $show-concat

 ;; Char and Bytes
 char->integer integer->char
 char-upcase char-downcase
 char-alphabetic? char-numeric? char-whitespace?
 char->string string-ref string->chars chars->string
 bytes-length bytes-ref make-bytes bytes-append
 bytes->list list->bytes
 string->bytes bytes->string

 ;; Numeric helpers (mod/div → Integral, abs/negate → Num, min/max → Ord)
 abs negate min max integer->string string->integer

 ;; IO
 print println read-line pure-io run-io

 ;; Mutable refs and file I/O
 make-ref read-ref write-ref
 read-file write-file file-exists?

 ;; Concurrency
 fork-io wait-thread
 new-mvar new-empty-mvar take-mvar put-mvar read-mvar modify-mvar
 new-chan send-chan recv-chan

 ;; STM (foreign-imported by rackton/control/stm; the user-facing names
 ;; are except-out of main's re-export so they're not auto-available)
 new-tvar read-tvar write-tvar
 retry or-else atomically
 stm-fmap stm-pure stm-ap stm-bind
 |$pure:STM|

 ;; Concurrent
 fork-c |$await-c:IO| |$yield-c:IO|

 ;; MonadIO
 |$lift-io:IO|

 ;; Identity + Concurrent Identity
 run-identity |$await-c:Identity| |$yield-c:Identity| |$pure:Identity|

 ;; List + Pair helpers (zip/take/drop/find/sort -> rackton/data/list,
 ;; swap -> rackton/data/tuple, Phase 2 slim)
 reverse append
 fst snd

 ;; Optics (view/set/over/preview/review/…) moved to rackton/data/lens.

 ;; Panic
 panic

 ;; Immutable Map / Set + group-by moved to rackton/data/map +
 ;; rackton/data/set (runtime in private/containers-runtime).

 ;; Float
 float-div integer->float float->integer abs-float

 ;; Numeric tower
 make-rational numerator denominator
 make-complex real-part imag-part magnitude
 div mod quot rem
 sqrt exp log sin cos tan **
 floor-real ceiling-real round-real truncate-real
 is-nan? is-infinite? atan2
 |$pi:Float| |$pi:Complex|
 to-rational

 ;; Error handling
 try raise-io

 ;; System surface
 random-integer random-float current-time-seconds
 list-directory getenv argv delete-file make-directory
 ;; System.Exit
 exit-with-code
 ;; System.IO
 stdin stdout stderr open-file-with-mode h-close
 h-put-str h-put-str-ln h-flush h-get-contents h-get-line
 ;; Numeric float formatting
 show-f-float show-e-float show-g-float)

;; ----- monomorphization log -----------------------------

;; The rackton macro emits one call to `set-rackton-monomorphized-
;; log-snapshot!` at the end of each elaborate, passing the list of
;; (method . impl) pairs that were resolved at compile time.  Tests
;; call `rackton-monomorphized-sites` to inspect the most recent
;; elaborate's log.
(define rackton-monomorphized-log-snapshot-value '())

(define (set-rackton-monomorphized-log-snapshot! v)
  (set! rackton-monomorphized-log-snapshot-value v))

(define (rackton-monomorphized-sites)
  rackton-monomorphized-log-snapshot-value)

;; Inlined-call-site log accessors.  Populated per
;; elaborate by codegen when it substitutes a small impl body in
;; place of a monomorphized call.
(define rackton-inlined-log-snapshot-value '())

(define (set-rackton-inlined-log-snapshot! v)
  (set! rackton-inlined-log-snapshot-value v))

(define (rackton-inlined-sites)
  rackton-inlined-log-snapshot-value)

;; ----- ADTs -------------------------------------------------------

(define-data-ctor None 0)
(define-data-ctor Some 1)

(define-data-ctor Nil  0)
(define-data-ctor Cons 2)

(define-data-ctor MkPair 2)

(define-data-ctor Ok  1)
(define-data-ctor Err 1)

(define-data-ctor MkUnit 0)

;; MkSum / MkProduct moved to rackton/data/monoid (Phase 2 slim).

;; MkState / MkEnv moved to rackton/control/monad/state + reader.

;; MkStateT moved to rackton/control/monad/state.
;; MkEnvT moved to rackton/control/monad/reader.

;; MkWriterT moved to rackton/control/monad/writer.
;; MkExceptT moved to rackton/control/monad/except.
;; Identity for the Mock Concurrent demo.
(define-data-ctor MkIdentity 1)
;; MkLens / MkPrism / MkTraversal moved to rackton/data/lens (Phase 2).

;; ----- Class dispatch tables -------------------------------------

;; Each dispatch table comes with (pos arity) so the wrapper can decide
;; when enough arguments have been collected.  See define-class-method
;; in private/dict.rkt — it accepts both as exact-nonnegative-integers.
(define $dispatch:+  (make-hasheq))  (define-class-method +  $dispatch:+  0 2)
(define $dispatch:-  (make-hasheq))  (define-class-method -  $dispatch:-  0 2)
(define $dispatch:*  (make-hasheq))  (define-class-method *  $dispatch:*  0 2)
;; abs / negate as Num methods, min / max as Ord methods.
(define $dispatch:abs    (make-hasheq))(define-class-method abs    $dispatch:abs    0 1)
(define $dispatch:negate (make-hasheq))(define-class-method negate $dispatch:negate 0 1)
(define $dispatch:min    (make-hasheq))(define-class-method min    $dispatch:min    0 2)
(define $dispatch:max    (make-hasheq))(define-class-method max    $dispatch:max    0 2)
(define $dispatch:== (make-hasheq))  (define-class-method == $dispatch:== 0 2)
(define $dispatch:/= (make-hasheq))  (define-class-method /= $dispatch:/= 0 2)
(define $dispatch:<  (make-hasheq))  (define-class-method <  $dispatch:<  0 2)
(define $dispatch:>  (make-hasheq))  (define-class-method >  $dispatch:>  0 2)
(define $dispatch:<= (make-hasheq))  (define-class-method <= $dispatch:<= 0 2)
(define $dispatch:>= (make-hasheq))  (define-class-method >= $dispatch:>= 0 2)
(define $dispatch:show (make-hasheq))(define-class-method show $dispatch:show 0 1)
;; Functor's fmap dispatches on the SECOND argument (the container).
(define $dispatch:fmap (make-hasheq))(define-class-method fmap $dispatch:fmap 1 2)
;; Applicative's fapply dispatches on the FIRST argument (the f (a->b)).
;; product dispatches on the FIRST argument (the f a).
;; liftA2 dispatches on the SECOND argument (after the combining function).
;; Defaults are provided via the class's default method bodies (cyclic).
(define $dispatch:fapply  (make-hasheq))(define-class-method fapply  $dispatch:fapply  0 2)
(define $dispatch:liftA2  (make-hasheq))(define-class-method liftA2  $dispatch:liftA2  1 3)
(define $dispatch:product (make-hasheq))(define-class-method product $dispatch:product 0 2)
;; Monad's bind (flatmap) dispatches on the SECOND argument (the
;; wrapped value); the first argument is the continuation `a -> m b`.
;; join dispatches on the FIRST argument (the m (m a)).
(define $dispatch:flatmap (make-hasheq))(define-class-method flatmap $dispatch:flatmap 1 2)
(define $dispatch:join    (make-hasheq))(define-class-method join    $dispatch:join    0 1)
;; Bifunctor's bimap/first/second dispatch on the value (the `p a b`).
;; bimap takes 3 args, value is arg 2.  first/second take 2 args, value is arg 1.
(define $dispatch:bimap  (make-hasheq))(define-class-method bimap  $dispatch:bimap  2 3)
(define $dispatch:first  (make-hasheq))(define-class-method first  $dispatch:first  1 2)
(define $dispatch:second (make-hasheq))(define-class-method second $dispatch:second 1 2)
;; Foldable's foldr dispatches on the container (arg 2). length/to-list
;; dispatch on the container as arg 0.
(define $dispatch:foldr   (make-hasheq))(define-class-method foldr   $dispatch:foldr   2 3)
(define $dispatch:length  (make-hasheq))(define-class-method length  $dispatch:length  0 1)
(define $dispatch:to-list (make-hasheq))(define-class-method to-list $dispatch:to-list 0 1)
;; Semigroup's <> dispatches on the first arg (the value carrying the
;; Semigroup-constrained type).
(define $dispatch:<> (make-hasheq))(define-class-method <> $dispatch:<> 0 2)
;; Traversable's traverse: user sees (a -> f b) -> t a -> f (t b), so
;; the t-container is at user-position 1.  The elaborator prepends one
;; dict argument (the resolved pure-impl), shifting dispatch position
;; to 2 and total arity to 3.
(define $dispatch:traverse (make-hasheq))
(define-class-method traverse $dispatch:traverse 2 3)
;; MonadEnv's `local-en` dispatcher.  It's positional on the second
;; argument (the `(m a)`); the first argument is `(-> r r)`.
;; Only base instances (Env / EnvT) get registered through this
;; dispatcher — lifted needs-dict instances are routed at compile
;; time with their inner-pure dicts attached.
(define $dispatch:local-en (make-hasheq))
(define-class-method local-en $dispatch:local-en 1 2)
;; MonadWriter's `listen` (arity 1, pos 0) and `censor` (arity 2,
;; pos 1).  Same caveat — needs-dict instances are handled at
;; compile time; only base instances reach this dispatcher.
(define $dispatch:listen (make-hasheq))
(define-class-method listen $dispatch:listen 0 1)
(define $dispatch:censor (make-hasheq))
(define-class-method censor $dispatch:censor 1 2)

;; Runtime catch-e dispatcher.  Compile-time inst-dispatch handles
;; concrete call sites, but polymorphic-monad bodies (e.g.
;; `(MonadError e m) =>`) reach call sites whose types are tvars.
;; At runtime we look up catch-e by the (m a) value's outer ctor tag.
(define $dispatch:catch-e (make-hasheq))
(define-class-method catch-e $dispatch:catch-e 0 2)

;; pure-via-witness resolves `pure :: a -> m a` from any
;; m-value at runtime by walking the ctor chain.  Used by needs-dict
;; instance method closures (ExceptT's flatmap, catch-e, ...) which need
;; the inner monad's `pure` to lift / rewrap values.
;;
;; Two registries feed it:
;;   $pure-by-tag         — concrete bases (IO, Maybe, List, ...): the
;;                          pure impl directly.
;;   $pure-witness-derivers — wrapper transformers (MkExceptT, ...):
;;                          a (witness -> pure-impl) that unwraps the
;;                          inner value, recurses, and re-wraps.  Carved
;;                          transformer modules register their own here
;;                          (the re-wrap needs the module-local ctor), so
;;                          pure-via-witness stays extensible without the
;;                          prelude knowing every transformer.
(define $pure-by-tag (make-hasheq))
(define (register-pure-impl! tag impl)
  (hash-set! $pure-by-tag tag impl))

(define $pure-witness-derivers (make-hasheq))
(define (register-pure-witness-deriver! tag deriver)
  (hash-set! $pure-witness-derivers tag deriver))

(define (pure-via-witness witness)
  (define tag (dispatch-tag witness))
  (cond
    [(hash-ref $pure-witness-derivers tag #f) => (lambda (d) (d witness))]
    [(hash-ref $pure-by-tag tag #f) => (lambda (impl) impl)]
    [else
     (error 'pure-via-witness
            "no pure-via-witness impl for tag: ~v" tag)]))

;; Derive the INNER monad's pure from a transformer witness whose single
;; field IS the inner monad value (e.g. MkExceptT holding (m (Result e
;; a))).  Codegen emits calls to this from the runtime registrations of
;; value-dispatched needs-dict methods (ExceptT's flatmap/catch-e/...).
(define (inner-pure-from-witness wrapped)
  (pure-via-witness (vector-ref (struct->vector wrapped) 1)))

;; Find the witness among a method's runtime args: the first arg that is
;; a struct with the given ctor tag (the value the method dispatched
;; on), then derive its inner monad's pure.  Codegen emits a call to
;; this from a value-dispatched needs-dict method's runtime
;; registration — picking the witness by tag avoids depending on the
;; method's dispatch-arg POSITION (which differs between the class's
;; find-dispatch-pos and the runtime dispatcher for e.g. flatmap).
;; Variadic so codegen passes args directly (NOT via `list`, which a
;; #lang rackton module rebinds to the List ctor).
(define (inner-pure-from-args tag . args)
  (let loop ([as args])
    (cond
      [(null? as)
       (error 'inner-pure-from-args "no witness with tag ~v in args" tag)]
      [(and (struct? (car as)) (eq? (dispatch-tag (car as)) tag))
       (inner-pure-from-witness (car as))]
      [else (loop (cdr as))])))

;; ----- Num Integer ------------------------------------------------

(register-instance-method! $dispatch:+  'Integer (lambda (x y) (rkt:+  x y)))
(register-instance-method! $dispatch:-  'Integer (lambda (x y) (rkt:-  x y)))
(register-instance-method! $dispatch:*  'Integer (lambda (x y) (rkt:*  x y)))
(register-instance-method! $dispatch:abs    'Integer (lambda (x) (rkt:abs x)))
(register-instance-method! $dispatch:negate 'Integer (lambda (x) (rkt:- x)))

;; ----- Num / Eq / Ord / Show Float --------------------------------

(register-instance-method! $dispatch:+  'Float (lambda (x y) (rkt:+ x y)))
(register-instance-method! $dispatch:-  'Float (lambda (x y) (rkt:- x y)))
(register-instance-method! $dispatch:*  'Float (lambda (x y) (rkt:* x y)))
(register-instance-method! $dispatch:abs    'Float (lambda (x) (rkt:abs x)))
(register-instance-method! $dispatch:negate 'Float (lambda (x) (rkt:- x)))
(register-instance-method! $dispatch:== 'Float (lambda (x y) (rkt:= x y)))
(register-instance-method! $dispatch:/= 'Float (lambda (x y) (rkt:not (rkt:= x y))))
(register-instance-method! $dispatch:<  'Float (lambda (x y) (rkt:<  x y)))
(register-instance-method! $dispatch:>  'Float (lambda (x y) (rkt:>  x y)))
(register-instance-method! $dispatch:<= 'Float (lambda (x y) (rkt:<= x y)))
(register-instance-method! $dispatch:>= 'Float (lambda (x y) (rkt:>= x y)))
(register-instance-method! $dispatch:min 'Float (lambda (x y) (rkt:min x y)))
(register-instance-method! $dispatch:max 'Float (lambda (x y) (rkt:max x y)))
(register-instance-method! $dispatch:show 'Float (lambda (x) (rkt:number->string x)))

;; ----- Eq instances ----------------------------------------------

(register-instance-method! $dispatch:== 'Integer (lambda (x y) (rkt:= x y)))
(register-instance-method! $dispatch:== 'Boolean (lambda (x y) (if x y (not y))))
(register-instance-method! $dispatch:== 'String  (lambda (x y) (string=? x y)))

(register-instance-method! $dispatch:/= 'Integer (lambda (x y) (not (rkt:= x y))))
(register-instance-method! $dispatch:/= 'Boolean (lambda (x y) (not (if x y (not y)))))
(register-instance-method! $dispatch:/= 'String  (lambda (x y) (not (string=? x y))))

;; ----- Ord Integer -----------------------------------------------

(register-instance-method! $dispatch:<  'Integer (lambda (x y) (rkt:<  x y)))
(register-instance-method! $dispatch:>  'Integer (lambda (x y) (rkt:>  x y)))
(register-instance-method! $dispatch:<= 'Integer (lambda (x y) (rkt:<= x y)))
(register-instance-method! $dispatch:>= 'Integer (lambda (x y) (rkt:>= x y)))
(register-instance-method! $dispatch:min 'Integer (lambda (x y) (rkt:min x y)))
(register-instance-method! $dispatch:max 'Integer (lambda (x y) (rkt:max x y)))

;; Ord String (lex order).
(register-instance-method! $dispatch:<  'String (lambda (x y) (string<? x y)))
(register-instance-method! $dispatch:>  'String (lambda (x y) (string>? x y)))
(register-instance-method! $dispatch:<= 'String (lambda (x y) (string<=? x y)))
(register-instance-method! $dispatch:>= 'String (lambda (x y) (string>=? x y)))
(register-instance-method! $dispatch:min 'String
                           (lambda (x y) (if (string<? x y) x y)))
(register-instance-method! $dispatch:max 'String
                           (lambda (x y) (if (string<? x y) y x)))

;; ----- Show instances --------------------------------------------

(register-instance-method! $dispatch:show 'Integer
                           (lambda (x) (number->string x)))
(register-instance-method! $dispatch:show 'Boolean
                           (lambda (x) (if x "True" "False")))
(register-instance-method! $dispatch:show 'String
                           (lambda (x) (~a "\"" x "\"")))

;; ----- Char / Bytes instances -----------------------------------

(register-instance-method! $dispatch:==   'Char  (lambda (x y) (char=? x y)))
(register-instance-method! $dispatch:/=   'Char  (lambda (x y) (not (char=? x y))))
(register-instance-method! $dispatch:<    'Char  (lambda (x y) (char<? x y)))
(register-instance-method! $dispatch:>    'Char  (lambda (x y) (char>? x y)))
(register-instance-method! $dispatch:<=   'Char  (lambda (x y) (char<=? x y)))
(register-instance-method! $dispatch:>=   'Char  (lambda (x y) (char>=? x y)))
(register-instance-method! $dispatch:show 'Char  (lambda (c) (~v c)))

(register-instance-method! $dispatch:==   'Bytes (lambda (x y) (bytes=? x y)))
(register-instance-method! $dispatch:/=   'Bytes (lambda (x y) (not (bytes=? x y))))
(register-instance-method! $dispatch:show 'Bytes (lambda (b) (~v b)))

;; ----- Combinators ----------------------------------------------

(define (id x) x)
(define/curried (compose f g) (lambda (x) (f (g x))))
(define/curried (flip f x y) ((f y) x))
(define (const x) (lambda (_y) x))

;; Mtl-flavoured combinators.  Each needs-dict body's dict-args
;; appear as leading params in the same order build-dict-skolems
;; produces (own return-typed methods sorted + superclass closure
;; appended).

;; asks :: (MonadEnv r m) => (-> r a) -> m a
;; MonadEnv's return-typed own methods sorted = (ask-en); Monad's
;; super closure = (pure).
(define/curried (asks $dict-ask-en $dict-pure f)
  (fmap f $dict-ask-en))

;; gets :: (MonadState s m) => (-> s a) -> m a
;; MonadState's own methods sorted = (get-st, modify-st, put-st);
;; Monad super = (pure).
(define/curried (gets $dict-get-st $dict-modify-st $dict-put-st $dict-pure f)
  (fmap f $dict-get-st))

;; void :: (Functor f) => f a -> f Unit — no dicts; fmap is positional.
(define (void fa) (fmap (const MkUnit) fa))

;; when / unless :: (Applicative f) => Boolean -> f Unit -> f Unit.
;; One dict for Applicative's `pure`.
(define/curried (when $dict-pure b fu)
  (if b fu ($dict-pure MkUnit)))
(define/curried (unless $dict-pure b fu)
  (if b ($dict-pure MkUnit) fu))

;; ----- Stdlib ----------------------------------------------------

(define (not b) (if b #f #t))
(define/curried (and a b) (if a b #f))
(define/curried (or  a b) (if a #t b))

(define (filter p xs)
  (match xs
    [(Nil)        Nil]
    [(Cons h t)   (if (p h) (Cons h (filter p t)) (filter p t))]))

;; ----- Strings -------------------------------------------------

(define (string-length s) (rkt:string-length s))
(define/curried (string-append a b) (rkt:string-append a b))
(define/curried (substring s start end) (rkt:substring s start end))

(define/curried (string-prefix? p s)
  (and (rkt:<= (rkt:string-length p) (rkt:string-length s))
       (rkt:string=? p (rkt:substring s 0 (rkt:string-length p)))))

(define/curried (string-split sep s)
  ;; Split `s` on every occurrence of `sep`.  Behaves like Racket's
  ;; `regexp-split` on a literal regex but takes a plain string.
  (rkt-list->rackton
   (rkt:regexp-split (rkt:regexp-quote sep) s)))

(define/curried (string-join sep ss)
  ;; Inverse of string-split.  Collect the Rackton list down to a
  ;; Racket list, then use `string-join` from racket/string.
  (let loop ([ss ss] [acc '()])
    (match ss
      [(Nil)        (rkt:string-join (rkt:reverse acc) sep)]
      [(Cons h t)   (loop t (cons h acc))])))

;; A variadic concatenation used by derived Show instances.  The
;; Rackton-typed `string-append` is binary; this helper sidesteps the
;; binary signature for codegen-emitted strings.
(define $show-concat
  (lambda strs (apply rkt:string-append strs)))

;; ----- Numeric helpers -----------------------------------------
;; mod / div migrated to the Integral class.
;; abs / negate migrated to Num and min / max to Ord — runtime
;; impls registered against the per-instance dispatch tables
;; below.

(define (integer->string n) (rkt:number->string n))
(define (string->integer s)
  (define n (rkt:string->number s))
  (if (rkt:and n (exact-integer? n)) (Some n) None))

;; ----- Char / Bytes ops ----------------------------------------

(define (char->integer c) (rkt:char->integer c))
(define (integer->char n)
  (with-handlers ([exn:fail:contract? (lambda (_) None)])
    (Some (rkt:integer->char n))))
(define (char-upcase c)   (rkt:char-upcase c))
(define (char-downcase c) (rkt:char-downcase c))
(define (char-alphabetic? c) (rkt:char-alphabetic? c))
(define (char-numeric?    c) (rkt:char-numeric?    c))
(define (char-whitespace? c) (rkt:char-whitespace? c))
(define (char->string c)  (rkt:string c))

(define/curried (string-ref s i)
  (cond
    [(rkt:and (rkt:<= 0 i) (rkt:< i (rkt:string-length s)))
     (Some (rkt:string-ref s i))]
    [else None]))

;; Build a rackton List from a Racket sequence (shared helper; the
;; Map/Set runtime moved out but string/bytes conversions still need it).
(define (rkt-seq->list xs)
  (let loop ([xs (rkt:reverse xs)] [acc Nil])
    (cond [(null? xs) acc]
          [else (loop (cdr xs) (Cons (car xs) acc))])))

(define (string->chars s)
  (rkt-seq->list (rkt:string->list s)))

(define (chars->string cs)
  (apply rkt:string
         (let loop ([cs cs] [acc '()])
           (match cs
             [(Nil) (rkt:reverse acc)]
             [(Cons h t) (loop t (cons h acc))]))))

(define (bytes-length b) (rkt:bytes-length b))

(define/curried (bytes-ref b i)
  (cond
    [(rkt:and (rkt:<= 0 i) (rkt:< i (rkt:bytes-length b)))
     (Some (rkt:bytes-ref b i))]
    [else None]))

(define/curried (make-bytes n v) (rkt:make-bytes n v))

(define/curried (bytes-append a b) (rkt:bytes-append a b))

(define (bytes->list b)
  (rkt-seq->list (rkt:bytes->list b)))

(define (list->bytes xs)
  (apply rkt:bytes
         (let loop ([xs xs] [acc '()])
           (match xs
             [(Nil) (rkt:reverse acc)]
             [(Cons h t) (loop t (cons h acc))]))))

(define (string->bytes s) (rkt:string->bytes/utf-8 s))
(define (bytes->string b)
  (with-handlers ([exn:fail:contract? (lambda (_) None)])
    (Some (rkt:bytes->string/utf-8 b))))

;; ----- IO monad ------------------------------------------------

(struct $io (thunk) #:transparent)

(define (run-io io) (($io-thunk io)))

(define (print s)   ($io (lambda () (display   s) MkUnit)))
(define (println s) ($io (lambda () (displayln s) MkUnit)))
(define read-line
  ($io (lambda ()
         (define line (rkt:read-line))
         (if (eof-object? line) "" line))))
(define (pure-io x) ($io (lambda () x)))

;; ----- pure (return-typed Applicative method) ---------------------
;; The Rackton-level `pure` is resolved at compile time by the
;; elaborator (see resolve-method-uses! in private/infer.rkt) to one
;; of these per-instance names based on the expected return type at
;; each call site.
;; Per-method dispatch tables for the return-typed methods `pure` and
;; `mempty`, keyed by the result type's ctor tag (e.g. 'Maybe, 'String).
;; Codegen emits `(lookup-return-method $dispatch:pure 'Tag 'pure)` at
;; call sites; plain (non-needs-dict) instances — both these prelude ones
;; and user-defined ones in other modules — register here.  This is the
;; return-typed analogue of the positional `$dispatch:fmap` &c. tables.
;; (Registrations are at the end of this file, once every impl exists.)
;; One table per return-typed method (the authoritative set is
;; env-return-typed-methods over prelude-env: pure mempty pi get-st
;; put-st modify-st ask-en tell-w throw-e await-c yield-c).  Every one
;; must exist as a binding because codegen routes return-typed call sites
;; / dict-args through `$dispatch:<m>`; tell-w / throw-e have only
;; needs-dict instances (no plain base) but still get a table so a stray
;; route resolves to a binding rather than an unbound-identifier error.
(define $dispatch:pure      (make-hasheq))
(define $dispatch:mempty    (make-hasheq))
(define $dispatch:pi        (make-hasheq))
(define $dispatch:get-st    (make-hasheq))
(define $dispatch:put-st    (make-hasheq))
(define $dispatch:modify-st (make-hasheq))
(define $dispatch:ask-en    (make-hasheq))
(define $dispatch:tell-w    (make-hasheq))
(define $dispatch:throw-e   (make-hasheq))
(define $dispatch:await-c   (make-hasheq))
(define $dispatch:yield-c   (make-hasheq))
(define $dispatch:lift-io   (make-hasheq))
(define $dispatch:lift      (make-hasheq))  ; MonadTrans.lift (no base instance)

(define (|$pure:Maybe|  x) (Some x))
(define (|$pure:List|   x) (Cons x Nil))
(define (|$pure:Result| x) (Ok x))
(define (|$pure:IO|     x) ($io (lambda () x)))

;; Register the non-needs-dict pures with their witness
;; ctor tags so pure-via-witness can find them at runtime.  Lists
;; have two ctors (Nil, Cons); register both.  Result has two too.
(register-pure-impl! '$ctor:None  |$pure:Maybe|)
(register-pure-impl! '$ctor:Some  |$pure:Maybe|)
(register-pure-impl! '$ctor:Nil   |$pure:List|)
(register-pure-impl! '$ctor:Cons  |$pure:List|)
(register-pure-impl! '$ctor:Ok    |$pure:Result|)
(register-pure-impl! '$ctor:Err   |$pure:Result|)
(register-pure-impl! '$io          |$pure:IO|)

;; ----- Monoid mempty (return-typed) -------------------------------
;; Sum / Product moved to rackton/data/monoid (Phase 2 slim).
(define |$mempty:String|  "")
(define |$mempty:List|    Nil)

;; ----- Semigroup <> -----------------------------------------------
(define (semigroup-list-<> xs ys)
  (match xs
    [(Nil)      ys]
    [(Cons h t) (Cons h (semigroup-list-<> t ys))]))
(register-instance-method! $dispatch:<> 'String
                           (lambda (a b) (rkt:string-append a b)))
(register-instance-method! $dispatch:<> '$ctor:Nil  semigroup-list-<>)
(register-instance-method! $dispatch:<> '$ctor:Cons semigroup-list-<>)

;; ----- Traversable traverse ---------------------------------------
;; Each impl receives the resolved `pure-impl` as its leading argument
;; (the elaborator inserts it based on the inferred `f`).  Inner
;; effectful combinators like `fmap`/`liftA2` dispatch on their value
;; arg at runtime and so work generically for any `f`.

(define (traverse-Maybe-impl pure-impl f m)
  (match m
    [(None)   (pure-impl None)]
    [(Some x) (fmap Some (f x))]))
(register-instance-method! $dispatch:traverse '$ctor:None traverse-Maybe-impl)
(register-instance-method! $dispatch:traverse '$ctor:Some traverse-Maybe-impl)

(define (traverse-List-impl pure-impl f xs)
  (match xs
    [(Nil)         (pure-impl Nil)]
    [(Cons h rest) (liftA2 Cons (f h) (traverse-List-impl pure-impl f rest))]))
(register-instance-method! $dispatch:traverse '$ctor:Nil  traverse-List-impl)
(register-instance-method! $dispatch:traverse '$ctor:Cons traverse-List-impl)

(define (io-fmap f io)
  ($io (lambda () (f (run-io io)))))

(define (io-ap iof iox)
  ($io (lambda ()
         (define f (run-io iof))
         (define a (run-io iox))
         (f a))))

(define (io-liftA2 g x y)
  ($io (lambda ()
         (define a (run-io x))
         (define b (run-io y))
         (g a b))))

(define (io-bind f io)
  ($io (lambda () (run-io (f (run-io io))))))

(register-instance-method! $dispatch:fmap    '$io io-fmap)
(register-instance-method! $dispatch:fapply  '$io io-ap)
(register-instance-method! $dispatch:liftA2  '$io io-liftA2)
(register-instance-method! $dispatch:flatmap '$io io-bind)

;; ----- Mutable refs (in IO) -----------------------------------

(define (make-ref v) ($io (lambda () (box v))))
(define (read-ref r) ($io (lambda () (unbox r))))
(define/curried (write-ref r v) ($io (lambda () (set-box! r v) MkUnit)))

;; ----- Concurrency ---------------------------------
;;
;; ThreadId wraps a Racket thread; MVar is a box guarded by two
;; semaphores so put/take block on full/empty respectively; Chan is
;; a thin wrapper over make-async-channel (unbounded, non-blocking
;; send).  All operations are IO thunks.

(struct $mvar (cell filled empty) #:transparent)

(define (fork-io action)
  ($io (lambda ()
         (define t (thread (lambda () (run-io action))))
         t)))

(define (wait-thread t)
  ($io (lambda () (thread-wait t) MkUnit)))

(define (new-mvar v)
  ($io (lambda ()
         ($mvar (box v) (make-semaphore 1) (make-semaphore 0)))))

(define new-empty-mvar
  ($io (lambda ()
         ($mvar (box #f) (make-semaphore 0) (make-semaphore 1)))))

(define (take-mvar m)
  ($io (lambda ()
         (semaphore-wait ($mvar-filled m))
         (define v (unbox ($mvar-cell m)))
         (set-box! ($mvar-cell m) #f)
         (semaphore-post ($mvar-empty m))
         v)))

(define/curried (put-mvar m v)
  ($io (lambda ()
         (semaphore-wait ($mvar-empty m))
         (set-box! ($mvar-cell m) v)
         (semaphore-post ($mvar-filled m))
         MkUnit)))

;; read-mvar takes the filled-permit, reads, then re-posts the
;; filled-permit so other readers/takers can proceed.  The empty
;; semaphore is left untouched — the cell is still occupied.
(define (read-mvar m)
  ($io (lambda ()
         (semaphore-wait ($mvar-filled m))
         (define v (unbox ($mvar-cell m)))
         (semaphore-post ($mvar-filled m))
         v)))

;; modify-mvar atomically takes, applies `f`, puts the result back.
;; Holds the filled-permit across the function call so concurrent
;; takers / readers block until the modification completes.
(define/curried (modify-mvar m f)
  ($io (lambda ()
         (semaphore-wait ($mvar-filled m))
         (define v (unbox ($mvar-cell m)))
         (set-box! ($mvar-cell m) (f v))
         (semaphore-post ($mvar-filled m))
         MkUnit)))

(define new-chan
  ($io (lambda () (make-async-channel))))

(define/curried (send-chan ch v)
  ($io (lambda () (async-channel-put ch v) MkUnit)))

(define (recv-chan ch)
  ($io (lambda () (async-channel-get ch))))

;; ----- STM -----------------------------------------
;;
;; TVar is a box + a version counter.  STM is a thunk taking a
;; transaction log; the log is an equal?-hash from TVar to
;; (cons value flag) where flag is either 'read (with the version
;; observed) or 'write.  atomically runs the thunk against a fresh
;; log; on commit, takes a global lock, verifies all read versions
;; still match each TVar's current version, applies writes (bumping
;; versions), and returns the result.  Mismatch → retry the whole
;; transaction.

;; Each TVar also carries a `wake-signal` semaphore.  On
;; commit, every WRITTEN TVar's wake-signal is posted so retrying
;; transactions watching that TVar can resume.  `retry` now waits
;; (via `sync`) on the union of the read-logged TVars' wake-signals,
;; then restarts.  No more busy-loop.
(struct $tvar (cell version-box wake-signal) #:transparent)
(struct $stm  (thunk)             #:transparent)

(define $stm-retry-sentinel (gensym 'stm-retry))
(define (stm-retry?-exn? e) (eq? e $stm-retry-sentinel))

(define $atomically-lock (make-semaphore 1))

;; Log entries: a TVar maps to (list 'read   value version)
;;                          OR (list 'write  value version-observed-on-first-read-or-#f)
;; A TVar that's first read then written transitions read→write.

(define (log-read! log tv)
  (define existing (hash-ref log tv #f))
  (cond
    [existing (cadr existing)]   ;; current logged value
    [else
     (define v   (unbox ($tvar-cell tv)))
     (define ver (unbox ($tvar-version-box tv)))
     (hash-set! log tv (list 'read v ver))
     v]))

(define (log-write! log tv v)
  (define existing (hash-ref log tv #f))
  (cond
    [existing
     (define mode (car existing))
     (define obs-ver (caddr existing))
     (hash-set! log tv (list 'write v obs-ver))]
    [else
     (hash-set! log tv (list 'write v #f))]))

(define (new-tvar v)
  ($stm (lambda (_log)
          ;; Each TVar's wake-signal starts unposted; commits
          ;; that write to it post it so retrying watchers can advance.
          ($tvar (box v) (box 0) (make-semaphore 0)))))

(define (read-tvar tv)
  ($stm (lambda (log) (log-read! log tv))))

(define/curried (write-tvar tv v)
  ($stm (lambda (log) (log-write! log tv v) MkUnit)))

(define retry
  ($stm (lambda (_log) (raise $stm-retry-sentinel))))

(define/curried (or-else s1 s2)
  ($stm (lambda (log)
          (with-handlers ([stm-retry?-exn?
                           (lambda (_)
                             ;; First branch retried — fall through.
                             (($stm-thunk s2) log))])
            (($stm-thunk s1) log)))))

(define (atomically stm)
  ($io (lambda ()
         (let loop ()
           (define log (make-hash))
           (define result-box (box #f))
           (define retried?
             (with-handlers ([stm-retry?-exn? (lambda (_) #t)])
               (set-box! result-box (($stm-thunk stm) log))
               #f))
           (cond
             [retried?
              ;; Block on the wake-signals of every TVar
              ;; the transaction read before retrying.  When any
              ;; writer commits to one of these TVars, its
              ;; wake-signal is posted and `sync` returns.  Then we
              ;; restart from a fresh log.
              (define read-tvars
                (for/list ([(tv entry) (in-hash log)]
                           #:when (eq? (car entry) 'read))
                  tv))
              (cond
                [(null? read-tvars)
                 ;; No reads recorded — `retry` was called blindly.
                 ;; Falling back to immediate restart matches Haskell.
                 (loop)]
                [else
                 (apply sync (map $tvar-wake-signal read-tvars))
                 (loop)])]
             [else
              ;; Commit: acquire global lock; verify read versions;
              ;; apply writes (bumping versions); release.
              (semaphore-wait $atomically-lock)
              (define all-ok?
                (for/and ([(tv entry) (in-hash log)])
                  (define mode    (car entry))
                  (define obs-ver (caddr entry))
                  ;; 'write entries with no observed version (never
                  ;; read) skip the check; otherwise the observed
                  ;; version must still match the TVar's current.
                  (cond
                    [(eq? mode 'read)
                     (= (unbox ($tvar-version-box tv)) obs-ver)]
                    [(and (eq? mode 'write) (number? obs-ver))
                     (= (unbox ($tvar-version-box tv)) obs-ver)]
                    [else #t])))
              (cond
                [all-ok?
                 (for ([(tv entry) (in-hash log)])
                   (define mode (car entry))
                   (define v    (cadr entry))
                   ;; NOTE: this looks like Racket's `when` but our
                   ;; `define/curried (when …)` above shadows the
                   ;; macro in this module — so we use `cond`
                   ;; explicitly to avoid an arity error.
                   (cond
                     [(eq? mode 'write)
                      (set-box! ($tvar-cell tv) v)
                      (set-box! ($tvar-version-box tv)
                                (+ 1 (unbox ($tvar-version-box tv))))
                      ;; Wake all watchers of this TVar.
                      ;; Each retry-waiter consumes one post via sync.
                      (semaphore-post ($tvar-wake-signal tv))]))
                 (semaphore-post $atomically-lock)
                 (unbox result-box)]
                [else
                 (semaphore-post $atomically-lock)
                 (loop)])])))))

;; STM Functor / Applicative / Monad method impls.  The instances now
;; live in rackton/control/stm, which foreign-imports these and
;; registers them under STM's #:runtime-tag ('$stm) — so no dispatch
;; registration here.  register-pure-impl! '$stm stays (the witness-based
;; pure table, used by needs-dict transformer code over an STM inner).

(define (stm-fmap f s)
  ($stm (lambda (log) (f (($stm-thunk s) log)))))

(define (stm-pure x) ($stm (lambda (_log) x)))
(define (stm-ap sf sa)
  ($stm (lambda (log)
          (define f (($stm-thunk sf) log))
          (define a (($stm-thunk sa) log))
          (f a))))
(define (stm-bind f s)
  ($stm (lambda (log)
          (define a (($stm-thunk s) log))
          (($stm-thunk (f a)) log))))

(define |$pure:STM| stm-pure)
(register-pure-impl! '$stm |$pure:STM|)

;; ----- Concurrent class + Future -----------------
;;
;; Future a is a thin wrapper around an MVar a; the Concurrent IO
;; instance spawns a thread that runs the IO action and puts its
;; result, so await-c can read.

(struct $future (mvar) #:transparent)

;; fork-c is positional (dispatches on the (m a) arg).  await-c
;; and yield-c are return-typed (m appears only in the return), so
;; the inferer resolves them at compile time to `$method:Tcon`
;; globals that must be defined here.

(define $dispatch:fork-c  (make-hasheq))
(define-class-method fork-c  $dispatch:fork-c  0 1)

(define (fork-c-io io)
  ($io (lambda ()
         (define m ($mvar (box #f) (make-semaphore 0) (make-semaphore 1)))
         (thread (lambda ()
                   (define v (run-io io))
                   (semaphore-wait ($mvar-empty m))
                   (set-box! ($mvar-cell m) v)
                   (semaphore-post ($mvar-filled m))))
         ($future m))))

(define (await-c-io fut)
  ($io (lambda ()
         (define m ($future-mvar fut))
         (semaphore-wait ($mvar-filled m))
         (define v (unbox ($mvar-cell m)))
         ;; Re-post filled so multiple awaiters can all read.
         (semaphore-post ($mvar-filled m))
         v)))

(register-instance-method! $dispatch:fork-c  '$io fork-c-io)

(define |$await-c:IO| await-c-io)
(define |$yield-c:IO| ($io (lambda () (sleep 0) MkUnit)))

;; MonadIO IO: lift-io is the identity on IO actions.
(define (|$lift-io:IO| io) io)

;; ----- Identity monad + Concurrent Identity -------

(define (run-identity i)
  (match i [(MkIdentity x) x]))

;; Functor / Applicative / Monad for Identity
(define (identity-fmap f i)
  (match i [(MkIdentity x) (MkIdentity (f x))]))
(register-instance-method! $dispatch:fmap '$ctor:MkIdentity identity-fmap)

(define |$pure:Identity| MkIdentity)
(register-pure-impl! '$ctor:MkIdentity |$pure:Identity|)

(define (identity-ap ifn ix)
  (match ifn
    [(MkIdentity f)
     (match ix [(MkIdentity x) (MkIdentity (f x))])]))
(register-instance-method! $dispatch:fapply '$ctor:MkIdentity identity-ap)

(define (identity-liftA2 g ia ib)
  (match ia
    [(MkIdentity a)
     (match ib [(MkIdentity b) (MkIdentity (g a b))])]))
(register-instance-method! $dispatch:liftA2 '$ctor:MkIdentity identity-liftA2)

(define (identity-bind f i)
  (match i [(MkIdentity x) (f x)]))
(register-instance-method! $dispatch:flatmap '$ctor:MkIdentity identity-bind)

;; Concurrent Identity — fork-c runs the computation immediately
;; and uses the bare value as the Future; await-c rewraps in
;; Identity.  Since Identity is a transparent monad, no synchronization
;; needed.
(define (fork-c-identity m)
  ;; m :: Identity a == (MkIdentity x).  Return (MkIdentity x) — the
  ;; "Future a" representation at Identity is just the raw value.
  m)
(define (await-c-identity fut)
  ;; fut is the raw a value (after do-notation extracted from
  ;; MkIdentity); wrap back as Identity a.
  (MkIdentity fut))
(register-instance-method! $dispatch:fork-c '$ctor:MkIdentity fork-c-identity)

(define |$await-c:Identity| await-c-identity)
(define |$yield-c:Identity| (MkIdentity MkUnit))

;; ----- File I/O ------------------------------------------------

(define (read-file path)
  ($io (lambda () (file->string path))))
(define/curried (write-file path contents)
  ($io (lambda ()
         (with-output-to-file path #:exists 'replace
           (lambda () (display contents)))
         MkUnit)))
(define (file-exists? path)
  ($io (lambda () (rkt:file-exists?-impl path))))

;; Bridge for racket/base's file-exists?  We aliased it under our own
;; name above; here we reach Racket's definition directly.
(require (rename-in racket/base [file-exists? rkt:file-exists?-impl]))

;; ----- List helpers -------------------------------------------

(define (reverse xs)
  (let loop ([xs xs] [acc Nil])
    (match xs
      [(Nil) acc]
      [(Cons h t) (loop t (Cons h acc))])))

(define/curried (append xs ys)
  (match xs
    [(Nil) ys]
    [(Cons h t) (Cons h (append t ys))]))

;; zip / take / drop / find / split-at / merge-lists / sort moved to
;; rackton/data/list (Phase 2 slim).

;; ----- Pair helpers -------------------------------------------

(define (fst p)  (match p [(MkPair a _) a]))
(define (snd p)  (match p [(MkPair _ b) b]))
;; swap moved to rackton/data/tuple (Phase 2 slim).

;; Optics primitives (Lens / Prism / Traversal) moved to
;; rackton/data/lens (Phase 2 slim).

;; ----- panic ---------------------------------------------------

(define (panic msg) (error 'rackton-panic msg))

;; Map / Set (and group-by) moved to rackton/data/map + rackton/data/set
;; (Phase 2 slim); runtime lives in private/containers-runtime.rkt and is
;; reached via `foreign`.

;; ----- Float ops ---------------------------------------------

;; Fractional method dispatcher
(define $dispatch:float-div (make-hasheq))
(define-class-method float-div $dispatch:float-div 0 2)
(register-instance-method! $dispatch:float-div 'Float
                           (lambda (x y) (rkt:/ x y)))

(define (integer->float n) (rkt:exact->inexact n))
(define (float->integer x) (rkt:inexact->exact (rkt:truncate x)))
(define (abs-float x) (rkt:abs x))

;; ----- Numeric tower ------------------------------
;;
;; All four new numeric types build on Racket's built-in numbers:
;; Rational = exact non-integer rationals, Complex = numbers with
;; an imaginary part.  Their dispatch tags are added in dict.rkt's
;; dispatch-tag.

(define/curried (make-rational n d) (rkt:/ n d))
(define (numerator   r) (rkt:numerator   r))
(define (denominator r) (rkt:denominator r))

(require (only-in racket/base
                  [numerator   rkt:numerator]
                  [denominator rkt:denominator]
                  [make-rectangular rkt:make-rectangular]
                  [real-part rkt:real-part]
                  [imag-part rkt:imag-part]
                  [magnitude rkt:magnitude]
                  [expt rkt:expt]
                  [log rkt:log]
                  [exp rkt:exp]
                  [sin rkt:sin]
                  [cos rkt:cos]
                  [tan rkt:tan]
                  [floor rkt:floor]
                  [ceiling rkt:ceiling]
                  [round rkt:round]
                  [exact->inexact rkt:e->i]
                  [atan rkt:atan]
                  [remainder rkt:remainder])
         (only-in racket/math [pi rkt:pi] [nan? rkt:nan?] [infinite? rkt:infinite?]))

(define/curried (make-complex re im) (rkt:make-rectangular re im))
(define (real-part c) (rkt:real-part c))
(define (imag-part c) (rkt:imag-part c))
(define (magnitude c) (rkt:magnitude c))

;; Num / Eq / Ord / Show for Rational + Complex.  The existing
;; $dispatch:+ / $dispatch:- / etc. tables get new entries.
(register-instance-method! $dispatch:+ 'Rational (lambda (x y) (rkt:+ x y)))
(register-instance-method! $dispatch:- 'Rational (lambda (x y) (rkt:- x y)))
(register-instance-method! $dispatch:* 'Rational (lambda (x y) (rkt:* x y)))
(register-instance-method! $dispatch:abs    'Rational (lambda (x) (rkt:abs x)))
(register-instance-method! $dispatch:negate 'Rational (lambda (x) (rkt:- x)))
(register-instance-method! $dispatch:== 'Rational (lambda (x y) (rkt:= x y)))
(register-instance-method! $dispatch:/= 'Rational (lambda (x y) (not (rkt:= x y))))
(register-instance-method! $dispatch:<  'Rational (lambda (x y) (rkt:< x y)))
(register-instance-method! $dispatch:>  'Rational (lambda (x y) (rkt:> x y)))
(register-instance-method! $dispatch:<= 'Rational (lambda (x y) (rkt:<= x y)))
(register-instance-method! $dispatch:>= 'Rational (lambda (x y) (rkt:>= x y)))
(register-instance-method! $dispatch:min 'Rational (lambda (x y) (rkt:min x y)))
(register-instance-method! $dispatch:max 'Rational (lambda (x y) (rkt:max x y)))
(register-instance-method! $dispatch:show 'Rational
                           (lambda (r)
                             (format "~a/~a" (rkt:numerator r) (rkt:denominator r))))
(register-instance-method! $dispatch:float-div 'Rational
                           (lambda (x y) (rkt:/ x y)))

(register-instance-method! $dispatch:+ 'Complex (lambda (x y) (rkt:+ x y)))
(register-instance-method! $dispatch:- 'Complex (lambda (x y) (rkt:- x y)))
(register-instance-method! $dispatch:* 'Complex (lambda (x y) (rkt:* x y)))
(register-instance-method! $dispatch:abs    'Complex (lambda (x) (rkt:magnitude x)))
(register-instance-method! $dispatch:negate 'Complex (lambda (x) (rkt:- x)))
(register-instance-method! $dispatch:== 'Complex (lambda (x y) (rkt:= x y)))
(register-instance-method! $dispatch:/= 'Complex (lambda (x y) (not (rkt:= x y))))
(register-instance-method! $dispatch:show 'Complex
                           (lambda (c) (format "~v" c)))
(register-instance-method! $dispatch:float-div 'Complex
                           (lambda (x y) (rkt:/ x y)))

;; ----- Integral class ---------------------------------------

(define $dispatch:div  (make-hasheq))
(define-class-method div  $dispatch:div  0 2)
(define $dispatch:mod  (make-hasheq))
(define-class-method mod  $dispatch:mod  0 2)
(define $dispatch:quot (make-hasheq))
(define-class-method quot $dispatch:quot 0 2)
(define $dispatch:rem  (make-hasheq))
(define-class-method rem  $dispatch:rem  0 2)

(register-instance-method! $dispatch:div  'Integer
                           (lambda (a b) (rkt:quotient a b)))
(register-instance-method! $dispatch:mod  'Integer
                           (lambda (a b) (rkt:modulo a b)))
(register-instance-method! $dispatch:quot 'Integer
                           (lambda (a b) (rkt:quotient a b)))
(register-instance-method! $dispatch:rem  'Integer
                           (lambda (a b) (rkt:remainder a b)))

;; ----- Real class -------------------------------------------

(define $dispatch:to-rational (make-hasheq))
(define-class-method to-rational $dispatch:to-rational 0 1)
(register-instance-method! $dispatch:to-rational 'Integer
                           (lambda (n) n))
(register-instance-method! $dispatch:to-rational 'Float
                           (lambda (x) (rkt:inexact->exact x)))
(register-instance-method! $dispatch:to-rational 'Rational
                           (lambda (x) x))

;; ----- Floating class ---------------------------------------

(define |$pi:Float|   rkt:pi)
(define |$pi:Complex| (rkt:make-rectangular rkt:pi 0))

(define $dispatch:exp  (make-hasheq))
(define-class-method exp  $dispatch:exp  0 1)
(define $dispatch:log  (make-hasheq))
(define-class-method log  $dispatch:log  0 1)
(define $dispatch:sqrt (make-hasheq))
(define-class-method sqrt $dispatch:sqrt 0 1)
(define $dispatch:sin  (make-hasheq))
(define-class-method sin  $dispatch:sin  0 1)
(define $dispatch:cos  (make-hasheq))
(define-class-method cos  $dispatch:cos  0 1)
(define $dispatch:tan  (make-hasheq))
(define-class-method tan  $dispatch:tan  0 1)
(define $dispatch:**   (make-hasheq))
(define-class-method **   $dispatch:**   0 2)

(register-instance-method! $dispatch:exp  'Float (lambda (x) (rkt:exp x)))
(register-instance-method! $dispatch:log  'Float (lambda (x) (rkt:log x)))
(register-instance-method! $dispatch:sqrt 'Float (lambda (x) (rkt:sqrt x)))
(register-instance-method! $dispatch:sin  'Float (lambda (x) (rkt:sin x)))
(register-instance-method! $dispatch:cos  'Float (lambda (x) (rkt:cos x)))
(register-instance-method! $dispatch:tan  'Float (lambda (x) (rkt:tan x)))
(register-instance-method! $dispatch:**   'Float (lambda (x y) (rkt:expt x y)))

(register-instance-method! $dispatch:exp  'Complex (lambda (x) (rkt:exp x)))
(register-instance-method! $dispatch:log  'Complex (lambda (x) (rkt:log x)))
(register-instance-method! $dispatch:sqrt 'Complex (lambda (x) (rkt:sqrt x)))
(register-instance-method! $dispatch:sin  'Complex (lambda (x) (rkt:sin x)))
(register-instance-method! $dispatch:cos  'Complex (lambda (x) (rkt:cos x)))
(register-instance-method! $dispatch:tan  'Complex (lambda (x) (rkt:tan x)))
(register-instance-method! $dispatch:**   'Complex (lambda (x y) (rkt:expt x y)))

;; ----- RealFrac class ---------------------------------------

(define $dispatch:floor-real    (make-hasheq))
(define-class-method floor-real    $dispatch:floor-real    0 1)
(define $dispatch:ceiling-real  (make-hasheq))
(define-class-method ceiling-real  $dispatch:ceiling-real  0 1)
(define $dispatch:round-real    (make-hasheq))
(define-class-method round-real    $dispatch:round-real    0 1)
(define $dispatch:truncate-real (make-hasheq))
(define-class-method truncate-real $dispatch:truncate-real 0 1)

(define (to-int x)
  (rkt:inexact->exact x))

(register-instance-method! $dispatch:floor-real    'Float
                           (lambda (x) (to-int (rkt:floor    x))))
(register-instance-method! $dispatch:ceiling-real  'Float
                           (lambda (x) (to-int (rkt:ceiling  x))))
(register-instance-method! $dispatch:round-real    'Float
                           (lambda (x) (to-int (rkt:round    x))))
(register-instance-method! $dispatch:truncate-real 'Float
                           (lambda (x) (to-int (rkt:truncate x))))

(register-instance-method! $dispatch:floor-real    'Rational
                           (lambda (x) (rkt:floor    x)))
(register-instance-method! $dispatch:ceiling-real  'Rational
                           (lambda (x) (rkt:ceiling  x)))
(register-instance-method! $dispatch:round-real    'Rational
                           (lambda (x) (rkt:round    x)))
(register-instance-method! $dispatch:truncate-real 'Rational
                           (lambda (x) (rkt:truncate x)))

;; ----- RealFloat class --------------------------------------

(define $dispatch:is-nan?      (make-hasheq))
(define-class-method is-nan?      $dispatch:is-nan?      0 1)
(define $dispatch:is-infinite? (make-hasheq))
(define-class-method is-infinite? $dispatch:is-infinite? 0 1)
(define $dispatch:atan2        (make-hasheq))
(define-class-method atan2        $dispatch:atan2        0 2)

(register-instance-method! $dispatch:is-nan?      'Float
                           (lambda (x) (rkt:nan? x)))
(register-instance-method! $dispatch:is-infinite? 'Float
                           (lambda (x) (rkt:infinite? x)))
(register-instance-method! $dispatch:atan2        'Float
                           (lambda (y x) (rkt:atan y x)))

;; ----- try / raise-io ---------------------------------------

(define (try io)
  ($io (lambda ()
         (with-handlers ([exn:fail?
                          (lambda (e) (Err (exn-message e)))])
           (Ok (run-io io))))))

(define (raise-io msg)
  ($io (lambda () (error 'rackton-raise-io msg))))

;; ----- System surface ----------------------------------------

(define/curried (random-integer lo hi)
  ($io (lambda () (rkt:random lo hi))))

(define random-float
  ($io (lambda () (rkt:exact->inexact (rkt:random)))))

(define current-time-seconds
  ($io (lambda () (rkt:current-seconds))))

(define (rkt-list->rackton lst)
  (let loop ([lst (rkt:reverse lst)] [acc Nil])
    (cond
      [(null? lst) acc]
      [else (loop (cdr lst) (Cons (car lst) acc))])))

(define (list-directory path)
  ($io (lambda ()
         (rkt-list->rackton
          (map rkt:path->string (rkt:directory-list path))))))

(define (getenv name)
  ($io (lambda ()
         (define v (rkt:getenv name))
         (if v (Some v) None))))

(define argv
  ($io (lambda ()
         (rkt-list->rackton (vector->list (rkt:argv))))))

(define (delete-file path)
  ($io (lambda () (rkt:delete-file path) MkUnit)))

(define (make-directory path)
  ($io (lambda () (rkt:make-directory path) MkUnit)))

;; System.Exit — terminate the process with the given status code.
;; `exit` does not return, so the IO action's result type is free.
(define (exit-with-code code)
  ($io (lambda () (exit code))))

;; ----- System.IO: handles -------------------------------------
;; A Handle is a Racket port (input or output); operations that don't
;; match the port's direction error at runtime, matching Haskell.  The
;; std handles are captured at module load.

(define stdin  (current-input-port))
(define stdout (current-output-port))
(define stderr (current-error-port))

;; open-file-with-mode: 0 = read, 1 = write (truncate), 2 = append.
;; The Rackton wrapper maps an IOMode to the integer code.
(define/curried (open-file-with-mode path mode)
  ($io (lambda ()
         (cond
           [(rkt:= mode 0) (open-input-file path)]
           [(rkt:= mode 1) (open-output-file path #:exists 'truncate/replace)]
           [else           (open-output-file path #:exists 'append)]))))

(define (h-close h)
  ($io (lambda ()
         (if (input-port? h) (close-input-port h) (close-output-port h))
         MkUnit)))

(define/curried (h-put-str h s)
  ($io (lambda () (write-string s h) MkUnit)))

(define/curried (h-put-str-ln h s)
  ($io (lambda () (write-string s h) (newline h) MkUnit)))

(define (h-flush h)
  ($io (lambda () (flush-output h) MkUnit)))

(define (h-get-contents h)
  ($io (lambda () (port->string h))))

;; h-get-line: (Some line) or None at end-of-file.
(define (h-get-line h)
  ($io (lambda ()
         (define line (rkt:read-line h))
         (if (eof-object? line) None (Some line)))))

;; ----- Numeric: float formatting ------------------------------
;; showFFloat / showEFloat / showGFloat.  `prec` is the number of
;; digits after the point, or negative for full precision (the Rackton
;; wrapper maps None -> -1).  These are pure String -> String
;; computations (not IO), built on racket/format's ~r.

;; ~r renders an exponent as "e+02"; Haskell writes "e2" / "e-2"
;; (no '+', no leading zeros).  Reformat the exponent to match.
(define (normalize-exp s)
  (define m (regexp-match #rx"^(.*)e([-+]?[0-9]+)$" s))
  (if m
      (rkt:string-append (cadr m) "e"
                         (rkt:number->string (rkt:string->number (caddr m))))
      s))

(define/curried (show-f-float prec x)
  (if (rkt:< prec 0)
      (~r x #:notation 'positional)
      (~r x #:notation 'positional #:precision (list '= prec))))

(define/curried (show-e-float prec x)
  (normalize-exp
   (if (rkt:< prec 0)
       (~r x #:notation 'exponential)
       (~r x #:notation 'exponential #:precision (list '= prec)))))

;; General: fixed notation inside [0.1, 1e7), scientific outside
;; (matching Haskell's showGFloat); zero is rendered fixed.
(define/curried (show-g-float prec x)
  (define ax (rkt:abs x))
  (cond
    [(rkt:= x 0.0)             (show-f-float prec x)]
    [(rkt:< ax 0.1)            (show-e-float prec x)]
    [(rkt:>= ax 10000000.0)    (show-e-float prec x)]
    [else                      (show-f-float prec x)]))

;; ----- Functor / Monad instance impls ------------------------

;; Maybe — both `None` and `Some` tags share the same impl, which
;; pattern-matches at runtime.
(define maybe-fmap
  (lambda (f m)
    (match m
      [(None)   None]
      [(Some x) (Some (f x))])))
(register-instance-method! $dispatch:fmap '$ctor:None  maybe-fmap)
(register-instance-method! $dispatch:fmap '$ctor:Some  maybe-fmap)

(define maybe-flatmap
  (lambda (f m)
    (match m
      [(None)   None]
      [(Some x) (f x)])))
(register-instance-method! $dispatch:flatmap '$ctor:None  maybe-flatmap)
(register-instance-method! $dispatch:flatmap '$ctor:Some  maybe-flatmap)

;; List
(define (list-fmap f xs)
  (match xs
    [(Nil)        Nil]
    [(Cons h t)   (Cons (f h) (list-fmap f t))]))
(register-instance-method! $dispatch:fmap '$ctor:Nil   list-fmap)
(register-instance-method! $dispatch:fmap '$ctor:Cons  list-fmap)

;; concatMap — apply `f` to each element and append the resulting lists.
(define (list-flatmap f xs)
  (match xs
    [(Nil)        Nil]
    [(Cons h t)   (semigroup-list-<> (f h) (list-flatmap f t))]))
(register-instance-method! $dispatch:flatmap '$ctor:Nil   list-flatmap)
(register-instance-method! $dispatch:flatmap '$ctor:Cons  list-flatmap)

;; Result e
(define result-fmap
  (lambda (f r)
    (match r
      [(Err x) (Err x)]
      [(Ok  v) (Ok (f v))])))
(register-instance-method! $dispatch:fmap '$ctor:Err   result-fmap)
(register-instance-method! $dispatch:fmap '$ctor:Ok    result-fmap)

(define result-flatmap
  (lambda (f r)
    (match r
      [(Err x) (Err x)]
      [(Ok  v) (f v)])))
(register-instance-method! $dispatch:flatmap '$ctor:Err   result-flatmap)
(register-instance-method! $dispatch:flatmap '$ctor:Ok    result-flatmap)

;; State monad runtime -> rackton/control/monad/state, Env (Reader)
;; runtime -> rackton/control/monad/reader (Phase 2 slim; the modules
;; regenerate it from pure Rackton source).

;; StateT s m runtime moved to rackton/control/monad/state (pure
;; Rackton — the module regenerates run/eval/exec/get/put/modify/lift,
;; the Functor/Applicative/Monad impls, and the mtl pass-throughs).

;; EnvT r m runtime moved to rackton/control/monad/reader (pure Rackton).

;; ----- Applicative instances ------------------------------------

;; Maybe
(define maybe-ap
  (lambda (mf mx)
    (match mf
      [(None)   None]
      [(Some f) (fmap f mx)])))
(register-instance-method! $dispatch:fapply '$ctor:None maybe-ap)
(register-instance-method! $dispatch:fapply '$ctor:Some maybe-ap)

;; Per-instance liftA2 impls (not derived from fapply/fmap so that
;; user-supplied multi-arg lambdas can be applied with both args at
;; once — see the note in prelude.rkt's Applicative class.)
(define (maybe-liftA2 g mx my)
  (match mx
    [(None)   None]
    [(Some x)
     (match my
       [(None)   None]
       [(Some y) (Some (g x y))])]))
(register-instance-method! $dispatch:liftA2 '$ctor:None maybe-liftA2)
(register-instance-method! $dispatch:liftA2 '$ctor:Some maybe-liftA2)

;; List — cartesian product semantics
(define (list-ap fs xs)
  (let cat ([fs fs])
    (match fs
      [(Nil)         Nil]
      [(Cons f rest)
       (let prepend ([mapped (list-fmap f xs)])
         (match mapped
           [(Nil)      (cat rest)]
           [(Cons h t) (Cons h (prepend t))]))])))
(register-instance-method! $dispatch:fapply '$ctor:Nil  list-ap)
(register-instance-method! $dispatch:fapply '$ctor:Cons list-ap)

(define (list-liftA2 g xs ys)
  ;; cartesian: for each x in xs, for each y in ys, (g x y).
  (let outer ([xs xs])
    (match xs
      [(Nil)        Nil]
      [(Cons x xr)
       (let inner ([ys ys])
         (match ys
           [(Nil)         (outer xr)]
           [(Cons y yr)   (Cons (g x y) (inner yr))]))])))
(register-instance-method! $dispatch:liftA2 '$ctor:Nil  list-liftA2)
(register-instance-method! $dispatch:liftA2 '$ctor:Cons list-liftA2)

;; Result e
(define result-ap
  (lambda (rf rx)
    (match rf
      [(Err e) (Err e)]
      [(Ok  f) (fmap f rx)])))
(register-instance-method! $dispatch:fapply '$ctor:Err result-ap)
(register-instance-method! $dispatch:fapply '$ctor:Ok  result-ap)

(define (result-liftA2 g rx ry)
  (match rx
    [(Err e) (Err e)]
    [(Ok x)
     (match ry
       [(Err e) (Err e)]
       [(Ok y)  (Ok (g x y))])]))
(register-instance-method! $dispatch:liftA2 '$ctor:Err result-liftA2)
(register-instance-method! $dispatch:liftA2 '$ctor:Ok  result-liftA2)

;; ----- Bifunctor instances --------------------------------------

(define pair-bimap
  (lambda (f g p)
    (match p
      [(MkPair x y) (MkPair (f x) (g y))])))
(register-instance-method! $dispatch:bimap '$ctor:MkPair pair-bimap)
(define pair-first  (lambda (f p) (pair-bimap f id      p)))
(define pair-second (lambda (g p) (pair-bimap id     g  p)))
(register-instance-method! $dispatch:first  '$ctor:MkPair pair-first)
(register-instance-method! $dispatch:second '$ctor:MkPair pair-second)

(define result-bimap
  (lambda (f g r)
    (match r
      [(Err e) (Err (f e))]
      [(Ok  v) (Ok  (g v))])))
(register-instance-method! $dispatch:bimap '$ctor:Err result-bimap)
(register-instance-method! $dispatch:bimap '$ctor:Ok  result-bimap)
(define result-first  (lambda (f r) (result-bimap f id    r)))
(define result-second (lambda (g r) (result-bimap id    g r)))
(register-instance-method! $dispatch:first  '$ctor:Err result-first)
(register-instance-method! $dispatch:first  '$ctor:Ok  result-first)
(register-instance-method! $dispatch:second '$ctor:Err result-second)
(register-instance-method! $dispatch:second '$ctor:Ok  result-second)

;; ----- Foldable instances ---------------------------------------

;; List
(define (list-foldr f z xs)
  (match xs
    [(Nil)         z]
    [(Cons h rest) (f h (list-foldr f z rest))]))
(register-instance-method! $dispatch:foldr '$ctor:Nil  list-foldr)
(register-instance-method! $dispatch:foldr '$ctor:Cons list-foldr)

;; Maybe
(define (maybe-foldr f z m)
  (match m
    [(None)   z]
    [(Some x) (f x z)]))
(register-instance-method! $dispatch:foldr '$ctor:None maybe-foldr)
(register-instance-method! $dispatch:foldr '$ctor:Some maybe-foldr)

;; Defaults expressed via dispatched foldr — registered once per ctor.
(define (default-length xs)
  (foldr (lambda (_x n) (rkt:+ n 1)) 0 xs))
(define (default-to-list xs)
  (foldr (lambda (x acc) (Cons x acc)) Nil xs))
(define (sum xs)
  (foldr (lambda (a b) (rkt:+ a b)) 0 xs))

;; ----- mconcat (free function with needs-dict signature) ----------
;; Receives the resolved `mempty` for `a` as a leading argument (the
;; elaborator inserts it at every call site based on the free-
;; function detection in var-dict-requirements).  Inner `<>`
;; calls dispatch on the running accumulator's tag and remain
;; generic.
(define/curried (mconcat mempty-impl xs)
  (foldr (lambda (x acc) (<> x acc)) mempty-impl xs))

(for ([tag (in-list '($ctor:Nil $ctor:Cons $ctor:None $ctor:Some))])
  (register-instance-method! $dispatch:length  tag default-length)
  (register-instance-method! $dispatch:to-list tag default-to-list))

;; WriterT w m runtime moved to rackton/control/monad/writer (pure
;; Rackton — value-dispatched fmap/fapply/flatmap combine logs via
;; runtime-dispatched <>; pure/tell thread inner pure + the log mempty).

;; ----- Per-instance compile-time-direct impls ---------
;;
;; The elaborator routes a class-method call at a concrete needs-
;; dict instance directly to one of these by name, bypassing the
;; runtime dispatch table.  Each impl receives the instance's dict
;; args (resolved from the instance qual context, in declaration
;; order) as leading parameters, followed by the user's args.
;;
;; (StateT/EnvT/WriterT class-method impls are regenerated in
;; rackton/control/monad/{state,reader,writer}.)

;; ExceptT e m runtime moved to rackton/control/monad/except (pure
;; Rackton — its value-dispatched fapply/flatmap/catch-e derive the
;; inner pure from the dispatch-arg witness, and it registers a
;; pure-via-witness deriver for MkExceptT so nested ExceptT works).

;; ----- mtl-style class instance impls ------------------
;; Naming: `$<method>:<head-tcon>`.  Lifted instances over a
;; transformer take one dict per return-typed method of the inner
;; class as leading args, in method-declaration order (see
;; build-dict-skolems in infer.rkt).

;; ----- MonadState s X -------------------------------------------

;; $get-st/$put-st/$modify-st for State and StateT moved to
;; rackton/control/monad/state.

;; ($get-st/$put-st/$modify-st:EnvT moved to
;; rackton/control/monad/reader.)

;; ($get-st/$put-st/$modify-st:WriterT moved to
;; rackton/control/monad/writer.)

;; ($get-st/$put-st/$modify-st:ExceptT moved to
;; rackton/control/monad/except.)

;; ----- MonadEnv r X ---------------------------------------------

;; $ask-en/$local-en for Env and EnvT moved to
;; rackton/control/monad/reader.

;; MonadEnv's `local-en` is positional (not return-typed), so it never
;; appears in the dict-args.  Dict-arg order for MonadEnv is own
;; return-typed sorted (ask-en) + Monad super closure (pure).
;; ($ask-en/$local-en:StateT moved to rackton/control/monad/state.)

;; ($ask-en/$local-en:WriterT moved to rackton/control/monad/writer.)

;; ($ask-en/$local-en:ExceptT moved to rackton/control/monad/except.)

;; ----- MonadWriter w X ------------------------------------------

;; ($tell-w/$listen/$censor:WriterT moved to
;; rackton/control/monad/writer.)

;; MonadWriter dict-arg order: own (tell-w), then super closure of
;; ((Monoid w) (Monad m)) → (mempty, pure).
;; ($tell-w:StateT moved to rackton/control/monad/state; $tell-w:EnvT
;; to reader; $tell-w:ExceptT to except.)

;; ----- MonadError e X -------------------------------------------

;; ($throw-e/$catch-e for StateT/EnvT/WriterT/ExceptT moved to their
;; respective rackton/control/monad/* modules.)

;; ----- runtime registrations for transformer-side -----
;;
;; The mtl-style `$method:T` impls accept dict args to match the
;; call-site layout when the elaborator resolves needs-dict
;; instances at compile time.  The lifted catch-e impls don't use
;; those dict args inside their bodies — the recursion happens via
;; runtime-dispatched `catch-e` / `local-en` on the inner value.
;;
;; That means we can register the bodies as plain runtime closures
;; (no dict slots), so polymorphic-monad call sites that fall through
;; to the runtime dispatcher succeed instead of crashing with
;; "no instance".  The compile-time inst-dispatch path keeps using
;; the curried `$method:T` impls; this just adds a parallel path.

;; catch-e on transformer-wrapped values.
;; (catch-e for MkStateT/MkEnvT/MkWriterT registered in
;; rackton/control/monad/{state,reader,writer}.)

;; listen / censor on WriterT registered in
;; rackton/control/monad/writer.

;; local-en on transformer-wrapped values.  These all recurse via
;; runtime local-en on the inner value, so register the same impl
;; without the unused dict slots.
;; (local-en for MkStateT/MkWriterT/MkExceptT registered in their
;; rackton/control/monad/* modules.)

;; ----- default `join` and `product` for prelude instances ----------
;;
;; In user code, the elaborator falls back to a class's default method
;; body whenever an instance omits the method.  The prelude is hand-
;; rolled instead of going through codegen, so we do that fallback
;; manually here: every monad ctor that registered `flatmap` gets
;; `join` derived from `flatmap`, and every applicative ctor that
;; registered `liftA2` gets `product` derived from `liftA2`.

(define default-join    (lambda (mma) (flatmap (lambda (m) m) mma)))
(define default-product (lambda (x y) (liftA2 MkPair x y)))

(for ([ctor (in-hash-keys $dispatch:flatmap)])
  (register-instance-method! $dispatch:join ctor default-join))

(for ([ctor (in-hash-keys $dispatch:liftA2)])
  (register-instance-method! $dispatch:product ctor default-product))


;; ----- return-typed dispatch tables: register the plain prelude impls
;; Keyed by the result type's ctor tag (matching what codegen extracts
;; from the resolved `$pure:Tag` / `$mempty:Tag` impl name).  The
;; needs-dict transformer pures (StateT/EnvT/WriterT/ExceptT) are NOT
;; registered here — their call sites carry dict args and resolve via
;; the direct provided reference, not this table.
(register-instance-method! $dispatch:pure 'Maybe    |$pure:Maybe|)
(register-instance-method! $dispatch:pure 'List     |$pure:List|)
(register-instance-method! $dispatch:pure 'Result   |$pure:Result|)
(register-instance-method! $dispatch:pure 'IO       |$pure:IO|)
;; $pure:State / $pure:Env register from rackton/control/monad/state +
;; reader; $pure:STM from rackton/control/stm.
(register-instance-method! $dispatch:pure 'Identity |$pure:Identity|)

(register-instance-method! $dispatch:mempty 'String  |$mempty:String|)
(register-instance-method! $dispatch:mempty 'List    |$mempty:List|)
;; Sum / Product mempty register themselves from rackton/data/monoid.

;; Other return-typed methods (Floating.pi, MonadState.get-st,
;; MonadEnv.ask-en, Concurrent.await-c/yield-c).  Only the base
;; (non-needs-dict) instances are registered — the lifted transformer
;; instances are needs-dict and resolve via the direct provided ref.
(register-instance-method! $dispatch:pi 'Float   |$pi:Float|)
(register-instance-method! $dispatch:pi 'Complex |$pi:Complex|)

;; get-st/put-st/modify-st:State register from rackton/control/monad/state;
;; ask-en:Env from rackton/control/monad/reader.
;; tell-w / throw-e have only needs-dict instances — no plain base to
;; register; their tables stay empty.

(register-instance-method! $dispatch:await-c 'IO       |$await-c:IO|)
(register-instance-method! $dispatch:await-c 'Identity |$await-c:Identity|)
(register-instance-method! $dispatch:yield-c 'IO       |$yield-c:IO|)
(register-instance-method! $dispatch:lift-io 'IO       |$lift-io:IO|)
(register-instance-method! $dispatch:yield-c 'Identity |$yield-c:Identity|)
