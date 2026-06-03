#lang racket/base

;; Rackton — compile-time side of the prelude.
;;
;; The prelude is itself a Rackton program: a list of class declarations,
;; instance declarations, ADT definitions, and combinators.  We parse and
;; elaborate that program at module-load time and expose the resulting
;; typing environment as `prelude-env`.  The runtime side (dispatch tables,
;; struct definitions, builtin instance registrations) lives in
;; private/prelude-runtime.rkt; the two must keep their names in sync.

(require "surface.rkt"
         "infer.rkt"
         "env.rkt")

(provide prelude-env)

;; ----- Prelude source ----------------------------------------------

(define prelude-source-forms
  '(;; --- Eq ----------------------------------------------------------

    (protocol (Eq a)
      (: == (-> a (-> a Boolean)))
      (: /= (-> a (-> a Boolean)))
      (define (/= x y) (if (== x y) #f #t)))

    ;; --- Ord (Eq is a superclass) -------------------------------

    (protocol (Ord [a => Eq])
      (: <  (-> a (-> a Boolean)))
      (: >  (-> a (-> a Boolean)))
      (: <= (-> a (-> a Boolean)))
      (: >= (-> a (-> a Boolean)))
      ;; Min and max as Ord methods (comparison-based,
      ;; doesn't need numeric ops) with default impls in terms of <.
      (: min (-> a (-> a a)))
      (: max (-> a (-> a a)))
      (define (>  x y) (<  y x))
      (define (<= x y) (if (<  x y) #t (== x y)))
      (define (>= x y) (if (>  x y) #t (== x y)))
      (define (min x y) (if (< x y) x y))
      (define (max x y) (if (< x y) y x)))

    ;; --- Num ----------------------------------------------------

    (protocol (Num a)
      (: +      (-> a (-> a a)))
      (: -      (-> a (-> a a)))
      (: *      (-> a (-> a a)))
      ;; Abs and negate as Num methods, polymorphic over
      ;; the numeric tower (Integer / Float / Rational / Complex).
      (: abs    (-> a a))
      (: negate (-> a a)))

    ;; --- Show ---------------------------------------------------

    (protocol (Show a)
      (: show (-> a String)))

    ;; --- Builtin instances --------------------------------------
    ;; Bodies of the form `(racket τ (vars) 0)` etc. are placeholders;
    ;; only the type discipline matters here.  The actual runtime
    ;; implementations live in prelude-runtime.rkt.

    (instance (Num Integer)
      (define (+ x y) (racket Integer (x y) 0))
      (define (- x y) (racket Integer (x y) 0))
      (define (* x y) (racket Integer (x y) 0))
      (define (abs    x) (racket Integer (x) 0))
      (define (negate x) (racket Integer (x) 0)))

    (instance (Eq Integer)
      (define (== x y) (racket Boolean (x y) #f)))

    (instance (Eq Boolean)
      (define (== x y) (if x y (if y #f #t))))

    (instance (Eq String)
      (define (== x y) (racket Boolean (x y) #f)))

    (instance (Ord Integer)
      (define (< x y) (racket Boolean (x y) #f)))

    (instance (Show Integer)
      (define (show x) (racket String (x) "")))

    (instance (Show Boolean)
      (define (show x) (if x "True" "False")))

    (instance (Show String)
      (define (show x) x))

    ;; Ord String (lex order) so min/max work on strings.
    (instance (Ord String)
      (define (< x y) (racket Boolean (x y) #f)))

    ;; --- ADTs ---------------------------------------------------

    (data (Maybe a)    None (Some a))
    (data (List a)     Nil  (Cons a (List a)))
    (data (Pair a b)   (Pair a b))
    (data (Result e a) (Err e) (Ok a))
    (data Unit Unit)

    ;; --- Char and Bytes primitives (opaque ADTs; values come from
    ;; Racket's reader as `#\A` and `#"…"`) -----------------------

    (data Char)
    (data Bytes)

    (instance (Eq Char)   (define (== x y) (racket Boolean (x y) #f)))
    (instance (Ord Char)  (define (< x y) (racket Boolean (x y) #f)))
    (instance (Show Char) (define (show c) (racket String (c) "")))

    (instance (Eq Bytes)  (define (== x y) (racket Boolean (x y) #f)))
    (instance (Show Bytes)(define (show b) (racket String (b) "")))

    (: char->integer (-> Char Integer))
    (define (char->integer c) (racket Integer (c) 0))

    (: integer->char (-> Integer (Maybe Char)))
    (define (integer->char n) (racket (Maybe Char) (n) None))

    (: char-upcase   (-> Char Char))
    (define (char-upcase c) (racket Char (c) c))
    (: char-downcase (-> Char Char))
    (define (char-downcase c) (racket Char (c) c))

    (: char-alphabetic? (-> Char Boolean))
    (define (char-alphabetic? c) (racket Boolean (c) #f))
    (: char-numeric?    (-> Char Boolean))
    (define (char-numeric? c)    (racket Boolean (c) #f))
    (: char-whitespace? (-> Char Boolean))
    (define (char-whitespace? c) (racket Boolean (c) #f))

    (: char->string  (-> Char String))
    (define (char->string c) (racket String (c) ""))

    (: string-ref     (-> String (-> Integer (Maybe Char))))
    (define (string-ref s i) (racket (Maybe Char) (s i) None))

    (: string->chars (-> String (List Char)))
    (define (string->chars s) (racket (List Char) (s) Nil))

    (: chars->string (-> (List Char) String))
    (define (chars->string cs) (racket String (cs) ""))

    (: bytes-length  (-> Bytes Integer))
    (define (bytes-length b) (racket Integer (b) 0))

    (: bytes-ref     (-> Bytes (-> Integer (Maybe Integer))))
    (define (bytes-ref b i) (racket (Maybe Integer) (b i) None))

    (: make-bytes    (-> Integer (-> Integer Bytes)))
    (define (make-bytes n v) (racket Bytes (n v) #f))

    (: bytes-append  (-> Bytes (-> Bytes Bytes)))
    (define (bytes-append a b) (racket Bytes (a b) #f))

    (: bytes->list   (-> Bytes (List Integer)))
    (define (bytes->list b) (racket (List Integer) (b) Nil))

    (: list->bytes   (-> (List Integer) Bytes))
    (define (list->bytes xs) (racket Bytes (xs) #f))

    (: string->bytes (-> String Bytes))
    (define (string->bytes s) (racket Bytes (s) #f))

    (: bytes->string (-> Bytes (Maybe String)))
    (define (bytes->string b) (racket (Maybe String) (b) None))

    ;; --- Combinators --------------------------------------------

    (: id (-> a a))
    (define (id x) x)

    (: const (-> a (-> b a)))
    (define (const x) (lambda (_y) x))

    (: flip (-> (-> a (-> b c)) (-> b (-> a c))))
    (define (flip f) (lambda (x) (lambda (y) ((f y) x))))

    (: compose (-> (-> b c) (-> (-> a b) (-> a c))))
    (define (compose f) (lambda (g) (lambda (x) (f (g x)))))

    ;; --- Functor / Applicative / Monad (higher-kinded) --------
    ;;
    ;; The hierarchy is Functor → Applicative → Monad.  `pure` is a
    ;; return-typed class method: its type `a -> f a` carries the
    ;; class parameter only in the result, so the elaborator picks
    ;; the per-instance impl at compile time from the expected return
    ;; type at each call site.  See `return-typed-method?` in
    ;; private/infer.rkt and the resolution table consumed by
    ;; private/codegen.rkt.

    (protocol (Functor (f :: (-> * *)))
      (: fmap (-> (-> a b) (-> (f a) (f b)))))

    ;; Applicative has three derivable methods — fapply, liftA2, product —
    ;; arranged in a default cycle so an instance can pick whichever
    ;; primitive it finds most natural and the other two derive.  The
    ;; default-cycle direction is fapply ← product ← liftA2 ← fapply.  An
    ;; instance must define at least one of the three; omitting all of
    ;; them is rejected at compile time (see check-default-cycles in
    ;; private/infer.rkt).
    (protocol (Applicative [f => Functor])
      (: pure    (-> a (f a)))
      (: fapply  (-> (f (-> a b)) (-> (f a) (f b))))
      (: liftA2  (-> (-> a (-> b c)) (-> (f a) (-> (f b) (f c)))))
      (: product (-> (f a) (-> (f b) (f (Pair a b)))))
      (define (fapply ff fa)
        (fmap (lambda (p) (match p [(Pair f x) (f x)])) (product ff fa)))
      (define (liftA2 g x y) (fapply (fmap g x) y))
      (define (product x y) (liftA2 Pair x y))
      ;; Cross-class derivation of the Functor superclass: an Applicative
      ;; instance written with `#:derive-superclasses` (supplying `pure`
      ;; and `fapply`) gets `Functor` for free via `fmap f = pure f <*>`.
      (#:derive Functor
        (define (fmap f x) (fapply (pure f) x))))

    ;; Monad has two derivable methods — flatmap and join — with mutual
    ;; defaults.  An instance must define at least one.  flatmap takes
    ;; the continuation first and the monadic value second; the
    ;; ordering matches `flip (>>=)` from Haskell.
    ;; Monad also carries cross-class derivations for its Applicative and
    ;; Functor superclasses: an instance written with
    ;; `#:derive-superclasses` need only supply `pure` (the irreducible
    ;; Applicative primitive) and one of `flatmap`/`join`; `fmap` and the
    ;; Applicative combinators are synthesized from these.
    (protocol (Monad [m => Applicative])
      (: flatmap (-> (-> a (m b)) (-> (m a) (m b))))
      (: join    (-> (m (m a)) (m a)))
      (define (flatmap f ma) (join (fmap f ma)))
      (define (join mma)     (flatmap (lambda (m) m) mma))
      ;; The derived bodies use only `flatmap` and `pure` — never `fmap`,
      ;; `fapply`, or `liftA2` — so a synthesized superclass instance never
      ;; forward-references a class registered after it.  `liftA2` is
      ;; provided too (not left to its default, which would call `fmap`);
      ;; `product` then derives from `liftA2`.
      (#:derive Functor
        (define (fmap f x) (flatmap (lambda (a) (pure (f a))) x)))
      (#:derive Applicative
        (define (fapply ff fx)
          (flatmap (lambda (g) (flatmap (lambda (x) (pure (g x))) fx)) ff))
        ;; Apply `g` with an n-ary call `(g a b)`, not a curried `((g a) b)`:
        ;; `product`'s default passes the raw 2-ary `Pair` constructor as
        ;; `g`, and a constructor cannot be partially applied.
        (define (liftA2 g x y)
          (flatmap (lambda (a) (flatmap (lambda (b) (pure (g a b))) y)) x))))

    ;; Maybe
    (instance (Functor Maybe)
      (define (fmap f m)
        (match m
          [(None)   None]
          [(Some x) (Some (f x))])))

    (instance (Applicative Maybe)
      (define (pure x) (Some x))
      (define (fapply mf mx)
        (match mf
          [(None)   None]
          [(Some f) (fmap f mx)])))

    (instance (Monad Maybe)
      (define (flatmap f m)
        (match m
          [(None)   None]
          [(Some x) (f x)])))

    ;; List
    (instance (Functor List)
      (define (fmap f xs)
        (match xs
          [(Nil)        Nil]
          [(Cons h t)   (Cons (f h) (fmap f t))])))

    (instance (Applicative List)
      (define (pure x) (Cons x Nil))
      ;; cartesian-product semantics — for each function in `fs` apply
      ;; it to every `x` in `xs`, concatenating.  We can't yet call
      ;; the top-level `append` (it's defined later in the prelude), so
      ;; we inline a local concat helper.
      (define (fapply fs xs)
        (letrec ([cat (lambda (a b)
                        (match a
                          [(Nil)      b]
                          [(Cons h t) (Cons h (cat t b))]))])
          (match fs
            [(Nil)         Nil]
            [(Cons f rest) (cat (fmap f xs) (fapply rest xs))]))))

    ;; concatMap semantics — apply `f` to every element and concatenate
    ;; the resulting lists.  As with `fapply` above, the top-level
    ;; `append` isn't defined yet, so we inline the same `cat` helper.
    (instance (Monad List)
      (define (flatmap f xs)
        (letrec ([cat (lambda (a b)
                        (match a
                          [(Nil)      b]
                          [(Cons h t) (Cons h (cat t b))]))])
          (match xs
            [(Nil)        Nil]
            [(Cons h t)   (cat (f h) (flatmap f t))]))))

    ;; Result e (the error type is fixed; we map over the success type)
    (instance (Functor (Result e))
      (define (fmap f r)
        (match r
          [(Err x) (Err x)]
          [(Ok  v) (Ok (f v))])))

    (instance (Applicative (Result e))
      (define (pure x) (Ok x))
      (define (fapply rf rx)
        (match rf
          [(Err e) (Err e)]
          [(Ok  f) (fmap f rx)])))

    (instance (Monad (Result e))
      (define (flatmap f r)
        (match r
          [(Err x) (Err x)]
          [(Ok  v) (f v)])))

    ;; --- Bifunctor ---------------------------------------------
    ;;
    ;; Two-parameter higher-kinded class for shapes with TWO "slots"
    ;; that can be mapped over independently — `Pair a b` and
    ;; `Result e a` are the canonical instances.  `bimap` dispatches
    ;; on the third positional argument (the `p a b` value); `first`
    ;; and `second` are derived defaults that fix one side via `id`.

    (protocol (Bifunctor (p :: (-> * (-> * *))))
      (: bimap  (-> (-> a c) (-> (-> b d) (-> (p a b) (p c d)))))
      (: first  (-> (-> a c) (-> (p a b) (p c b))))
      (: second (-> (-> b d) (-> (p a b) (p a d))))
      (define (first  f x) (bimap f  id x))
      (define (second g x) (bimap id g  x)))

    (instance (Bifunctor Pair)
      (define (bimap f g p)
        (match p
          [(Pair x y) (Pair (f x) (g y))])))

    (instance (Bifunctor Result)
      (define (bimap f g r)
        (match r
          [(Err e) (Err (f e))]
          [(Ok  v) (Ok  (g v))])))

    ;; --- Category / Arrow --------------------------------------
    ;;
    ;; Arrows (Hughes) generalize plain functions and Kleisli arrows.
    ;; The hierarchy is Category → Arrow.  Method names are non-infix
    ;; and chosen to avoid clashing with existing prelude names:
    ;; `ident`/`comp` (Category — distinct from the standalone `id`
    ;; function and the backward `compose`) and
    ;; `arr`/`on-first`/`on-second`/`split`/`fanout` (Arrow — distinct
    ;; from `Bifunctor`'s `first`/`second`).  `ident` and `arr` are
    ;; return-typed: the class parameter `cat` appears only in the
    ;; result, so each call site resolves the instance at compile time
    ;; from the expected type — the same mechanism as `pure`/`mempty`.
    ;; The canonical instance is the function arrow `(->)`, where every
    ;; combinator collapses to ordinary function plumbing.

    (protocol (Category (cat :: (-> * (-> * *))))
      (: ident (cat a a))
      (: comp (-> (cat a b) (-> (cat b c) (cat a c)))))

    (protocol (Arrow [cat => Category])
      (: arr       (-> (-> a b) (cat a b)))
      (: on-first  (-> (cat a b) (cat (Pair a c) (Pair b c))))
      (: on-second (-> (cat a b) (cat (Pair c a) (Pair c b))))
      (: split     (-> (cat a b) (-> (cat c d) (cat (Pair a c) (Pair b d)))))
      (: fanout    (-> (cat a b) (-> (cat a c) (cat a (Pair b c)))))
      ;; on-second/split/fanout derive from `arr`, `on-first`, and the
      ;; superclass `comp`, so an instance need only supply `arr` and
      ;; `on-first` (plus Category's `ident`/`comp`).  `swap`/`dup` are
      ;; inlined as lambdas.
      (define (on-second g)
        (comp (arr (lambda (p) (match p [(Pair a b) (Pair b a)])))
              (comp (on-first g)
                    (arr (lambda (p) (match p [(Pair a b) (Pair b a)]))))))
      (define (split f g) (comp (on-first f) (on-second g)))
      (define (fanout f g)
        (comp (arr (lambda (x) (Pair x x))) (split f g))))

    (instance (Category (->))
      (define ident (lambda (x) x))
      (define (comp f g) (lambda (x) (g (f x)))))

    (instance (Arrow (->))
      (define (arr f) f)
      (define (on-first f)
        (lambda (p) (match p [(Pair a c) (Pair (f a) c)]))))

    ;; ArrowChoice routes a `Result` through one of two arrows by
    ;; branch.  `Err` is the Left (active) branch and `Ok` the Right, so
    ;; `on-left` transforms the `Err` payload and passes `Ok` through.
    ;; on-right/fork/fanin derive from on-left + arr + comp, so an
    ;; instance need only supply `on-left`.  `mirror` (swap branches)
    ;; and the same-typed-Result collapse are inlined as lambdas.
    (protocol (ArrowChoice [cat => Arrow])
      (: on-left  (-> (cat a b) (cat (Result a x) (Result b x))))
      (: on-right (-> (cat a b) (cat (Result x a) (Result x b))))
      (: fork     (-> (cat a b) (-> (cat c d) (cat (Result a c) (Result b d)))))
      (: fanin    (-> (cat a c) (-> (cat b c) (cat (Result a b) c))))
      (define (on-right g)
        (comp (arr (lambda (r) (match r [(Err x) (Ok x)] [(Ok x) (Err x)])))
              (comp (on-left g)
                    (arr (lambda (r) (match r [(Err x) (Ok x)] [(Ok x) (Err x)]))))))
      (define (fork f g) (comp (on-left f) (on-right g)))
      (define (fanin f g)
        (comp (fork f g)
              (arr (lambda (r) (match r [(Err c) c] [(Ok c) c]))))))

    (instance (ArrowChoice (->))
      (define (on-left f)
        (lambda (r) (match r [(Err a) (Err (f a))] [(Ok x) (Ok x)]))))

    ;; ArrowApply makes the arrow a first-class value that can be fed in
    ;; alongside its argument and run.  `arrow-app` is return-typed (a
    ;; constant arrow), so it resolves at compile time like `ident`.
    (protocol (ArrowApply [cat => Arrow])
      (: arrow-app (cat (Pair (cat a b) a) b)))

    (instance (ArrowApply (->))
      (define arrow-app (lambda (p) (match p [(Pair f x) (f x)]))))

    ;; ArrowLoop ties a feedback channel: the `c` half of the output is
    ;; fed back as the `c` half of the input.  Deliberately NO instance
    ;; for `(->)`: a lawful function-arrow loop needs laziness to tie the
    ;; recursive knot, which strict Rackton functions cannot do, so
    ;; `arrow-loop` (and proc `rec`) over a plain function is correctly a
    ;; type error.  A user arrow whose representation supports feedback
    ;; (e.g. a circuit/stream arrow with a unit delay) can define one.
    (protocol (ArrowLoop [cat => Arrow])
      (: arrow-loop (-> (cat (Pair a c) (Pair b c)) (cat a b))))

    ;; --- Small stdlib ------------------------------------------

    (: not (-> Boolean Boolean))
    (define (not b) (if b #f #t))

    (: and (-> Boolean (-> Boolean Boolean)))
    (define (and a b) (if a b #f))

    (: or (-> Boolean (-> Boolean Boolean)))
    (define (or a b) (if a #t b))

    ;; --- Foldable ---------------------------------------------
    ;;
    ;; Generalizes the right-fold over any container type with one
    ;; type parameter.  Replaces the previous List-only `foldr` and
    ;; `length` with class methods; runtime single-dispatch on the
    ;; container value routes to the right instance.

    (protocol (Foldable (t :: (-> * *)))
      (: foldr   (-> (-> a (-> b b)) (-> b (-> (t a) b))))
      (: length  (-> (t a) Integer))
      (: to-list (-> (t a) (List a)))
      (define (length  xs) (foldr (lambda (_x n)   (+ n 1))      0   xs))
      (define (to-list xs) (foldr (lambda (x acc)  (Cons x acc))  Nil xs)))

    (instance (Foldable List)
      (define (foldr f z xs)
        (match xs
          [(Nil)         z]
          [(Cons h rest) (f h (foldr f z rest))])))

    (instance (Foldable Maybe)
      (define (foldr f z m)
        (match m
          [(None)   z]
          [(Some x) (f x z)])))

    (: sum ((Foldable t) => (-> (t Integer) Integer)))
    (define (sum xs) (foldr (lambda (a b) (+ a b)) 0 xs))

    ;; --- Traversable -------------------------------------------
    ;;
    ;; `traverse` walks a container `t` calling an `Applicative`-
    ;; effectful function on each element and rebuilds the container
    ;; inside that applicative.  At each concrete call site the
    ;; elaborator resolves the `Applicative f` constraint and inserts
    ;; the corresponding `pure-impl` as the leading argument (the
    ;; dict-passing path).

    (protocol (Traversable (t :: (-> * *)))
      (: traverse ((Applicative f) =>
                   (-> (-> a (f b)) (-> (t a) (f (t b)))))))

    (instance (Traversable Maybe)
      (define (traverse f m)
        (match m
          [(None)   (pure None)]
          [(Some x) (fmap Some (f x))])))

    (instance (Traversable List)
      (define (traverse f xs)
        (match xs
          [(Nil)         (pure Nil)]
          [(Cons h rest) (liftA2 Cons (f h) (traverse f rest))])))

    ;; --- Semigroup / Monoid ----------------------------------
    ;;
    ;; `Semigroup` carries an associative `mappend`.  `Monoid`
    ;; refines it with a left+right identity `mempty`.  `mempty` is a
    ;; return-typed class member — the elaborator picks the
    ;; instance from the expected type at each call site (see
    ;; @secref{Return-typed_dispatch} for the mechanism).

    (protocol (Semigroup a)
      (: mappend (-> a (-> a a))))

    (protocol (Monoid [a => Semigroup])
      (: mempty a))

    (instance (Semigroup String)
      ;; `string-append` is defined later in the prelude; use a host
      ;; escape so the load order doesn't bite us.
      (define (mappend a b) (racket String (a b) #f)))

    (instance (Monoid String)
      (define mempty ""))

    (instance (Semigroup (List a))
      ;; cartesian concat; `append` is defined later in this prelude,
      ;; so inline a local cat helper as the Applicative List instance
      ;; does.
      (define (mappend xs ys)
        (letrec ([cat (lambda (as bs)
                        (match as
                          [(Nil)      bs]
                          [(Cons h t) (Cons h (cat t bs))]))])
          (cat xs ys))))

    (instance (Monoid (List a))
      (define mempty Nil))

    ;; `Sum` / `Product` (additive/multiplicative Monoid newtypes) moved
    ;; to rackton/data/monoid (Phase 2 slim).

    ;; --- mconcat ------------------------------------------------
    ;;
    ;; `mconcat : (Monoid a) => (List a) -> a` carries a Monoid-
    ;; constrained `a` whose `mempty` is resolved per call site.  The
    ;; elaborator prepends the resolved `$mempty:T` impl as a leading
    ;; argument; the runtime implementation lives in prelude-runtime.
    ;; A polymorphic body is not provided here because Rackton can't
    ;; yet rewrite user-written needs-dict bodies.
    (: mconcat ((Monoid a) => (-> (List a) a)))

    ;; State monad -> rackton/control/monad/state and Env (Reader) monad
    ;; -> rackton/control/monad/reader (Phase 2 slim; pure Rackton, the
    ;; modules regenerate their runtime).  The MonadState/MonadEnv classes
    ;; and the StateT/EnvT transformers stay below.

    ;; StateT s m (state over an inner monad) moved to
    ;; rackton/control/monad/state (Phase 2 slim, finding 2026-05-30:
    ;; the needs-dict-body machinery makes it authorable as pure
    ;; Rackton — no hand-written runtime needed).  That module owns
    ;; every mtl instance where StateT is the outer transformer.

    ;; EnvT r m (env-passing over an inner monad) moved to
    ;; rackton/control/monad/reader (Phase 2 slim, pure Rackton).  That
    ;; module owns every mtl instance where EnvT is the outer transformer.

    ;; WriterT w m (accumulating writer over an inner monad) moved to
    ;; rackton/control/monad/writer (Phase 2 slim, pure Rackton).  That
    ;; module owns every mtl instance where WriterT is the outer
    ;; transformer.

    ;; ExceptT e m (typed exceptions over an inner monad) moved to
    ;; rackton/control/monad/except (Phase 2 slim, pure Rackton — its
    ;; value-dispatched methods derive the inner pure from a witness).
    ;; That module owns every mtl instance where ExceptT is the outer
    ;; transformer.

    ;; --- mtl-style classes ---------------------------
    ;;
    ;; Polymorphic effect interfaces.  Code can ask for "any monad
    ;; with state / env / log / errors" via these classes; the
    ;; concrete transformer the caller picks chooses the instance.
    ;; Each class has a fundep `m -> X` so the inferer can recover
    ;; the effect's payload type from the monad.  See @secref{mtl-
    ;; style_classes_(Phase_31)} in the docs.

    ;; ----- MonadState -------------------------------------------

    (protocol (MonadState s [m => Monad])
      (#:fundep m -> s)
      (: get-st    (m s))
      (: put-st    (-> s (m Unit)))
      (: modify-st (-> (-> s s) (m Unit))))

    ;; (MonadState instances for State and StateT moved to
    ;; rackton/control/monad/state)

    ;; (MonadState (EnvT r m) moved to rackton/control/monad/reader)

    ;; (MonadState (WriterT w m) moved to rackton/control/monad/writer)

    ;; (MonadState (ExceptT e m) moved to rackton/control/monad/except)

    ;; ----- MonadEnv (Reader) ------------------------------------

    (protocol (MonadEnv r [m => Monad])
      (#:fundep m -> r)
      (: ask-en   (m r))
      (: local-en (-> (-> r r) (-> (m a) (m a)))))

    ;; (MonadEnv instances for Env and EnvT moved to
    ;; rackton/control/monad/reader; MonadEnv (StateT s m) to
    ;; rackton/control/monad/state)

    ;; (MonadEnv (WriterT w m) moved to rackton/control/monad/writer)

    ;; (MonadEnv (ExceptT e m) moved to rackton/control/monad/except)

    ;; ----- MonadWriter ------------------------------------------

    (protocol (MonadWriter [w => Monoid] [m => Monad])
      (#:fundep m -> w)
      (: tell-w (-> w (m Unit)))
      (: listen (-> (m a) (m (Pair a w))))
      (: censor (-> (-> w w) (-> (m a) (m a)))))

    ;; (MonadWriter (WriterT w m) moved to rackton/control/monad/writer)

    ;; (MonadWriter (StateT s m) moved to rackton/control/monad/state)

    ;; (MonadWriter (EnvT r m) moved to rackton/control/monad/reader)

    ;; (MonadWriter (ExceptT e m) moved to rackton/control/monad/except)

    ;; ----- MonadError -------------------------------------------

    (protocol (MonadError e [m => Monad])
      (#:fundep m -> e)
      (: throw-e (-> e (m a)))
      (: catch-e (-> (m a) (-> (-> e (m a)) (m a)))))

    ;; (MonadError (ExceptT e m) moved to rackton/control/monad/except)

    ;; (MonadError (StateT s m) moved to rackton/control/monad/state)

    ;; (MonadError (EnvT r m) moved to rackton/control/monad/reader)

    ;; (MonadError (WriterT w m) moved to rackton/control/monad/writer)

    ;; ----- mtl-style combinators ---------------------
    ;;
    ;; `asks` / `gets` thread a pure transformation through the
    ;; reader / state effect.  `void`, `when`, `unless` are basic
    ;; Functor / Applicative helpers.

    (: asks ((MonadEnv r m) => (-> (-> r a) (m a))))
    (define (asks f) (fmap f ask-en))

    (: gets ((MonadState s m) => (-> (-> s a) (m a))))
    (define (gets f) (fmap f get-st))

    (: void ((Functor f) => (-> (f a) (f Unit))))
    (define (void fa) (fmap (const Unit) fa))

    (: when ((Applicative f) => (-> Boolean (-> (f Unit) (f Unit)))))
    (define (when b fu)
      (if b fu (pure Unit)))

    (: unless ((Applicative f) => (-> Boolean (-> (f Unit) (f Unit)))))
    (define (unless b fu)
      (if b (pure Unit) fu))

    (: filter (-> (-> a Boolean) (-> (List a) (List a))))
    (define (filter p xs)
      (match xs
        [(Nil)        Nil]
        [(Cons h t)   (if (p h)
                          (Cons h (filter p t))
                          (filter p t))]))

    ;; --- Strings ----------------------------------------------

    (: string-length (-> String Integer))
    (define (string-length s) (racket Integer (s) 0))

    (: string-append (-> String (-> String String)))
    (define (string-append a b) (racket String (a b) ""))

    (: substring (-> String (-> Integer (-> Integer String))))
    (define (substring s start end) (racket String (s start end) ""))

    (: string-prefix? (-> String (-> String Boolean)))
    (define (string-prefix? p s) (racket Boolean (p s) #f))

    (: string-split (-> String (-> String (List String))))
    (define (string-split sep s) (racket (List String) (sep s) Nil))

    (: string-join (-> String (-> (List String) String)))
    (define (string-join sep ss) (racket String (sep ss) ""))

    ;; --- Numeric helpers --------------------------------------
    ;; `mod` and `div` migrated to the Integral class.
    ;; `abs` / `negate` migrated to Num; `min` / `max` to Ord.

    (: integer->string (-> Integer String))
    (define (integer->string n) (racket String (n) ""))

    (: string->integer (-> String (Maybe Integer)))
    (define (string->integer s) (racket (Maybe Integer) (s) None))

    ;; --- IO ---------------------------------------------------

    (data (IO a))

    (instance (Functor IO)
      (define (fmap f io) (racket (IO b) (f io) #f)))

    (instance (Applicative IO)
      (define (pure x) (racket (IO a) (x) #f))
      (define (fapply iof iox) (racket (IO b) (iof iox) #f)))

    (instance (Monad IO)
      (define (flatmap f io) (racket (IO b) (io f) #f)))

    ;; --- MonadIO ----------------------------------------
    ;;
    ;; Lift an IO action into any monad that ultimately bottoms out at
    ;; IO.  The base instance (IO itself) is the identity; the
    ;; transformer instances (in rackton/control/monad/trans) lift
    ;; through one layer at a time.  lift-io is return-typed (dispatches
    ;; on the result monad), so the class lives here in the prelude and
    ;; instances register cross-module via the dispatch table.
    (protocol (MonadIO [m => Monad])
      (: lift-io (-> (IO a) (m a))))

    (instance (MonadIO IO)
      (define (lift-io io) (racket (IO a) (io) #f)))

    ;; --- MonadTrans -------------------------------------
    ;;
    ;; A monad transformer @racket[t] can lift an action of its inner
    ;; monad @racket[m] into @racket[t m].  @racket[t] has kind
    ;; @racket[(* -> *) -> (* -> *)].  lift is return-typed (dispatches
    ;; on the transformer), so the class lives here and the instances
    ;; (one per transformer) live in rackton/control/monad/trans.
    (protocol (MonadTrans (t :: (-> (-> * *) (-> * *))))
      (: lift ((Monad m) => (-> (m a) (t m a)))))

    ;; --- Ptr + Storable (raw memory) --------------------
    ;; The opaque pointer type lives here (rather than in
    ;; rackton/foreign/ptr) because Storable.peek is return-typed
    ;; (its element type appears only in the result), so the class —
    ;; and the type its methods mention — must be in the prelude, like
    ;; the MonadIO precedent.  No prelude instances: the Storable
    ;; instances (with ffi peek/poke bodies) live in rackton/foreign/ptr.
    ;; `peek` is return-typed; `poke` dispatches on its value argument.
    (data (Ptr a))

    (protocol (Storable a)
      (: peek (-> (Ptr a) (IO a)))
      (: poke (-> (Ptr a) (-> a (IO Unit)))))

    (: print     (-> String (IO Unit)))
    (define (print s) (racket (IO Unit) (s) #f))

    (: println   (-> String (IO Unit)))
    (define (println s) (racket (IO Unit) (s) #f))

    (: read-line (IO String))
    (define read-line (racket (IO String) () #f))

    (: pure-io   (-> a (IO a)))
    (define (pure-io x) (racket (IO a) (x) #f))

    (: run-io    (-> (IO a) a))
    (define (run-io io) (racket a (io) #f))

    ;; Mutable references (Ref), file I/O, try/raise-io, and the System
    ;; surface moved to rackton/system (Phase 2 slim; runtime in
    ;; private/prelude-runtime via `foreign`).

    ;; Concurrency primitives (ThreadId/MVar/Chan + fork-io/mvar/chan
    ;; ops) moved to rackton/control/concurrent (Phase 2 slim; runtime in
    ;; private/prelude-runtime via `foreign`).  The Concurrent class +
    ;; its instances + Future stay below.

    ;; STM (TVar/STM types, instances, ops) moved to rackton/control/stm
    ;; (Phase 2 slim; runtime in private/prelude-runtime via `foreign`).

    ;; --- Concurrent class -------------------------
    ;;
    ;; `Future a` is a handle to a forked computation's result.
    ;; `Concurrent m` abstracts fork/await/yield across monads.
    ;; The Concurrent IO instance is the only one provided here;
    ;; later additions may include STM (transactional) or Mock
    ;; (pure-time-step) instances.

    (data (Future a))

    (protocol (Concurrent [m => Monad])
      (: fork-c   (-> (m a) (m (Future a))))
      (: await-c  (-> (Future a) (m a)))
      (: yield-c  (m Unit)))

    (instance (Concurrent IO)
      (define (fork-c io)
        (racket (IO (Future a)) (io)    #f))
      (define (await-c fut)
        (racket (IO a)          (fut)   #f))
      (define yield-c
        (racket (IO Unit)       ()      #f)))

    ;; --- Identity monad + Concurrent Identity -----
    ;;
    ;; A trivial Mock for polymorphic-Concurrent code: fork-c runs
    ;; the computation immediately and stuffs its result into a
    ;; Future; await-c reads it back.  Useful for deterministic
    ;; unit tests of polymorphic concurrent code.

    (data (Identity a) (Identity a))

    (: run-identity (-> (Identity a) a))
    (define (run-identity i)
      (match i [(Identity x) x]))

    (instance (Functor Identity)
      (define (fmap f i)
        (match i [(Identity x) (Identity (f x))])))

    (instance (Applicative Identity)
      (define (pure x)        (Identity x))
      (define (fapply ifn ix)
        (match ifn
          [(Identity f)
           (match ix [(Identity x) (Identity (f x))])])))

    (instance (Monad Identity)
      (define (flatmap f i)
        (match i [(Identity x) (f x)])))

    (instance (Concurrent Identity)
      (define (fork-c m)
        (match m
          [(Identity x) (Identity (racket (Future a) (x) #f))]))
      (define (await-c fut)
        (Identity (racket a (fut) #f)))
      (define yield-c
        (Identity Unit)))

    ;; Optics (Lens / Prism / Traversal) moved to rackton/data/lens
    ;; (Phase 2 slim).

    ;; (File I/O moved to rackton/system)

    ;; --- List & pair helpers ---------------------------------

    (: append (-> (List a) (-> (List a) (List a))))
    (define (append xs ys)
      (match xs
        [(Nil)        ys]
        [(Cons h t)   (Cons h (append t ys))]))

    (: reverse (-> (List a) (List a)))
    (define (reverse xs)
      (foldr (lambda (h acc) (append acc (Cons h Nil))) Nil xs))

    (: fst (-> (Pair a b) a))
    (define (fst p) (match p [(Pair a _) a]))

    (: snd (-> (Pair a b) b))
    (define (snd p) (match p [(Pair _ b) b]))

    ;; zip / take / drop / find / split-at / merge-lists / sort /
    ;; concat-map moved to rackton/data/list; swap to rackton/data/tuple
    ;; (Phase 2 slim).

    ;; Unrecoverable failure with a message.  Typed at bottom so it can
    ;; appear anywhere; raises at runtime.
    (: panic (-> String a))
    (define (panic msg) (racket a (msg) #f))

    ;; Map / Set (and group-by) moved to rackton/data/map +
    ;; rackton/data/set (Phase 2 slim; runtime via
    ;; private/containers-runtime reached through `foreign`).

    ;; --- Float type + instances ----------------------------

    (instance (Num Float)
      (define (+ x y) (racket Float (x y) 0.0))
      (define (- x y) (racket Float (x y) 0.0))
      (define (* x y) (racket Float (x y) 0.0))
      (define (abs    x) (racket Float (x) 0.0))
      (define (negate x) (racket Float (x) 0.0)))

    (instance (Eq Float)
      (define (== x y) (racket Boolean (x y) #f)))

    (instance (Ord Float)
      (define (< x y) (racket Boolean (x y) #f)))

    (instance (Show Float)
      (define (show x) (racket String (x) "")))

    (protocol (Fractional [a => Num])
      (: float-div (-> a (-> a a))))

    (instance (Fractional Float)
      (define (float-div x y) (racket Float (x y) 0.0)))

    (: integer->float (-> Integer Float))
    (define (integer->float n) (racket Float (n) 0.0))

    (: float->integer (-> Float Integer))
    (define (float->integer x) (racket Integer (x) 0))

    (: abs-float (-> Float Float))
    (define (abs-float x) (racket Float (x) 0.0))

    ;; --- Rational + Complex types -----------------

    (data Rational)
    (data Complex)

    (: make-rational (-> Integer (-> Integer Rational)))
    (define (make-rational n d) (racket Rational (n d) #f))

    (: numerator   (-> Rational Integer))
    (define (numerator   r) (racket Integer (r) 0))

    (: denominator (-> Rational Integer))
    (define (denominator r) (racket Integer (r) 1))

    (: make-complex (-> Float (-> Float Complex)))
    (define (make-complex re im) (racket Complex (re im) #f))

    (: real-part (-> Complex Float))
    (define (real-part c) (racket Float (c) 0.0))

    (: imag-part (-> Complex Float))
    (define (imag-part c) (racket Float (c) 0.0))

    (: magnitude (-> Complex Float))
    (define (magnitude c) (racket Float (c) 0.0))

    ;; Eq / Ord / Show for Rational
    (instance (Eq Rational)
      (define (== x y) (racket Boolean (x y) #f)))
    (instance (Ord Rational)
      (define (< x y) (racket Boolean (x y) #f)))
    (instance (Show Rational)
      (define (show x) (racket String (x) "")))

    ;; Num / Fractional for Rational
    (instance (Num Rational)
      (define (+ x y) (racket Rational (x y) #f))
      (define (- x y) (racket Rational (x y) #f))
      (define (* x y) (racket Rational (x y) #f))
      (define (abs    x) (racket Rational (x) #f))
      (define (negate x) (racket Rational (x) #f)))
    (instance (Fractional Rational)
      (define (float-div x y) (racket Rational (x y) #f)))

    ;; Eq / Show for Complex
    (instance (Eq Complex)
      (define (== x y) (racket Boolean (x y) #f)))
    (instance (Show Complex)
      (define (show x) (racket String (x) "")))

    (instance (Num Complex)
      (define (+ x y) (racket Complex (x y) #f))
      (define (- x y) (racket Complex (x y) #f))
      (define (* x y) (racket Complex (x y) #f))
      (define (abs    x) (racket Complex (x) #f))
      (define (negate x) (racket Complex (x) #f)))
    (instance (Fractional Complex)
      (define (float-div x y) (racket Complex (x y) #f)))

    ;; --- Integral class ----------------------------

    (protocol (Integral [a => Num])
      (: div  (-> a (-> a a)))
      (: mod  (-> a (-> a a)))
      (: quot (-> a (-> a a)))
      (: rem  (-> a (-> a a))))

    (instance (Integral Integer)
      (define (div  a b) (racket Integer (a b) 0))
      (define (mod  a b) (racket Integer (a b) 0))
      (define (quot a b) (racket Integer (a b) 0))
      (define (rem  a b) (racket Integer (a b) 0)))

    ;; --- Real class --------------------------------

    (protocol (Real [a => Num Ord])
      (: to-rational (-> a Rational)))

    (instance (Real Integer)
      (define (to-rational n) (racket Rational (n) #f)))
    (instance (Real Float)
      (define (to-rational x) (racket Rational (x) #f)))
    (instance (Real Rational)
      (define (to-rational x) (racket Rational (x) #f)))

    ;; --- Floating class ----------------------------

    (protocol (Floating [a => Fractional])
      (: pi   a)
      (: exp  (-> a a))
      (: log  (-> a a))
      (: sqrt (-> a a))
      (: sin  (-> a a))
      (: cos  (-> a a))
      (: tan  (-> a a))
      (: **   (-> a (-> a a))))

    (instance (Floating Float)
      (define pi   (racket Float () 0.0))
      (define (exp  x)   (racket Float (x)   0.0))
      (define (log  x)   (racket Float (x)   0.0))
      (define (sqrt x)   (racket Float (x)   0.0))
      (define (sin  x)   (racket Float (x)   0.0))
      (define (cos  x)   (racket Float (x)   0.0))
      (define (tan  x)   (racket Float (x)   0.0))
      (define (**   x y) (racket Float (x y) 0.0)))

    (instance (Floating Complex)
      (define pi   (racket Complex () #f))
      (define (exp  x)   (racket Complex (x)   #f))
      (define (log  x)   (racket Complex (x)   #f))
      (define (sqrt x)   (racket Complex (x)   #f))
      (define (sin  x)   (racket Complex (x)   #f))
      (define (cos  x)   (racket Complex (x)   #f))
      (define (tan  x)   (racket Complex (x)   #f))
      (define (**   x y) (racket Complex (x y) #f)))

    ;; --- RealFrac class ----------------------------
    ;;
    ;; Methods are named `*-real` to avoid clashing with potential
    ;; future Racket-side helpers; the rounding operations all
    ;; produce Integer regardless of `a`.

    (protocol (RealFrac [a => Real Fractional])
      (: floor-real    (-> a Integer))
      (: ceiling-real  (-> a Integer))
      (: round-real    (-> a Integer))
      (: truncate-real (-> a Integer)))

    (instance (RealFrac Float)
      (define (floor-real    x) (racket Integer (x) 0))
      (define (ceiling-real  x) (racket Integer (x) 0))
      (define (round-real    x) (racket Integer (x) 0))
      (define (truncate-real x) (racket Integer (x) 0)))
    (instance (RealFrac Rational)
      (define (floor-real    x) (racket Integer (x) 0))
      (define (ceiling-real  x) (racket Integer (x) 0))
      (define (round-real    x) (racket Integer (x) 0))
      (define (truncate-real x) (racket Integer (x) 0)))

    ;; --- RealFloat class ---------------------------

    (protocol (RealFloat [a => RealFrac Floating])
      (: is-nan?      (-> a Boolean))
      (: is-infinite? (-> a Boolean))
      (: atan2        (-> a (-> a a))))

    (instance (RealFloat Float)
      (define (is-nan?      x)   (racket Boolean (x)   #f))
      (define (is-infinite? x)   (racket Boolean (x)   #f))
      (define (atan2        y x) (racket Float   (y x) 0.0)))

    ;; --- Eq / Ord / Show for the core containers ----------------
    ;;
    ;; Structural instances for Maybe / List / Pair / Result / Unit,
    ;; defined in pure Rackton (element ops come from the element
    ;; constraints).  Placed late so the `Show` bodies can use
    ;; `string-append`.  These let `==`, comparison, and `show` (and
    ;; the native test framework's `check-equal?`) work on the
    ;; containers that pervade user programs, matching Coalton.

    (instance ((Eq a) => (Eq (Maybe a)))
      (define (== m1 m2)
        (match m1
          [(None)   (match m2 [(None) #t] [(Some _) #f])]
          [(Some x) (match m2 [(None) #f] [(Some y) (== x y)])])))

    (instance ((Eq a) => (Eq (List a)))
      (define (== xs ys)
        (match xs
          [(Nil)      (match ys [(Nil) #t] [(Cons _ _) #f])]
          [(Cons h t) (match ys
                        [(Nil)        #f]
                        [(Cons h2 t2) (if (== h h2) (== t t2) #f)])])))

    (instance ((Eq a) (Eq b) => (Eq (Pair a b)))
      (define (== p1 p2)
        (match p1
          [(Pair x1 y1)
           (match p2 [(Pair x2 y2) (if (== x1 x2) (== y1 y2) #f)])])))

    (instance ((Eq e) (Eq a) => (Eq (Result e a)))
      (define (== r1 r2)
        (match r1
          [(Err x) (match r2 [(Err y) (== x y)] [(Ok  _) #f])]
          [(Ok  x) (match r2 [(Err _) #f]        [(Ok  y) (== x y)])])))

    (instance (Eq Unit)
      (define (== _u1 _u2) #t))

    ;; Ord: only `<` is primitive; >, <=, >=, min, max derive.  None
    ;; sorts before Some; lists and pairs compare lexicographically.
    (instance ((Ord a) => (Ord (Maybe a)))
      (define (< m1 m2)
        (match m1
          [(None)   (match m2 [(None) #f] [(Some _) #t])]
          [(Some x) (match m2 [(None) #f] [(Some y) (< x y)])])))

    (instance ((Ord a) => (Ord (List a)))
      (define (< xs ys)
        (match xs
          [(Nil)      (match ys [(Nil) #f] [(Cons _ _) #t])]
          [(Cons h t) (match ys
                        [(Nil)        #f]
                        [(Cons h2 t2) (if (< h h2)
                                          #t
                                          (if (== h h2) (< t t2) #f))])])))

    (instance ((Ord a) (Ord b) => (Ord (Pair a b)))
      (define (< p1 p2)
        (match p1
          [(Pair x1 y1)
           (match p2
             [(Pair x2 y2)
              (if (< x1 x2) #t (if (== x1 x2) (< y1 y2) #f))])])))

    ;; Show: human-readable renderings; used in check-equal? failure
    ;; messages and `show`.
    (instance ((Show a) => (Show (Maybe a)))
      (define (show m)
        (match m
          [(None)   "None"]
          [(Some x) (string-append "(Some " (string-append (show x) ")"))])))

    (instance ((Show a) => (Show (List a)))
      (define (show xs)
        (letrec ([elems (lambda (ys)
                          (match ys
                            [(Nil)          ""]
                            [(Cons h (Nil)) (show h)]
                            [(Cons h t)     (string-append
                                             (show h)
                                             (string-append ", " (elems t)))]))])
          (string-append "[" (string-append (elems xs) "]")))))

    (instance ((Show a) (Show b) => (Show (Pair a b)))
      (define (show p)
        (match p
          [(Pair x y)
           (string-append "(" (string-append (show x)
                            (string-append ", " (string-append (show y) ")"))))])))

    (instance ((Show e) (Show a) => (Show (Result e a)))
      (define (show r)
        (match r
          [(Err x) (string-append "(Err " (string-append (show x) ")"))]
          [(Ok  x) (string-append "(Ok " (string-append (show x) ")"))])))

    (instance (Show Unit)
      (define (show _u) "Unit"))

    ;; try / raise-io and the System surface (random / time / env /
    ;; directories) moved to rackton/system (Phase 2 slim).
    ))

(define prelude-env
  (parameterize ([current-prelude-build? #t])
    (let ([forms (for/list ([f (in-list prelude-source-forms)])
                   (parse-top (datum->syntax #f f)))])
      (infer-program forms initial-env))))
