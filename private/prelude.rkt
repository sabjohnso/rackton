#lang racket/base

;; Rackton — compile-time side of the Phase-3 prelude.
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

    (define-class (Eq a)
      (: == (-> a (-> a Boolean)))
      (: /= (-> a (-> a Boolean)))
      (define (/= x y) (if (== x y) #f #t)))

    ;; --- Ord (Eq is a superclass) -------------------------------

    (define-class ((Eq a) => (Ord a))
      (: <  (-> a (-> a Boolean)))
      (: >  (-> a (-> a Boolean)))
      (: <= (-> a (-> a Boolean)))
      (: >= (-> a (-> a Boolean)))
      ;; Phase 44: min and max as Ord methods (comparison-based,
      ;; doesn't need numeric ops) with default impls in terms of <.
      (: min (-> a (-> a a)))
      (: max (-> a (-> a a)))
      (define (>  x y) (<  y x))
      (define (<= x y) (if (<  x y) #t (== x y)))
      (define (>= x y) (if (>  x y) #t (== x y)))
      (define (min x y) (if (< x y) x y))
      (define (max x y) (if (< x y) y x)))

    ;; --- Num ----------------------------------------------------

    (define-class (Num a)
      (: +      (-> a (-> a a)))
      (: -      (-> a (-> a a)))
      (: *      (-> a (-> a a)))
      ;; Phase 44: abs and negate as Num methods, polymorphic over
      ;; the numeric tower (Integer / Float / Rational / Complex).
      (: abs    (-> a a))
      (: negate (-> a a)))

    ;; --- Show ---------------------------------------------------

    (define-class (Show a)
      (: show (-> a String)))

    ;; --- Builtin instances --------------------------------------
    ;; Bodies of the form `(racket τ (vars) 0)` etc. are placeholders;
    ;; only the type discipline matters here.  The actual runtime
    ;; implementations live in prelude-runtime.rkt.

    (define-instance (Num Integer)
      (define (+ x y) (racket Integer (x y) 0))
      (define (- x y) (racket Integer (x y) 0))
      (define (* x y) (racket Integer (x y) 0))
      (define (abs    x) (racket Integer (x) 0))
      (define (negate x) (racket Integer (x) 0)))

    (define-instance (Eq Integer)
      (define (== x y) (racket Boolean (x y) #f)))

    (define-instance (Eq Boolean)
      (define (== x y) (if x y (if y #f #t))))

    (define-instance (Eq String)
      (define (== x y) (racket Boolean (x y) #f)))

    (define-instance (Ord Integer)
      (define (< x y) (racket Boolean (x y) #f)))

    (define-instance (Show Integer)
      (define (show x) (racket String (x) "")))

    (define-instance (Show Boolean)
      (define (show x) (if x "True" "False")))

    (define-instance (Show String)
      (define (show x) x))

    ;; Phase 44: Ord String (lex order) so min/max work on strings.
    (define-instance (Ord String)
      (define (< x y) (racket Boolean (x y) #f)))

    ;; --- ADTs ---------------------------------------------------

    (define-data (Maybe a)    None (Some a))
    (define-data (List a)     Nil  (Cons a (List a)))
    (define-data (Pair a b)   (MkPair a b))
    (define-data (Result e a) (Err e) (Ok a))
    (define-data Unit MkUnit)

    ;; --- Char and Bytes primitives (opaque ADTs; values come from
    ;; Racket's reader as `#\A` and `#"…"`) -----------------------

    (define-data Char)
    (define-data Bytes)

    (define-instance (Eq Char)   (define (== x y) (racket Boolean (x y) #f)))
    (define-instance (Ord Char)  (define (< x y) (racket Boolean (x y) #f)))
    (define-instance (Show Char) (define (show c) (racket String (c) "")))

    (define-instance (Eq Bytes)  (define (== x y) (racket Boolean (x y) #f)))
    (define-instance (Show Bytes)(define (show b) (racket String (b) "")))

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

    ;; --- Functor / Applicative / Monad (higher-kinded) --------
    ;;
    ;; The hierarchy is Functor → Applicative → Monad.  `pure` is a
    ;; return-typed class method: its type `a -> f a` carries the
    ;; class parameter only in the result, so the elaborator picks
    ;; the per-instance impl at compile time from the expected return
    ;; type at each call site.  See `return-typed-method?` in
    ;; private/infer.rkt and the resolution table consumed by
    ;; private/codegen.rkt.

    (define-class (Functor (f :: (-> * *)))
      (: fmap (-> (-> a b) (-> (f a) (f b)))))

    (define-class ((Functor f) => (Applicative (f :: (-> * *))))
      (: pure  (-> a (f a)))
      (: <*>   (-> (f (-> a b)) (-> (f a) (f b))))
      (: liftA2 (-> (-> a (-> b c)) (-> (f a) (-> (f b) (f c)))))
      (define (liftA2 g x y) (<*> (fmap g x) y)))

    (define-class ((Applicative m) => (Monad (m :: (-> * *))))
      (: >>= (-> (m a) (-> (-> a (m b)) (m b)))))

    ;; Maybe
    (define-instance (Functor Maybe)
      (define (fmap f m)
        (match m
          [(None)   None]
          [(Some x) (Some (f x))])))

    (define-instance (Applicative Maybe)
      (define (pure x) (Some x))
      (define (<*> mf mx)
        (match mf
          [(None)   None]
          [(Some f) (fmap f mx)])))

    (define-instance (Monad Maybe)
      (define (>>= m f)
        (match m
          [(None)   None]
          [(Some x) (f x)])))

    ;; List
    (define-instance (Functor List)
      (define (fmap f xs)
        (match xs
          [(Nil)        Nil]
          [(Cons h t)   (Cons (f h) (fmap f t))])))

    (define-instance (Applicative List)
      (define (pure x) (Cons x Nil))
      ;; cartesian-product semantics — for each function in `fs` apply
      ;; it to every `x` in `xs`, concatenating.  We can't yet call
      ;; the top-level `append` (it's defined later in the prelude), so
      ;; we inline a local concat helper.
      (define (<*> fs xs)
        (letrec ([cat (lambda (a b)
                        (match a
                          [(Nil)      b]
                          [(Cons h t) (Cons h (cat t b))]))])
          (match fs
            [(Nil)         Nil]
            [(Cons f rest) (cat (fmap f xs) (<*> rest xs))]))))

    ;; Result e (the error type is fixed; we map over the success type)
    (define-instance (Functor (Result e))
      (define (fmap f r)
        (match r
          [(Err x) (Err x)]
          [(Ok  v) (Ok (f v))])))

    (define-instance (Applicative (Result e))
      (define (pure x) (Ok x))
      (define (<*> rf rx)
        (match rf
          [(Err e) (Err e)]
          [(Ok  f) (fmap f rx)])))

    (define-instance (Monad (Result e))
      (define (>>= r f)
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

    (define-class (Bifunctor (p :: (-> * (-> * *))))
      (: bimap  (-> (-> a c) (-> (-> b d) (-> (p a b) (p c d)))))
      (: first  (-> (-> a c) (-> (p a b) (p c b))))
      (: second (-> (-> b d) (-> (p a b) (p a d))))
      (define (first  f x) (bimap f  id x))
      (define (second g x) (bimap id g  x)))

    (define-instance (Bifunctor Pair)
      (define (bimap f g p)
        (match p
          [(MkPair x y) (MkPair (f x) (g y))])))

    (define-instance (Bifunctor Result)
      (define (bimap f g r)
        (match r
          [(Err e) (Err (f e))]
          [(Ok  v) (Ok  (g v))])))

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

    (define-class (Foldable (t :: (-> * *)))
      (: foldr   (-> (-> a (-> b b)) (-> b (-> (t a) b))))
      (: length  (-> (t a) Integer))
      (: to-list (-> (t a) (List a)))
      (define (length  xs) (foldr (lambda (_x n)   (+ n 1))      0   xs))
      (define (to-list xs) (foldr (lambda (x acc)  (Cons x acc))  Nil xs)))

    (define-instance (Foldable List)
      (define (foldr f z xs)
        (match xs
          [(Nil)         z]
          [(Cons h rest) (f h (foldr f z rest))])))

    (define-instance (Foldable Maybe)
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
    ;; dict-passing path described in Phase 20).

    (define-class (Traversable (t :: (-> * *)))
      (: traverse ((Applicative f) =>
                   (-> (-> a (f b)) (-> (t a) (f (t b)))))))

    (define-instance (Traversable Maybe)
      (define (traverse f m)
        (match m
          [(None)   (pure None)]
          [(Some x) (fmap Some (f x))])))

    (define-instance (Traversable List)
      (define (traverse f xs)
        (match xs
          [(Nil)         (pure Nil)]
          [(Cons h rest) (liftA2 Cons (f h) (traverse f rest))])))

    ;; --- Semigroup / Monoid ----------------------------------
    ;;
    ;; `Semigroup` carries an associative `<>` (`sappend`).  `Monoid`
    ;; refines it with a left+right identity `mempty`.  `mempty` is a
    ;; return-typed class member — the elaborator picks the
    ;; instance from the expected type at each call site (see
    ;; @secref{Return-typed_dispatch} for the mechanism).

    (define-class (Semigroup a)
      (: <> (-> a (-> a a))))

    (define-class ((Semigroup a) => (Monoid a))
      (: mempty a))

    (define-instance (Semigroup String)
      ;; `string-append` is defined later in the prelude; use a host
      ;; escape so the load order doesn't bite us.
      (define (<> a b) (racket String (a b) #f)))

    (define-instance (Monoid String)
      (define mempty ""))

    (define-instance (Semigroup (List a))
      ;; cartesian concat; `append` is defined later in this prelude,
      ;; so inline a local cat helper as the Applicative List instance
      ;; does.
      (define (<> xs ys)
        (letrec ([cat (lambda (as bs)
                        (match as
                          [(Nil)      bs]
                          [(Cons h t) (Cons h (cat t bs))]))])
          (cat xs ys))))

    (define-instance (Monoid (List a))
      (define mempty Nil))

    ;; --- Sum / Product newtypes for additive/multiplicative Monoid -

    (define-newtype Sum     (MkSum     Integer))
    (define-newtype Product (MkProduct Integer))

    (: get-sum     (-> Sum Integer))
    (define (get-sum s)     (match s [(MkSum n) n]))

    (: get-product (-> Product Integer))
    (define (get-product p) (match p [(MkProduct n) n]))

    (define-instance (Semigroup Sum)
      (define (<> a b)
        (match a [(MkSum x)
                  (match b [(MkSum y) (MkSum (+ x y))])])))

    (define-instance (Monoid Sum)
      (define mempty (MkSum 0)))

    (define-instance (Semigroup Product)
      (define (<> a b)
        (match a [(MkProduct x)
                  (match b [(MkProduct y) (MkProduct (* x y))])])))

    (define-instance (Monoid Product)
      (define mempty (MkProduct 1)))

    ;; --- mconcat ------------------------------------------------
    ;;
    ;; `mconcat : (Monoid a) => (List a) -> a` carries a Monoid-
    ;; constrained `a` whose `mempty` is resolved per call site.  The
    ;; elaborator prepends the resolved `$mempty:T` impl as a leading
    ;; argument; the runtime implementation lives in prelude-runtime.
    ;; A polymorphic body is not provided here because Rackton can't
    ;; yet rewrite user-written needs-dict bodies — see Phase 24 docs.
    (: mconcat ((Monoid a) => (-> (List a) a)))

    ;; --- State monad ----------------------------------------
    ;;
    ;; A computation that threads a state value through a chain of
    ;; operations.  Internally a function `s -> (Pair s a)`.

    (define-newtype (State s a)
      (MkState (-> s (Pair s a))))

    (: run-state    (-> (State s a) (-> s (Pair s a))))
    (: eval-state   (-> (State s a) (-> s a)))
    (: exec-state   (-> (State s a) (-> s s)))
    (: get-state    (State s s))
    (: put-state    (-> s (State s Unit)))
    (: modify-state (-> (-> s s) (State s Unit)))

    (define (run-state st)
      (match st [(MkState f) f]))

    (define (eval-state st s)
      (match ((run-state st) s) [(MkPair _ a) a]))

    (define (exec-state st s)
      (match ((run-state st) s) [(MkPair s2 _) s2]))

    (define get-state    (MkState (lambda (s) (MkPair s s))))
    (define (put-state s) (MkState (lambda (_) (MkPair s MkUnit))))
    (define (modify-state f) (MkState (lambda (s) (MkPair (f s) MkUnit))))

    (define-instance (Functor (State s))
      (define (fmap f st)
        (MkState (lambda (s)
                   (match ((run-state st) s)
                     [(MkPair s2 a) (MkPair s2 (f a))])))))

    (define-instance (Applicative (State s))
      (define (pure a) (MkState (lambda (s) (MkPair s a))))
      (define (<*> sf sa)
        (MkState (lambda (s)
                   (match ((run-state sf) s)
                     [(MkPair s2 f)
                      (match ((run-state sa) s2)
                        [(MkPair s3 a) (MkPair s3 (f a))])])))))

    (define-instance (Monad (State s))
      (define (>>= st f)
        (MkState (lambda (s)
                   (match ((run-state st) s)
                     [(MkPair s2 a) ((run-state (f a)) s2)])))))

    ;; --- Env monad (a/k/a Reader; renamed to avoid the Scheme
    ;; reader-name collision) ----------------------------------

    (define-newtype (Env r a)
      (MkEnv (-> r a)))

    (: run-env (-> (Env r a) (-> r a)))
    (: ask     (Env r r))
    (: local   (-> (-> r r) (-> (Env r a) (Env r a))))

    (define (run-env e) (match e [(MkEnv f) f]))

    (define ask (MkEnv (lambda (r) r)))

    (define (local f e)
      (MkEnv (lambda (r) ((run-env e) (f r)))))

    (define-instance (Functor (Env r))
      (define (fmap f e)
        (MkEnv (lambda (r) (f ((run-env e) r))))))

    (define-instance (Applicative (Env r))
      (define (pure a) (MkEnv (lambda (_) a)))
      (define (<*> ef ea)
        (MkEnv (lambda (r) (((run-env ef) r) ((run-env ea) r))))))

    (define-instance (Monad (Env r))
      (define (>>= e f)
        (MkEnv (lambda (r) ((run-env (f ((run-env e) r))) r)))))

    ;; --- StateT s m: state-passing over an inner monad m ---------

    (define-newtype (StateT s m a)
      (MkStateT (-> s (m (Pair s a)))))

    (: run-state-t    (-> (StateT s m a) (-> s (m (Pair s a)))))
    (: eval-state-t   ((Functor m) => (-> (StateT s m a) (-> s (m a)))))
    (: exec-state-t   ((Functor m) => (-> (StateT s m a) (-> s (m s)))))
    (: get-state-t    ((Applicative m) => (StateT s m s)))
    (: put-state-t    ((Applicative m) => (-> s (StateT s m Unit))))
    (: modify-state-t ((Applicative m) => (-> (-> s s) (StateT s m Unit))))
    (: lift-state-t   ((Functor m) => (-> (m a) (StateT s m a))))

    ;; Bodies are hand-written in prelude-runtime.rkt so we can use
    ;; inner-monad pure/fmap impls directly, avoiding the body-
    ;; rewriting limitation the prelude inherits from Phase 24.

    (define-instance ((Monad m) => (Functor (StateT s m)))
      (define (fmap f st) (racket (StateT s m b) (f st) #f)))

    (define-instance ((Monad m) => (Applicative (StateT s m)))
      (define (pure  a)    (racket (StateT s m a) (a) #f))
      (define (<*>   sf sa)(racket (StateT s m b) (sf sa) #f)))

    (define-instance ((Monad m) => (Monad (StateT s m)))
      (define (>>= st f) (racket (StateT s m b) (st f) #f)))

    ;; --- EnvT r m: env-passing over an inner monad m -------------

    (define-newtype (EnvT r m a)
      (MkEnvT (-> r (m a))))

    (: run-env-t  (-> (EnvT r m a) (-> r (m a))))
    (: ask-t      ((Applicative m) => (EnvT r m r)))
    (: local-t    (-> (-> r r) (-> (EnvT r m a) (EnvT r m a))))
    (: lift-env-t (-> (m a) (EnvT r m a)))

    (define-instance ((Monad m) => (Functor (EnvT r m)))
      (define (fmap f e) (racket (EnvT r m b) (f e) #f)))

    (define-instance ((Monad m) => (Applicative (EnvT r m)))
      (define (pure  a)    (racket (EnvT r m a) (a) #f))
      (define (<*>   ef ea)(racket (EnvT r m b) (ef ea) #f)))

    (define-instance ((Monad m) => (Monad (EnvT r m)))
      (define (>>= e f) (racket (EnvT r m b) (e f) #f)))

    ;; --- WriterT w m: accumulating writer over an inner monad ---
    ;;
    ;; WriterT pairs an inner-monad action with a `Monoid w` log that
    ;; gets accumulated via `<>`.  pure inserts mempty, tell writes
    ;; one log entry, and lift hoists an arbitrary m-action.

    (define-newtype (WriterT w m a)
      (MkWriterT (m (Pair w a))))

    (: run-writer-t  (-> (WriterT w m a) (m (Pair w a))))
    (: eval-writer-t ((Functor m) => (-> (WriterT w m a) (m a))))
    (: exec-writer-t ((Functor m) => (-> (WriterT w m a) (m w))))
    (: tell          ((Applicative m) => (-> w (WriterT w m Unit))))
    (: lift-writer-t ((Functor m) (Monoid w) => (-> (m a) (WriterT w m a))))

    (define-instance ((Functor m) => (Functor (WriterT w m)))
      (define (fmap f wa) (racket (WriterT w m b) (f wa) #f)))

    (define-instance ((Monad m) (Monoid w) => (Applicative (WriterT w m)))
      (define (pure  a)     (racket (WriterT w m a) (a) #f))
      (define (<*>   wf wa) (racket (WriterT w m b) (wf wa) #f)))

    (define-instance ((Monad m) (Semigroup w) => (Monad (WriterT w m)))
      (define (>>= wa f) (racket (WriterT w m b) (wa f) #f)))

    ;; --- ExceptT e m: typed exceptions over an inner monad ------

    (define-newtype (ExceptT e m a)
      (MkExceptT (m (Result e a))))

    (: run-except-t  (-> (ExceptT e m a) (m (Result e a))))
    (: throw-error   ((Applicative m) => (-> e (ExceptT e m a))))
    (: catch-error   ((Monad m) => (-> (ExceptT e m a)
                                       (-> (-> e (ExceptT e m a))
                                           (ExceptT e m a)))))
    (: lift-except-t ((Functor m) => (-> (m a) (ExceptT e m a))))

    (define-instance ((Functor m) => (Functor (ExceptT e m)))
      (define (fmap f ea) (racket (ExceptT e m b) (f ea) #f)))

    (define-instance ((Monad m) => (Applicative (ExceptT e m)))
      (define (pure  a)     (racket (ExceptT e m a) (a) #f))
      (define (<*>   ef ea) (racket (ExceptT e m b) (ef ea) #f)))

    (define-instance ((Monad m) => (Monad (ExceptT e m)))
      (define (>>= ea f) (racket (ExceptT e m b) (ea f) #f)))

    ;; --- mtl-style classes (Phase 31) ---------------------------
    ;;
    ;; Polymorphic effect interfaces.  Code can ask for "any monad
    ;; with state / env / log / errors" via these classes; the
    ;; concrete transformer the caller picks chooses the instance.
    ;; Each class has a fundep `m -> X` so the inferer can recover
    ;; the effect's payload type from the monad.  See @secref{mtl-
    ;; style_classes_(Phase_31)} in the docs.

    ;; ----- MonadState -------------------------------------------

    (define-class ((Monad m) => (MonadState s (m :: (-> * *))))
      (#:fundep m -> s)
      (: get-st    (m s))
      (: put-st    (-> s (m Unit)))
      (: modify-st (-> (-> s s) (m Unit))))

    (define-instance (MonadState s (State s))
      (define get-st        get-state)
      (define (put-st x)    (put-state x))
      (define (modify-st f) (modify-state f)))

    (define-instance ((Monad m) => (MonadState s (StateT s m)))
      (define get-st        get-state-t)
      (define (put-st x)    (put-state-t x))
      (define (modify-st f) (modify-state-t f)))

    (define-instance ((MonadState s m) => (MonadState s (EnvT r m)))
      (define get-st        (lift-env-t get-st))
      (define (put-st x)    (lift-env-t (put-st x)))
      (define (modify-st f) (lift-env-t (modify-st f))))

    (define-instance ((MonadState s m) (Monoid w) => (MonadState s (WriterT w m)))
      (define get-st        (lift-writer-t get-st))
      (define (put-st x)    (lift-writer-t (put-st x)))
      (define (modify-st f) (lift-writer-t (modify-st f))))

    (define-instance ((MonadState s m) => (MonadState s (ExceptT e m)))
      (define get-st        (lift-except-t get-st))
      (define (put-st x)    (lift-except-t (put-st x)))
      (define (modify-st f) (lift-except-t (modify-st f))))

    ;; ----- MonadEnv (Reader) ------------------------------------

    (define-class ((Monad m) => (MonadEnv r (m :: (-> * *))))
      (#:fundep m -> r)
      (: ask-en   (m r))
      (: local-en (-> (-> r r) (-> (m a) (m a)))))

    (define-instance (MonadEnv r (Env r))
      (define ask-en     ask)
      (define (local-en f e) (local f e)))

    (define-instance ((Monad m) => (MonadEnv r (EnvT r m)))
      (define ask-en     ask-t)
      (define (local-en f e) (local-t f e)))

    (define-instance ((MonadEnv r m) => (MonadEnv r (StateT s m)))
      (define ask-en     (lift-state-t ask-en))
      (define (local-en f sm)
        (racket (StateT s m a) (f sm) #f)))

    (define-instance ((MonadEnv r m) (Monoid w) => (MonadEnv r (WriterT w m)))
      (define ask-en     (lift-writer-t ask-en))
      (define (local-en f wm)
        (racket (WriterT w m a) (f wm) #f)))

    (define-instance ((MonadEnv r m) => (MonadEnv r (ExceptT e m)))
      (define ask-en     (lift-except-t ask-en))
      (define (local-en f em)
        (racket (ExceptT e m a) (f em) #f)))

    ;; ----- MonadWriter ------------------------------------------

    (define-class ((Monoid w) (Monad m) => (MonadWriter w (m :: (-> * *))))
      (#:fundep m -> w)
      (: tell-w (-> w (m Unit)))
      (: listen (-> (m a) (m (Pair a w))))
      (: censor (-> (-> w w) (-> (m a) (m a)))))

    (define-instance ((Monoid w) (Monad m) => (MonadWriter w (WriterT w m)))
      (define (tell-w x)    (tell x))
      (define (listen wm)   (racket (WriterT w m (Pair a w))    (wm)   #f))
      (define (censor f wm) (racket (WriterT w m a)             (f wm) #f)))

    (define-instance ((MonadWriter w m) => (MonadWriter w (StateT s m)))
      (define (tell-w x)    (lift-state-t (tell-w x)))
      (define (listen sm)   (racket (StateT s m (Pair a w))     (sm)   #f))
      (define (censor f sm) (racket (StateT s m a)              (f sm) #f)))

    (define-instance ((MonadWriter w m) => (MonadWriter w (EnvT r m)))
      (define (tell-w x)    (lift-env-t (tell-w x)))
      (define (listen em)   (racket (EnvT r m (Pair a w))       (em)   #f))
      (define (censor f em) (racket (EnvT r m a)                (f em) #f)))

    (define-instance ((MonadWriter w m) => (MonadWriter w (ExceptT e m)))
      (define (tell-w x)    (lift-except-t (tell-w x)))
      (define (listen ex)   (racket (ExceptT e m (Pair a w))    (ex)   #f))
      (define (censor f ex) (racket (ExceptT e m a)             (f ex) #f)))

    ;; ----- MonadError -------------------------------------------

    (define-class ((Monad m) => (MonadError e (m :: (-> * *))))
      (#:fundep m -> e)
      (: throw-e (-> e (m a)))
      (: catch-e (-> (m a) (-> (-> e (m a)) (m a)))))

    (define-instance ((Monad m) => (MonadError e (ExceptT e m)))
      (define (throw-e e)    (throw-error e))
      (define (catch-e ea h) (catch-error ea h)))

    (define-instance ((MonadError e m) => (MonadError e (StateT s m)))
      (define (throw-e ev)   (lift-state-t (throw-e ev)))
      (define (catch-e sm h)
        (racket (StateT s m a) (sm h) #f)))

    (define-instance ((MonadError e m) => (MonadError e (EnvT r m)))
      (define (throw-e ev)   (lift-env-t (throw-e ev)))
      (define (catch-e em h)
        (racket (EnvT r m a) (em h) #f)))

    (define-instance ((MonadError e m) (Monoid w) =>
                      (MonadError e (WriterT w m)))
      (define (throw-e ev)   (lift-writer-t (throw-e ev)))
      (define (catch-e wm h)
        (racket (WriterT w m a) (wm h) #f)))

    ;; ----- mtl-style combinators (Phase 32) ---------------------
    ;;
    ;; `asks` / `gets` thread a pure transformation through the
    ;; reader / state effect.  `void`, `when`, `unless` are basic
    ;; Functor / Applicative helpers.

    (: asks ((MonadEnv r m) => (-> (-> r a) (m a))))
    (define (asks f) (fmap f ask-en))

    (: gets ((MonadState s m) => (-> (-> s a) (m a))))
    (define (gets f) (fmap f get-st))

    (: void ((Functor f) => (-> (f a) (f Unit))))
    (define (void fa) (fmap (const MkUnit) fa))

    (: when ((Applicative f) => (-> Boolean (-> (f Unit) (f Unit)))))
    (define (when b fu)
      (if b fu (pure MkUnit)))

    (: unless ((Applicative f) => (-> Boolean (-> (f Unit) (f Unit)))))
    (define (unless b fu)
      (if b (pure MkUnit) fu))

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
    ;; `mod` and `div` migrated to the Integral class (Phase 40).
    ;; `abs` / `negate` migrated to Num; `min` / `max` to Ord (Phase 44).

    (: integer->string (-> Integer String))
    (define (integer->string n) (racket String (n) ""))

    (: string->integer (-> String (Maybe Integer)))
    (define (string->integer s) (racket (Maybe Integer) (s) None))

    ;; --- IO ---------------------------------------------------

    (define-data (IO a))

    (define-instance (Functor IO)
      (define (fmap f io) (racket (IO b) (f io) #f)))

    (define-instance (Applicative IO)
      (define (pure x) (racket (IO a) (x) #f))
      (define (<*> iof iox) (racket (IO b) (iof iox) #f)))

    (define-instance (Monad IO)
      (define (>>= io f) (racket (IO b) (io f) #f)))

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

    ;; --- Mutable references ----------------------------------

    (define-data (Ref a))

    (: make-ref  (-> a (IO (Ref a))))
    (define (make-ref v) (racket (IO (Ref a)) (v) #f))

    (: read-ref  (-> (Ref a) (IO a)))
    (define (read-ref r) (racket (IO a) (r) #f))

    (: write-ref (-> (Ref a) (-> a (IO Unit))))
    (define (write-ref r v) (racket (IO Unit) (r v) #f))

    ;; --- Concurrency (Phase 36) ------------------------------
    ;;
    ;; Thin wrappers over Racket's threads + semaphores + async
    ;; channels.  All operations live in IO.  See "Concurrency
    ;; primitives (Phase 36)" in the docs for usage patterns.

    (define-data ThreadId)
    (define-data (MVar a))
    (define-data (Chan a))

    (: fork-io     (-> (IO a) (IO ThreadId)))
    (define (fork-io action) (racket (IO ThreadId) (action) #f))

    (: wait-thread (-> ThreadId (IO Unit)))
    (define (wait-thread tid) (racket (IO Unit) (tid) #f))

    (: new-mvar       (-> a (IO (MVar a))))
    (define (new-mvar v) (racket (IO (MVar a)) (v) #f))

    (: new-empty-mvar (IO (MVar a)))
    (define new-empty-mvar (racket (IO (MVar a)) () #f))

    (: take-mvar      (-> (MVar a) (IO a)))
    (define (take-mvar m) (racket (IO a) (m) #f))

    (: put-mvar       (-> (MVar a) (-> a (IO Unit))))
    (define (put-mvar m v) (racket (IO Unit) (m v) #f))

    (: read-mvar      (-> (MVar a) (IO a)))
    (define (read-mvar m) (racket (IO a) (m) #f))

    (: modify-mvar    (-> (MVar a) (-> (-> a a) (IO Unit))))
    (define (modify-mvar m f) (racket (IO Unit) (m f) #f))

    (: new-chan  (IO (Chan a)))
    (define new-chan (racket (IO (Chan a)) () #f))

    (: send-chan (-> (Chan a) (-> a (IO Unit))))
    (define (send-chan ch v) (racket (IO Unit) (ch v) #f))

    (: recv-chan (-> (Chan a) (IO a)))
    (define (recv-chan ch) (racket (IO a) (ch) #f))

    ;; --- STM (Phase 41) --------------------------------------
    ;;
    ;; Optimistic-concurrency software transactional memory.  An
    ;; STM action is composed monadically; `atomically` runs it
    ;; against a transaction log, verifies read versions at
    ;; commit time under a global commit lock, and applies writes
    ;; or restarts on a version mismatch (or on `retry`).

    (define-data (TVar a))
    (define-data (STM a))

    (define-instance (Functor STM)
      (define (fmap f s) (racket (STM b) (f s) #f)))

    (define-instance (Applicative STM)
      (define (pure x)      (racket (STM a) (x)    #f))
      (define (<*>  sf sa)  (racket (STM b) (sf sa) #f)))

    (define-instance (Monad STM)
      (define (>>= s f) (racket (STM b) (s f) #f)))

    (: new-tvar   (-> a (STM (TVar a))))
    (define (new-tvar v) (racket (STM (TVar a)) (v) #f))

    (: read-tvar  (-> (TVar a) (STM a)))
    (define (read-tvar tv) (racket (STM a) (tv) #f))

    (: write-tvar (-> (TVar a) (-> a (STM Unit))))
    (define (write-tvar tv v) (racket (STM Unit) (tv v) #f))

    (: retry (STM a))
    (define retry (racket (STM a) () #f))

    (: or-else (-> (STM a) (-> (STM a) (STM a))))
    (define (or-else s1 s2) (racket (STM a) (s1 s2) #f))

    (: atomically (-> (STM a) (IO a)))
    (define (atomically s) (racket (IO a) (s) #f))

    ;; --- Concurrent class (Phase 43) -------------------------
    ;;
    ;; `Future a` is a handle to a forked computation's result.
    ;; `Concurrent m` abstracts fork/await/yield across monads.
    ;; The Concurrent IO instance is the only one provided here;
    ;; future phases may add STM (transactional) or Mock (pure-
    ;; time-step) instances.

    (define-data (Future a))

    (define-class ((Monad m) => (Concurrent (m :: (-> * *))))
      (: fork-c   (-> (m a) (m (Future a))))
      (: await-c  (-> (Future a) (m a)))
      (: yield-c  (m Unit)))

    (define-instance (Concurrent IO)
      (define (fork-c io)
        (racket (IO (Future a)) (io)    #f))
      (define (await-c fut)
        (racket (IO a)          (fut)   #f))
      (define yield-c
        (racket (IO Unit)       ()      #f)))

    ;; --- Identity monad + Concurrent Identity (Phase 44) -----
    ;;
    ;; A trivial Mock for polymorphic-Concurrent code: fork-c runs
    ;; the computation immediately and stuffs its result into a
    ;; Future; await-c reads it back.  Useful for deterministic
    ;; unit tests of polymorphic concurrent code.

    (define-data (Identity a) (MkIdentity a))

    (: run-identity (-> (Identity a) a))
    (define (run-identity i)
      (match i [(MkIdentity x) x]))

    (define-instance (Functor Identity)
      (define (fmap f i)
        (match i [(MkIdentity x) (MkIdentity (f x))])))

    (define-instance (Applicative Identity)
      (define (pure x)        (MkIdentity x))
      (define (<*>  ifn ix)
        (match ifn
          [(MkIdentity f)
           (match ix [(MkIdentity x) (MkIdentity (f x))])])))

    (define-instance (Monad Identity)
      (define (>>= i f)
        (match i [(MkIdentity x) (f x)])))

    (define-instance (Concurrent Identity)
      (define (fork-c m)
        (match m
          [(MkIdentity x) (MkIdentity (racket (Future a) (x) #f))]))
      (define (await-c fut)
        (MkIdentity (racket a (fut) #f)))
      (define yield-c
        (MkIdentity MkUnit)))

    ;; --- Lens / optics (Phase 46) ----------------------------
    ;;
    ;; Simple (getter, setter) pair encoding.  Each `(Lens s a)`
    ;; packs a function to extract an `a` from `s` and a function
    ;; to inject a new `a` back into an existing `s`, producing a
    ;; new `s`.  view / set / over operate on a single lens;
    ;; lens-compose chains two lenses through a nested structure.

    (define-data (Lens s a)
      (MkLens (-> s a) (-> s (-> a s))))

    (: view (-> (Lens s a) (-> s a)))
    (define (view l s)
      (match l [(MkLens g _) (g s)]))

    (: set (-> (Lens s a) (-> a (-> s s))))
    (define (set l v s)
      (match l [(MkLens _ ps) ((ps s) v)]))

    (: over (-> (Lens s a) (-> (-> a a) (-> s s))))
    (define (over l f s)
      (match l [(MkLens g ps) ((ps s) (f (g s)))]))

    (: lens-compose
       (-> (Lens s a) (-> (Lens a b) (Lens s b))))
    (define (lens-compose outer inner)
      (MkLens
       (lambda (s) (view inner (view outer s)))
       (lambda (s b)
         (set outer (set inner b (view outer s)) s))))

    ;; --- Prisms (Phase 48) -----------------------------------
    ;;
    ;; A prism focuses on a single sum-type constructor.  preview
    ;; returns Some when the target ctor matches, None otherwise.
    ;; review always succeeds — it builds the target ctor.

    (define-data (Prism s a)
      (MkPrism (-> s (Maybe a)) (-> a s)))

    (: preview (-> (Prism s a) (-> s (Maybe a))))
    (define (preview p s)
      (match p [(MkPrism extract _) (extract s)]))

    (: review  (-> (Prism s a) (-> a s)))
    (define (review p a)
      (match p [(MkPrism _ build) (build a)]))

    ;; --- Traversals (Phase 48) -------------------------------
    ;;
    ;; A traversal focuses on zero-or-more sub-parts.  to-list-of
    ;; gathers them; over-of transforms all of them.

    (define-data (Traversal s a)
      (MkTraversal (-> s (List a))
                   (-> (-> a a) (-> s s))))

    (: to-list-of (-> (Traversal s a) (-> s (List a))))
    (define (to-list-of t s)
      (match t [(MkTraversal get-all _) (get-all s)]))

    (: over-of    (-> (Traversal s a) (-> (-> a a) (-> s s))))
    (define (over-of t f s)
      (match t [(MkTraversal _ modify-all) ((modify-all f) s)]))

    ;; A built-in traversal that focuses on every element of a
    ;; List.  Folds (length-preserving) via map.
    (: list-traversal (Traversal (List a) a))
    (define list-traversal
      (MkTraversal id (lambda (f) (lambda (xs) (fmap f xs)))))

    ;; Promote a Lens to a Traversal with a single focus.
    (: lens-as-traversal (-> (Lens s a) (Traversal s a)))
    (define (lens-as-traversal l)
      (MkTraversal
       (lambda (s) (Cons (view l s) Nil))
       (lambda (f) (lambda (s) (over l f s)))))

    ;; --- File I/O --------------------------------------------

    (: read-file    (-> String (IO String)))
    (define (read-file path) (racket (IO String) (path) #f))

    (: write-file   (-> String (-> String (IO Unit))))
    (define (write-file path contents) (racket (IO Unit) (path contents) #f))

    (: file-exists? (-> String (IO Boolean)))
    (define (file-exists? path) (racket (IO Boolean) (path) #f))

    ;; --- List & pair helpers ---------------------------------

    (: append (-> (List a) (-> (List a) (List a))))
    (define (append xs ys)
      (match xs
        [(Nil)        ys]
        [(Cons h t)   (Cons h (append t ys))]))

    (: reverse (-> (List a) (List a)))
    (define (reverse xs)
      (foldr (lambda (h acc) (append acc (Cons h Nil))) Nil xs))

    (: zip (-> (List a) (-> (List b) (List (Pair a b)))))
    (define (zip as bs)
      (match as
        [(Nil) Nil]
        [(Cons a at)
         (match bs
           [(Nil) Nil]
           [(Cons b bt)
            (Cons (MkPair a b) (zip at bt))])]))

    (: take (-> Integer (-> (List a) (List a))))
    (define (take n xs)
      (if (<= n 0)
          Nil
          (match xs
            [(Nil)        Nil]
            [(Cons h t)   (Cons h (take (- n 1) t))])))

    (: drop (-> Integer (-> (List a) (List a))))
    (define (drop n xs)
      (if (<= n 0)
          xs
          (match xs
            [(Nil)        Nil]
            [(Cons _ t)   (drop (- n 1) t)])))

    (: find (-> (-> a Boolean) (-> (List a) (Maybe a))))
    (define (find p xs)
      (match xs
        [(Nil)        None]
        [(Cons h t)   (if (p h) (Some h) (find p t))]))

    (: fst (-> (Pair a b) a))
    (define (fst p) (match p [(MkPair a _) a]))

    (: snd (-> (Pair a b) b))
    (define (snd p) (match p [(MkPair _ b) b]))

    (: swap (-> (Pair a b) (Pair b a)))
    (define (swap p) (match p [(MkPair a b) (MkPair b a)]))

    ;; Merge sort over (Ord a).  O(n log n) stable.

    (: split-at (-> Integer (-> (List a) (Pair (List a) (List a)))))
    (define (split-at n xs)
      (if (== n 0)
          (MkPair Nil xs)
          (match xs
            [(Nil) (MkPair Nil Nil)]
            [(Cons h t)
             (let ([rest (split-at (- n 1) t)])
               (MkPair (Cons h (fst rest)) (snd rest)))])))

    (: merge-lists ((Ord a) => (-> (List a) (-> (List a) (List a)))))
    (define (merge-lists xs ys)
      (match xs
        [(Nil) ys]
        [(Cons hx tx)
         (match ys
           [(Nil) xs]
           [(Cons hy ty)
            (if (< hx hy)
                (Cons hx (merge-lists tx ys))
                (Cons hy (merge-lists xs ty)))])]))

    (: sort ((Ord a) => (-> (List a) (List a))))
    (define (sort xs)
      (let ([n (length xs)])
        (if (< n 2)
            xs
            ;; Phase 40: was `(div n 2)` when div was a free Integer
            ;; helper; div is now an Integral class method declared
            ;; later in the prelude, so we host-escape the quotient
            ;; here to keep sort's definition order-independent.
            (let ([halves (split-at (racket Integer (n) 0) xs)])
              (merge-lists (sort (fst halves))
                           (sort (snd halves)))))))

    ;; Unrecoverable failure with a message.  Typed at bottom so it can
    ;; appear anywhere; raises at runtime.
    (: panic (-> String a))
    (define (panic msg) (racket a (msg) #f))

    ;; --- Immutable Map and Set ------------------------------

    (define-data (Map k v))
    (define-data (Set a))

    (: empty-map (Map k v))
    (define empty-map (racket (Map k v) () #f))

    (: map-insert ((Eq k) => (-> k (-> v (-> (Map k v) (Map k v))))))
    (define (map-insert k v m) (racket (Map k v) (k v m) #f))

    (: map-lookup ((Eq k) => (-> k (-> (Map k v) (Maybe v)))))
    (define (map-lookup k m) (racket (Maybe v) (k m) None))

    (: map-delete ((Eq k) => (-> k (-> (Map k v) (Map k v)))))
    (define (map-delete k m) (racket (Map k v) (k m) #f))

    (: map-keys (-> (Map k v) (List k)))
    (define (map-keys m) (racket (List k) (m) Nil))

    (: map-values (-> (Map k v) (List v)))
    (define (map-values m) (racket (List v) (m) Nil))

    (: map-size (-> (Map k v) Integer))
    (define (map-size m) (racket Integer (m) 0))

    (: map-fold (-> (-> k (-> v (-> b b))) (-> b (-> (Map k v) b))))
    (define (map-fold f z m) (racket b (f z m) z))

    (: empty-set (Set a))
    (define empty-set (racket (Set a) () #f))

    (: set-insert ((Eq a) => (-> a (-> (Set a) (Set a)))))
    (define (set-insert x s) (racket (Set a) (x s) #f))

    (: set-member? ((Eq a) => (-> a (-> (Set a) Boolean))))
    (define (set-member? x s) (racket Boolean (x s) #f))

    (: set-delete ((Eq a) => (-> a (-> (Set a) (Set a)))))
    (define (set-delete x s) (racket (Set a) (x s) #f))

    (: set-size (-> (Set a) Integer))
    (define (set-size s) (racket Integer (s) 0))

    (: set-to-list (-> (Set a) (List a)))
    (define (set-to-list s) (racket (List a) (s) Nil))

    ;; --- List helpers leaning on Map -----------------------

    (: concat-map (-> (-> a (List b)) (-> (List a) (List b))))
    (define (concat-map f xs)
      (foldr (lambda (x acc) (append (f x) acc)) Nil xs))

    (: group-by ((Eq k) => (-> (-> a k) (-> (List a) (Map k (List a))))))
    (define (group-by key xs)
      (foldr (lambda (x m)
               (let ([k (key x)])
                 (match (map-lookup k m)
                   [(None)     (map-insert k (Cons x Nil) m)]
                   [(Some lst) (map-insert k (Cons x lst) m)])))
             empty-map
             xs))

    ;; --- Float type + instances ----------------------------

    (define-instance (Num Float)
      (define (+ x y) (racket Float (x y) 0.0))
      (define (- x y) (racket Float (x y) 0.0))
      (define (* x y) (racket Float (x y) 0.0))
      (define (abs    x) (racket Float (x) 0.0))
      (define (negate x) (racket Float (x) 0.0)))

    (define-instance (Eq Float)
      (define (== x y) (racket Boolean (x y) #f)))

    (define-instance (Ord Float)
      (define (< x y) (racket Boolean (x y) #f)))

    (define-instance (Show Float)
      (define (show x) (racket String (x) "")))

    (define-class ((Num a) => (Fractional a))
      (: float-div (-> a (-> a a))))

    (define-instance (Fractional Float)
      (define (float-div x y) (racket Float (x y) 0.0)))

    (: integer->float (-> Integer Float))
    (define (integer->float n) (racket Float (n) 0.0))

    (: float->integer (-> Float Integer))
    (define (float->integer x) (racket Integer (x) 0))

    (: abs-float (-> Float Float))
    (define (abs-float x) (racket Float (x) 0.0))

    ;; --- Phase 40: Rational + Complex types -----------------

    (define-data Rational)
    (define-data Complex)

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
    (define-instance (Eq Rational)
      (define (== x y) (racket Boolean (x y) #f)))
    (define-instance (Ord Rational)
      (define (< x y) (racket Boolean (x y) #f)))
    (define-instance (Show Rational)
      (define (show x) (racket String (x) "")))

    ;; Num / Fractional for Rational
    (define-instance (Num Rational)
      (define (+ x y) (racket Rational (x y) #f))
      (define (- x y) (racket Rational (x y) #f))
      (define (* x y) (racket Rational (x y) #f))
      (define (abs    x) (racket Rational (x) #f))
      (define (negate x) (racket Rational (x) #f)))
    (define-instance (Fractional Rational)
      (define (float-div x y) (racket Rational (x y) #f)))

    ;; Eq / Show for Complex
    (define-instance (Eq Complex)
      (define (== x y) (racket Boolean (x y) #f)))
    (define-instance (Show Complex)
      (define (show x) (racket String (x) "")))

    (define-instance (Num Complex)
      (define (+ x y) (racket Complex (x y) #f))
      (define (- x y) (racket Complex (x y) #f))
      (define (* x y) (racket Complex (x y) #f))
      (define (abs    x) (racket Complex (x) #f))
      (define (negate x) (racket Complex (x) #f)))
    (define-instance (Fractional Complex)
      (define (float-div x y) (racket Complex (x y) #f)))

    ;; --- Phase 40: Integral class ----------------------------

    (define-class ((Num a) => (Integral a))
      (: div  (-> a (-> a a)))
      (: mod  (-> a (-> a a)))
      (: quot (-> a (-> a a)))
      (: rem  (-> a (-> a a))))

    (define-instance (Integral Integer)
      (define (div  a b) (racket Integer (a b) 0))
      (define (mod  a b) (racket Integer (a b) 0))
      (define (quot a b) (racket Integer (a b) 0))
      (define (rem  a b) (racket Integer (a b) 0)))

    ;; --- Phase 40: Real class --------------------------------

    (define-class ((Num a) (Ord a) => (Real a))
      (: to-rational (-> a Rational)))

    (define-instance (Real Integer)
      (define (to-rational n) (racket Rational (n) #f)))
    (define-instance (Real Float)
      (define (to-rational x) (racket Rational (x) #f)))
    (define-instance (Real Rational)
      (define (to-rational x) (racket Rational (x) #f)))

    ;; --- Phase 40: Floating class ----------------------------

    (define-class ((Fractional a) => (Floating a))
      (: pi   a)
      (: exp  (-> a a))
      (: log  (-> a a))
      (: sqrt (-> a a))
      (: sin  (-> a a))
      (: cos  (-> a a))
      (: tan  (-> a a))
      (: **   (-> a (-> a a))))

    (define-instance (Floating Float)
      (define pi   (racket Float () 0.0))
      (define (exp  x)   (racket Float (x)   0.0))
      (define (log  x)   (racket Float (x)   0.0))
      (define (sqrt x)   (racket Float (x)   0.0))
      (define (sin  x)   (racket Float (x)   0.0))
      (define (cos  x)   (racket Float (x)   0.0))
      (define (tan  x)   (racket Float (x)   0.0))
      (define (**   x y) (racket Float (x y) 0.0)))

    (define-instance (Floating Complex)
      (define pi   (racket Complex () #f))
      (define (exp  x)   (racket Complex (x)   #f))
      (define (log  x)   (racket Complex (x)   #f))
      (define (sqrt x)   (racket Complex (x)   #f))
      (define (sin  x)   (racket Complex (x)   #f))
      (define (cos  x)   (racket Complex (x)   #f))
      (define (tan  x)   (racket Complex (x)   #f))
      (define (**   x y) (racket Complex (x y) #f)))

    ;; --- Phase 40: RealFrac class ----------------------------
    ;;
    ;; Methods are named `*-real` to avoid clashing with potential
    ;; future Racket-side helpers; the rounding operations all
    ;; produce Integer regardless of `a`.

    (define-class ((Real a) (Fractional a) => (RealFrac a))
      (: floor-real    (-> a Integer))
      (: ceiling-real  (-> a Integer))
      (: round-real    (-> a Integer))
      (: truncate-real (-> a Integer)))

    (define-instance (RealFrac Float)
      (define (floor-real    x) (racket Integer (x) 0))
      (define (ceiling-real  x) (racket Integer (x) 0))
      (define (round-real    x) (racket Integer (x) 0))
      (define (truncate-real x) (racket Integer (x) 0)))
    (define-instance (RealFrac Rational)
      (define (floor-real    x) (racket Integer (x) 0))
      (define (ceiling-real  x) (racket Integer (x) 0))
      (define (round-real    x) (racket Integer (x) 0))
      (define (truncate-real x) (racket Integer (x) 0)))

    ;; --- Phase 40: RealFloat class ---------------------------

    (define-class ((RealFrac a) (Floating a) => (RealFloat a))
      (: is-nan?      (-> a Boolean))
      (: is-infinite? (-> a Boolean))
      (: atan2        (-> a (-> a a))))

    (define-instance (RealFloat Float)
      (define (is-nan?      x)   (racket Boolean (x)   #f))
      (define (is-infinite? x)   (racket Boolean (x)   #f))
      (define (atan2        y x) (racket Float   (y x) 0.0)))

    ;; --- try / raise-io ------------------------------------

    (: try (-> (IO a) (IO (Result String a))))
    (define (try io) (racket (IO (Result String a)) (io) #f))

    (: raise-io (-> String (IO a)))
    (define (raise-io msg) (racket (IO a) (msg) #f))

    ;; --- System surface ------------------------------------

    (: random-integer (-> Integer (-> Integer (IO Integer))))
    (define (random-integer lo hi) (racket (IO Integer) (lo hi) #f))

    (: random-float (IO Float))
    (define random-float (racket (IO Float) () #f))

    (: current-time-seconds (IO Integer))
    (define current-time-seconds (racket (IO Integer) () #f))

    (: list-directory (-> String (IO (List String))))
    (define (list-directory path) (racket (IO (List String)) (path) #f))

    (: getenv (-> String (IO (Maybe String))))
    (define (getenv name) (racket (IO (Maybe String)) (name) #f))

    (: argv (IO (List String)))
    (define argv (racket (IO (List String)) () #f))

    (: delete-file (-> String (IO Unit)))
    (define (delete-file path) (racket (IO Unit) (path) #f))

    (: make-directory (-> String (IO Unit)))
    (define (make-directory path) (racket (IO Unit) (path) #f))))

(define prelude-env
  (let ([forms (for/list ([f (in-list prelude-source-forms)])
                 (parse-top (datum->syntax #f f)))])
    (infer-program forms initial-env)))
