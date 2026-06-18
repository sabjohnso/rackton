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

(provide prelude-env
         prelude-source-forms)

;; ----- Prelude source ----------------------------------------------

(define prelude-source-forms
  '(;; --- Eq ----------------------------------------------------------

    (protocol (Eq a)
      (: == (-> a (-> a Boolean)))
      (: /= (-> a (-> a Boolean)))
      (define (/= x y) (if (== x y) #f #t))
      ;; `==` is an equivalence relation.  Each law is phrased as an
      ;; implication (`if cond then … else #t`) so it stays Boolean
      ;; without having to compare two Boolean results.
      #:laws
        ([reflexivity  (All ([x : a]) (== x x))]
         [symmetry     (All ([x : a] [y : a]) (if (== x y) (== y x) #t))]
         [transitivity (All ([x : a] [y : a] [z : a])
                         (if (== x y) (if (== y z) (== x z) #t) #t))]))

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
      (define (max x y) (if (< x y) y x))
      ;; `<=` is a total order: reflexive, antisymmetric, transitive, and
      ;; total.  Antisymmetry compares with `==` from the Eq superclass.
      #:laws
        ([reflexivity  (All ([x : a]) (<= x x))]
         [antisymmetry (All ([x : a] [y : a])
                         (if (<= x y) (if (<= y x) (== x y) #t) #t))]
         [transitivity (All ([x : a] [y : a] [z : a])
                         (if (<= x y) (if (<= y z) (<= x z) #t) #t))]
         [totality     (All ([x : a] [y : a]) (if (<= x y) #t (<= y x)))]))

    ;; --- Num ----------------------------------------------------

    (protocol (Num a)
      (: +      (-> a (-> a a)))
      (: -      (-> a (-> a a)))
      (: *      (-> a (-> a a)))
      ;; Abs and negate as Num methods, polymorphic over
      ;; the numeric tower (Integer / Float / Rational / Complex).
      (: abs    (-> a a))
      (: negate (-> a a))
      ;; `+` and `*` form commutative monoids that distribute, and
      ;; `negate` is the additive inverse witnessed by `-`.  Stating the
      ;; equations needs equality, so the laws assume `(Eq a)` without
      ;; making it a superclass of Num.
      #:laws
        ([add-commutative  ((Eq a) => (All ([x : a] [y : a]) (== (+ x y) (+ y x))))]
         [add-associative  ((Eq a) => (All ([x : a] [y : a] [z : a])
                             (== (+ (+ x y) z) (+ x (+ y z)))))]
         [mul-commutative  ((Eq a) => (All ([x : a] [y : a]) (== (* x y) (* y x))))]
         [mul-associative  ((Eq a) => (All ([x : a] [y : a] [z : a])
                             (== (* (* x y) z) (* x (* y z)))))]
         [distributive     ((Eq a) => (All ([x : a] [y : a] [z : a])
                             (== (* x (+ y z)) (+ (* x y) (* x z)))))]
         [subtract-negate  ((Eq a) => (All ([x : a] [y : a])
                             (== (- x y) (+ x (negate y)))))]))

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
    (data (Either a b) (Left a) (Right b))
    (data Unit Unit)

    ;; --- Char and Bytes primitives (opaque ADTs; values come from
    ;; Racket's reader as `#\A` and `#"…"`) -----------------------

    (data Char)
    (data Bytes)

    (instance (Eq Char)   (define (== x y) (racket Boolean (x y) #f)))
    (instance (Ord Char)  (define (< x y) (racket Boolean (x y) #f)))
    (instance (Show Char) (define (show c) (racket String (c) "")))

    ;; --- Symbol primitive (opaque ADT; values come from the reader as
    ;; quoted identifiers, e.g. `'foo`) --------------------------------

    (data Symbol)

    (instance (Eq Symbol)   (define (== x y) (racket Boolean (x y) #f)))
    (instance (Ord Symbol)  (define (< x y) (racket Boolean (x y) #f)))
    (instance (Show Symbol) (define (show s) (racket String (s) "")))

    (: symbol->string (-> Symbol String))
    (define (symbol->string s) (racket String (s) ""))

    (: string->symbol (-> String Symbol))
    (define (string->symbol s) (racket Symbol (s) 'x))

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
      (: fmap (-> (-> a b) (-> (f a) (f b))))
      ;; `fmap` preserves identity and composition.  The laws are
      ;; universally quantified over the element types: each variable
      ;; that is not a class parameter (`a`, `b`, `c`) is skolemized, and
      ;; the container is compared via an assumed `(Eq (f a))` / `(Eq (f
      ;; c))` rather than pinning the element to a concrete type.
      #:laws
        ([identity ((Eq (f a)) =>
           (All ([xs : (f a)]) (== (fmap (lambda (x) x) xs) xs)))]
         [composition ((Eq (f c)) =>
           (All ([g : (-> b c)] [h : (-> a b)] [xs : (f a)])
             (== (fmap (lambda (x) (g (h x))) xs)
                 (fmap g (fmap h xs)))))]))

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
      ;; instance written with `#:derive-supers` (supplying `pure`
      ;; and `fapply`) gets `Functor` for free via `fmap f = pure f <*>`.
      #:derive
      ([Functor
        (define (fmap f x) (fapply (pure f) x))])
      ;; The applicative laws, universally quantified over the element
      ;; types.  `pure` is return-typed; where it appears in result
      ;; position the surrounding `fapply` does not pin, its container is
      ;; fixed to the law's instance with an `(ann … (f …))`.
      #:laws
        ([identity ((Eq (f a)) =>
           (All ([v : (f a)]) (== (fapply (pure (lambda (x) x)) v) v)))]
         [homomorphism ((Eq (f b)) =>
           (All ([g : (-> a b)] [x : a])
             (== (fapply (pure g) (pure x))
                 (ann (pure (g x)) (f b)))))]
         [interchange ((Eq (f b)) =>
           (All ([u : (f (-> a b))] [y : a])
             (== (fapply u (ann (pure y) (f a)))
                 (fapply (ann (pure (lambda (g) (g y)))
                              (f (-> (-> a b) b)))
                         u))))]))

    ;; Monad has two derivable methods — flatmap and join — with mutual
    ;; defaults.  An instance must define at least one.  flatmap takes
    ;; the continuation first and the monadic value second; the
    ;; ordering matches `flip (>>=)` from Haskell.
    ;; Monad also carries cross-class derivations for its Applicative and
    ;; Functor superclasses: an instance written with
    ;; `#:derive-supers` need only supply `pure` (the irreducible
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
      #:derive
      ([Functor
        (define (fmap f x) (flatmap (lambda (a) (pure (f a))) x))]
       [Applicative
        (define (fapply ff fx)
          (flatmap (lambda (g) (flatmap (lambda (x) (pure (g x))) fx)) ff))
        ;; Apply `g` with an n-ary call `(g a b)`, not a curried `((g a) b)`:
        ;; `product`'s default passes the raw 2-ary `Pair` constructor as
        ;; `g`, and a constructor cannot be partially applied.
        (define (liftA2 g x y)
          (flatmap (lambda (a) (flatmap (lambda (b) (pure (g a b))) y)) x))])
      ;; The three monad laws, universally quantified over the element
      ;; types and their Kleisli arrows.  Left-identity is now the true
      ;; `pure x >>= k = k x` rather than a `(+1)` specialization.  A
      ;; monadic value supplied by `pure` in a position the surrounding
      ;; `flatmap` does not pin gets an `(ann … (m a))`.
      #:laws
        ([left-identity ((Eq (m b)) =>
           (All ([k : (-> a (m b))] [x : a])
             (== (flatmap k (ann (pure x) (m a))) (k x))))]
         [right-identity ((Eq (m a)) =>
           (All ([mx : (m a)]) (== (flatmap (lambda (x) (pure x)) mx) mx)))]
         [associativity ((Eq (m c)) =>
           (All ([j : (-> a (m b))] [k : (-> b (m c))] [mx : (m a)])
             (== (flatmap k (flatmap j mx))
                 (flatmap (lambda (x) (flatmap k (j x))) mx))))]))

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

    ;; Either a (the left type is fixed; we map over the right)
    (instance (Functor (Either a))
      (define (fmap f r)
        (match r
          [(Left x)  (Left x)]
          [(Right v) (Right (f v))])))

    (instance (Applicative (Either a))
      (define (pure x) (Right x))
      (define (fapply rf rx)
        (match rf
          [(Left e)  (Left e)]
          [(Right f) (fmap f rx)])))

    (instance (Monad (Either a))
      (define (flatmap f r)
        (match r
          [(Left x)  (Left x)]
          [(Right v) (f v)])))

    ;; --- Bifunctor ---------------------------------------------
    ;;
    ;; Two-parameter higher-kinded class for shapes with TWO "slots"
    ;; that can be mapped over independently — `Pair a b` and
    ;; `Either a b` are the canonical instances.  `bimap` dispatches
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

    (instance (Bifunctor Either)
      (define (bimap f g r)
        (match r
          [(Left e)  (Left  (f e))]
          [(Right v) (Right (g v))])))

    ;; --- Category / Arrow (monoidal-category generalization) ---
    ;;
    ;; Arrows (Hughes) generalize plain functions and Kleisli arrows.
    ;; The hierarchy is Category → Arrow → {ArrowChoice, ArrowApply,
    ;; ArrowLoop}.  Rather than hard-wiring the strict `Pair`/`Either`,
    ;; each arrow carries its own PRODUCT `p` and COPRODUCT `s`, each
    ;; determined by the arrow via a functional dependency.  `(->)` uses
    ;; the strict `Pair`/`Either`; a lazy arrow can use lazy ones, which
    ;; is what makes a lawful `ArrowLoop` possible (the strict product
    ;; forces the loop's feedback channel).
    ;;
    ;; `Prod`/`Coprod` are the monoidal tensors — intro + elim — so the
    ;; `proc` notation can build and take apart products/coproducts without
    ;; naming a concrete type.  (Named `Prod`/`Coprod`, matching the
    ;; `mk-prod`/`prod-fst` method prefix, to avoid clashing with the
    ;; multiplicative-monoid newtype `Product` in rackton/data/monoid.)

    (protocol (Prod (p :: (-> * (-> * *))))
      (: mk-prod   (-> a (-> b (p a b))))
      (: prod-fst  (-> (p a b) a))
      (: prod-snd  (-> (p a b) b)))

    (protocol (Coprod (s :: (-> * (-> * *))))
      (: inj-left  (-> a (s a b)))
      (: inj-right (-> b (s a b)))
      (: co-elim   (-> (-> a c) (-> (-> b c) (-> (s a b) c)))))

    (instance (Prod Pair)
      (define (mk-prod a b) (Pair a b))
      (define (prod-fst q) (match q [(Pair a _) a]))
      (define (prod-snd q) (match q [(Pair _ b) b])))

    (instance (Coprod Either)
      (define (inj-left  a) (Left  a))
      (define (inj-right b) (Right b))
      (define (co-elim f g s) (match s [(Left a) (f a)] [(Right b) (g b)])))

    ;; Category has no product.  `comp` is standard (right-to-left)
    ;; composition, matching function `compose` and Haskell's `.`:
    ;; `(comp g f)` runs `f` first, then `g`.  `ident` is return-typed
    ;; (resolved from the expected type, like `pure`).
    (protocol (Category (cat :: (-> * (-> * *))))
      (: ident (cat a a))
      (: comp (-> (cat b c) (-> (cat a b) (cat a c)))))

    ;; Arrow over a product `p` (determined by the arrow via the fundep).
    ;; Signatures use `(p a c)` in place of the old `(Pair a c)`.
    (protocol (Arrow (cat :: (-> * (-> * *))) (p :: (-> * (-> * *))))
      (#:requires (Category cat) (Prod p))
      (#:fundep cat -> p)
      ;; Minimal complete definition: `arr` plus EITHER `on-first` OR
      ;; `split` — they derive from each other (an Applicative-style
      ;; default cycle, broken by defining one).  The swap/dup helpers
      ;; are built from the product tensor `mk-prod`/`prod-fst`/`prod-snd`.
      ;; Crucially `split` derives from `on-first` DIRECTLY (via swap),
      ;; not through `on-second`, so an instance that defines only
      ;; `on-first` doesn't loop; `on-first`/`on-second` derive from
      ;; `split` via the Category identity `ident`.  An instance whose
      ;; product needs special handling — e.g. a LAZY arrow that must
      ;; leave the untouched component unforced — defines the derived
      ;; ones itself, overriding the defaults (see rackton/data/arrow-lazy).
      (: arr       (-> (-> a b) (cat a b)))
      (: on-first  (-> (cat a b) (cat (p a c) (p b c))))
      (: on-second (-> (cat a b) (cat (p c a) (p c b))))
      (: split     (-> (cat a b) (-> (cat c d) (cat (p a c) (p b d)))))
      (: fanout    (-> (cat a b) (-> (cat a c) (cat a (p b c)))))
      ;; first f  = f *** id
      (define (on-first f) (split f ident))
      ;; second g = id *** g
      (define (on-second g) (split ident g))
      ;; f *** g = first f >>> arr swap >>> first g >>> arr swap
      ;;           (comp is right-to-left, so reads bottom-up)
      (define (split f g)
        (comp (arr (lambda (q) (mk-prod (prod-snd q) (prod-fst q))))
              (comp (on-first g)
                    (comp (arr (lambda (q) (mk-prod (prod-snd q) (prod-fst q))))
                          (on-first f)))))
      ;; f &&& g = arr dup >>> (f *** g)
      (define (fanout f g)
        (comp (split f g) (arr (lambda (x) (mk-prod x x))))))

    (instance (Category (->))
      (define ident (lambda (x) x))
      (define (comp f g) (lambda (x) (f (g x)))))

    ;; Only the primitives; on-second/split/fanout come from the Arrow
    ;; defaults.  (The strict arrow's runtime impls are hand-registered
    ;; in prelude-runtime.rkt as a direct, swap-free reference version.)
    (instance (Arrow (->) Pair)
      (define (arr f) f)
      (define (on-first f)
        (lambda (q) (match q [(Pair a c) (Pair (f a) c)]))))

    ;; ArrowChoice routes a coproduct `s` through one of two arrows by
    ;; branch; on-right/fork/fanin derive from on-left via `inj-*`/`co-elim`.
    (protocol (ArrowChoice (cat :: (-> * (-> * *)))
                           (p :: (-> * (-> * *)))
                           (s :: (-> * (-> * *))))
      (#:requires (Arrow cat p) (Coprod s))
      (#:fundep cat -> p s)
      ;; Minimal complete definition: EITHER `on-left` OR `fork` — the
      ;; coproduct analog of Arrow's on-first/split cycle.  The
      ;; mirror/untag helpers are built from the coproduct tensor
      ;; `inj-left`/`inj-right`/`co-elim`.  `fork` derives from `on-left`
      ;; DIRECTLY (via mirror), not through `on-right`, so an instance
      ;; defining only `on-left` doesn't loop; `on-left`/`on-right`
      ;; derive from `fork` via the Category identity `ident`.  A lazy
      ;; arrow overrides them, as with Arrow.
      (: on-left  (-> (cat a b) (cat (s a x) (s b x))))
      (: on-right (-> (cat a b) (cat (s x a) (s x b))))
      (: fork     (-> (cat a b) (-> (cat c d) (cat (s a c) (s b d)))))
      (: fanin    (-> (cat a c) (-> (cat b c) (cat (s a b) c))))
      ;; left f  = f +++ id
      (define (on-left f) (fork f ident))
      ;; right g = id +++ g
      (define (on-right g) (fork ident g))
      ;; f +++ g = left f >>> arr mirror >>> left g >>> arr mirror
      ;;           (comp is right-to-left, so reads bottom-up)
      (define (fork f g)
        (comp (arr (lambda (e) (co-elim inj-right inj-left e)))
              (comp (on-left g)
                    (comp (arr (lambda (e) (co-elim inj-right inj-left e)))
                          (on-left f)))))
      ;; f ||| g = (f +++ g) >>> arr untag
      (define (fanin f g)
        (comp (arr (lambda (e) (co-elim (lambda (x) x) (lambda (x) x) e)))
              (fork f g))))

    ;; Only the primitive; on-right/fork/fanin come from the ArrowChoice
    ;; defaults.  Runtime impls are hand-registered in prelude-runtime.rkt.
    (instance (ArrowChoice (->) Pair Either)
      (define (on-left f)
        (lambda (r) (match r [(Left a) (Left (f a))] [(Right x) (Right x)]))))

    ;; ArrowApply makes the arrow a first-class value fed in with its
    ;; argument.  `arrow-app` is return-typed (a constant arrow).
    (protocol (ArrowApply (cat :: (-> * (-> * *))) (p :: (-> * (-> * *))))
      (#:requires (Arrow cat p))
      (#:fundep cat -> p)
      (: arrow-app (cat (p (cat a b) a) b)))

    (instance (ArrowApply (->) Pair)
      (define arrow-app (lambda (q) (match q [(Pair f x) (f x)]))))

    ;; ArrowLoop ties a feedback channel: the second half of the output is
    ;; fed back as the second half of the input.  No `(->)` instance — a
    ;; lawful function-arrow loop needs laziness to tie the recursive knot,
    ;; which the strict `Pair` cannot, so `arrow-loop` / proc `rec` over a
    ;; plain function is correctly a type error.  An arrow with a lazy
    ;; product (see rackton/data/lazy's lazy-function arrow) can define one.
    (protocol (ArrowLoop (cat :: (-> * (-> * *))) (p :: (-> * (-> * *))))
      (#:requires (Arrow cat p))
      (#:fundep cat -> p)
      (: arrow-loop (-> (cat (p a c) (p b c)) (cat a b))))

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
      (define (to-list xs) (foldr (lambda (x acc)  (Cons x acc))  Nil xs))
      ;; `to-list` reconstructs the elements by folding with `Cons`/`Nil`,
      ;; and `length` agrees with the length of that list.  Quantified
      ;; over the element type `a`: the list comparison is via an assumed
      ;; `(Eq (List a))`, the count via the prelude's `(Eq Integer)`.
      #:laws
        ([to-list-folds ((Eq (List a)) =>
           (All ([xs : (t a)])
             (== (to-list xs) (foldr (lambda (x acc) (Cons x acc)) Nil xs))))]
         [length-counts (All ([xs : (t a)])
             (== (length xs) (length (to-list xs))))]))

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
                   (-> (-> a (f b)) (-> (t a) (f (t b))))))
      ;; The identity law, witnessed by the `Maybe` applicative whose
      ;; `pure` is `Some`: traversing with `Some` wraps the whole
      ;; container, i.e. `traverse Some == Some`.  Quantified over the
      ;; element type `a`; comparing the results needs `Eq (Maybe (t
      ;; a))`, which follows from an assumed `(Eq (t a))`.  (`Maybe`
      ;; stands in as the applicative because the prelude has no
      ;; `Identity` functor.)
      #:laws
        ([identity-maybe ((Eq (t a)) =>
           (All ([xs : (t a)])
             (== (traverse (lambda (x) (Some x)) xs) (Some xs))))]))

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
      (: mappend (-> a (-> a a)))
      ;; `mappend` is associative.  Equality is needed only to state the
      ;; law, so it is assumed via `(Eq a)` rather than required of every
      ;; Semigroup instance.
      #:laws
        ([associativity ((Eq a) =>
           (All ([x : a] [y : a] [z : a])
             (== (mappend (mappend x y) z)
                 (mappend x (mappend y z)))))]))

    (protocol (Monoid [a => Semigroup])
      (: mempty a)
      ;; `mempty` is a two-sided identity for `mappend`.  Its type is
      ;; return-typed but pinned here by `mappend`'s argument, so the law
      ;; resolves it to the law's own (skolem) instance.
      #:laws
        ([left-identity  ((Eq a) => (All ([x : a]) (== (mappend mempty x) x)))]
         [right-identity ((Eq a) => (All ([x : a]) (== (mappend x mempty) x)))]))

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

    ;; --- Array flattening ---------------------------------------
    ;;
    ;; Collapse a nested `(Array n (Array m a))` into a flat
    ;; `(Array (* n m) a)`.  Both have the same type and differ only in
    ;; element order: `flatten-major` is row-major (outer index slowest),
    ;; `flatten-minor` is column-major (outer index fastest).  Runtime
    ;; impls live in private/array-runtime.rkt (re-provided by
    ;; prelude-runtime); the sizes n and m are recovered from the array
    ;; lengths at runtime.
    (: flatten-major (-> (Array n (Array m a)) (Array (* n m) a)))
    (: flatten-minor (-> (Array n (Array m a)) (Array (* n m) a)))

    ;; Size-preserving map, strict left/right folds, and an applicative
    ;; traverse over an array of any (including polymorphic) size.
    ;; Runtime impls in array-runtime / prelude-runtime.
    (: array-map   (-> (-> a b) (-> (Array n a) (Array n b))))
    (: array-fold  (-> (-> b (-> a b)) (-> b (-> (Array n a) b))))
    (: array-foldr (-> (-> a (-> b b)) (-> b (-> (Array n a) b))))
    ;; mapM/traverse-style: apply an effectful function to each element
    ;; and rebuild the array inside the applicative (pure is resolved and
    ;; prepended at each call site, like mconcat's mempty).
    (: array-traverse ((Applicative f) =>
                       (-> (-> a (f b)) (-> (Array n a) (f (Array n b))))))

    ;; --- Enum ---------------------------------------------------
    ;;
    ;; A type whose values map to and from the integers, after Haskell's
    ;; `Enum`.  `integer->enum` is the integer→value direction (Haskell
    ;; `toEnum`) and is RETURN-TYPED (`a` appears only in the result, so
    ;; it is resolved at the call site from the expected type, like
    ;; `pure` / `mempty`); `enum->integer` is the value→integer direction
    ;; (`fromEnum`).  `succ` / `pred` step by one and default through the
    ;; two conversions, so a minimal instance supplies only the
    ;; conversions.
    (protocol (Enum a)
      (: succ          (-> a a))
      (: pred          (-> a a))
      (: integer->enum (-> Integer a))
      (: enum->integer (-> a Integer))
      (define (succ x) (integer->enum (+ (enum->integer x) 1)))
      (define (pred x) (integer->enum (- (enum->integer x) 1)))
      ;; `succ`/`pred` move one position, witnessed on the integer
      ;; index by `enum->integer`.  The comparison is between `Integer`s,
      ;; whose `Eq`/`Num` instances are already in scope, so no law
      ;; context is needed.
      #:laws
        ([succ-step (All ([x : a])
                      (== (enum->integer (succ x)) (+ (enum->integer x) 1)))]
         [pred-step (All ([x : a])
                      (== (enum->integer (pred x)) (- (enum->integer x) 1)))]))

    ;; Integer is its own enumeration: both conversions are the identity,
    ;; so `succ`/`pred` are +1/-1.  Like `Ord Integer` (which defines only
    ;; `<`), the type side gives just the primitives; the runtime in
    ;; prelude-runtime materializes every method.
    (instance (Enum Integer)
      (define (integer->enum i) (racket Integer (i) 0))
      (define (enum->integer n) (racket Integer (n) 0)))

    ;; Range builders — free functions with an `Enum`-constrained `a`,
    ;; exactly like `mconcat` over `Monoid`.  The elaborator prepends the
    ;; resolved `integer->enum` impl; `enum->integer` dispatches per call.
    ;; Their runtime bodies live in prelude-runtime (Rackton can't yet
    ;; rewrite a user-written needs-dict body).  Both produce strict
    ;; `List`s, so — unlike Haskell's lazy `enumFrom` — there is no
    ;; unbounded form, and a zero step in `enum-from-then-to` yields a
    ;; single element rather than diverging.
    (: enum-from-to      ((Enum a) => (-> a (-> a (List a)))))
    (: enum-from-then-to ((Enum a) => (-> a (-> a (-> a (List a))))))

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

    ;; --- Exact complex (ComplexExact) --------------
    ;; The exact counterpart of Complex, backed by Racket's exact
    ;; non-real numbers.  Components are exact integers (the Gaussian
    ;; integers); the constructor and accessors are integer-valued, the
    ;; exact parallel of make-rational / numerator / denominator.  No
    ;; Ord (complex has no total order), no Fractional / Floating (those
    ;; leave the exact-integer world).

    (data ComplexExact)

    (: make-complex-exact (-> Integer (-> Integer ComplexExact)))
    (define (make-complex-exact re im) (racket ComplexExact (re im) #f))

    (: real-part-exact (-> ComplexExact Integer))
    (define (real-part-exact c) (racket Integer (c) 0))

    (: imag-part-exact (-> ComplexExact Integer))
    (define (imag-part-exact c) (racket Integer (c) 0))

    (instance (Eq ComplexExact)
      (define (== x y) (racket Boolean (x y) #f)))
    (instance (Show ComplexExact)
      (define (show x) (racket String (x) "")))
    (instance (Num ComplexExact)
      (define (+ x y) (racket ComplexExact (x y) #f))
      (define (- x y) (racket ComplexExact (x y) #f))
      (define (* x y) (racket ComplexExact (x y) #f))
      (define (abs    x) (racket ComplexExact (x) #f))
      (define (negate x) (racket ComplexExact (x) #f)))

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
    ;; Structural instances for Maybe / List / Pair / Either / Unit,
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

    ;; (Eq/Ord/Show for Pair are provided structurally by the variadic
    ;;  Tuple instances — Pair is the binary tuple.)

    (instance ((Eq a) (Eq b) => (Eq (Either a b)))
      (define (== r1 r2)
        (match r1
          [(Left x)  (match r2 [(Left y)  (== x y)] [(Right _) #f])]
          [(Right x) (match r2 [(Left _)  #f]       [(Right y) (== x y)])])))

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
                                             (string-append " " (elems t)))]))])
          (string-append "[" (string-append (elems xs) "]")))))

    (instance ((Show a) (Show b) => (Show (Either a b)))
      (define (show r)
        (match r
          [(Left x)  (string-append "(Left "  (string-append (show x) ")"))]
          [(Right x) (string-append "(Right " (string-append (show x) ")"))])))

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
