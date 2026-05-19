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
      (define (>  x y) (<  y x))
      (define (<= x y) (if (<  x y) #t (== x y)))
      (define (>= x y) (if (>  x y) #t (== x y))))

    ;; --- Num ----------------------------------------------------

    (define-class (Num a)
      (: + (-> a (-> a a)))
      (: - (-> a (-> a a)))
      (: * (-> a (-> a a))))

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
      (define (* x y) (racket Integer (x y) 0)))

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

    ;; --- ADTs ---------------------------------------------------

    (define-data (Maybe a)    None (Some a))
    (define-data (List a)     Nil  (Cons a (List a)))
    (define-data (Pair a b)   (MkPair a b))
    (define-data (Result e a) (Err e) (Ok a))
    (define-data Unit MkUnit)

    ;; --- Combinators --------------------------------------------

    (: id (-> a a))
    (define (id x) x)

    (: const (-> a (-> b a)))
    (define (const x) (lambda (_y) x))

    ;; --- Functor / Applicative / Monad (higher-kinded) --------
    ;;
    ;; The hierarchy is Functor → Applicative → Monad.  `pure` would
    ;; normally live on Applicative, but the current single-dispatch
    ;; runtime can only resolve a class method when its type has a
    ;; positional argument mentioning a class parameter (see
    ;; find-dispatch-pos in private/infer.rkt).  `pure :: a -> f a`
    ;; carries `f` only in its result, so it is provided as separate
    ;; per-monad names (`pure-io`, etc.) until a future phase adds
    ;; type-directed monomorphization at call sites.

    (define-class (Functor (f :: (-> * *)))
      (: fmap (-> (-> a b) (-> (f a) (f b)))))

    (define-class ((Functor f) => (Applicative (f :: (-> * *))))
      (: <*>   (-> (f (-> a b)) (-> (f a) (f b))))
      (: liftA2 (-> (-> a (-> b c)) (-> (f a) (-> (f b) (f c)))))
      ;; The default uses an explicit one-argument-at-a-time wrapper
      ;; for `g` because Rackton's multi-argument lambdas compile to
      ;; multi-argument Racket functions, not curried ones — partial
      ;; application of `g` at the host level would error.  The inner
      ;; eta wraps `g` so each application step stays single-argument.
      (define (liftA2 g x y)
        (<*> (fmap (lambda (xa) (lambda (yb) (g xa yb))) x) y)))

    (define-class ((Applicative m) => (Monad (m :: (-> * *))))
      (: >>= (-> (m a) (-> (-> a (m b)) (m b)))))

    ;; Maybe
    (define-instance (Functor Maybe)
      (define (fmap f m)
        (match m
          [(None)   None]
          [(Some x) (Some (f x))])))

    (define-instance (Applicative Maybe)
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

    ;; --- Numeric helpers --------------------------------------

    (: mod (-> Integer (-> Integer Integer)))
    (define (mod a b) (racket Integer (a b) 0))

    (: div (-> Integer (-> Integer Integer)))
    (define (div a b) (racket Integer (a b) 0))

    (: abs (-> Integer Integer))
    (define (abs n) (racket Integer (n) 0))

    (: min (-> Integer (-> Integer Integer)))
    (define (min a b) (racket Integer (a b) 0))

    (: max (-> Integer (-> Integer Integer)))
    (define (max a b) (racket Integer (a b) 0))

    (: integer->string (-> Integer String))
    (define (integer->string n) (racket String (n) ""))

    (: string->integer (-> String (Maybe Integer)))
    (define (string->integer s) (racket (Maybe Integer) (s) None))

    ;; --- IO ---------------------------------------------------

    (define-data (IO a))

    (define-instance (Functor IO)
      (define (fmap f io) (racket (IO b) (f io) #f)))

    (define-instance (Applicative IO)
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
            (let ([halves (split-at (div n 2) xs)])
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
      (define (* x y) (racket Float (x y) 0.0)))

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

    (: sqrt (-> Float Float))
    (define (sqrt x) (racket Float (x) 0.0))

    (: integer->float (-> Integer Float))
    (define (integer->float n) (racket Float (n) 0.0))

    (: float->integer (-> Float Integer))
    (define (float->integer x) (racket Integer (x) 0))

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
