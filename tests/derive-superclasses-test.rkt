#lang racket/base

;; Cross-class derivation: a Monad instance opts in with
;; `#:derive-superclasses` and bundles only the irreducible primitives
;; that no derivation can supply (`pure` and one of `flatmap`/`join`).
;; The compiler then synthesizes the missing `Functor` and `Applicative`
;; instances, filling each superclass method from (in order):
;;   1. a primitive bundled in the instance body (this is how `pure` enters),
;;   2. the deriving class's `#:derive S` table,
;;   3. S's own intra-class default,
;;   4. else a compile-time error naming the gap.
;;
;; The `pure` floor: `pure : (-> a (f a))` cannot be derived — `fmap`,
;; `flatmap`, `join` all consume an existing `f a`; none manufacture one.
;; So the user always writes `pure`; everything else is mechanical.

(require rackunit
         (only-in racket/base parameterize)
         "../main.rkt")

;; ----- derive Functor + Applicative from a simple Monad ------------

(rackton
  (data (Box a) (MkBox a))

  ;; Only pure (Applicative floor) and flatmap (Monad primitive) given;
  ;; Functor Box and the rest of Applicative Box are synthesized.
  (instance (Monad Box) #:derive-superclasses
    (define (pure x)      (MkBox x))
    (define (flatmap f b) (match b [(MkBox x) (f x)])))

  ;; fmap derived from flatmap + pure.
  (: fm Integer)
  (define fm
    (match (fmap (lambda (x) (+ x 1)) (MkBox 41)) [(MkBox v) v]))

  ;; fapply derived from flatmap + fmap.
  (: ap Integer)
  (define ap
    (match (fapply (MkBox (lambda (x) (+ x 1))) (MkBox 41)) [(MkBox v) v]))

  ;; liftA2 derived (Applicative intra-class default over derived fapply).
  (: l2 Integer)
  (define l2
    (match (liftA2 (lambda (a b) (+ a b)) (MkBox 3) (MkBox 4)) [(MkBox v) v]))

  ;; pure itself remains usable.
  (: pu Integer)
  (define pu (match (pure 7) [(MkBox v) v]))

  ;; the Monad layer still works.
  (: fl Integer)
  (define fl
    (match (flatmap (lambda (x) (MkBox (+ x 10))) (MkBox 5)) [(MkBox v) v])))

(test-case "derived Functor: fmap works"            (check-equal? fm 42))
(test-case "derived Applicative: fapply works"      (check-equal? ap 42))
(test-case "derived Applicative: liftA2 works"      (check-equal? l2 7))
(test-case "bundled pure works"                     (check-equal? pu 7))
(test-case "Monad flatmap still works"              (check-equal? fl 15))

;; ----- overlap: a hand-written Functor + derive fills only the gap --

(rackton
  (data (Bin a) (MkBin a))

  ;; User supplies Functor explicitly; derivation must NOT re-synthesize
  ;; it (no duplicate-instance error) and must still derive Applicative.
  (instance (Functor Bin)
    (define (fmap f b) (match b [(MkBin x) (MkBin (f x))])))

  (instance (Monad Bin) #:derive-superclasses
    (define (pure x)      (MkBin x))
    (define (flatmap f b) (match b [(MkBin x) (f x)])))

  (: bin-ap Integer)
  (define bin-ap
    (match (fapply (MkBin (lambda (x) (+ x 1))) (MkBin 41)) [(MkBin v) v])))

(test-case "overlap: derive only the missing superclass (Applicative)"
  (check-equal? bin-ap 42))

;; ----- negative: bundling without pure is a compile-time error -----

(test-case "deriving superclasses without pure is rejected, naming pure"
  (define ((expand src))
    (parameterize ([current-namespace (make-base-namespace)])
      (eval src)))
  (check-exn
   exn:fail?
   (expand
    '(module no-pure racket/base
       (require rackton)
       (rackton
        (data (Cell a) (MkCell a))
        ;; pure is the irreducible Applicative floor — no derivation can
        ;; supply it, so synthesizing Applicative Cell must fail here.
        (instance (Monad Cell) #:derive-superclasses
          (define (flatmap f c) (match c [(MkCell x) (f x)]))))))))

;; ----- parametric: synthesized instances inherit the context ------

(rackton
  ;; A parametric monad wrapping an inner monad.  Its instance carries a
  ;; `(Monad m)` context; the synthesized Functor/Applicative (WrapT m)
  ;; instances must inherit it, or their derived bodies (which use the
  ;; WrapT-level flatmap/pure, in turn requiring `(Monad m)`) won't check.
  (data (WrapT m a) (MkWrapT (m a)))

  (instance ((Monad m) => (Monad (WrapT m))) #:derive-superclasses
    (define (pure x) (MkWrapT (pure x)))
    (define (flatmap f w)
      (match w
        [(MkWrapT mx)
         (MkWrapT (flatmap (lambda (a) (match (f a) [(MkWrapT my) my])) mx))])))

  ;; Derived fmap over WrapT-of-Maybe.
  (: wrapped Integer)
  (define wrapped
    (match (fmap (lambda (x) (+ x 1)) (MkWrapT (Some 41)))
      [(MkWrapT m) (match m [(Some v) v] [(None) 0])])))

(test-case "parametric: synthesized instances inherit the (Monad m) context"
  (check-equal? wrapped 42))

;; ----- cross-module: synthesized superclass instances escape -------

(rackton
  (require "derive-lib-box.rkt")

  ;; The Functor/Applicative DBox instances were synthesized in the
  ;; library's elaboration; they must cross the module boundary so these
  ;; derived calls resolve here.
  (: imported-fmap Integer)
  (define imported-fmap
    (match (fmap (lambda (x) (+ x 1)) (MkDBox 41)) [(MkDBox v) v]))

  (: imported-ap Integer)
  (define imported-ap
    (match (fapply (MkDBox (lambda (x) (+ x 1))) (MkDBox 41)) [(MkDBox v) v])))

(test-case "cross-module: derived Functor DBox escapes"     (check-equal? imported-fmap 42))
(test-case "cross-module: derived Applicative DBox escapes" (check-equal? imported-ap 42))

;; ----- derive Functor FROM an Applicative instance ----------------

(rackton
  (data (Af a) (MkAf a))

  ;; Only the Applicative primitives (pure + fapply) are given; the
  ;; Functor instance is synthesized via `fmap f x = fapply (pure f) x`
  ;; (the Applicative class's own #:derive Functor clause).
  (instance (Applicative Af) #:derive-superclasses
    (define (pure x) (MkAf x))
    (define (fapply ff fx)
      (match ff [(MkAf f) (match fx [(MkAf x) (MkAf (f x))])])))

  (: af-fmap Integer)
  (define af-fmap
    (match (fmap (lambda (x) (+ x 1)) (MkAf 41)) [(MkAf v) v])))

(test-case "derive Functor from an Applicative instance"
  (check-equal? af-fmap 42))

;; ----- product / liftA2 over a derived instance (regression) ------
;; `product`'s default is `(liftA2 Pair x y)`, passing the raw 2-ary
;; Pair constructor as liftA2's function.  The derived liftA2 must apply
;; it with an n-ary call `(g a b)`, not a curried `((g a) b)` — a
;; constructor cannot be partially applied.

(rackton
  (data (Bx a) (MkBx a))
  (instance (Monad Bx) #:derive-superclasses
    (define (pure x) (MkBx x))
    (define (flatmap f b) (match b [(MkBx x) (f x)])))

  (: prod Integer)
  (define prod
    (match (product (MkBx 3) (MkBx 4))
      [(MkBx p) (match p [(Pair a b) (+ a b)])]))

  ;; liftA2 with a real (curried) function argument still works too.
  (: la2 Integer)
  (define la2
    (match (liftA2 (lambda (a b) (* a b)) (MkBx 3) (MkBx 4)) [(MkBx v) v])))

(test-case "derived product passes the bare Pair constructor"
  (check-equal? prod 7))
(test-case "derived liftA2 with a function argument"
  (check-equal? la2 12))
