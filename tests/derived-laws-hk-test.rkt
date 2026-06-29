#lang racket/base

;; Feature 9 / Phase 2: generated law bundles for higher-kinded classes,
;; best-effort with per-law skipping.
;;
;; A law is RUNNABLE iff (a) no binder has a function type — we have no
;; function generator — and (b) its body uses no return-typed method
;; (`pure` / `mempty` / a user method whose dispatch is by result type),
;; which cannot resolve from a passed dictionary.  A bundle is generated
;; for the class's runnable laws only; a class with no runnable law emits
;; no bundle.  These tests pin:
;;
;;   - a higher-kinded class (Functor-shaped) whose `identity` law is
;;     runnable but whose `composition` law has function binders: the
;;     bundle runs exactly the identity law (1 property), proving the
;;     function-binder law was skipped;
;;   - a monoid-shaped class with a return-typed `unit2`: its `assoc` law
;;     runs while the `left-unit` law (which uses `unit2`) is skipped —
;;     and, crucially, the module still COMPILES (a naive bundle over the
;;     return-typed law would not type-check);
;;   - a class whose every law is unrunnable emits no `<Class>-laws`.

(require (prefix-in ru: rackunit)
         (for-syntax racket/base)
         racket/port
         "../main.rkt")

;; ----- higher-kinded: identity runs, composition (function binders) skipped -----

(rackton
  (require "../unit.rkt")

  (protocol (Box2 (f :: (-> * *)))
    (: map2 (-> (-> a b) (-> (f a) (f b))))
    :laws
      ([identity ((Eq (f a)) =>
         (All ([xs : (f a)]) (== (map2 (lambda (x) x) xs) xs)))]
       [composition ((Eq (f c)) =>
         (All ([g : (-> b c)] [h : (-> a b)] [xs : (f a)])
           (== (map2 (lambda (x) (g (h x))) xs)
               (map2 g (map2 h xs)))))]))

  (data (Box a) (MkBox a))
  (instance ((Eq a) => (Eq (Box a)))
    (define (== p q) (match p [(MkBox x) (match q [(MkBox y) (== x y)])])))
  (instance ((Show a) => (Show (Box a)))
    (define (show p) (match p [(MkBox x) (string-append "Box " (show x))])))
  (instance (Box2 Box)
    (define (map2 f b) (match b [(MkBox x) (MkBox (f x))])))
  (: gen-box (Gen (Box Integer)))
  (define gen-box (fmap (lambda (n) (MkBox n)) (int-range 1 20)))

  (: box-summary (IO Summary))
  (define box-summary (run-tests (Box2-laws gen-box))))

(define box-counts #f)
(define _drop1
 (with-output-to-string
   (lambda ()
     (define s (run-io box-summary))
     (set! box-counts (cons (summary-passed s) (summary-failed s))))))

(ru:test-case "higher-kinded identity law runs; composition (function binders) is skipped"
  ;; Exactly ONE property ran (identity) — composition was skipped.
  (ru:check-equal? (car box-counts) 1)
  (ru:check-equal? (cdr box-counts) 0))

;; ----- return-typed method: assoc runs, left-unit (uses unit2) skipped -----

(rackton
  (require "../unit.rkt")

  (protocol (MyMonoid a)
    (: combine2 (-> a (-> a a)))
    (: unit2 a)                          ;; return-typed (dispatch by result)
    :laws
      ([assoc ((Eq a) =>
         (All ([x : a] [y : a] [z : a])
           (== (combine2 (combine2 x y) z)
               (combine2 x (combine2 y z)))))]
       [left-unit ((Eq a) =>
         (All ([x : a]) (== (combine2 unit2 x) x)))]))

  (data Tally (MkTally Integer))
  (instance (Eq Tally)
    (define (== p q) (match p [(MkTally x) (match q [(MkTally y) (== x y)])])))
  (instance (Show Tally)
    (define (show p) (match p [(MkTally x) (integer->string x)])))
  (instance (MyMonoid Tally)
    (define (combine2 a b)
      (match a [(MkTally x) (match b [(MkTally y) (MkTally (+ x y))])]))
    (define unit2 (MkTally 0)))
  (: gen-tally (Gen Tally))
  (define gen-tally (fmap (lambda (n) (MkTally n)) (int-range 1 20)))

  (: mono-summary (IO Summary))
  (define mono-summary (run-tests (MyMonoid-laws gen-tally))))

(define mono-counts #f)
(define _drop2
 (with-output-to-string
   (lambda ()
     (define s (run-io mono-summary))
     (set! mono-counts (cons (summary-passed s) (summary-failed s))))))

(ru:test-case "associativity law runs; the return-typed left-unit law is skipped"
  (ru:check-equal? (car mono-counts) 1)
  (ru:check-equal? (cdr mono-counts) 0))

;; ----- a class with no runnable law emits no bundle -----

(define-syntax-rule (compile-rackton form ...)
  (eval #'(rackton form ...)
        (variable-reference->namespace (#%variable-reference))))

(ru:test-case "a protocol whose laws are all unrunnable still compiles"
  ;; `only-composition` has function binders, so it is skipped; the
  ;; protocol itself is fine.
  (ru:check-not-exn
    (lambda ()
      (compile-rackton
       (require "../unit.rkt")
       (protocol (OnlyFun (f :: (-> * *)))
         (: omap (-> (-> a b) (-> (f a) (f b))))
         :laws
           ([only-composition ((Eq (f c)) =>
              (All ([g : (-> b c)] [h : (-> a b)] [xs : (f a)])
                (== (omap (lambda (x) (g (h x))) xs)
                    (omap g (omap h xs)))))]))))))

(ru:test-case "a protocol with no runnable law emits no <Class>-laws bundle"
  (ru:check-exn exn:fail?
    (lambda ()
      (compile-rackton
       (require "../unit.rkt")
       (protocol (OnlyFun (f :: (-> * *)))
         (: omap (-> (-> a b) (-> (f a) (f b))))
         :laws
           ([only-composition ((Eq (f c)) =>
              (All ([g : (-> b c)] [h : (-> a b)] [xs : (f a)])
                (== (omap (lambda (x) (g (h x))) xs)
                    (omap g (omap h xs)))))]))
       (define ignored OnlyFun-laws)))))
