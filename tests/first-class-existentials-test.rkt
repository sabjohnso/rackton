#lang racket/base

;; Feature 7: first-class existential types — anonymous
;; `(Exists (a) (C a => body))` types, the dual of the rank-N
;; `(All (a) (C a => body))` forall (constraints use INFIX `=>`).
;;
;; PHASE 1: the type-system plumbing — the `texists` core node +
;; `ty:exists` surface node and the parse/type-vars/subst/print/unify
;; sweep.
;;
;; PHASE 2: `pack` — annotation-driven introduction.  Checking an
;; expression against an expected existential (here written
;; `(ann e (Exists …))`) unifies the witness type with the expression's
;; type, discharges the `:where` constraints at the pack site, and hides
;; the witness behind the existential.  (`open`, the eliminator, is
;; Phase 3 — so packed values still cannot be *used*, only constructed
;; and carried; the runtime value is the bare witness.)

;; NB: import everything from main.rkt EXCEPT `#%module-begin`.  main.rkt
;; doubles as a module language, so a plain `(require "../main.rkt")`
;; would install rackton's `#%module-begin` and govern the `(module+
;; test …)` submodule — feeding its ordinary Racket forms to the rackton
;; parser.  The prefixed type modules avoid the name clashes with main's
;; re-exports.
(require (for-syntax racket/base)
         rackunit
         racket/set
         (except-in "../main.rkt" #%module-begin)
         (prefix-in ty: "../private/types.rkt")
         (prefix-in srf: "../private/surface.rkt")
         (prefix-in u: "../private/unify.rkt")
         (prefix-in sc: "../private/scheme-codec.rkt"))

;; Compile-time rejection: evaluating the rackton block must raise.
(define-syntax-rule (check-rackton-compile-error form ...)
  (check-exn
   exn:fail?
   (lambda ()
     (eval #'(rackton form ...)
           (variable-reference->namespace (#%variable-reference))))))

;; ===== PHASE 2: pack (runs at module level) ==========================

;; A value packs into an existential via an annotation; the witness type
;; is hidden, so values of DIFFERENT types share one element type and sit
;; in one homogeneous list.  `open` isn't here yet, so we observe only
;; that it type-checks and that the runtime list has the right shape (the
;; existential is erased — each element is its bare witness value).
(rackton
  (: xs (List (Exists (a) ((Show a) => a))))
  (define xs
    (Cons (ann 42 (Exists (a) ((Show a) => a)))
          (Cons (ann "hi" (Exists (a) ((Show a) => a)))
                (Cons (ann #t (Exists (a) ((Show a) => a)))
                      Nil))))
  (: how-many Integer)
  (define how-many (length xs)))

;; An existential with no constraints packs any value.
(rackton
  (: anything (Exists (a) a))
  (define anything (ann 42 (Exists (a) a))))

;; ===== PHASE 3: open =================================================

;; `(open e (a x) body)` unpacks: `a` is a fresh rigid type, `x` the
;; witness value, and the packed `Show` dictionary is in scope, so
;; `(show x)` resolves and dispatches on the runtime value.  Used here to
;; render a heterogeneous list — the payoff of the whole feature.
(rackton
  (: render (-> (Exists (a) ((Show a) => a)) String))
  (define (render e) (open e (a x) (show x)))

  (: items (List (Exists (a) ((Show a) => a))))
  (define items
    (Cons (ann 42 (Exists (a) ((Show a) => a)))
          (Cons (ann "hi" (Exists (a) ((Show a) => a)))
                (Cons (ann #t (Exists (a) ((Show a) => a))) Nil))))

  (: render-all (-> (List (Exists (a) ((Show a) => a))) (List String)))
  (define (render-all ys)
    (match ys
      [(Nil) Nil]
      [(Cons e rest) (Cons (render e) (render-all rest))]))

  (: shown-all (List String))
  (define shown-all (render-all items))

  (: shown-one String)
  (define shown-one (render (ann 7 (Exists (a) ((Show a) => a))))))

(module+ test

  ;; ----- Phase 1: surface parse of (Exists …) -----------------------

  (let ([ast (srf:parse-type #'(Exists (a) ((Show a) => a)))])
    (check-pred srf:ty:exists? ast "(Exists …) parses to a ty:exists node")
    (check-equal? (srf:ty:exists-vars ast) '(a)))

  ;; ----- Phase 1: types.rkt sweep -----------------------------------

  (define t-int (ty:tcon 'Integer))
  (define (ex-show)
    (ty:texists '(a) (ty:mqual (list (ty:pred 'Show (list (ty:tvar 'a))))
                               (ty:tvar 'a))))

  (check-true (ty:type? (ex-show)) "a texists is a type")

  (check-equal? (ty:type-vars (ty:texists '(a) (ty:tvar 'a))) (seteq))
  (check-equal? (ty:type-vars (ty:texists '(a) (ty:make-arrow (ty:tvar 'a)
                                                              (ty:tvar 'b))))
                (seteq 'b)
                "only the un-bound var is free")

  (check-equal? (ty:apply-subst (ty:subst-singleton 'a t-int)
                                (ty:texists '(a) (ty:tvar 'a)))
                (ty:texists '(a) (ty:tvar 'a))
                "a bound var shadows the substitution")
  (check-equal? (ty:apply-subst (ty:subst-singleton 'b t-int)
                                (ty:texists '(a) (ty:make-arrow (ty:tvar 'a)
                                                                (ty:tvar 'b))))
                (ty:texists '(a) (ty:make-arrow (ty:tvar 'a) t-int))
                "a free var is substituted under the binder")

  (check-equal? (ty:type->datum (ex-show))
                '(Exists (a) ((Show a) => a)))

  ;; ----- Phase 5: cross-module type codec round-trip ----------------

  (check-equal? (sc:sexp->type (ty:type->datum (ex-show))) (ex-show)
                "a texists round-trips through the sidecar type codec")

  ;; ----- Phase 1: unify.rkt alpha-equivalence -----------------------

  (check-true (ty:subst? (u:unify (ty:texists '(a) (ty:tvar 'a))
                                  (ty:texists '(b) (ty:tvar 'b))))
              "alpha-equivalent existentials unify")
  (check-exn u:exn:fail:unify?
             (lambda () (u:unify (ty:texists '(a) (ty:tvar 'a))
                                 (ty:texists '(a) t-int))))
  (check-exn u:exn:fail:unify?
             (lambda () (u:unify (ty:texists '(a) (ty:tvar 'a))
                                 (ty:tforall '(a) (ty:tvar 'a))))
             "existential and universal quantifiers are distinct")

  ;; ----- Phase 2: pack behaviour ------------------------------------

  (check-equal? how-many 3
                "heterogeneous values pack into one existential element type")

  ;; The witness is erased: the packed value is the bare datum.
  (check-equal? anything 42
                "a packed value is runtime-transparent (the bare witness)")

  ;; The `:where` constraint is discharged at the pack site: packing a
  ;; value of a ground type with no `Show` instance (a concrete function
  ;; type) is rejected.  (A *polymorphic* witness leaves the constraint
  ;; over a free var — deferred, not a hard error — so the witness here is
  ;; ground: `(-> Integer Integer)`.)
  (check-rackton-compile-error
   (define bad (ann (lambda (x) (+ x 1)) (Exists (a) ((Show a) => a)))))

  ;; Likewise for a different missing constraint: no `Eq` on functions.
  (check-rackton-compile-error
   (define bad2 (ann (lambda (x) (+ x 1)) (Exists (a) ((Eq a) => a)))))

  ;; ----- Phase 3: open behaviour ------------------------------------

  (check-equal? shown-one "7"
                "open unpacks and `show` dispatches on the witness")
  (check-equal? shown-all (Cons "42" (Cons "\"hi\"" (Cons "True" Nil)))
                "open inside a fold renders a heterogeneous list")

  ;; Escape check: the opened type must not leak.  `(open e (a x) x)`
  ;; returns the witness itself, whose type is the fresh rigid `a` — that
  ;; would escape the `open`, so it is rejected.
  (check-rackton-compile-error
   (define leak
     (open (ann 42 (Exists (a) ((Show a) => a))) (a x) x))))
