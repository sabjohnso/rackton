#lang racket/base

;; Feature 7: first-class existential types — anonymous `(Exists (a) (=> C
;; body))` types, the dual of the rank-N `(All (a) …)` forall.
;;
;; PHASE 1 (this file, so far): the type-system plumbing only — the
;; `texists` core node + `ty:exists` surface node and the
;; parse/type-vars/subst/print/unify sweep.  There is no pack/open yet,
;; so a first-class existential cannot be *constructed* — Phase 1's
;; observable behaviour is at the type level (it parses, resolves,
;; kind-checks, prints, and unifies by alpha-equivalence) plus a
;; signature that merely *mentions* one compiling.  Pack (Phase 2) and
;; open (Phase 3) add runnable programs to this file.

(module+ test
  (require rackunit
           racket/set
           "../private/types.rkt"
           "../private/surface.rkt"
           (only-in "../main.rkt" rackton))

  (define t-int (tcon 'Integer))
  ;; (Exists (a) (=> (Show a) a)) at the core level.
  (define (ex-show)
    (texists '(a) (mqual (list (pred 'Show (list (tvar 'a)))) (tvar 'a))))

  ;; ----- surface: parse (Exists …) ----------------------------------

  (let ([ast (parse-type #'(Exists (a) (=> (Show a) a)))])
    (check-pred ty:exists? ast "(Exists …) parses to a ty:exists node")
    (check-equal? (ty:exists-vars ast) '(a)))

  ;; ----- types.rkt sweep --------------------------------------------

  (check-true (type? (ex-show)) "a texists is a type")

  ;; A bound existential var is NOT free in the surrounding type.
  (check-equal? (type-vars (texists '(a) (tvar 'a))) (seteq))
  (check-equal? (type-vars (texists '(a) (make-arrow (tvar 'a) (tvar 'b))))
                (seteq 'b)
                "only the un-bound var is free")

  ;; Substitution skips a shadowed bound var, but reaches a free one.
  (check-equal? (apply-subst (subst-singleton 'a t-int)
                             (texists '(a) (tvar 'a)))
                (texists '(a) (tvar 'a))
                "a bound var shadows the substitution")
  (check-equal? (apply-subst (subst-singleton 'b t-int)
                             (texists '(a) (make-arrow (tvar 'a) (tvar 'b))))
                (texists '(a) (make-arrow (tvar 'a) t-int))
                "a free var is substituted under the binder")

  ;; Printer renders the surface `Exists` form.
  (check-equal? (type->datum (ex-show))
                '(Exists (a) ((Show a) => a)))

  ;; ----- unify.rkt: alpha-equivalence -------------------------------

  (local-require "../private/unify.rkt")

  ;; Two alpha-equivalent existentials unify.
  (check-true (subst? (unify (texists '(a) (tvar 'a))
                             (texists '(b) (tvar 'b))))
              "alpha-equivalent existentials unify")

  ;; Different bodies do not.
  (check-exn exn:fail:unify?
             (lambda () (unify (texists '(a) (tvar 'a))
                               (texists '(a) t-int))))

  ;; An existential is NOT a universal: `(Exists a. a)` ≠ `(All a. a)`.
  (check-exn exn:fail:unify?
             (lambda () (unify (texists '(a) (tvar 'a))
                               (tforall '(a) (tvar 'a))))
             "existential and universal quantifiers are distinct")

  ;; ----- pipeline: a signature mentioning an existential compiles ---
  ;; No pack yet, so the argument can't be constructed; we only check
  ;; that the embedded existential resolves + kind-checks + compiles.

  (rackton
    (: ignore-exists (-> (Exists (a) (=> (Show a) a)) Integer))
    (define (ignore-exists e) 0))

  (check-true (procedure? ignore-exists)
              "a function with an existential parameter type compiles"))
