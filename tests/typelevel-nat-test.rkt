#lang racket/base

;; Phase 3 — the type-level natural-number layer: `Nat`-kinded literals.
;;
;; A numeric literal may now appear in type position; it denotes a
;; type-level natural of kind `Nat` (a `tnat` in the core type AST).
;; This phase only adds the kind and the literals — arithmetic on them
;; (Phase 4) and a type that consumes them, e.g. fixed-size arrays
;; (Phase 5), come later.  So the checks here are unit-level (parsing,
;; unification, the scheme codec) plus one integration check that the
;; kind system rejects a literal where a value type is expected.

(require rackunit
         (for-syntax racket/base)
         (only-in "../private/types.rkt"
                  tnat tvar tcon make-tapp empty-subst subst-singleton
                  type->datum kind-nat)
         (only-in "../private/unify.rkt" unify exn:fail:unify?)
         (only-in "../private/surface.rkt"
                  parse-type ty:nat ty:nat? ty:nat-value ty:app? ty:app-args)
         (only-in "../private/scheme-codec.rkt" sexp->type encode-kind decode-kind)
         "../main.rkt")

(define-syntax-rule (check-rackton-compile-error form ...)
  (check-exn
   exn:fail?
   (lambda ()
     (eval #'(rackton form ...)
           (variable-reference->namespace (#%variable-reference))))))

;; ----- parsing: a numeric literal in type position is a tnat -------

(test-case "a numeric literal parses to a type-level nat"
  (define n (parse-type (datum->syntax #f 3)))
  (check-true (ty:nat? n))
  (check-equal? (ty:nat-value n) 3)
  ;; …and as an argument: (Vec 3 a) has the nat in head position 0.
  (define t (parse-type (datum->syntax #f '(Vec 3 a))))
  (check-true (ty:app? t))
  (check-equal? (ty:nat-value (car (ty:app-args t))) 3))

;; ----- unification: nats are equal iff their values match ----------

(test-case "tnat unifies with an equal tnat, not an unequal one"
  (check-equal? (unify (tnat 3) (tnat 3)) empty-subst)
  (check-exn exn:fail:unify? (lambda () (unify (tnat 3) (tnat 4)))))

(test-case "a type variable binds to a tnat"
  (check-equal? (unify (tvar 'a) (tnat 5))
                (subst-singleton 'a (tnat 5))))

;; ----- the scheme codec round-trips nats and the Nat kind ----------

(test-case "tnat round-trips through the type codec"
  (define t (make-tapp (tcon 'Vec) (list (tnat 4) (tvar 'a))))
  (check-equal? (sexp->type (type->datum t)) t))

(test-case "kind-nat round-trips through the kind codec"
  (check-equal? (decode-kind (encode-kind (kind-nat))) (kind-nat)))

;; ----- kind integration: a nat is not a value type ----------------

(test-case "a nat literal where a value type is expected is a kind error"
  ;; Maybe :: * -> *, but 3 :: Nat — the argument kinds disagree.
  (check-rackton-compile-error
   (: x (Maybe 3))
   (define x None)))
