#lang racket/base

;; Tests for the kind-level substitution and unifier in types.rkt —
;; the kind analogue of the type unifier.  Kinds are `kind-star`,
;; `(kind-arr dom cod)`, and `(kvar name)`; unify-kind returns a kind
;; substitution or raises exn:fail:kind-unify.

(module+ test
  (require rackunit
           "types.rkt")

  (define ks kstar)
  (define (ka d c) (kind-arr d c))
  (define (kv n) (kvar n))

  ;; ----- apply-ksubst --------------------------------------------------

  (check-equal? (apply-ksubst (ksubst-singleton 'a ks) (kv 'a)) ks)
  (check-equal? (apply-ksubst (ksubst-singleton 'a ks) (kv 'b)) (kv 'b)
                "an unrelated kvar is left alone")
  (check-equal? (apply-ksubst (ksubst-singleton 'a ks) (ka (kv 'a) (kv 'a)))
                (ka ks ks)
                "substitution reaches into arrow kinds")
  (check-equal? (apply-ksubst empty-ksubst (ka (kv 'a) ks)) (ka (kv 'a) ks))

  ;; ----- unify-kind: success -------------------------------------------

  (check-equal? (unify-kind ks ks) empty-ksubst
                "* unifies with * trivially")

  (check-equal? (apply-ksubst (unify-kind (kv 'a) ks) (kv 'a)) ks
                "a kvar binds to *")
  (check-equal? (apply-ksubst (unify-kind ks (kv 'a)) (kv 'a)) ks
                "binding works in either direction")

  ;; `a -> *` unified with `* -> b` forces a = * and b = *.
  (let ([s (unify-kind (ka (kv 'a) ks) (ka ks (kv 'b)))])
    (check-equal? (apply-ksubst s (kv 'a)) ks)
    (check-equal? (apply-ksubst s (kv 'b)) ks))

  ;; `a` unified with `* -> *` binds a to the arrow.
  (check-equal? (apply-ksubst (unify-kind (kv 'a) (ka ks ks)) (kv 'a))
                (ka ks ks))

  ;; Unifying a kvar with itself is the empty substitution.
  (check-equal? (unify-kind (kv 'a) (kv 'a)) empty-ksubst)

  ;; ----- unify-kind: failure -------------------------------------------

  (check-exn exn:fail:kind-unify?
             (lambda () (unify-kind ks (ka ks ks)))
             "* does not unify with an arrow (applying a *)")
  (check-exn exn:fail:kind-unify?
             (lambda () (unify-kind (ka ks ks) ks)))
  (check-exn exn:fail:kind-unify?
             (lambda () (unify-kind (ka ks ks) (ka ks (ka ks ks))))
             "arrows whose parts mismatch")

  ;; Occurs check: a = (a -> *) has no finite solution.
  (check-exn exn:fail:kind-unify?
             (lambda () (unify-kind (kv 'a) (ka (kv 'a) ks)))
             "occurs check rejects an infinite kind")

  ;; ----- composition ---------------------------------------------------

  (let* ([s1 (ksubst-singleton 'a (kv 'b))]
         [s2 (ksubst-singleton 'b ks)]
         [s  (ksubst-compose s2 s1)])
    (check-equal? (apply-ksubst s (kv 'a)) ks
                  "compose applies s2 through s1's range")
    (check-equal? (apply-ksubst s (kv 'b)) ks))

  ;; ----- helpers -------------------------------------------------------

  (check-equal? (kind-arrow* (list ks ks) ks) (ka ks (ka ks ks))
                "kind-arrow* right-folds into curried arrows")
  (check-equal? (kind-arrow* '() ks) ks)
  (check-equal? (arity->star-kind 2) (ka ks (ka ks ks)))
  (check-equal? (arity->star-kind 0) ks)

  (check-equal? (default-kind (ka (kv 'a) (kv 'b))) (ka ks ks)
                "default-kind replaces every residual kvar with *")
  (check-equal? (default-kind ks) ks)

  (check-equal? (kind->datum (kv 'a)) '?
                "a kvar renders as ? in diagnostics")
  (check-equal? (kind->datum (ka ks ks)) '(-> * *)))
