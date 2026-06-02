#lang racket/base

;; `let` and `where` bindings accept patterns, not just identifiers, so a
;; binding can destructure its right-hand side.  This subsumes the old
;; `match-let` form (now removed):
;;   - `let`   keeps PARALLEL semantics — every RHS is evaluated in the
;;     surrounding scope, then each pattern destructures its own value;
;;   - `where` keeps SEQUENTIAL semantics — a later binding sees the
;;     pattern variables bound earlier.
;; A non-variable pattern lowers to an IRREFUTABLE match (a failure
;; panics), exactly as `match-let` did; a plain identifier binding is
;; unchanged (and still let-polymorphic).

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

(define-syntax-rule (check-rackton-compile-error form ...)
  (check-exn
   exn:fail?
   (lambda ()
     (eval #'(rackton form ...)
           (variable-reference->namespace (#%variable-reference))))))

;; ----- let: parallel destructuring of independent values ----------

(rackton
  (: pair-sum Integer)
  (define pair-sum
    (let ([(MkPair a b) (MkPair 7 35)]
          [(Cons h _)   (Cons 100 Nil)])
      (+ a (+ b h)))))

(test-case "let destructures multiple independent values"
  (check-equal? pair-sum 142))

;; ----- let: mixing plain and pattern bindings ---------------------

(rackton
  (: mixed Integer)
  (define mixed
    (let ([(MkPair a b) (MkPair 10 20)]
          [c            (+ 1 2)])
      (+ a (+ b c)))))

(test-case "let mixes plain and destructuring bindings"
  (check-equal? mixed 33))

;; ----- let: a plain binding stays let-polymorphic -----------------

(rackton
  (: poly Boolean)
  (define poly
    ;; `id` used at two types within one body proves the binding is
    ;; generalised (monomorphic binding would unify Integer with String).
    (let ([f (lambda (x) x)])
      (if (== (f 1) 1) (f #t) #f))))

(test-case "plain let binding is still generalised"
  (check-true poly))

;; ----- where: sequential destructuring ----------------------------

(rackton
  (: seq Integer)
  (define seq
    ;; The second binding destructures a value built from the first —
    ;; only possible because `where` is sequential.
    (where ([(MkPair a b) (MkPair 3 4)]
            [(MkPair c d) (MkPair (+ a b) (* a b))])
      (+ c d))))

(test-case "where destructures sequentially (later sees earlier)"
  (check-equal? seq 19))

;; ----- match-let is gone ------------------------------------------

(test-case "match-let is no longer a recognized form"
  (check-rackton-compile-error
   (: x Integer)
   (define x (match-let ([(MkPair a b) (MkPair 1 2)]) (+ a b)))))
