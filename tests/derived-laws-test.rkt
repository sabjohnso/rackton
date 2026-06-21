#lang racket/base

;; Feature 9 / Phase 1: runnable law bundles generated from a protocol's
;; `#:laws`.
;;
;; A protocol that declares `#:laws` in a module that imports
;; `rackton/unit` auto-emits a `<Class>-laws` function: a normal binding
;; of type `(… (Show a) => (-> (Gen a) Test))`, one property per law,
;; whose failure message names the law and labels each binder by source
;; name.  These tests pin:
;;
;;   - a lawful instance passes (no failing properties);
;;   - an unlawful instance is caught, and the failure message names the
;;     law (`Combine: associativity`) and shows labeled binders (`x = …`);
;;   - the gate: a protocol with laws but NO unit import emits no bundle
;;     (the `<Class>-laws` name is unbound), while WITH the import it is
;;     bound and type-checks;
;;   - the generated bundle is `provide`-able and usable from another
;;     module (see derived-laws-lib.rkt).

(require (prefix-in ru: rackunit)
         (for-syntax racket/base)
         racket/port
         "../main.rkt")

;; ----- same-module: lawful passes, unlawful is caught -----

(rackton
  (require "../unit.rkt")

  ;; A user protocol with one law.  The `(Eq a) =>` context lets the
  ;; equation compare results without making Eq a superprotocol.
  (protocol (Combine a)
    (: combine (-> a (-> a a)))
    #:laws
      ([associativity ((Eq a) =>
        (All ([x : a] [y : a] [z : a])
          (== (combine (combine x y) z)
              (combine x (combine y z)))))]))

  ;; A lawful instance: integer addition is associative.
  (data Sum (MkSum Integer))
  (instance (Eq Sum)
    (define (== a b) (match a [(MkSum x) (match b [(MkSum y) (== x y)])])))
  (instance (Show Sum)
    (define (show a) (match a [(MkSum x) (integer->string x)])))
  (instance (Combine Sum)
    (define (combine a b)
      (match a [(MkSum x) (match b [(MkSum y) (MkSum (+ x y))])])))
  (: gen-sum (Gen Sum))
  (define gen-sum (fmap (lambda (n) (MkSum n)) (int-range 1 20)))

  ;; An unlawful instance: subtraction is NOT associative.
  (data Diff (MkDiff Integer))
  (instance (Eq Diff)
    (define (== a b) (match a [(MkDiff x) (match b [(MkDiff y) (== x y)])])))
  (instance (Show Diff)
    (define (show a) (match a [(MkDiff x) (integer->string x)])))
  (instance (Combine Diff)
    (define (combine a b)
      (match a [(MkDiff x) (match b [(MkDiff y) (MkDiff (- x y))])])))
  (: gen-diff (Gen Diff))
  (define gen-diff (fmap (lambda (n) (MkDiff n)) (int-range 1 20)))

  ;; The generated bundle, driven by each type's generator.
  (: good-summary (IO Summary))
  (define good-summary (run-tests (Combine-laws gen-sum)))
  (: bad-summary (IO Summary))
  (define bad-summary (run-tests (Combine-laws gen-diff))))

(define good-counts #f)
(define bad-output
  (with-output-to-string
    (lambda ()
      (define s (run-io good-summary))
      (set! good-counts (cons (summary-passed s) (summary-failed s))))))
(define bad-counts #f)
(define bad-text
  (with-output-to-string
    (lambda ()
      (define s (run-io bad-summary))
      (set! bad-counts (cons (summary-passed s) (summary-failed s))))))

(ru:test-case "generated Combine-laws passes for a lawful instance"
  (ru:check-equal? (cdr good-counts) 0)
  (ru:check-true   (> (car good-counts) 0)))

(ru:test-case "generated Combine-laws catches a non-associative instance"
  (ru:check-true (> (cdr bad-counts) 0)))

(ru:test-case "the failure message names the law and labels the binders"
  (ru:check-true (regexp-match? #rx"Combine: associativity" bad-text)
                 "failure should name the law")
  (ru:check-true (regexp-match? #rx"x = " bad-text)
                 "failure should label binders by source name"))

;; ----- the unit-import gate -----

(define-syntax-rule (compile-rackton form ...)
  (eval #'(rackton form ...)
        (variable-reference->namespace (#%variable-reference))))

(ru:test-case "no unit import: no <Class>-laws bundle is emitted"
  ;; Referencing the would-be bundle name is an unbound-identifier error,
  ;; because without rackton/unit in scope nothing is generated.
  (ru:check-exn exn:fail?
    (lambda ()
      (compile-rackton
       (protocol (Gated a)
         (: op (-> a Boolean))
         #:laws ([trivial (All ([x : a]) (op x))]))
       (define ignored Gated-laws)))))

(ru:test-case "with unit import: the bundle is emitted and type-checks"
  (ru:check-not-exn
    (lambda ()
      (compile-rackton
       (require "../unit.rkt")
       (protocol (Gated a)
         (: op (-> a Boolean))
         #:laws ([trivial (All ([x : a]) (op x))]))
       (define ignored Gated-laws)))))

;; ----- cross-module: the generated bundle is exported and reused -----
;;
;; The lib (a `#lang rackton` module) declares protocol `Merge` with a
;; law and exports the generated `Merge-laws` plus a lawful type and
;; generator.  This block, in a different module, calls the imported
;; bundle.

(rackton
  (require "../unit.rkt")
  (require "derived-laws-lib.rkt")
  (: xmod-summary (IO Summary))
  (define xmod-summary (run-tests (Merge-laws gen-thing))))

(define xmod-counts #f)
(define xmod-output
  (with-output-to-string
    (lambda ()
      (define s (run-io xmod-summary))
      (set! xmod-counts (cons (summary-passed s) (summary-failed s))))))

(ru:test-case "an imported generated bundle runs against an imported instance"
  (ru:check-equal? (cdr xmod-counts) 0)
  (ru:check-true   (> (car xmod-counts) 0)))
