#lang racket/base

;; Pattern destructuring in `lambda` / `λ` parameter positions.
;;
;; `define` function heads already accept constructor patterns
;; (see define-patterns-test.rkt); these tests pin the same support
;; for anonymous `lambda`, reusing the same single-clause, irrefutable
;; desugaring.  A bare-identifier parameter list keeps its old meaning.

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

;; ----- positive: lambda with constructor-pattern params -----------

(rackton
  ;; Single Pair pattern.
  (: swap (-> (Pair Integer Integer) (Pair Integer Integer)))
  (define swap (lambda ((Pair x y)) (Pair y x)))

  (: r-swap (Pair Integer Integer))
  (define r-swap (swap (Pair 1 2)))

  ;; Nested pattern.
  (: addfst (-> (Pair (Pair Integer Integer) Integer) Integer))
  (define addfst (lambda ((Pair (Pair a b) c)) (+ (+ a b) c)))

  (: r-nest Integer)
  (define r-nest (addfst (Pair (Pair 10 20) 5)))

  ;; Mixed: plain id then a pattern.
  (: scale (-> Integer (Pair Integer Integer) (Pair Integer Integer)))
  (define scale (lambda (k (Pair x y)) (Pair (* k x) (* k y))))

  (: r-scale (Pair Integer Integer))
  (define r-scale (scale 3 (Pair 2 7)))

  ;; λ spelling, used inline as a higher-order argument.
  (: r-map (List Integer))
  (define r-map
    (fmap (λ ((Pair x y)) (+ x y))
          (list (Pair 1 2) (Pair 3 4))))
  ;; built in the same surface so both sides are Rackton `List`s
  (: r-map-expected (List Integer))
  (define r-map-expected (list 3 7))

  ;; A bare-identifier lambda still behaves exactly as before.
  (: r-id Integer)
  (define r-id ((lambda (x) (+ x 1)) 41))

  (provide r-swap r-nest r-scale r-map r-map-expected r-id))

(test-case "lambda destructures a Pair parameter"
  (check-equal? r-swap (Pair 2 1)))

(test-case "lambda destructures a nested pattern"
  (check-equal? r-nest 35))

(test-case "lambda mixes plain-id and pattern params"
  (check-equal? r-scale (Pair 6 21)))

(test-case "λ pattern param works inline as a higher-order argument"
  (check-equal? r-map r-map-expected))

(test-case "bare-identifier lambda is unchanged"
  (check-equal? r-id 42))

;; ----- diagnostics: a malformed lambda names the form ------------

(define-syntax-rule (catch-rackton-error form ...)
  (with-handlers ([exn:fail? exn-message])
    (eval #'(rackton form ...)
          (variable-reference->namespace (#%variable-reference)))
    (error 'catch-rackton-error "expected an error, none raised")))

(test-case "lambda with a non-list parameter names the form, not 'unbound'"
  (define msg
    (catch-rackton-error
     (define f (lambda x x))))
  ;; The old, unhelpful message said "unbound identifier: lambda".
  (check-false (regexp-match? #rx"unbound identifier" msg))
  (check-regexp-match #rx"lambda" msg))

(test-case "lambda with a missing body names the form, not 'unbound'"
  (define msg
    (catch-rackton-error
     (define f (lambda (x)))))
  (check-false (regexp-match? #rx"unbound identifier" msg))
  (check-regexp-match #rx"lambda" msg))
