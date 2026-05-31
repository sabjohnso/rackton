#lang racket/base

;; rackton/data/tuple + rackton/data/functor — completing the PLAN:
;; curry / uncurry, and the flipped const-replace ($>).

(require rackunit
         "../main.rkt")

(rackton
  (require rackton/data/tuple
           rackton/data/functor)

  ;; curry: f over a Pair, applied to two args
  (: c-res Integer)
  (define c-res (curry (lambda (p) (match p [(MkPair a b) (+ a b)])) 3 4))

  ;; uncurry: a two-arg function applied to a Pair
  (: u-res Integer)
  (define u-res (uncurry (lambda (a b) (+ a b)) (MkPair 5 6)))

  ;; const-replace-flipped: fa $> b  replaces every element with b
  (: cf-res Integer)
  (define cf-res (match (const-replace-flipped (Some 5) 9) [(Some n) n] [(None) -1])))

;; ---------- assertions ---------------------------------------

(test-case "curry / uncurry"
  (check-equal? c-res 7)
  (check-equal? u-res 11))

(test-case "const-replace-flipped"
  (check-equal? cf-res 9))
