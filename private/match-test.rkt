#lang racket/base

;; Tests for private/match.rkt: pattern AST → racket/match pattern syntax.

(module+ test
  (require rackunit
           "surface.rkt"
           "match.rkt")

  (define (p s) (parse-pattern (datum->syntax #f s)))

  (define (pc s) (syntax->datum (compile-pattern (p s))))

  (check-equal? (pc '_) '_)
  (check-equal? (pc 'x) 'x)
  (check-equal? (pc '42) 42)
  (check-equal? (pc '#t) #t)
  (check-equal? (pc 'None) '(None))
  (check-equal? (pc '(Some x)) '(Some x))
  (check-equal? (pc '(Pair (Some a) b)) '(Pair (Some a) b))
  (check-equal? (pc '(Some _)) '(Some _)))
