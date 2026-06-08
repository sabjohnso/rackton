#lang racket/base

;; Tests for private/codegen.rkt: lowering of the typed-core AST to
;; Racket syntax, asserted at the syntax level via `syntax->datum`.
;; Type information is erased by this stage, so the lowering of a basic
;; expression needs no inference state — the method-resolution
;; parameters default to #f and the relevant branches are guarded.

(module+ test
  (require rackunit
           "codegen.rkt"
           "surface.rkt"
           "env.rkt")   ; initial-env for compile-top

  (define h #'codegen-test)
  (define (lower e) (syntax->datum (compile-expr e)))

  (test-case "literals lower to themselves"
    (check-equal? (lower (e:literal 5 h))     5)
    (check-equal? (lower (e:literal "hi" h))  "hi")
    (check-equal? (lower (e:literal #t h))    #t))

  (test-case "a variable lowers to its identifier"
    (check-equal? (lower (e:var 'x h)) 'x))

  (test-case "a single-argument lambda lowers to a surplus-absorbing case-lambda"
    ;; Over-application stays curried even for one parameter, so the
    ;; lowering is a case-lambda: an exact clause plus a rest clause
    ;; that applies the body's result to any extra arguments.
    (check-equal? (lower (e:lam '(x) (e:var 'x h) h))
                  '(case-lambda ((x) x) ((x . more) (apply (let () x) more)))))

  (test-case "a multi-argument lambda lowers to a curried case-lambda"
    (define d (lower (e:lam '(x y) (e:var 'x h) h)))
    (check-true (and (pair? d) (eq? (car d) 'case-lambda))))

  (test-case "application lowers to a direct call"
    (check-equal? (lower (e:app (e:var 'f h)
                                (list (e:literal 1 h) (e:literal 2 h)) h))
                  '(f 1 2)))

  (test-case "if lowers to if"
    (check-equal? (lower (e:if (e:literal #t h) (e:literal 1 h) (e:literal 2 h) h))
                  '(if #t 1 2)))

  (test-case "let lowers to let"
    (check-equal? (lower (e:let (list (cons 'x (e:literal 1 h))) (e:var 'x h) h))
                  '(let ((x 1)) x)))

  (test-case "a type ascription is erased, leaving the inner expression"
    (check-equal? (lower (e:ann (e:literal 7 h) #f h)) 7))

  (test-case "match lowers to match"
    (check-equal? (lower (e:match (e:literal 1 h)
                                  (list (clause (p:wild h) #f (e:literal 9 h) h))
                                  #f h))
                  '(match 1 (_ 9))))

  ;; ----- compile-top -------------------------------------------------

  (test-case "a definition lowers to (define name body)"
    (check-equal? (syntax->datum
                   (compile-top (top:def 'foo (e:literal 42 h) h) initial-env))
                  '(define foo 42)))

  (test-case "a data type lowers to per-constructor define-data-ctor"
    (define d (syntax->datum
               (compile-top
                (top:data 'Color '()
                          (list (data-ctor 'Red '() h '() '() #f))
                          h #f #f)
                initial-env)))
    (check-equal? d '(begin (define-data-ctor Red 0)))))
