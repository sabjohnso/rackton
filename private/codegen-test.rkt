#lang racket/base

;; Tests for private/codegen.rkt: lowering of the typed-core AST to
;; Racket syntax, asserted at the syntax level via `syntax->datum`.
;; Type information is erased by this stage, so the lowering of a basic
;; expression needs no inference state — the method-resolution
;; parameters default to #f and the relevant branches are guarded.

(module+ test
  (require rackunit
           racket/match
           "codegen.rkt"
           "surface.rkt"
           "env.rkt")   ; initial-env for compile-top

  (define h #'codegen-test)
  (define (lower e)
    (let-values ([(s _) (compile-expr e empty-cg-ctx (make-cg-st))])
      (syntax->datum s)))

  (test-case "literals lower to themselves"
    (check-equal? (lower (e:literal 5 h))     5)
    (check-equal? (lower (e:literal "hi" h))  "hi")
    (check-equal? (lower (e:literal #t h))    #t))

  (test-case "a variable lowers to its identifier"
    (check-equal? (lower (e:var 'x h)) 'x))

  (test-case "a single-argument lambda lowers to a self-curried letrec, body emitted once"
    ;; Over-application stays curried even for one parameter, but the body
    ;; must appear EXACTLY ONCE: the exact-arity clause holds it, and the
    ;; surplus clause re-enters that clause through the letrec self-name to
    ;; absorb extra arguments.  (Previously the body was duplicated into
    ;; both clauses, which made nested lambdas blow up 2^depth — the
    ;; do/let& compile-time cliff.)  The self-name is gensym'd, so match it.
    (check-true
     (match (lower (e:lam '(x) (e:var 'x h) h))
       [`(letrec ([,f (case-lambda
                        [(x) x]
                        [(x . more) (apply (,f-over x) more)])])
           ,f-ret)
        (and (eq? f f-over) (eq? f f-ret))]
       [_ #f])))

  (test-case "a multi-argument lambda lowers to a self-curried letrec"
    (define d (lower (e:lam '(x y) (e:var 'x h) h)))
    (check-true (and (pair? d) (eq? (car d) 'letrec))))

  (test-case "nested single-param lambdas keep the body linear (emitted once)"
    ;; Regression for the 2^depth program-size blowup: each e:lam used to
    ;; copy its body into both case-lambda clauses, so N nested lambdas
    ;; duplicated the innermost body 2^N times.  It must now appear once
    ;; regardless of nesting depth.
    (define (count-occurrences needle datum)
      (let loop ([d datum])
        (cond
          [(equal? d needle) 1]
          [(pair? d) (+ (loop (car d)) (loop (cdr d)))]
          [else 0])))
    (define (nest depth)
      (let loop ([n depth])
        (if (zero? n)
            (e:literal "MARK" h)
            (e:lam (list (string->symbol (format "x~a" n))) (loop (sub1 n)) h))))
    (for ([depth (in-list '(1 5 10))])
      (check-equal? (count-occurrences "MARK" (lower (nest depth))) 1
                    (format "body duplicated at nesting depth ~a" depth))))

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

  ;; compile-top returns (values syntax cg-st); take the syntax.
  (define (lower-top form) (let-values ([(s _) (compile-top form initial-env)])
                             (syntax->datum s)))

  (test-case "a definition lowers to (define name body)"
    (check-equal? (lower-top (top:def 'foo (e:literal 42 h) h))
                  '(define foo 42)))

  (test-case "a data type lowers to per-constructor define-data-ctor"
    (define d (lower-top
               (top:data 'Color '()
                         (list (data-ctor 'Red '() h '() '() #f))
                         h #f #f)))
    (check-equal? d '(begin (define-data-ctor Red 0)))))
