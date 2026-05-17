#lang racket/base

;; Tests for private/infer.rkt: Algorithm W with let-generalization, ADT
;; constructor / pattern typing, and skolemization of declared types.

(module+ test
  (require rackunit
           rackcheck
           racket/match
           "types.rkt"
           "env.rkt"
           "surface.rkt"
           "infer.rkt")

  ;; ----- helpers ---------------------------------------------------

  (define (infer-datum d [env initial-env])
    ;; Convenience: parse + infer an expression literal, return its
    ;; canonical type datum.
    (define ast (parse-expr (datum->syntax #f d)))
    (define-values (_s ty) (infer-expr/fresh ast env))
    (type->datum ty))

  (define (program-env forms)
    (infer-program (for/list ([f (in-list forms)])
                     (parse-top (datum->syntax #f f)))
                   initial-env))

  ;; ----- literal types --------------------------------------------

  (check-equal? (infer-datum '42)        'Integer)
  (check-equal? (infer-datum '#t)        'Boolean)
  (check-equal? (infer-datum '"hello")   'String)

  ;; ----- variable lookup ------------------------------------------

  (let ([env (env-extend-var initial-env 'x (scheme '() t-int))])
    (check-equal? (infer-datum 'x env) 'Integer))

  ;; ----- lambda + application -------------------------------------

  ;; identity has type (-> a a) for some a
  (check-match (infer-datum '(lambda (x) x))
               `(-> ,a ,b)
               (and (symbol? a) (eq? a b)))

  ;; constant function (-> a (-> b a))
  (check-match (infer-datum '(lambda (x y) x))
               `(-> ,a (-> ,b ,c))
               (and (eq? a c) (not (eq? a b))))

  ;; application instantiates
  (check-equal? (infer-datum '((lambda (x) x) 42)) 'Integer)
  (check-equal? (infer-datum '(+ 1 2)) 'Integer)
  (check-equal? (infer-datum '(< 1 2)) 'Boolean)

  ;; if both branches must agree, condition must be Boolean
  (check-equal? (infer-datum '(if #t 1 2)) 'Integer)
  (check-exn exn:fail? (lambda () (infer-datum '(if 1 1 2))))
  (check-exn exn:fail? (lambda () (infer-datum '(if #t 1 #t))))

  ;; ----- let-polymorphism (the cornerstone test) ------------------

  ;; In Hindley–Milner, `id` is generalized at the let so it can be used
  ;; at two different types in the body.  Without let-generalization,
  ;; the program below would fail to type-check.
  (check-equal?
   (infer-datum '(let ([id (lambda (x) x)])
                   (if (id #t) (id 1) (id 2))))
   'Integer)

  ;; ----- type ascription -----------------------------------------

  (check-equal? (infer-datum '(ann (lambda (x) x) (-> Integer Integer)))
                '(-> Integer Integer))

  ;; ----- ADT inference -------------------------------------------

  ;; Define Maybe then verify constructor and match typings.
  (define maybe-env
    (program-env
     '((define-data (Maybe a) None (Some a)))))

  ;; None should be polymorphic.
  (let ([info (env-ref-data maybe-env 'None)])
    (check-equal? (data-info-arity info) 0)
    (check-equal? (scheme->datum (data-info-scheme info))
                  '(All (a) (Maybe a))))

  ;; Some : ∀a. a → Maybe a
  (let ([info (env-ref-data maybe-env 'Some)])
    (check-equal? (data-info-arity info) 1)
    (check-equal? (scheme->datum (data-info-scheme info))
                  '(All (a) (-> a (Maybe a)))))

  ;; (Some 3) : Maybe Integer
  (check-equal? (infer-datum '(Some 3) maybe-env) '(Maybe Integer))

  ;; (match m [(None) 0] [(Some x) x]) where m : Maybe Integer  ⇒ Integer
  (check-equal? (infer-datum
                 '(let ([m (Some 7)])
                    (match m
                      [(None) 0]
                      [(Some x) x]))
                 maybe-env)
                'Integer)

  ;; If clauses have different result types, type-check fails.
  (check-exn exn:fail?
             (lambda ()
               (infer-datum
                '(let ([m (Some 7)])
                   (match m
                     [(None) #t]
                     [(Some x) x]))
                maybe-env)))

  ;; ----- top-level declarations ---------------------------------

  ;; A polymorphic identity declared up front and then defined.
  (define id-env
    (program-env
     '((: id (-> a a))
       (define (id x) x))))
  (check-equal? (scheme->datum (env-ref-var id-env 'id))
                '(All (a) (-> a a)))

  ;; The declaration is enforced: declaring something polymorphic but
  ;; defining it monomorphically must fail.
  (check-exn exn:fail?
             (lambda ()
               (program-env
                '((: bad (-> a a))
                  (define (bad x) 0)))))

  ;; A bad declaration that doesn't unify with the body must fail.
  (check-exn exn:fail?
             (lambda ()
               (program-env
                '((: bad (-> Integer Boolean))
                  (define (bad x) (+ x 1))))))

  ;; ----- recursive top-level definition -------------------------

  ;; Factorial: classical test that recursion works at the top level.
  (define fact-env
    (program-env
     '((define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))))))
  (check-equal? (scheme->datum (env-ref-var fact-env 'fact))
                '(-> Integer Integer))

  ;; map over Maybe: tests ADTs + polymorphism together.
  (define maybe-map-env
    (program-env
     '((define-data (Maybe a) None (Some a))
       (: map-maybe (-> (-> a b) (-> (Maybe a) (Maybe b))))
       (define (map-maybe f m)
         (match m
           [(None)   None]
           [(Some x) (Some (f x))])))))
  (check-equal?
   (scheme->datum (env-ref-var maybe-map-env 'map-maybe))
   '(All (a b) (-> (-> a b) (-> (Maybe a) (Maybe b)))))

  ;; ----- principal-type stability under α-renaming (property) --

  ;; Renaming a lambda's parameter must produce α-equivalent types.
  (define (canonicalize-type-datum d)
    ;; Replace every unique symbol that's not Integer/Boolean/String/Unit
    ;; with a positional label a0, a1, ...  Captures structural shape only.
    (define seen (make-hasheq))
    (define counter (box 0))
    (define (label sym)
      (cond
        [(memq sym '(Integer Boolean String Unit -> List Maybe All)) sym]
        [else
         (or (hash-ref seen sym #f)
             (let* ([n (unbox counter)]
                    [lbl (string->symbol (format "$~a" n))])
               (set-box! counter (add1 n))
               (hash-set! seen sym lbl)
               lbl))]))
    (let loop ([d d])
      (cond
        [(symbol? d) (label d)]
        [(pair? d)   (cons (loop (car d)) (loop (cdr d)))]
        [else d])))

  (check-property
   (property identity-stable-under-renaming
             ([x (gen:choice (gen:const 'x) (gen:const 'y) (gen:const 'foo))])
     (equal?
      (canonicalize-type-datum (infer-datum `(lambda (,x) ,x)))
      (canonicalize-type-datum (infer-datum '(lambda (x) x)))))))
