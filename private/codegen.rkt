#lang racket/base

;; Rackton — code generation.
;;
;; Translates surface AST (after type-checking) into syntax objects
;; that Racket can evaluate.  Type information has done its job by
;; this point and is erased; no runtime tags remain.
;;
;; Surface form  → Racket form
;;   e:literal v       → v
;;   e:var x           → x
;;   e:lam (p ...) b   → (lambda (p ...) b)
;;   e:app h (a ...)   → (h a ...)
;;   e:let ((x e) ...) → (let ((x e) ...) body)
;;   e:if c t e        → (if c t e)
;;   e:ann e _         → e  (ascription erased)
;;   e:match s ((p b) ...) → (match s (p b) ...)
;;
;;   top:def name e         → (define name e)
;;   top:dec _ _            → #f  (no runtime form)
;;   top:data _ _ (ctors)   → (begin (define-data-ctor C arity) ...)

(provide compile-expr
         compile-top)

(require racket/match
         (for-template racket/base
                       racket/match
                       "adt.rkt")
         "surface.rkt"
         "match.rkt")

(define (compile-expr e)
  (match e
    [(e:literal v stx)   (datum->syntax stx v stx)]
    [(e:var name stx)    (datum->syntax stx name stx)]

    [(e:lam params body stx)
     (define param-stxs
       (for/list ([n (in-list params)])
         (datum->syntax stx n stx)))
     (with-syntax ([(p ...) param-stxs]
                   [bdy (compile-expr body)])
       (syntax/loc stx (lambda (p ...) bdy)))]

    [(e:app head args stx)
     (with-syntax ([h (compile-expr head)]
                   [(a ...) (for/list ([x (in-list args)]) (compile-expr x))])
       (syntax/loc stx (h a ...)))]

    [(e:let bindings body stx)
     (with-syntax
      ([(binding ...)
        (for/list ([b (in-list bindings)])
          (with-syntax ([x (datum->syntax stx (car b) stx)]
                        [r (compile-expr (cdr b))])
            #'(x r)))]
       [bdy (compile-expr body)])
       (syntax/loc stx (let (binding ...) bdy)))]

    [(e:if c t e stx)
     (with-syntax ([cc (compile-expr c)]
                   [tt (compile-expr t)]
                   [ee (compile-expr e)])
       (syntax/loc stx (if cc tt ee)))]

    [(e:ann expr _ _)
     (compile-expr expr)]

    [(e:match scrut clauses stx)
     (with-syntax
      ([sc (compile-expr scrut)]
       [(cl ...)
        (for/list ([c (in-list clauses)])
          (with-syntax ([pat (compile-pattern (clause-pattern c))]
                        [bd  (compile-expr (clause-body c))])
            #'[pat bd]))])
       (syntax/loc stx (match sc cl ...)))]))

(define (compile-top form)
  (match form
    [(top:dec _ _ _) #f]
    [(top:def name expr stx)
     (with-syntax ([n (datum->syntax stx name stx)]
                   [e (compile-expr expr)])
       (syntax/loc stx (define n e)))]
    [(top:data tname tparams ctors stx)
     (with-syntax
      ([(ctor-form ...)
        (for/list ([c (in-list ctors)])
          (with-syntax ([nm  (datum->syntax stx (data-ctor-name c) stx)]
                        [arr (length (data-ctor-field-types c))])
            #'(define-data-ctor nm arr)))])
       (syntax/loc stx (begin ctor-form ...)))]))
