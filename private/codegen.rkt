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
         (for-template (except-in racket/base
                                  + - * < > <= >= = compose
                                  not and or length foldr filter
                                  substring string-length string-append
                                  modulo quotient abs min max
                                  number->string string->number
                                  read-line print println
                                  reverse append sort
                                  file-exists?
                                  sqrt)
                       (except-in racket/match ==)
                       "adt.rkt"
                       "dict.rkt"
                       "prelude-runtime.rkt")
         "types.rkt"
         "env.rkt"
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

    [(e:letrec bindings body stx)
     (with-syntax
      ([(binding ...)
        (for/list ([b (in-list bindings)])
          (with-syntax ([x (datum->syntax stx (car b) stx)]
                        [r (compile-expr (cdr b))])
            #'(x r)))]
       [bdy (compile-expr body)])
       (syntax/loc stx (letrec (binding ...) bdy)))]

    [(e:if c t e stx)
     (with-syntax ([cc (compile-expr c)]
                   [tt (compile-expr t)]
                   [ee (compile-expr e)])
       (syntax/loc stx (if cc tt ee)))]

    [(e:ann expr _ _)
     (compile-expr expr)]

    [(e:escape _ty _vars body _stx)
     ;; Body is an opaque Racket syntax object with the user's lexical
     ;; context; splice verbatim.
     body]

    [(e:match scrut clauses stx)
     (with-syntax
      ([sc (compile-expr scrut)]
       [(cl ...)
        (for/list ([c (in-list clauses)])
          (with-syntax ([pat (compile-pattern (clause-pattern c))]
                        [bd  (compile-expr (clause-body c))])
            #'[pat bd]))])
       (syntax/loc stx (match sc cl ...)))]))

(define (compile-top form env)
  (match form
    [(top:dec _ _ _) #f]
    [(top:alias _ _ _ _) #f]
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
       (syntax/loc stx (begin ctor-form ...)))]
    [(top:class supers head methods stx)
     (compile-class head methods stx env)]
    [(top:instance ctx head methods stx)
     (compile-instance head methods stx env)]
    [(top:require specs stx)
     (with-syntax ([(s ...) specs])
       (syntax/loc stx (require s ...)))]))

;; ----- class & instance codegen ------------------------------------

;; A class compiles to one dispatch table per *method* — different
;; methods of the same class must dispatch into their own tables, or
;; later registrations would overwrite earlier ones.  Each method's
;; dispatch-position is read out of the class-info so that runtime
;; dispatch happens on the right argument.
(define (compile-class head methods stx env)
  (define class-name (constraint-class head))
  (define cinfo (env-ref-class env class-name))
  (define method-names
    (for/list ([m (in-list methods)] #:when (method-sig? m))
      (method-sig-name m)))
  (define defs
    (for/list ([n (in-list method-names)])
      (define pos (hash-ref (class-info-dispatchpos cinfo) n 0))
      (with-syntax ([meth   (datum->syntax stx n stx)]
                    [table  (datum->syntax stx
                                           (method-dispatch-symbol n) stx)]
                    [pos-stx (datum->syntax stx pos stx)])
        #'(begin
            (define table (make-hasheq))
            (define-class-method meth table pos-stx)))))
  (with-syntax ([(def ...) defs])
    (syntax/loc stx (begin def ...))))

;; An instance compiles to a sequence of `register-instance-method!`
;; calls — one per (method × tag) pair.
(define (compile-instance head methods stx env)
  (define head-pred-class (constraint-class head))
  (define head-arg-types
    (for/list ([a (in-list (constraint-args head))])
      (ty-ast->type a)))
  (define tags (tags-for-instance-head head-arg-types env))

  (define cinfo (env-ref-class env head-pred-class))
  (unless cinfo
    (error 'compile-instance "unknown class: ~s" head-pred-class))
  (define user-impls
    (for/fold ([acc '()]) ([m (in-list methods)])
      (match m
        [(top:def name expr _) (cons (cons name expr) acc)])))
  (define all-method-bodies
    (let loop ([rest (hash-keys (class-info-methods cinfo))]
               [acc  '()])
      (cond
        [(null? rest) acc]
        [else
         (define m (car rest))
         (define body
           (cond
             [(assq m user-impls) => cdr]
             [(hash-ref (class-info-defaults cinfo) m #f)
              => (lambda (default)
                   ;; A class default was originally parsed in the
                   ;; defining module's lexical context.  Relocate its
                   ;; syntax handles to the *instance* site so identifier
                   ;; references resolve via the user module's imports.
                   (relocate-ast default stx))]
             [else
              (error 'compile-instance
                     "no impl or default for ~s in instance ~s"
                     m head-pred-class)]))
         (loop (cdr rest) (cons (cons m body) acc))])))

  (define register-forms
    (for*/list ([mb (in-list all-method-bodies)]
                [tag (in-list tags)])
      (with-syntax ([table   (datum->syntax stx
                                            (method-dispatch-symbol (car mb))
                                            stx)]
                    [impl    (compile-expr (cdr mb))]
                    [tag-sym (datum->syntax stx tag stx)])
        #'(register-instance-method! table 'tag-sym impl))))
  (with-syntax ([(register ...) register-forms])
    (syntax/loc stx (begin register ...))))

(define (method-dispatch-symbol method-name)
  (string->symbol (format "$dispatch:~a" method-name)))

;; Walk a surface AST, replacing every stx slot with `new-stx`.  Used
;; when applying a class's default method body inside an instance
;; defined in a different module: the body's identifiers must resolve
;; in the instance site's lexical scope, not the class's defining one.
(define (relocate-ast node new-stx)
  (define (R x) (relocate-ast x new-stx))
  (match node
    [(e:literal v _)     (e:literal v new-stx)]
    [(e:var n _)         (e:var n new-stx)]
    [(e:lam p body _)    (e:lam p (R body) new-stx)]
    [(e:app h args _)    (e:app (R h) (map R args) new-stx)]
    [(e:let bs body _)
     (e:let (for/list ([b (in-list bs)]) (cons (car b) (R (cdr b))))
            (R body) new-stx)]
    [(e:letrec bs body _)
     (e:letrec (for/list ([b (in-list bs)]) (cons (car b) (R (cdr b))))
               (R body) new-stx)]
    [(e:if a b c _)      (e:if (R a) (R b) (R c) new-stx)]
    [(e:ann e t _)       (e:ann (R e) (R t) new-stx)]
    [(e:escape t vs body _)
     ;; Escapes splice raw Racket syntax — relocating it would mean
     ;; rewriting that user-written code, which we don't want to do.
     (e:escape (R t) vs body new-stx)]
    [(e:match s cs _)
     (e:match (R s)
              (for/list ([c (in-list cs)])
                (clause (R (clause-pattern c)) (R (clause-body c)) new-stx))
              new-stx)]
    [(p:wild _)          (p:wild new-stx)]
    [(p:var n _)         (p:var n new-stx)]
    [(p:lit v _)         (p:lit v new-stx)]
    [(p:ctor n args _)   (p:ctor n (map R args) new-stx)]
    [(ty:var n _)        (ty:var n new-stx)]
    [(ty:con n _)        (ty:con n new-stx)]
    [(ty:app h args _)   (ty:app (R h) (map R args) new-stx)]
    [(ty:forall vs b _)  (ty:forall vs (R b) new-stx)]
    [(ty:qual cs b _)
     (ty:qual (for/list ([c (in-list cs)]) (R c)) (R b) new-stx)]
    [(constraint c args _) (constraint c (map R args) new-stx)]))

;; Convert a parsed type-AST to a core type, ignoring `All` and `qual`
;; wrappers.  Used here so we can inspect what the instance head names.
(define (ty-ast->type ast)
  (match ast
    [(ty:var n _) (tvar n)]
    [(ty:con n _) (tcon n)]
    [(ty:app h args _)
     (make-tapp (ty-ast->type h)
                (for/list ([a (in-list args)]) (ty-ast->type a)))]
    [(ty:forall _ body _) (ty-ast->type body)]
    [(ty:qual _ body _)   (ty-ast->type body)]))

;; Given a list of head-arg core types, return the dispatch tags for the
;; first one (single-parameter classes only in Phase 2).
(define (tags-for-instance-head head-arg-types env)
  (define t (car head-arg-types))
  (define head-tcon
    (match t
      [(tcon n) n]
      [(tapp (tcon n) _) n]
      [_
       (error 'tags-for-instance-head
              "instance head must be applied to a concrete type, got ~v" t)]))
  (cond
    [(eq? head-tcon 'Integer) '(Integer)]
    [(eq? head-tcon 'Boolean) '(Boolean)]
    [(eq? head-tcon 'String)  '(String)]
    [(eq? head-tcon 'Float)   '(Float)]
    [else
     (define ti (env-ref-tcon env head-tcon))
     (unless ti
       (error 'tags-for-instance-head
              "no tcon info for ~s when registering instance" head-tcon))
     (for/list ([c (in-list (tcon-info-ctors ti))])
       (string->symbol (format "$ctor:~a" c)))]))
