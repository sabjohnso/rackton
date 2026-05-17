#lang racket/base

;; Rackton — surface parser.
;;
;; Translates syntax objects from the surface language into a typed-core
;; source AST.  Used by both the `(rackton ...)` macro and the
;; `#lang rackton` reader; both feed the same elaboration pipeline.
;;
;; Lexical convention
;;   - Identifiers whose first character is a lowercase letter are
;;     "lowercase" — they denote type variables in type positions and
;;     pattern variables in pattern positions.
;;   - Every other identifier (uppercase letters, symbols like ->,
;;     punctuation) is a "non-lowercase" identifier and denotes a type
;;     constructor or value constructor depending on position.
;;
;; The AST exported below carries the originating syntax object in a
;; trailing `stx` slot so that downstream stages can produce sourcemap-
;; aware errors.

(provide (struct-out e:literal)
         (struct-out e:var)
         (struct-out e:lam)
         (struct-out e:app)
         (struct-out e:let)
         (struct-out e:if)
         (struct-out e:ann)
         (struct-out e:match)
         (struct-out e:escape)
         (struct-out clause)

         (struct-out ty:var)
         (struct-out ty:con)
         (struct-out ty:app)
         (struct-out ty:forall)
         (struct-out ty:qual)
         (struct-out constraint)

         (struct-out p:wild)
         (struct-out p:var)
         (struct-out p:lit)
         (struct-out p:ctor)

         (struct-out top:def)
         (struct-out top:dec)
         (struct-out top:data)
         (struct-out data-ctor)
         (struct-out top:class)
         (struct-out top:instance)
         (struct-out method-sig)
         (struct-out method-default)
         (struct-out top:require)
         (struct-out k:star)
         (struct-out k:arr)
         parse-kind-stx

         parse-expr
         parse-type
         parse-pattern
         parse-top
         parse-toplevel-list

         lowercase-id?)

(require syntax/parse)

;; ----- AST -----------------------------------------------------------

(struct e:literal (value stx) #:transparent)
(struct e:var     (name stx) #:transparent)
(struct e:lam     (params body stx) #:transparent)
(struct e:app     (head args stx) #:transparent)
(struct e:let     (bindings body stx) #:transparent)
(struct e:if      (test then else stx) #:transparent)
(struct e:ann     (expr type stx) #:transparent)
(struct e:match   (scrutinee clauses stx) #:transparent)
;; A host-language escape: (racket τ (var ...) body) drops into raw
;; Racket, returning a value typed as τ.  `vars` lists the Rackton
;; bindings that must be in scope.  `body` is a single Racket syntax
;; object that is spliced verbatim at codegen time.
(struct e:escape  (type vars body stx) #:transparent)
(struct clause    (pattern body stx) #:transparent)

(struct ty:var    (name stx) #:transparent)
(struct ty:con    (name stx) #:transparent)
(struct ty:app    (head args stx) #:transparent)
(struct ty:forall (vars body stx) #:transparent)
(struct ty:qual   (constraints body stx) #:transparent)
;; A constraint is `(C arg ...)` in surface syntax — class name + type args.
(struct constraint (class args stx) #:transparent)

(struct p:wild    (stx) #:transparent)
(struct p:var     (name stx) #:transparent)
(struct p:lit     (value stx) #:transparent)
(struct p:ctor    (name args stx) #:transparent)

(struct top:def      (name expr stx) #:transparent)
(struct top:dec      (name type stx) #:transparent)
(struct top:data     (name params ctors stx) #:transparent)
(struct data-ctor    (name field-types stx) #:transparent)
;; A class declaration carries an explicit list of parameters with kinds
;; (defaulting to *), the optional superclass list, the head class name,
;; and the body (signatures + defaults).
(struct top:class    (supers head methods stx) #:transparent)
(struct top:instance (context head methods stx) #:transparent)
;; Items inside a `define-class` body:
(struct method-sig     (name type stx) #:transparent)
(struct method-default (name expr stx) #:transparent)
;; A multi-file import: `(require "file.rkt" ...)` inside a rackton form.
;; Specs are the raw require specs (passed verbatim to Racket's require).
(struct top:require    (specs stx) #:transparent)

;; Kinds at the surface level — used to annotate class parameters.
(struct k:star ()        #:transparent)
(struct k:arr  (dom cod) #:transparent)

;; ----- deriving: instance synthesis --------------------------------

;; Build a head type expression `(tname a b …)` (or just `tname` if
;; un-parameterised) for use inside a synthesised instance head.
(define (data-head-type-ast tname tparams stx)
  (cond
    [(null? tparams) (ty:con tname stx)]
    [else (ty:app (ty:con tname stx)
                  (for/list ([p (in-list tparams)]) (ty:var p stx))
                  stx)]))

(define (a-name i) (string->symbol (format "a~a" i)))
(define (b-name i) (string->symbol (format "b~a" i)))

(define (ctor-x-pattern name arity stx)
  (p:ctor name
          (for/list ([i (in-range arity)]) (p:var (a-name i) stx))
          stx))

(define (ctor-y-pattern name arity stx)
  (p:ctor name
          (for/list ([i (in-range arity)]) (p:var (b-name i) stx))
          stx))

;; `(== a0 b0)` then `(== a1 b1)` … chained as nested ifs.
(define (chained-eq arity stx)
  (cond
    [(zero? arity) (e:literal #t stx)]
    [else
     (foldr
      (lambda (i acc)
        (e:if (e:app (e:var '== stx)
                     (list (e:var (a-name i) stx) (e:var (b-name i) stx))
                     stx)
              acc
              (e:literal #f stx)
              stx))
      (e:literal #t stx)
      (build-list arity values))]))

(define (synthesize-eq-instance tname tparams ctors stx)
  (define head-ty (data-head-type-ast tname tparams stx))
  (define ctx (for/list ([p (in-list tparams)])
                (constraint 'Eq (list (ty:var p stx)) stx)))
  (define head (constraint 'Eq (list head-ty) stx))
  ;; Outer match on x; for each ctor, inner match on y.
  (define eq-body
    (e:match
     (e:var 'x stx)
     (for/list ([c (in-list ctors)])
       (define name (data-ctor-name c))
       (define arity (length (data-ctor-field-types c)))
       (clause (ctor-x-pattern name arity stx)
               (e:match (e:var 'y stx)
                        (list (clause (ctor-y-pattern name arity stx)
                                      (chained-eq arity stx)
                                      stx)
                              (clause (p:wild stx)
                                      (e:literal #f stx)
                                      stx))
                        stx)
               stx))
     stx))
  (top:instance ctx head
                (list (top:def '== (e:lam '(x y) eq-body stx) stx))
                stx))

(define (synthesize-show-instance tname tparams ctors stx)
  (define head-ty (data-head-type-ast tname tparams stx))
  (define ctx (for/list ([p (in-list tparams)])
                (constraint 'Show (list (ty:var p stx)) stx)))
  (define head (constraint 'Show (list head-ty) stx))
  (define show-body
    (e:match
     (e:var 'x stx)
     (for/list ([c (in-list ctors)])
       (define name (data-ctor-name c))
       (define arity (length (data-ctor-field-types c)))
       (clause (ctor-x-pattern name arity stx)
               (cond
                 [(zero? arity)
                  (e:literal (symbol->string name) stx)]
                 [else
                  ;; Splice raw Racket: (string-append "(<Ctor>" " " (show a0) … ")")
                  (define arg-shows
                    (apply append
                           (for/list ([i (in-range arity)])
                             (list " " `(show ,(a-name i))))))
                  (define body-datum
                    `(string-append ,(format "(~a" name)
                                    ,@arg-shows
                                    ")"))
                  (e:escape (ty:con 'String stx)
                            (for/list ([i (in-range arity)]) (a-name i))
                            (datum->syntax stx body-datum stx)
                            stx)])
               stx))
     stx))
  (top:instance ctx head
                (list (top:def 'show (e:lam '(x) show-body stx) stx))
                stx))

;; ----- lexical classification ---------------------------------------

(define (lowercase-id? sym)
  (and (symbol? sym)
       (let ([s (symbol->string sym)])
         (and (positive? (string-length s))
              (let ([c (string-ref s 0)])
                (and (char-alphabetic? c)
                     (char-lower-case? c)))))))

(define (wildcard-symbol? sym) (eq? sym '_))

;; ----- expressions --------------------------------------------------

(define (parse-expr stx)
  (syntax-parse stx
    #:datum-literals (lambda λ let if ann match racket do <-)
    [n:number  (e:literal (syntax->datum #'n) stx)]
    [b:boolean (e:literal (syntax->datum #'b) stx)]
    [s:string  (e:literal (syntax->datum #'s) stx)]

    [(lambda (p:id ...) body)
     (e:lam (map syntax->datum (syntax->list #'(p ...)))
            (parse-expr #'body)
            stx)]
    [(λ (p:id ...) body)
     (e:lam (map syntax->datum (syntax->list #'(p ...)))
            (parse-expr #'body)
            stx)]

    [(let ([x:id rhs] ...) body)
     (e:let (for/list ([id (in-list (syntax->list #'(x ...)))]
                       [r  (in-list (syntax->list #'(rhs ...)))])
              (cons (syntax->datum id) (parse-expr r)))
            (parse-expr #'body)
            stx)]

    [(if c t e)
     (e:if (parse-expr #'c) (parse-expr #'t) (parse-expr #'e) stx)]

    [(ann e t)
     (e:ann (parse-expr #'e) (parse-type #'t) stx)]

    [(match scrut [pat body] ...+)
     (e:match (parse-expr #'scrut)
              (for/list ([p-stx (in-list (syntax->list #'(pat ...)))]
                         [b-stx (in-list (syntax->list #'(body ...)))])
                (clause (parse-pattern p-stx)
                        (parse-expr b-stx)
                        p-stx))
              stx)]

    ;; (racket τ (var ...) body) — host-language escape
    [(racket τ (v:id ...) body)
     (e:escape (parse-type #'τ)
               (map syntax->datum (syntax->list #'(v ...)))
               #'body
               stx)]

    ;; (do [x <- m1] [y <- m2] ... body)  desugars to nested >>= calls.
    ;; A statement is `[var <- expr]`; each binds the un-wrapped value
    ;; for the rest of the chain.  The trailing `body` is the final
    ;; computation.
    [(do stmt ...+ body)
     (parse-do (syntax->list #'(stmt ...)) #'body stx)]

    [x:id  (e:var (syntax->datum #'x) stx)]

    [(head arg ...+)
     (e:app (parse-expr #'head)
            (for/list ([a (in-list (syntax->list #'(arg ...)))])
              (parse-expr a))
            stx)]))

(define (parse-do stmts body-stx stx)
  (cond
    [(null? stmts) (parse-expr body-stx)]
    [else
     (define s (car stmts))
     (syntax-parse s
       #:datum-literals (<-)
       [[v:id <- expr]
        (e:app (e:var '>>= stx)
               (list (parse-expr #'expr)
                     (e:lam (list (syntax->datum #'v))
                            (parse-do (cdr stmts) body-stx stx)
                            stx))
               stx)]
       [_
        (raise-syntax-error 'parse-do
          "expected a binding `[var <- expr]` inside (do …)"
          s)])]))

;; ----- patterns -----------------------------------------------------

(define (parse-pattern stx)
  (syntax-parse stx
    [n:number  (p:lit (syntax->datum #'n) stx)]
    [b:boolean (p:lit (syntax->datum #'b) stx)]
    [s:string  (p:lit (syntax->datum #'s) stx)]
    [x:id
     (define name (syntax->datum #'x))
     (cond
       [(wildcard-symbol? name) (p:wild stx)]
       [(lowercase-id? name)    (p:var name stx)]
       [else                    (p:ctor name '() stx)])]
    [(ctor:id arg ...)
     #:fail-unless (not (lowercase-id? (syntax->datum #'ctor)))
     "constructor pattern head must be a non-lowercase identifier"
     (p:ctor (syntax->datum #'ctor)
             (for/list ([a (in-list (syntax->list #'(arg ...)))])
               (parse-pattern a))
             stx)]))

;; ----- types --------------------------------------------------------

(define (parse-type stx)
  (syntax-parse stx
    #:datum-literals (All =>)
    [(All (v:id ...) body)
     (ty:forall (map syntax->datum (syntax->list #'(v ...)))
                (parse-type #'body)
                stx)]
    ;; Qualified type: (constraint ...+ => body)
    [(c ...+ => body)
     (ty:qual (for/list ([cstx (in-list (syntax->list #'(c ...)))])
                (parse-constraint cstx))
              (parse-type #'body)
              stx)]
    [x:id
     (define name (syntax->datum #'x))
     (if (lowercase-id? name)
         (ty:var name stx)
         (ty:con name stx))]
    [(head arg ...+)
     (ty:app (parse-type #'head)
             (for/list ([a (in-list (syntax->list #'(arg ...)))])
               (parse-type a))
             stx)]))

;; Parse a constraint expression like `(Eq a)` or `(Foo (Maybe a))`.
;; The head must be a non-lowercase identifier (a class name).  The
;; constraint args may be plain types OR — when this constraint appears
;; as a class head — kind-annotated type vars `(var :: kind)`, in which
;; case the kind annotation is stripped and the resulting type-var
;; remembers its kind via the syntax property 'rackton:kind on its stx.
(define (parse-constraint stx)
  (syntax-parse stx
    [(name:id arg ...+)
     #:fail-unless (not (lowercase-id? (syntax->datum #'name)))
     "class name in a constraint must be a non-lowercase identifier"
     (constraint (syntax->datum #'name)
                 (for/list ([a (in-list (syntax->list #'(arg ...)))])
                   (parse-constraint-arg a))
                 stx)]))

;; Constraint args may be either plain types or kind-annotated type
;; variables `(var :: kind)`.  We parse the annotated form as a plain
;; ty:var whose stx carries the kind as a property; the caller (the
;; class-form handler) reads it back when computing param kinds.
(define (parse-constraint-arg stx)
  (syntax-parse stx
    #:datum-literals (::)
    [(v:id :: k)
     #:fail-unless (lowercase-id? (syntax->datum #'v))
     "kind-annotated class parameter must be a lowercase identifier"
     (define kind (parse-kind-stx #'k))
     (define annotated (syntax-property #'v 'rackton:kind kind))
     (ty:var (syntax->datum #'v) annotated)]
    [_ (parse-type stx)]))

;; Parse a kind expression: `*` or `(-> k1 k2)`.
(define (parse-kind-stx stx)
  (syntax-parse stx
    #:datum-literals (* ->)
    [* (k:star)]
    [(-> k1 k2) (k:arr (parse-kind-stx #'k1) (parse-kind-stx #'k2))]))

;; ----- top-level forms ----------------------------------------------

(define (parse-top stx)
  (syntax-parse stx
    #:datum-literals (define define-data define-class define-instance require : =>)
    [(require spec ...)
     (top:require (syntax->list #'(spec ...)) stx)]
    [(: name:id ty)
     (top:dec (syntax->datum #'name) (parse-type #'ty) stx)]

    [(define (f:id arg:id ...) body)
     (top:def (syntax->datum #'f)
              (e:lam (map syntax->datum (syntax->list #'(arg ...)))
                     (parse-expr #'body)
                     stx)
              stx)]
    [(define x:id e)
     (top:def (syntax->datum #'x) (parse-expr #'e) stx)]

    [(define-data (tname:id tparam:id ...) item ...)
     #:fail-unless (not (lowercase-id? (syntax->datum #'tname)))
     "data type name must be a non-lowercase identifier"
     (parse-data-form (syntax->datum #'tname)
                      (map syntax->datum (syntax->list #'(tparam ...)))
                      (syntax->list #'(item ...))
                      stx
                      #'tname)]
    [(define-data tname:id item ...)
     #:fail-unless (not (lowercase-id? (syntax->datum #'tname)))
     "data type name must be a non-lowercase identifier"
     (parse-data-form (syntax->datum #'tname)
                      '()
                      (syntax->list #'(item ...))
                      stx
                      #'tname)]

    ;; A class with a superclass list: ((C1 a) ... => (D a))
    [(define-class (sup ...+ => head) body ...)
     (top:class (for/list ([s (in-list (syntax->list #'(sup ...)))])
                  (parse-constraint s))
                (parse-constraint #'head)
                (for/list ([m (in-list (syntax->list #'(body ...)))])
                  (parse-class-method m))
                stx)]
    ;; A class with no superclasses: (D a)
    [(define-class head body ...)
     (top:class '()
                (parse-constraint #'head)
                (for/list ([m (in-list (syntax->list #'(body ...)))])
                  (parse-class-method m))
                stx)]

    ;; Instance with context: ((Eq a) ... => (Eq (Maybe a)))
    [(define-instance (ctx ...+ => head) body ...)
     (top:instance (for/list ([c (in-list (syntax->list #'(ctx ...)))])
                     (parse-constraint c))
                   (parse-constraint #'head)
                   (for/list ([m (in-list (syntax->list #'(body ...)))])
                     (parse-instance-method m))
                   stx)]
    [(define-instance head body ...)
     (top:instance '()
                   (parse-constraint #'head)
                   (for/list ([m (in-list (syntax->list #'(body ...)))])
                     (parse-instance-method m))
                   stx)]))

;; A method form inside `define-class`: either a `(: name type)` signature
;; or a `(define ...)` providing a default implementation.
(define (parse-class-method stx)
  (syntax-parse stx
    #:datum-literals (: define)
    [(: name:id ty)
     (method-sig (syntax->datum #'name) (parse-type #'ty) stx)]
    [(define (f:id arg:id ...) body)
     (method-default (syntax->datum #'f)
                     (e:lam (map syntax->datum (syntax->list #'(arg ...)))
                            (parse-expr #'body)
                            stx)
                     stx)]
    [(define x:id e)
     (method-default (syntax->datum #'x) (parse-expr #'e) stx)]))

;; An instance method must be a `define`.
(define (parse-instance-method stx)
  (syntax-parse stx
    #:datum-literals (define)
    [(define (f:id arg:id ...) body)
     (top:def (syntax->datum #'f)
              (e:lam (map syntax->datum (syntax->list #'(arg ...)))
                     (parse-expr #'body) stx)
              stx)]
    [(define x:id e)
     (top:def (syntax->datum #'x) (parse-expr #'e) stx)]))

;; Split a define-data body into constructor specs and an optional
;; `#:deriving Cls ...` tail.  Returns a list of top-level forms:
;; the top:data and any synthesized top:instance entries.
;;
;; For the synthesized instances we use the syntax handle of the *first
;; constructor spec* as the lexical-context anchor — that handle is an
;; actual identifier from the user's source and so carries the same
;; scope set as anything else the user wrote.  Using the whole-form's
;; syntax instead leaves the synthesised identifiers missing scopes
;; that show up only on individual identifier-leaf syntax objects.
(define (parse-data-form tname tparams items stx [tname-stx #f])
  (define-values (ctor-stxs deriving-classes)
    (let loop ([rem items] [acc '()])
      (cond
        [(null? rem) (values (reverse acc) '())]
        [(eq? (syntax->datum (car rem)) '#:deriving)
         (values (reverse acc)
                 (for/list ([c (in-list (cdr rem))])
                   (syntax->datum c)))]
        [else (loop (cdr rem) (cons (car rem) acc))])))
  (define ctors
    (for/list ([c (in-list ctor-stxs)]) (parse-data-ctor c)))
  (define data-form (top:data tname tparams ctors stx))
  (cond
    [(null? deriving-classes) data-form]
    [else
     (define ctx-stx
       ;; Anchor synthesised identifiers to a *leaf* identifier from the
       ;; user's source — the type name carries the user's full scope
       ;; set including module-level imports, which the whole-form syntax
       ;; sometimes does not.
       (cond
         [tname-stx tname-stx]
         [(pair? ctor-stxs) (car ctor-stxs)]
         [else stx]))
     (define derived
       (apply append
              (for/list ([cls (in-list deriving-classes)])
                (case cls
                  [(Eq)   (list (synthesize-eq-instance   tname tparams ctors ctx-stx))]
                  [(Show) (list (synthesize-show-instance tname tparams ctors ctx-stx))]
                  [else
                   (raise-syntax-error 'define-data
                     (format "cannot derive ~s — supported: Eq, Show" cls)
                     stx)]))))
     (cons data-form derived)]))

(define (parse-data-ctor stx)
  (syntax-parse stx
    [name:id
     #:fail-unless (not (lowercase-id? (syntax->datum #'name)))
     "data constructor name must be a non-lowercase identifier"
     (data-ctor (syntax->datum #'name) '() stx)]
    [(name:id ft ...+)
     #:fail-unless (not (lowercase-id? (syntax->datum #'name)))
     "data constructor name must be a non-lowercase identifier"
     (data-ctor (syntax->datum #'name)
                (for/list ([t (in-list (syntax->list #'(ft ...)))])
                  (parse-type t))
                stx)]))

(define (parse-toplevel-list stx-or-list)
  (define forms
    (cond
      [(syntax? stx-or-list) (syntax->list stx-or-list)]
      [(list? stx-or-list)   stx-or-list]
      [else (raise-argument-error 'parse-toplevel-list
                                  "syntax or list" stx-or-list)]))
  ;; A single surface form may parse to multiple AST entries (e.g.
  ;; `(define-data … #:deriving Eq Show)` desugars to the data plus
  ;; the two synthesized instances).  Flatten if so.
  (apply append
         (for/list ([f (in-list forms)])
           (define result (parse-top f))
           (if (list? result) result (list result)))))
