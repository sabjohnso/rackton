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
         (struct-out clause)

         (struct-out ty:var)
         (struct-out ty:con)
         (struct-out ty:app)
         (struct-out ty:forall)

         (struct-out p:wild)
         (struct-out p:var)
         (struct-out p:lit)
         (struct-out p:ctor)

         (struct-out top:def)
         (struct-out top:dec)
         (struct-out top:data)
         (struct-out data-ctor)

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
(struct clause    (pattern body stx) #:transparent)

(struct ty:var    (name stx) #:transparent)
(struct ty:con    (name stx) #:transparent)
(struct ty:app    (head args stx) #:transparent)
(struct ty:forall (vars body stx) #:transparent)

(struct p:wild    (stx) #:transparent)
(struct p:var     (name stx) #:transparent)
(struct p:lit     (value stx) #:transparent)
(struct p:ctor    (name args stx) #:transparent)

(struct top:def   (name expr stx) #:transparent)
(struct top:dec   (name type stx) #:transparent)
(struct top:data  (name params ctors stx) #:transparent)
(struct data-ctor (name field-types stx) #:transparent)

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
    #:datum-literals (lambda λ let if ann match)
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

    [x:id  (e:var (syntax->datum #'x) stx)]

    [(head arg ...+)
     (e:app (parse-expr #'head)
            (for/list ([a (in-list (syntax->list #'(arg ...)))])
              (parse-expr a))
            stx)]))

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
    #:datum-literals (All)
    [(All (v:id ...) body)
     (ty:forall (map syntax->datum (syntax->list #'(v ...)))
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

;; ----- top-level forms ----------------------------------------------

(define (parse-top stx)
  (syntax-parse stx
    #:datum-literals (define define-data :)
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

    [(define-data (tname:id tparam:id ...) ctor ...)
     #:fail-unless (not (lowercase-id? (syntax->datum #'tname)))
     "data type name must be a non-lowercase identifier"
     (top:data (syntax->datum #'tname)
               (map syntax->datum (syntax->list #'(tparam ...)))
               (for/list ([c (in-list (syntax->list #'(ctor ...)))])
                 (parse-data-ctor c))
               stx)]
    [(define-data tname:id ctor ...)
     #:fail-unless (not (lowercase-id? (syntax->datum #'tname)))
     "data type name must be a non-lowercase identifier"
     (top:data (syntax->datum #'tname) '()
               (for/list ([c (in-list (syntax->list #'(ctor ...)))])
                 (parse-data-ctor c))
               stx)]))

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
  (for/list ([f (in-list forms)]) (parse-top f)))
