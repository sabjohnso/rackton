#lang racket/base

;; Rackton — type AST, free type variables, and substitution.
;;
;; Tenets:
;;   - Encapsulation: only the public API surface below escapes this module.
;;     Other modules treat types abstractly through these operations.
;;   - Algebra-driven: substitution composition is associative and applying
;;     `(subst-compose s2 s1)` is the same as piping through s1 then s2.
;;     These laws are checked in private/types-test.rkt.
;;
;; Representation
;;   types     := (tvar α)
;;             |  (tcon C)
;;             |  (tapp head (listof type))
;;   schemes   := (scheme (listof α) body)
;;   substitutions are immutable eq-keyed hash tables from α to type.

(provide (struct-out tvar)
         (struct-out tcon)
         (struct-out tapp)
         (struct-out scheme)
         (struct-out qual)
         (struct-out pred)
         type?
         type-vars
         scheme-free-vars
         pred-vars
         qual-body-type

         empty-subst
         subst?
         subst-singleton
         subst-extend
         subst-compose
         subst-domain
         subst-ref
         apply-subst
         apply-subst/scheme

         ;; smart constructors
         mqual

         ;; readability helpers
         type->datum
         scheme->datum
         pred->datum

         ;; pre-built primitive types
         t-int t-bool t-string t-symbol t-unit
         t-arrow t-list

         ;; arrows and other shape predicates
         make-arrow
         arrow?
         arrow-dom
         arrow-cod
         make-tapp)

(require racket/set
         racket/match)

;; ----- Type AST ------------------------------------------------------

(struct tvar   (name)       #:transparent)
(struct tcon   (name)       #:transparent)
(struct tapp   (head args)  #:transparent)
(struct scheme (vars body)  #:transparent)

;; A class predicate, e.g. (Eq a) or (Ord Integer).
(struct pred   (class args) #:transparent)

;; A qualified type: `(qual (pred ...) body)` reads "ρ ::= π ... => τ".
;; Smart constructor `mqual` collapses an empty context to the bare body.
(struct qual   (constraints body) #:transparent)

(define (type? v)
  (or (tvar? v) (tcon? v) (tapp? v)))

(define (mqual constraints body)
  (cond
    [(null? constraints) body]
    [else (qual constraints body)]))

(define (qual-body-type t)
  (if (qual? t) (qual-body t) t))

;; ----- Pre-built primitive constructors ------------------------------

(define t-int    (tcon 'Integer))
(define t-bool   (tcon 'Boolean))
(define t-string (tcon 'String))
(define t-symbol (tcon 'Symbol))
(define t-unit   (tcon 'Unit))
(define t-arrow  (tcon '->))
(define t-list   (tcon 'List))

(define (make-arrow dom cod)
  (tapp t-arrow (list dom cod)))

(define (arrow? t)
  (match t
    [(tapp (tcon '->) (list _ _)) #t]
    [_ #f]))

(define (arrow-dom t)
  (match t [(tapp (tcon '->) (list d _)) d]))

(define (arrow-cod t)
  (match t [(tapp (tcon '->) (list _ c)) c]))

;; (make-tapp head args) collapses (tapp head '()) into head so that
;; nullary applications stay normalized.
(define (make-tapp head args)
  (if (null? args) head (tapp head args)))

;; ----- Free type variables -------------------------------------------

(define (type-vars t)
  (match t
    [(tvar a)        (seteq a)]
    [(tcon _)        (seteq)]
    [(tapp h args)   (for/fold ([acc (type-vars h)]) ([a (in-list args)])
                       (set-union acc (type-vars a)))]
    [(pred _ args)   (for/fold ([acc (seteq)]) ([a (in-list args)])
                       (set-union acc (type-vars a)))]
    [(qual cs body)  (for/fold ([acc (type-vars body)]) ([c (in-list cs)])
                       (set-union acc (type-vars c)))]))

;; The free type variables of a single predicate, exposed for callers
;; that don't want to dispatch through `type-vars`.
(define (pred-vars p)
  (type-vars p))

(define (scheme-free-vars sch)
  (match sch
    [(scheme vs body) (set-subtract (type-vars body) (list->seteq vs))]))

;; ----- Substitutions -------------------------------------------------

;; Represented as an immutable eq-keyed hash table:  α-symbol → type.
;; We only ever extend or compose, never mutate.

(define empty-subst (hasheq))

(define (subst? v)
  (and (hash? v) (hash-eq? v)))

(define (subst-singleton var ty)
  (hasheq var ty))

(define (subst-extend s var ty)
  (hash-set s var ty))

(define (subst-domain s)
  (list->seteq (hash-keys s)))

(define (subst-ref s var [default #f])
  (hash-ref s var default))

(define (apply-subst s t)
  (cond
    [(hash-empty? s) t]
    [else
     (match t
       [(tvar a)      (hash-ref s a t)]
       [(tcon _)      t]
       [(tapp h args) (tapp (apply-subst s h)
                            (for/list ([a (in-list args)])
                              (apply-subst s a)))]
       [(pred c args) (pred c (for/list ([a (in-list args)])
                                (apply-subst s a)))]
       [(qual cs body)
        (mqual (for/list ([c (in-list cs)]) (apply-subst s c))
               (apply-subst s body))])]))

(define (apply-subst/scheme s sch)
  (match sch
    [(scheme vs body)
     ;; Bound type variables shadow s.  Drop them before recursing.
     (define s*
       (for/fold ([acc s]) ([v (in-list vs)])
         (hash-remove acc v)))
     (scheme vs (apply-subst s* body))]))

;; (subst-compose s2 s1) is the substitution θ such that
;;   apply-subst θ t  =  apply-subst s2 (apply-subst s1 t)
;; for every type t.  Standard definition:
;;   θ(α) = apply-subst s2 (s1(α))      when α ∈ dom(s1)
;;   θ(α) = s2(α)                       when α ∈ dom(s2) \ dom(s1)
(define (subst-compose s2 s1)
  (define from-s1
    (for/fold ([acc empty-subst]) ([(v t) (in-hash s1)])
      (hash-set acc v (apply-subst s2 t))))
  (for/fold ([acc from-s1]) ([(v t) (in-hash s2)])
    (if (hash-has-key? s1 v) acc (hash-set acc v t))))

;; ----- Human-readable printing ---------------------------------------

(define (type->datum t)
  (match t
    [(tvar a) a]
    [(tcon n) n]
    [(tapp (tcon '->) (list d c))
     `(-> ,(type->datum d) ,(type->datum c))]
    [(tapp h args)
     `(,(type->datum h) ,@(map type->datum args))]
    [(qual cs body)
     `(,@(map pred->datum cs) => ,(type->datum body))]))

(define (pred->datum p)
  (match p
    [(pred c args) `(,c ,@(map type->datum args))]))

(define (scheme->datum sch)
  (match sch
    [(scheme '() body) (type->datum body)]
    [(scheme vs body)  `(All ,vs ,(type->datum body))]))
