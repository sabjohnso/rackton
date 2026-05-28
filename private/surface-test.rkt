#lang racket/base

;; Tests for private/surface.rkt: surface parser that turns syntax objects
;; into the typed-core source AST.

(module+ test
  (require rackunit
           "surface.rkt")

  ;; ----- helpers ----------------------------------------------------

  ;; `parse-expr` keeps a syntax handle for source locations.  Tests look
  ;; only at the AST shape, so strip syntax via a recursive eraser that
  ;; replaces the `stx` slot with #f.

  (define (strip v)
    (cond
      [(e:literal? v) (e:literal (e:literal-value v) #f)]
      [(e:var?     v) (e:var (e:var-name v) #f)]
      [(e:lam?     v) (e:lam (e:lam-params v) (strip (e:lam-body v)) #f)]
      [(e:app?     v) (e:app (strip (e:app-head v))
                             (map strip (e:app-args v)) #f)]
      [(e:let?     v) (e:let (for/list ([b (in-list (e:let-bindings v))])
                               (cons (car b) (strip (cdr b))))
                             (strip (e:let-body v))
                             #f)]
      [(e:if?      v) (e:if (strip (e:if-test v))
                            (strip (e:if-then v))
                            (strip (e:if-else v)) #f)]
      [(e:ann?     v) (e:ann (strip (e:ann-expr v)) (strip (e:ann-type v)) #f)]
      [(e:match?   v) (e:match (strip (e:match-scrutinee v))
                               (for/list ([c (in-list (e:match-clauses v))])
                                 (clause (strip (clause-pattern c))
                                         (and (clause-guard c)
                                              (strip (clause-guard c)))
                                         (strip (clause-body c))
                                         #f))
                               (e:match-irrefutable? v)
                               #f)]
      [(ty:var?    v) (ty:var (ty:var-name v) #f)]
      [(ty:con?    v) (ty:con (ty:con-name v) #f)]
      [(ty:app?    v) (ty:app (strip (ty:app-head v))
                              (map strip (ty:app-args v)) #f)]
      [(ty:forall? v) (ty:forall (ty:forall-vars v) (strip (ty:forall-body v)) #f)]
      [(ty:qual?   v) (ty:qual (map strip (ty:qual-constraints v))
                               (strip (ty:qual-body v))
                               #f)]
      [(constraint? v) (constraint (constraint-class v)
                                   (map strip (constraint-args v))
                                   #f)]
      [(p:wild?    v) (p:wild #f)]
      [(p:var?     v) (p:var (p:var-name v) #f)]
      [(p:lit?     v) (p:lit (p:lit-value v) #f)]
      [(p:ctor?    v) (p:ctor (p:ctor-name v) (map strip (p:ctor-args v)) #f)]
      [(top:def?   v) (top:def (top:def-name v) (strip (top:def-expr v)) #f)]
      [(top:dec?   v) (top:dec (top:dec-name v) (strip (top:dec-type v)) #f)]
      [(top:data?  v) (top:data (top:data-name v)
                                (top:data-params v)
                                (for/list ([c (in-list (top:data-ctors v))])
                                  (data-ctor (data-ctor-name c)
                                             (map strip (data-ctor-field-types c))
                                             #f
                                             (data-ctor-extra-tvars c)
                                             (data-ctor-extra-context c)
                                             (data-ctor-result-type c)))
                                #f
                                (top:data-abstract? v))]
      [(top:class? v) (top:class (map strip (top:class-supers v))
                                 (strip (top:class-head v))
                                 (map strip (top:class-methods v))
                                 #f)]
      [(top:instance? v) (top:instance (map strip (top:instance-context v))
                                       (strip (top:instance-head v))
                                       (for/list ([m (in-list (top:instance-methods v))])
                                         (strip m))
                                       #f)]
      [(method-sig? v) (method-sig (method-sig-name v)
                                   (strip (method-sig-type v)) #f)]
      [(method-default? v) (method-default (method-default-name v)
                                           (strip (method-default-expr v)) #f)]
      [else (error 'strip "unrecognized AST node: ~e" v)]))

  (define (pe s) (strip (parse-expr (datum->syntax #f s))))
  (define (pt s) (strip (parse-type (datum->syntax #f s))))
  (define (pp s) (strip (parse-pattern (datum->syntax #f s))))
  (define (ptop s) (strip (parse-top (datum->syntax #f s))))

  ;; ----- literals and variables ------------------------------------

  (check-equal? (pe '42)        (e:literal 42 #f))
  (check-equal? (pe '#t)        (e:literal #t #f))
  (check-equal? (pe '"hello")   (e:literal "hello" #f))
  (check-equal? (pe 'x)         (e:var 'x #f))

  ;; ----- lambda ----------------------------------------------------

  (check-equal? (pe '(lambda (x) x))
                (e:lam '(x) (e:var 'x #f) #f))
  (check-equal? (pe '(λ (x y) y))
                (e:lam '(x y) (e:var 'y #f) #f))

  ;; ----- application ----------------------------------------------

  (check-equal? (pe '(f a b))
                (e:app (e:var 'f #f)
                       (list (e:var 'a #f) (e:var 'b #f)) #f))

  ;; A nullary constructor reference is just a variable.
  (check-equal? (pe 'None) (e:var 'None #f))
  ;; A constructor applied to args is application.
  (check-equal? (pe '(Some 3))
                (e:app (e:var 'Some #f) (list (e:literal 3 #f)) #f))

  ;; ----- let ------------------------------------------------------

  (check-equal? (pe '(let ([x 1] [y 2]) (+ x y)))
                (e:let (list (cons 'x (e:literal 1 #f))
                             (cons 'y (e:literal 2 #f)))
                       (e:app (e:var '+ #f)
                              (list (e:var 'x #f) (e:var 'y #f)) #f)
                       #f))

  ;; ----- if -------------------------------------------------------

  (check-equal? (pe '(if #t 1 2))
                (e:if (e:literal #t #f) (e:literal 1 #f) (e:literal 2 #f) #f))

  ;; ----- ann ------------------------------------------------------

  (check-equal? (pe '(ann x Integer))
                (e:ann (e:var 'x #f) (ty:con 'Integer #f) #f))

  ;; ----- types ----------------------------------------------------

  (check-equal? (pt 'a)          (ty:var 'a #f))
  (check-equal? (pt 'Integer)    (ty:con 'Integer #f))
  (check-equal? (pt '(-> a b))   (ty:app (ty:con '-> #f)
                                         (list (ty:var 'a #f) (ty:var 'b #f)) #f))
  ;; Note: '-> is a capitalized symbol in our scheme — it begins with
  ;; a non-lowercase character, so the parser treats it as a tycon.
  (check-equal? (pt '(Maybe a))
                (ty:app (ty:con 'Maybe #f) (list (ty:var 'a #f)) #f))
  (check-equal? (pt '(All (a) (-> a a)))
                (ty:forall '(a)
                           (ty:app (ty:con '-> #f)
                                   (list (ty:var 'a #f) (ty:var 'a #f)) #f)
                           #f))

  ;; Variadic `->` right-associates: `(-> A B C)` ≡ `(-> A (-> B C))`.
  ;; The core type AST stays binary; sugar is expanded at parse time.
  (check-equal? (pt '(-> a b c))
                (pt '(-> a (-> b c))))
  (check-equal? (pt '(-> a b c d))
                (pt '(-> a (-> b (-> c d)))))
  ;; 0-arg-fn encoding still works: `(-> T)` ≡ `(-> Unit T)`.
  (check-equal? (pt '(-> Integer))
                (ty:app (ty:con '-> #f)
                        (list (ty:con 'Unit #f) (ty:con 'Integer #f)) #f))

  ;; Variadic `->` also applies to kind syntax.
  (check-equal? (parse-kind-stx (datum->syntax #f '(-> * * *)))
                (parse-kind-stx (datum->syntax #f '(-> * (-> * *)))))
  (check-equal? (parse-kind-stx (datum->syntax #f '(-> * * * *)))
                (parse-kind-stx (datum->syntax #f '(-> * (-> * (-> * *))))))

  ;; ----- patterns -------------------------------------------------

  (check-equal? (pp '_) (p:wild #f))
  (check-equal? (pp 'x) (p:var 'x #f))
  (check-equal? (pp 'None) (p:ctor 'None '() #f))
  (check-equal? (pp '(Some x))
                (p:ctor 'Some (list (p:var 'x #f)) #f))
  (check-equal? (pp '(Some (Some y)))
                (p:ctor 'Some
                        (list (p:ctor 'Some (list (p:var 'y #f)) #f)) #f))
  (check-equal? (pp '42) (p:lit 42 #f))
  (check-equal? (pp '#t) (p:lit #t #f))

  ;; ----- match ----------------------------------------------------

  (check-equal? (pe '(match m [(None)   0]
                              [(Some x) x]))
                (e:match
                  (e:var 'm #f)
                  (list
                   (clause (p:ctor 'None '() #f) #f (e:literal 0 #f) #f)
                   (clause (p:ctor 'Some (list (p:var 'x #f)) #f) #f
                           (e:var 'x #f)
                           #f))
                  #f #f))

  ;; ----- top-level decls -----------------------------------------

  (check-equal? (ptop '(: x Integer))
                (top:dec 'x (ty:con 'Integer #f) #f))

  (check-equal? (ptop '(define x 1))
                (top:def 'x (e:literal 1 #f) #f))

  ;; (define (f x) body) is sugar for (define f (lambda (x) body))
  (check-equal? (ptop '(define (id x) x))
                (top:def 'id
                         (e:lam '(x) (e:var 'x #f) #f)
                         #f))

  (check-equal? (ptop '(data (Maybe a) None (Some a)))
                (top:data 'Maybe '(a)
                          (list (data-ctor 'None '() #f '() '() #f)
                                (data-ctor 'Some
                                           (list (ty:var 'a #f))
                                           #f '() '() #f))
                          #f
                          #f))

  (check-equal? (ptop '(data Bool True False))
                (top:data 'Bool '()
                          (list (data-ctor 'True '() #f '() '() #f)
                                (data-ctor 'False '() #f '() '() #f))
                          #f
                          #f))

  ;; ----- qualified types -------------------------------------------

  ;; Single-constraint qualified type:   (Eq a) => (-> a (-> a Boolean))
  (check-equal? (pt '((Eq a) => (-> a (-> a Boolean))))
                (ty:qual
                  (list (constraint 'Eq (list (ty:var 'a #f)) #f))
                  (ty:app (ty:con '-> #f)
                          (list (ty:var 'a #f)
                                (ty:app (ty:con '-> #f)
                                        (list (ty:var 'a #f)
                                              (ty:con 'Boolean #f)) #f)) #f)
                  #f))

  ;; Two-constraint qualified type
  (check-equal? (pt '((Eq a) (Ord a) => a))
                (ty:qual
                  (list (constraint 'Eq  (list (ty:var 'a #f)) #f)
                        (constraint 'Ord (list (ty:var 'a #f)) #f))
                  (ty:var 'a #f)
                  #f))

  ;; ----- protocol ---------------------------------------------------

  ;; Bare class with one method sig
  (check-equal? (ptop '(protocol (Eq a)
                         (: == (-> a (-> a Boolean)))))
                (top:class
                  '()
                  (constraint 'Eq (list (ty:var 'a #f)) #f)
                  (list (method-sig '==
                                    (ty:app (ty:con '-> #f)
                                            (list (ty:var 'a #f)
                                                  (ty:app (ty:con '-> #f)
                                                          (list (ty:var 'a #f)
                                                                (ty:con 'Boolean #f)) #f)) #f)
                                    #f))
                  #f))

  ;; Class with superclass
  (check-equal? (ptop '(protocol ((Eq a) => (Ord a))
                         (: < (-> a (-> a Boolean)))))
                (top:class
                  (list (constraint 'Eq (list (ty:var 'a #f)) #f))
                  (constraint 'Ord (list (ty:var 'a #f)) #f)
                  (list (method-sig '<
                                    (ty:app (ty:con '-> #f)
                                            (list (ty:var 'a #f)
                                                  (ty:app (ty:con '-> #f)
                                                          (list (ty:var 'a #f)
                                                                (ty:con 'Boolean #f)) #f)) #f)
                                    #f))
                  #f))

  ;; Class with a default-method implementation
  (check-equal? (ptop '(protocol (Foo a)
                         (: bar (-> a Integer))
                         (define (bar _x) 0)))
                (top:class
                  '()
                  (constraint 'Foo (list (ty:var 'a #f)) #f)
                  (list (method-sig 'bar
                                    (ty:app (ty:con '-> #f)
                                            (list (ty:var 'a #f)
                                                  (ty:con 'Integer #f)) #f)
                                    #f)
                        (method-default 'bar
                                        (e:lam '(_x) (e:literal 0 #f) #f) #f))
                  #f))

  ;; ----- instance --------------------------------------------

  ;; Bare instance
  (check-equal? (ptop '(instance (Eq Integer)
                         (define == =)))
                (top:instance
                  '()
                  (constraint 'Eq (list (ty:con 'Integer #f)) #f)
                  (list (top:def '== (e:var '= #f) #f))
                  #f))

  ;; Instance with context
  (check-equal? (ptop '(instance ((Eq a) => (Eq (Maybe a)))
                         (define (== x y) x)))
                (top:instance
                  (list (constraint 'Eq (list (ty:var 'a #f)) #f))
                  (constraint 'Eq (list (ty:app (ty:con 'Maybe #f)
                                                (list (ty:var 'a #f)) #f)) #f)
                  (list (top:def '==
                                 (e:lam '(x y) (e:var 'x #f) #f)
                                 #f))
                  #f)))
