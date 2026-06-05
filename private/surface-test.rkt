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
      [(e:match*?  v) (e:match* (map strip (e:match*-scrutinees v))
                                (for/list ([c (in-list (e:match*-clauses v))])
                                  (clause* (map strip (clause*-patterns c))
                                           (and (clause*-guard c)
                                                (strip (clause*-guard c)))
                                           (strip (clause*-body c))
                                           #f))
                                (e:match*-irrefutable? v)
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
                                (top:data-abstract? v)
                                (top:data-runtime-tag v))]
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

  ;; ----- case-lambda / case-λ -------------------------------------
  ;;
  ;; A `case-lambda` desugars to an `e:lam` over fresh argument names
  ;; whose body is an `e:match*` matching all arguments at once — the
  ;; same shape the multi-clause `define` combiner emits.  The fresh
  ;; parameter names are gensyms, so the assertions reconstruct the
  ;; expected scrutinees from the actual params rather than pinning
  ;; literal symbols.

  (let ([v (pe '(case-lambda
                  [((Some x) (Some y)) (Some (+ x y))]
                  [(_ _)               None]))])
    (check-pred e:lam? v)
    (define params (e:lam-params v))
    (check-equal? (length params) 2)
    (define body (e:lam-body v))
    (check-pred e:match*? body)
    (check-equal? (e:match*-scrutinees body)
                  (list (e:var (car params) #f) (e:var (cadr params) #f)))
    (check-equal?
     (e:match*-clauses body)
     (list (clause* (list (p:ctor 'Some (list (p:var 'x #f)) #f)
                          (p:ctor 'Some (list (p:var 'y #f)) #f))
                    #f
                    (e:app (e:var 'Some #f)
                           (list (e:app (e:var '+ #f)
                                        (list (e:var 'x #f) (e:var 'y #f)) #f))
                           #f)
                    #f)
           (clause* (list (p:wild #f) (p:wild #f))
                    #f
                    (e:var 'None #f)
                    #f))))

  ;; `case-λ` is an alias for `case-lambda`; single-argument form.  The
  ;; first element of each clause is the parameter list, so a lone
  ;; constructor-pattern argument needs its own parens: `((Some x))`.
  (let ([v (pe '(case-λ
                  [(None)     0]
                  [((Some x)) x]))])
    (check-pred e:lam? v)
    (define params (e:lam-params v))
    (check-equal? (length params) 1)
    (define body (e:lam-body v))
    (check-pred e:match*? body)
    (check-equal? (e:match*-scrutinees body)
                  (list (e:var (car params) #f)))
    (check-equal?
     (e:match*-clauses body)
     (list (clause* (list (p:ctor 'None '() #f)) #f (e:literal 0 #f) #f)
           (clause* (list (p:ctor 'Some (list (p:var 'x #f)) #f))
                    #f (e:var 'x #f) #f))))

  ;; A clause may carry a `#:when` guard, just like `match`.
  (let ([v (pe '(case-lambda
                  [(x) #:when (> x 0) x]
                  [(_)               0]))])
    (define body (e:lam-body v))
    (check-pred e:match*? body)
    (check-equal?
     (e:match*-clauses body)
     (list (clause* (list (p:var 'x #f))
                    (e:app (e:var '> #f)
                           (list (e:var 'x #f) (e:literal 0 #f)) #f)
                    (e:var 'x #f)
                    #f)
           (clause* (list (p:wild #f)) #f (e:literal 0 #f) #f))))

  ;; All clauses must share one arity.
  (check-exn exn:fail:syntax?
             (lambda () (pe '(case-lambda
                               [(x)   x]
                               [(_ _) 0]))))

  ;; At least one clause is required.
  (check-exn exn:fail:syntax?
             (lambda () (pe '(case-lambda))))

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

  ;; The bare function arrow is a referenceable type constructor, so it
  ;; can appear unapplied (e.g. as an instance-head argument like
  ;; `(Arrow (->))`).  Both the bare literal `->` and the parenthesized
  ;; zero-arg `(->)` parse to the arrow tycon.  This must not disturb the
  ;; applied forms above.
  (check-equal? (pt '->)   (ty:con '-> #f))
  (check-equal? (pt '(->)) (ty:con '-> #f))

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
                          #f
                          #f))

  (check-equal? (ptop '(data Bool True False))
                (top:data 'Bool '()
                          (list (data-ctor 'True '() #f '() '() #f)
                                (data-ctor 'False '() #f '() '() #f))
                          #f
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

  ;; A simple `(-> x y)` arrow, used to keep the bound cases readable.
  (define (arr x y) (ty:app (ty:con '-> #f) (list x y) #f))

  ;; Superclass as a per-parameter bound: `[a => Eq]` desugars to a
  ;; super-constraint `(Eq a)` plus a plain `a` in the head.
  (check-equal? (ptop '(protocol (Ord [a => Eq])
                         (: < (-> a a))))
                (top:class
                  (list (constraint 'Eq (list (ty:var 'a #f)) #f))
                  (constraint 'Ord (list (ty:var 'a #f)) #f)
                  (list (method-sig '< (arr (ty:var 'a #f) (ty:var 'a #f)) #f))
                  #f))

  ;; Multiple superclasses on one parameter: `[a => Num Ord]`.
  (check-equal? (ptop '(protocol (Real [a => Num Ord])
                         (: r (-> a a))))
                (top:class
                  (list (constraint 'Num (list (ty:var 'a #f)) #f)
                        (constraint 'Ord (list (ty:var 'a #f)) #f))
                  (constraint 'Real (list (ty:var 'a #f)) #f)
                  (list (method-sig 'r (arr (ty:var 'a #f) (ty:var 'a #f)) #f))
                  #f))

  ;; Bounds on different parameters of a multi-parameter class.
  (check-equal? (ptop '(protocol (MonadWriter [w => Monoid] [m => Monad])
                         (: tell (-> w w))))
                (top:class
                  (list (constraint 'Monoid (list (ty:var 'w #f)) #f)
                        (constraint 'Monad  (list (ty:var 'm #f)) #f))
                  (constraint 'MonadWriter
                              (list (ty:var 'w #f) (ty:var 'm #f)) #f)
                  (list (method-sig 'tell (arr (ty:var 'w #f) (ty:var 'w #f)) #f))
                  #f))

  ;; Partially-applied bound: `[b => (Convert a)]` desugars to the
  ;; relational super-constraint `(Convert a b)` (bound var fills the
  ;; last slot).  `a` is a bare parameter.
  (check-equal? (ptop '(protocol (BiConvert a [b => (Convert a)])
                         (: backward (-> b a))))
                (top:class
                  (list (constraint 'Convert
                                    (list (ty:var 'a #f) (ty:var 'b #f)) #f))
                  (constraint 'BiConvert
                              (list (ty:var 'a #f) (ty:var 'b #f)) #f)
                  (list (method-sig 'backward
                                    (arr (ty:var 'b #f) (ty:var 'a #f)) #f))
                  #f))

  ;; Trailing `#:requires` clause for a relational superclass.
  (check-equal? (ptop '(protocol (BiConvert a b)
                         (#:requires (Convert a b))
                         (: backward (-> b a))))
                (top:class
                  (list (constraint 'Convert
                                    (list (ty:var 'a #f) (ty:var 'b #f)) #f))
                  (constraint 'BiConvert
                              (list (ty:var 'a #f) (ty:var 'b #f)) #f)
                  (list (method-sig 'backward
                                    (arr (ty:var 'b #f) (ty:var 'a #f)) #f))
                  #f))

  ;; The retired prefix superclass head is a parse error.
  (check-exn exn:fail?
             (lambda ()
               (ptop '(protocol ((Eq a) => (Ord a))
                        (: < (-> a a))))))

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
