#lang racket/base

;; Rackton — algebraic-data-type runtime.
;;
;; `(define-data-ctor name arity)` emits, for a constructor `name`:
;;   - a prefab struct `$ctor:name` with `arity` fields.  Prefab (not a
;;     plain transparent struct) so the type has a single global identity:
;;     a value built in one module instantiation — e.g. the REPL's eval
;;     namespace, or a module reloaded via dynamic-rerequire — is still
;;     recognised by the constructor's predicate and match pattern in
;;     another.  This is sound because Rackton keys constructors by bare
;;     name globally (see the "Runtime-representation tiers" section of
;;     the developer guide); prefab's by-name identity matches that
;;     model.  Structural `equal?`, `struct->vector`
;;     display, and name-based dispatch are all unchanged from transparent.
;;   - a value binding `$val:name`:
;;       * for arity 0, the singleton instance
;;       * for arity 1, the struct constructor procedure
;;       * for arity ≥ 2, a CURRIED procedure (a prefix-arity
;;         `case-lambda`) so the constructor is a first-class curried
;;         value — its type `(-> f0 … f_{n-1} T)` is curried, so it must
;;         apply stepwise (`((C a) b)`) as well as all at once (`(C a b)`)
;;   - a match expander `name` whose
;;       * pattern transformer accepts exactly `arity` sub-patterns
;;         and rewrites to a struct pattern
;;       * expression transformer rewrites a bare reference to the
;;         value binding, a saturated application to a direct struct
;;         call, and a partial application through the curried value
;;         binding
;;
;; Constructor identifiers therefore work both as expressions and as
;; match patterns, mirroring Coalton/Haskell ergonomics.

(provide define-data-ctor define-tuple-ctor)

(require (for-syntax racket/base
                     racket/list
                     syntax/parse
                     racket/syntax))

(require racket/match)

;; A constructor's VALUE binding (`$val:name`) must behave as a curried
;; procedure, because its Hindley–Milner type `(-> f0 … f_{n-1} T)` is
;; curried: passed by reference it may be applied all at once (`(f a b)`)
;; or one argument at a time (`((f a) b)`).  This helper builds, at
;; expansion time, a prefix-arity `case-lambda`: every prefix arity 1..n
;; returns a curried continuation over the remaining parameters, and the
;; full-arity clause runs `make` (the direct struct/tuple constructor
;; call).  The body — the `make` call — is emitted once, at full arity;
;; shorter clauses re-enter the whole procedure through the `letrec`
;; self-name.  Unlike `codegen.rkt`'s `build-curried-lambda` for ordinary
;; lambdas, there is no over-application (arity > n) clause: a
;; constructor's result is the data type `T`, never a function, so a
;; well-typed program never applies it beyond its arity.
(begin-for-syntax
  ;; The `case-lambda` clauses for the parameters this level still binds.
  ;; `full-term` runs when the level is fully applied (the `make` call at
  ;; the top, the self-call at partial levels); `self-call` re-invokes the
  ;; whole procedure at exact arity to continue a partial application.
  (define (curried-arity-dispatch params full-term self-call ctx)
    (define n (length params))
    (define fixed-clauses
      (for/list ([k (in-range 1 (add1 n))])
        (define prefix (take params k))
        (define rest   (drop params k))
        (with-syntax ([(p ...) prefix])
          (cond
            [(null? rest)
             (with-syntax ([term full-term]) #'[(p ...) term])]
            [else
             (with-syntax ([inner (curried-arity-dispatch rest self-call self-call ctx)])
               #'[(p ...) inner])]))))
    (with-syntax ([(clause ...) fixed-clauses])
      (syntax/loc ctx (case-lambda clause ...))))

  ;; A curried procedure over `param-stxs` whose full-arity body is
  ;; `(make param …)`.  Used for arity ≥ 2; arity 0/1 need no currying.
  (define (build-curried-ctor make-id param-stxs ctx)
    (define self (car (generate-temporaries '(curried-ctor))))
    (with-syntax ([f self] [mk make-id] [(p ...) param-stxs])
      (with-syntax ([self-call #'(f p ...)])
        (with-syntax ([dispatch (curried-arity-dispatch
                                 param-stxs #'(mk p ...) #'self-call ctx)])
          (syntax/loc ctx (letrec ([f dispatch]) f)))))))

;; `(define-tuple-ctor name arity maker)` defines a constructor that is
;; backed by the tuple representation rather than its own struct: the
;; value binding `$val:name` builds a tuple via `maker` (the n-ary
;; `rackton-tuple-make`), and the match expander destructures the tuple's
;; underlying vector.  This is how `Pair` becomes the binary tuple — its
;; values are `Tuple`-tagged vectors, interchangeable with `(tuple a b)`.
;; `maker` is supplied by the caller so this module needn't depend on
;; prelude-runtime (which would be a cycle).
(define-syntax (define-tuple-ctor stx)
  (syntax-parse stx
    [(_ name:id arity:nat maker:id)
     (define n (syntax->datum #'arity))
     (define pat-ids (for/list ([i (in-range n)]) (format-id #'name "p~a" i)))
     (define arg-ids (for/list ([i (in-range n)]) (format-id #'name "a~a" i)))
     (with-syntax ([$val      (format-id #'name "$val:~a" #'name)]
                   [(pat ...) pat-ids]
                   [(arg ...) arg-ids]
                   [nlit      n])
       (with-syntax ([$val-def
                      (if (< n 2)
                          #'(define ($val arg ...) (maker arg ...))
                          (with-syntax ([curried (build-curried-ctor
                                                  #'maker arg-ids #'name)])
                            #'(define $val curried)))])
         #'(begin
             $val-def
             (define-match-expander name
               (lambda (mstx)
                 (syntax-parse mstx
                   [(_ pat ...) #'(vector pat ...)]))
               (lambda (estx)
                 (syntax-parse estx
                   [_:id #'$val]
                   ;; Exact arity: the direct `maker` call.  Any other
                   ;; count (partial application) routes through the
                   ;; curried value binding.
                   [(_ a (... ...))
                    (if (= (length (syntax->list #'(a (... ...)))) nlit)
                        #'(maker a (... ...))
                        #'($val a (... ...)))]))))))]))

(define-syntax (define-data-ctor stx)
  (syntax-parse stx
    [(_ name:id arity:nat)
     (define n (syntax->datum #'arity))
     (define field-ids
       (for/list ([i (in-range n)])
         (format-id #'name "f~a" i)))
     (define pat-ids
       (for/list ([i (in-range n)])
         (format-id #'name "p~a" i)))
     (define arg-ids
       (for/list ([i (in-range n)])
         (format-id #'name "a~a" i)))
     (with-syntax ([$ctor  (format-id #'name "$ctor:~a" #'name)]
                   [$ctor? (format-id #'name "$ctor:~a?" #'name)]
                   [$val   (format-id #'name "$val:~a" #'name)]
                   [(field ...) field-ids]
                   [(pat ...)   pat-ids]
                   [(arg ...)   arg-ids]
                   [nlit        n])
       (cond
         [(zero? n)
          #'(begin
              (struct $ctor () #:prefab)
              (define $val ($ctor))
              (define-match-expander name
                (lambda (mstx)
                  (syntax-parse mstx
                    [(_) #'(? $ctor?)]))
                (lambda (estx)
                  (syntax-parse estx
                    [_:id #'$val]))))]
         [else
          ;; For arity ≥ 2 the value binding is a curried procedure so the
          ;; constructor works as a first-class curried value (its type
          ;; `(-> f0 … f_{n-1} T)` is curried); arity 1 needs no currying,
          ;; so its bare value is the raw struct constructor.
          (with-syntax ([$val-def
                         (if (< n 2)
                             #'(define $val $ctor)
                             (with-syntax ([curried (build-curried-ctor
                                                     #'$ctor arg-ids #'name)])
                               #'(define $val curried)))])
            #'(begin
                (struct $ctor (field ...) #:prefab)
                $val-def
                (define-match-expander name
                  (lambda (mstx)
                    (syntax-parse mstx
                      [(_ pat ...) #'($ctor pat ...)]))
                  (lambda (estx)
                    (syntax-parse estx
                      [_:id #'$val]
                      ;; Exact arity: the direct struct call.  Any other
                      ;; count (partial application) routes through the
                      ;; curried value binding.
                      [(_ a (... ...))
                       (if (= (length (syntax->list #'(a (... ...)))) nlit)
                           #'($ctor a (... ...))
                           #'($val a (... ...)))])))))]))]))
