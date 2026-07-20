#lang racket/base

;; Shared rackcheck generators for the internal type AST (types.rkt).
;;
;; One canonical generator set, used by the property tests in
;; types-test.rkt, unify-test.rkt, and scheme-codec-test.rkt — rather
;; than three near-identical copies.
;;
;; The generators live in a `test` submodule (required elsewhere as
;; `(submod "type-gen.rkt" test)`) so the rackcheck dependency stays
;; build-context — `raco setup --check-pkg-deps` would otherwise flag a
;; top-level rackcheck require as an undeclared *runtime* dependency.
;;
;; Naming invariant the codec relies on: type-VARIABLE names are
;; lowercase, type-CONSTRUCTOR names are uppercase (plus `->`).  The
;; round-trip codec recovers the tvar/tcon distinction from this casing,
;; so the generators must respect it.

(module+ test
  (require rackcheck
           "types.rkt")

  (provide gen:tvar-name
           gen:tcon-name
           gen:class-name
           gen:type
           gen:pred
           gen:qual-type
           gen:nested-qual-type
           gen:scheme
           gen:kind
           gen:kind-scheme
           gen:subst)

  (define gen:tvar-name
    (gen:choice (gen:const 'a) (gen:const 'b) (gen:const 'c) (gen:const 'd)))

  (define gen:tcon-name
    (gen:choice (gen:const 'Integer) (gen:const 'Boolean) (gen:const 'String)
                (gen:const 'Unit) (gen:const 'List) (gen:const 'Maybe)
                (gen:const '->)))

  (define gen:class-name
    (gen:choice (gen:const 'Eq) (gen:const 'Show) (gen:const 'Ord)
                (gen:const 'Functor) (gen:const 'Monad)))

  ;; A type up to `depth` nested applications.  Uses `make-tapp` so every
  ;; generated type is canonical (flattened), matching the inference
  ;; engine's invariant.
  (define (gen:type depth)
    (cond
      [(<= depth 0)
       (gen:choice (gen:let ([n gen:tvar-name]) (tvar n))
                   (gen:let ([n gen:tcon-name]) (tcon n)))]
      [else
       (gen:choice
        (gen:let ([n gen:tvar-name]) (tvar n))
        (gen:let ([n gen:tcon-name]) (tcon n))
        (gen:let ([n gen:tcon-name]
                  [args (gen:list (gen:type (sub1 depth)) #:max-length 3)])
          (make-tapp (tcon n) args)))]))

  ;; A class predicate `(C t …)` with at least one type argument.
  (define (gen:pred depth)
    (gen:let ([c gen:class-name]
              [a0 (gen:type depth)]
              [rest (gen:list (gen:type depth) #:max-length 1)])
      (pred c (cons a0 rest))))

  ;; A type body that is either bare or qualified by a non-empty context.
  (define (gen:qual-type depth)
    (gen:choice
     (gen:type depth)
     (gen:let ([p0 (gen:pred (max 0 (sub1 depth)))]
               [ps (gen:list (gen:pred (max 0 (sub1 depth))) #:max-length 2)]
               [body (gen:type depth)])
       (mqual (cons p0 ps) body))))

  ;; A body under up to `layers` *nested* contexts:
  ;; `(C a) => ((D b) => τ)`.  `gen:qual-type` only ever produces one
  ;; layer, so a law about peeling nested contexts needs this.
  (define (gen:nested-qual-type depth layers)
    (cond
      [(<= layers 0) (gen:type depth)]
      [else
       (gen:let ([ps (gen:list (gen:pred (max 0 (sub1 depth))) #:max-length 2)]
                 [body (gen:nested-qual-type depth (sub1 layers))])
         (mqual ps body))]))

  ;; A scheme: a (possibly empty) list of quantified vars over a
  ;; possibly-qualified body.
  (define (gen:scheme depth)
    (gen:let ([vars (gen:list gen:tvar-name #:max-length 4)]
              [body (gen:qual-type depth)])
      (scheme vars body)))

  ;; A kind: `*`, a kvar (so generalisation has something to quantify),
  ;; or an arrow of kinds.
  (define (gen:kind depth)
    (cond
      [(<= depth 0) (gen:choice (gen:const (kind-star))
                                (gen:const (kvar 'ka))
                                (gen:const (kvar 'kb)))]
      [else (gen:choice
             (gen:const (kind-star))
             (gen:const (kvar 'ka))
             (gen:let ([a (gen:kind (sub1 depth))]
                       [b (gen:kind (sub1 depth))])
               (kind-arr a b)))]))

  ;; A kind scheme: a generated kind with its free kvars quantified.
  (define (gen:kind-scheme depth)
    (gen:let ([k (gen:kind depth)])
      (generalize-kind k)))

  ;; A substitution built from up to four (tvar ↦ type) bindings.
  (define (gen:subst depth)
    (gen:let ([entries (gen:list (gen:let ([n gen:tvar-name]
                                           [t (gen:type depth)])
                                   (cons n t))
                                 #:max-length 4)])
      (for/fold ([s empty-subst]) ([kv (in-list entries)])
        (subst-extend s (car kv) (cdr kv))))))
