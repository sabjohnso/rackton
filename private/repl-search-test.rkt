#lang racket/base

;; Property tests for the type-directed search machinery in
;; repl-search.rkt.
;;
;; `split-quals` flattens a possibly-nested qualified type into a body
;; and the constraints guarding it.  Every match rule below it reads
;; the body as an arrow spine, so a leftover `qual` layer would hide a
;; candidate's argument positions.  Three laws pin it:
;;
;;   normalization    the body it returns is never itself qualified
;;   nesting invariance   nested contexts and one merged context agree
;;   round trip       split ∘ mqual = identity on an unqualified body
;;
;; A fourth law covers the query side: constraining a query can only
;; shrink its result set.

(module+ test
  (require rackunit
           rackcheck
           racket/list
           "types.rkt"
           "prelude.rkt"
           "repl-search.rkt"
           (submod "repl-search.rkt" private-for-test)
           (submod "type-gen.rkt" test))

  ;; Constraint order carries no meaning — compare as multisets.
  (define (same-preds? a b)
    (and (= (length a) (length b))
         (for/and ([p (in-list a)]) (member p b))
         (for/and ([p (in-list b)]) (member p a))))

  (check-property
   (property normalization
             ([t (gen:nested-qual-type 2 3)])
     (define-values (body _preds) (split-quals t))
     (not (qual? body))))

  (check-property
   (property nesting-invariance
             ([ps1 (gen:list (gen:pred 1) #:max-length 2)]
              [ps2 (gen:list (gen:pred 1) #:max-length 2)]
              [t (gen:type 2)])
     (define-values (nested-body nested-preds)
       (split-quals (mqual ps1 (mqual ps2 t))))
     (define-values (flat-body flat-preds)
       (split-quals (mqual (append ps1 ps2) t)))
     (and (equal? nested-body flat-body)
          (same-preds? nested-preds flat-preds))))

  (check-property
   (property mqual-round-trip
             ([ps (gen:list (gen:pred 1) #:max-length 3)]
              [t (gen:type 2)])
     (define-values (body preds) (split-quals (mqual ps t)))
     (and (equal? body t) (same-preds? preds ps))))

  ;; Constraining a query can only remove candidates: the match rules
  ;; conjoin the query's predicates onto whatever the unconstrained
  ;; query already required.  Run against the real prelude, so the
  ;; instances doing the filtering are the ones users search.
  (define prelude-entries (env-search-entries prelude-env))

  (define (hit-names query kind)
    (define hits (search-entries prelude-entries query
                                 #:kind kind #:env prelude-env))
    (if (symbol? hits) '() (map car hits)))

  (check-property
   (property constraining-a-query-only-shrinks-it
             ([body (gen:choice (gen:const '(-> a a))
                                (gen:const '(-> Integer a))
                                (gen:const '(-> a Integer))
                                (gen:const '(-> (List a) a)))]
              [class (gen:choice (gen:const 'Num)
                                 (gen:const 'Eq)
                                 (gen:const 'Show)
                                 (gen:const 'Additive-Magma))]
              [kind (gen:choice (gen:const 'signature)
                                (gen:const 'returns)
                                (gen:const 'accepts))])
     (define loose (hit-names body kind))
     (define tight (hit-names `((,class a) => ,body) kind))
     (for/and ([n (in-list tight)]) (and (member n loose) #t)))))
