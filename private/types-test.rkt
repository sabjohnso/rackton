#lang racket/base

;; Tests for private/types.rkt: type AST, free type variables, substitution
;; composition, and substitution application.
;;
;; Algebraic laws checked:
;;   - apply-subst empty-subst t  ≡  t
;;   - apply-subst (compose s2 s1) t  ≡  apply-subst s2 (apply-subst s1 t)
;;   - associativity: apply-subst (compose s3 (compose s2 s1)) t
;;                  ≡ apply-subst (compose (compose s3 s2) s1) t
;;   - type-vars (apply-subst s t)
;;     ⊆ ⋃ { type-vars (s α) | α ∈ type-vars t } ∪ (type-vars t \ dom(s))

(module+ test
  (require rackunit
           rackcheck
           racket/set
           "types.rkt")

  ;; ----- generators --------------------------------------------------

  (define gen:tcon-name
    (gen:choice (gen:const 'Integer)
                (gen:const 'Boolean)
                (gen:const 'String)
                (gen:const 'Unit)
                (gen:const 'List)
                (gen:const 'Maybe)
                (gen:const '->)))

  (define gen:tvar-name
    (gen:choice (gen:const 'a) (gen:const 'b) (gen:const 'c) (gen:const 'd)))

  (define (gen:type depth)
    (cond
      [(<= depth 0)
       (gen:choice (gen:let ([n gen:tvar-name]) (tvar n))
                   (gen:let ([n gen:tcon-name]) (tcon n)))]
      [else
       (gen:choice
        (gen:let ([n gen:tvar-name]) (tvar n))
        (gen:let ([n gen:tcon-name]) (tcon n))
        (gen:let ([h (gen:type (sub1 depth))]
                  [args (gen:list (gen:type (sub1 depth)) #:max-length 3)])
          (if (null? args) h (tapp h args))))]))

  (define (gen:subst depth)
    (gen:let ([entries (gen:list (gen:let ([n gen:tvar-name]
                                           [t (gen:type depth)])
                                   (cons n t))
                                 #:max-length 4)])
      (for/fold ([s empty-subst]) ([kv (in-list entries)])
        (subst-extend s (car kv) (cdr kv)))))

  ;; ----- examples ----------------------------------------------------

  (check-true  (type? (tvar 'a)))
  (check-true  (type? (tcon 'Integer)))
  (check-true  (type? (tapp (tcon '->) (list (tvar 'a) (tvar 'b)))))
  (check-false (type? 'not-a-type))

  (check-equal? (type-vars (tvar 'a)) (seteq 'a))
  (check-equal? (type-vars t-int)     (seteq))
  (check-equal? (type-vars (make-arrow (tvar 'a) (tvar 'b)))
                (seteq 'a 'b))
  (check-equal? (type-vars (tapp (tcon 'List) (list (tvar 'a))))
                (seteq 'a))

  (check-equal? (apply-subst empty-subst (tvar 'a)) (tvar 'a))
  (check-equal? (apply-subst (subst-singleton 'a t-int) (tvar 'a)) t-int)
  (check-equal? (apply-subst (subst-singleton 'a t-int) (tvar 'b)) (tvar 'b))
  (check-equal? (apply-subst (subst-singleton 'a t-int)
                             (make-arrow (tvar 'a) (tvar 'a)))
                (make-arrow t-int t-int))

  (check-true (arrow? (make-arrow t-int t-bool)))
  (check-equal? (arrow-dom (make-arrow t-int t-bool)) t-int)
  (check-equal? (arrow-cod (make-arrow t-int t-bool)) t-bool)

  (check-equal? (type->datum (make-arrow t-int t-bool))
                '(-> Integer Boolean))
  (check-equal? (type->datum (tapp (tcon 'List) (list (tvar 'a))))
                '(List a))
  (check-equal? (scheme->datum (scheme '() t-int)) 'Integer)
  (check-equal? (scheme->datum (scheme '(a) (make-arrow (tvar 'a) (tvar 'a))))
                '(All (a) (-> a a)))

  (check-equal? (scheme-free-vars (scheme '(a) (make-arrow (tvar 'a) (tvar 'b))))
                (seteq 'b))

  ;; ----- properties --------------------------------------------------

  (check-property
   (property identity-subst ([t (gen:type 3)])
     (equal? (apply-subst empty-subst t) t)))

  (check-property
   (property compose-applies-as-pipeline
             ([t  (gen:type 3)]
              [s1 (gen:subst 2)]
              [s2 (gen:subst 2)])
     (equal? (apply-subst (subst-compose s2 s1) t)
             (apply-subst s2 (apply-subst s1 t)))))

  (check-property
   (property compose-associative
             ([t  (gen:type 3)]
              [s1 (gen:subst 2)]
              [s2 (gen:subst 2)]
              [s3 (gen:subst 2)])
     (equal? (apply-subst (subst-compose s3 (subst-compose s2 s1)) t)
             (apply-subst (subst-compose (subst-compose s3 s2) s1) t))))

  (check-property
   (property fv-of-subst-bounded
             ([t (gen:type 3)]
              [s (gen:subst 2)])
     ;; Every free var of (apply-subst s t) must come either from s applied
     ;; to a free var of t, or from a free var of t that s does not touch.
     (define dom (list->seteq (hash-keys s)))
     (define expected
       (for/fold ([acc (seteq)]) ([α (in-set (type-vars t))])
         (set-union acc
                    (cond
                      [(hash-has-key? s α) (type-vars (hash-ref s α))]
                      [else                (seteq α)]))))
     (subset? (type-vars (apply-subst s t)) expected)))

  ;; ----- qualified types and predicates ----------------------------

  ;; A predicate (Eq Integer) has no free tvars.
  (check-equal? (type-vars (pred 'Eq (list t-int))) (seteq))
  ;; (Eq a) is parameterised over `a`.
  (check-equal? (type-vars (pred 'Eq (list (tvar 'a)))) (seteq 'a))

  ;; Qualified types collect free tvars across constraints AND body.
  (check-equal?
   (type-vars (mqual (list (pred 'Eq (list (tvar 'a))))
                     (make-arrow (tvar 'a) t-bool)))
   (seteq 'a))

  ;; Empty-context mqual collapses to the plain body.
  (check-equal? (mqual '() (tvar 'a)) (tvar 'a))

  ;; Non-empty mqual preserves the qual wrapper.
  (let ([q (mqual (list (pred 'Eq (list (tvar 'a)))) (tvar 'a))])
    (check-true (qual? q))
    (check-equal? (qual-constraints q)
                  (list (pred 'Eq (list (tvar 'a)))))
    (check-equal? (qual-body q) (tvar 'a)))

  ;; Substitution descends into both constraints and body.
  (let* ([before (mqual (list (pred 'Eq (list (tvar 'a))))
                        (make-arrow (tvar 'a) t-bool))]
         [after  (apply-subst (subst-singleton 'a t-int) before)])
    (check-equal? (qual-constraints after)
                  (list (pred 'Eq (list t-int))))
    (check-equal? (qual-body after) (make-arrow t-int t-bool))))
