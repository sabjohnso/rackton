#lang racket/base

;; Tests for private/entail.rkt: predicate entailment under a class env.
;;
;; Algebraic laws checked:
;;   - Reflexivity: Γ ⊢ p when p ∈ Γ.
;;   - Superclass: Γ ⊢ p implies Γ ⊢ q for every superclass q of p.
;;   - Instance discharge: a matching instance plus entailment of its
;;     context discharges the target.
;;   - Failure: no instance and no membership ⇒ entailment fails.

(module+ test
  (require rackunit
           racket/match
           "types.rkt"
           "env.rkt"
           "entail.rkt")

  ;; ----- helpers ---------------------------------------------------

  (define (mk-class name params supers methods)
    (class-info name params
                (for/hasheq ([p params]) (values p (kind-star)))
                supers
                (for/hasheq ([m methods]) (values (car m) (cdr m)))
                (hasheq)
                (for/hasheq ([m methods]) (values (car m) 0))
                '()
                (hasheq)
                '()))

  ;; A class env with:
  ;;   class Eq a
  ;;   class Eq a => Ord a   (Eq is a superclass of Ord)
  ;;   instance Eq Integer
  ;;   instance Eq a => Eq (Maybe a)
  ;;   instance Ord Integer
  (define class-env
    (let* ([e empty-env]
           [e (env-extend-class
               e 'Eq
               (mk-class 'Eq '(a) '()
                         (list (cons '== (scheme '(a)
                                                 (mqual (list (pred 'Eq (list (tvar 'a))))
                                                        (make-arrow (tvar 'a)
                                                                    (make-arrow (tvar 'a) t-bool))))))))]
           [e (env-extend-class
               e 'Ord
               (mk-class 'Ord '(a) (list (pred 'Eq (list (tvar 'a))))
                         (list (cons '< (scheme '(a)
                                                (mqual (list (pred 'Ord (list (tvar 'a))))
                                                       (make-arrow (tvar 'a)
                                                                   (make-arrow (tvar 'a) t-bool))))))))]
           [e (env-extend-instance e 'Eq
                                   (instance-info (pred 'Eq (list t-int)) '() (hasheq) (hasheq)))]
           [e (env-extend-instance
               e 'Eq
               (instance-info (pred 'Eq (list (tapp (tcon 'Maybe) (list (tvar 'a)))))
                              (list (pred 'Eq (list (tvar 'a))))
                              (hasheq)
                              (hasheq)))]
           [e (env-extend-instance e 'Ord
                                   (instance-info (pred 'Ord (list t-int)) '() (hasheq) (hasheq)))])
      e))

  ;; ----- reflexivity ---------------------------------------------

  (check-true (entail? class-env
                       (list (pred 'Eq (list (tvar 'a))))
                       (pred 'Eq (list (tvar 'a)))))

  ;; ----- instance discharge --------------------------------------

  ;; (Eq Integer) is a registered instance: discharge with no hyps.
  (check-true (entail? class-env '() (pred 'Eq (list t-int))))

  ;; (Eq (Maybe Integer)) follows from instance (Eq a) => (Eq (Maybe a))
  ;; combined with (Eq Integer).
  (check-true
   (entail? class-env '()
            (pred 'Eq (list (tapp (tcon 'Maybe) (list t-int))))))

  ;; ----- superclass derivation -----------------------------------

  ;; (Ord a) entails (Eq a) through superclass.
  (check-true
   (entail? class-env (list (pred 'Ord (list (tvar 'a))))
            (pred 'Eq (list (tvar 'a)))))

  ;; ----- failure --------------------------------------------------

  ;; No instance for (Eq String) and not a hypothesis.
  (check-false (entail? class-env '() (pred 'Eq (list t-string))))

  ;; A variable target with no matching hyp fails (cannot dispatch on a tvar).
  (check-false (entail? class-env '() (pred 'Eq (list (tvar 'a)))))

  ;; ----- reduce-context -----------------------------------------

  ;; reduce-context simplifies preds in head-normal form (those with tvar
  ;; heads remain; concrete preds dischargeable by instances are removed).
  (check-equal? (reduce-context class-env '() (list (pred 'Eq (list t-int))))
                '())

  ;; (Eq a) is in HNF (variable head) — kept as-is.
  (check-equal? (reduce-context class-env '()
                                (list (pred 'Eq (list (tvar 'a)))))
                (list (pred 'Eq (list (tvar 'a)))))

  ;; Mixed: concrete one drops out, variable one stays.
  (check-equal? (reduce-context class-env '()
                                (list (pred 'Eq (list t-int))
                                      (pred 'Eq (list (tvar 'a)))))
                (list (pred 'Eq (list (tvar 'a))))))
