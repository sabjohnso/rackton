#lang racket/base

;; Tests for private/env.rkt: type / data-constructor / type-constructor
;; environments and the initial built-in env.

(module+ test
  (require rackunit
           racket/set
           "types.rkt"
           "surface.rkt"
           "env.rkt")

  ;; ----- value-binding env ------------------------------------------

  (define e0 (env-extend-var empty-env 'x (scheme '() t-int)))
  (check-equal? (env-ref-var e0 'x)        (scheme '() t-int))
  (check-equal? (env-ref-var e0 'missing)  #f)
  (check-equal? (env-ref-var e0 'missing 'd) 'd)

  ;; ----- data-constructor env ---------------------------------------

  (define some-info
    (data-info 'Maybe 'Some 1
               (scheme '(a) (make-arrow (tvar 'a)
                                        (tapp (tcon 'Maybe) (list (tvar 'a)))))))
  (define e1 (env-extend-data e0 'Some some-info))
  (check-equal? (env-ref-data e1 'Some) some-info)
  (check-false  (env-ref-data e1 'None))

  ;; ----- type-constructor env ---------------------------------------

  (define maybe-info (tcon-info 'Maybe 1 '(None Some)))
  (define e2 (env-extend-tcon e1 'Maybe maybe-info))
  (check-equal? (env-ref-tcon e2 'Maybe) maybe-info)

  ;; ----- free-vars of the value env --------------------------------

  ;; (scheme '() (tvar 'free)) contributes 'free; (scheme '(a) ...)
  ;; binds 'a so it must not appear.
  (define e3
    (env-extend-var
     (env-extend-var empty-env 'x (scheme '() (tvar 'free)))
     'id (scheme '(a) (make-arrow (tvar 'a) (tvar 'a)))))
  (check-equal? (env-vars-free-vars e3) (seteq 'free))

  ;; ----- substitution lifting --------------------------------------

  (define e4 (apply-subst/env (subst-singleton 'free t-int) e3))
  (check-equal? (env-ref-var e4 'x) (scheme '() t-int))
  ;; Bound type variables inside schemes are NOT substituted.
  (check-equal? (env-ref-var e4 'id)
                (scheme '(a) (make-arrow (tvar 'a) (tvar 'a))))

  ;; ----- initial-env smoke -----------------------------------------

  (check-equal? (env-ref-var initial-env '+)
                (scheme '()
                        (make-arrow t-int (make-arrow t-int t-int))))
  (check-equal? (env-ref-var initial-env '<)
                (scheme '()
                        (make-arrow t-int (make-arrow t-int t-bool))))

  ;; ----- class env extensions ---------------------------------------

  (define eq-info
    (class-info 'Eq '(a) '()
                (hasheq '== (scheme '(a)
                                    (mqual (list (pred 'Eq (list (tvar 'a))))
                                           (make-arrow (tvar 'a)
                                                       (make-arrow (tvar 'a) t-bool)))))
                (hasheq)))
  (define eq-env (env-extend-class empty-env 'Eq eq-info))

  (check-equal? (env-ref-class eq-env 'Eq) eq-info)
  (check-false  (env-ref-class eq-env 'NoSuchClass))
  (check-equal? (env-ref-method-class eq-env '==) 'Eq)
  (check-false  (env-ref-method-class eq-env 'missing-method))

  ;; instance registry
  (define eq-int-inst
    (instance-info (pred 'Eq (list t-int)) '()
                   (hasheq '== (e:var '= #f))))
  (define eq-env2 (env-extend-instance eq-env 'Eq eq-int-inst))
  (check-equal? (env-instances eq-env2 'Eq) (list eq-int-inst))
  (check-equal? (env-instances eq-env2 'Ord) '()))
