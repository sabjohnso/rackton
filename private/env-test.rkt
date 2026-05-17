#lang racket/base

;; Tests for private/env.rkt: type / data-constructor / type-constructor
;; environments and the initial built-in env.

(module+ test
  (require rackunit
           racket/set
           "types.rkt"
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
                        (make-arrow t-int (make-arrow t-int t-bool)))))
