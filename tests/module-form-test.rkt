#lang racket/base

;; Using `rackton` as the language in a (module name LANG body ...)
;; form.  Bodies should auto-wrap in (rackton/main ...) and every
;; definition should auto-provide, matching #lang rackton behavior.

(require rackunit)

;; ----- basic case: a typed definition -----

(module mod-x rackton
  (provide (all-defined-out))
  (: x Integer)
  (define x 3))

(require 'mod-x)

(test-case "module-form: typed integer def"
  (check-equal? x 3))

;; ----- ADT + pattern-matching function -----

(module mod-maybe rackton
  (provide (all-defined-out))
  (define-data (Maybe a) None (Some a))

  (: from-maybe (-> a (-> (Maybe a) a)))
  (define (from-maybe d m)
    (match m
      [(None)   d]
      [(Some x) x])))

(require 'mod-maybe)

(test-case "module-form: ADT and pattern match"
  (check-equal? (from-maybe 0 (Some 7)) 7)
  (check-equal? (from-maybe 0 None) 0))

;; ----- class method + instance, called from inside the module -----

(module mod-class rackton
  (provide (all-defined-out))
  (define-data Color  Red  Green  Blue)

  (instance (Eq Color)
    (define (== a b)
      (match a
        [Red   (match b [Red   #t] [_  #f])]
        [Green (match b [Green #t] [_  #f])]
        [Blue  (match b [Blue  #t] [_  #f])])))

  ;; Exercise the instance internally so an outer racket/base test
  ;; doesn't need to import Rackton's `==` to assert on it.
  (: red-eq-red    Boolean)
  (define red-eq-red    (== Red Red))
  (: red-eq-green  Boolean)
  (define red-eq-green  (== Red Green))
  (: blue-eq-blue  Boolean)
  (define blue-eq-blue  (== Blue Blue)))

(require 'mod-class)

(test-case "module-form: class instance dispatch"
  (check-equal? red-eq-red    #t)
  (check-equal? red-eq-green  #f)
  (check-equal? blue-eq-blue  #t))

;; ----- regression: (require rackton) from racket/base still works -----

(module mod-embed racket/base
  (require rackton)
  (provide answer)
  (rackton
    (: answer Integer)
    (define answer 42)))

(require 'mod-embed)

(test-case "embedded (rackton ...) macro still works"
  (check-equal? answer 42))
