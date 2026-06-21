#lang rackton

;; Feature 9 / Phase 1: cross-module fixture for derived-laws-test.rkt.
;;
;; A `#lang rackton` library that declares a lawful protocol `Merge` and
;; exports the auto-generated `Merge-laws` bundle alongside a lawful type
;; and its generator, so an importer can run the bundle against the
;; imported instance.

(require "../unit.rkt")

(provide (protocol-out Merge)
         Merge-laws
         (data-out Thing)
         gen-thing)

(protocol (Merge a)
  (: merge (-> a (-> a a)))
  #:laws
    ([associativity ((Eq a) =>
      (All ([x : a] [y : a] [z : a])
        (== (merge (merge x y) z)
            (merge x (merge y z)))))]))

(data Thing (MkThing Integer))

(instance (Eq Thing)
  (define (== a b) (match a [(MkThing x) (match b [(MkThing y) (== x y)])])))
(instance (Show Thing)
  (define (show a) (match a [(MkThing x) (integer->string x)])))
(instance (Merge Thing)
  (define (merge a b)
    (match a [(MkThing x) (match b [(MkThing y) (MkThing (+ x y))])])))

(: gen-thing (Gen Thing))
(define gen-thing (fmap (lambda (n) (MkThing n)) (int-range 1 20)))
