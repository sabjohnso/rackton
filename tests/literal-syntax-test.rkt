#lang racket/base

;; Bracket/brace literal syntax (Feature 8).  Three additive literal
;; forms, distinguished by the reader's `paren-shape` property:
;;
;;   [v1 v2 ...]   list literal   => (List a)   (Cons/Nil chain)
;;   {k1 v1 ...}   map literal     => (Map k v)  (nested map-insert)
;;   #{m1 m2 ...}  set literal     => (Set a)    (nested set-insert)
;;
;; Parens keep every current meaning; (list ..), (array ..), #(..) are
;; untouched.  Map/Set constructors are promoted into the prelude, so the
;; literals need no import.  Duplicate map keys: last write wins.

(require rackunit rackcheck
         (for-syntax racket/base)
         "../main.rkt")

(define-syntax-rule (check-rackton-compile-error form ...)
  (check-exn
   exn:fail?
   (lambda ()
     (eval #'(rackton form ...)
           (variable-reference->namespace (#%variable-reference))))))

;; All the well-typed literals live in one block so the names escape into
;; this module for value-level checks.
(rackton
  ;; ----- list literals -----
  (define lst-empty   [])
  (define lst-three   [1 2 3])
  (define lst-nested  [[1 2] [3 4]])

  ;; element-position evaluation: each entry is an ordinary expression
  (define (inc x) (+ x 1))
  (define lst-eval    [(inc 1) (inc 2) (inc 3)])

  ;; ----- map literals -----
  (define map-empty   {})
  (define map-two     {1 10 2 20})
  ;; duplicate key: last write wins
  (define map-dup     {1 "a" 1 "b"})

  ;; ----- set literals -----
  (define set-empty   #{})
  (define set-three   #{1 2 3})
  ;; duplicate member collapses
  (define set-dup     #{1 1 2})

  ;; ----- list patterns ----- (applied here so the bracket literals stay
  ;; inside the rackton block; results escape for the value checks)
  (: classify (-> (List Integer) String))
  (define (classify xs)
    (match xs
      [[]        "empty"]
      [[a b c]   "three"]
      [_         "other"]))
  (define classify-three (classify [1 2 3]))
  (define classify-empty (classify []))
  (define classify-other (classify [9]))

  (: sum3 (-> (List Integer) Integer))
  (define (sum3 xs)
    (match xs
      [[a b c]   (+ a (+ b c))]
      [_         0]))
  (define sum3-456 (sum3 [4 5 6]))

  ;; ----- parens still mean what they meant -----
  (define paren-list  (list 1 2 3))
  (define applied     (inc 41)))

;; ===== list literals ================================================

(test-case "empty list literal is Nil"
  (check-equal? lst-empty Nil))

(test-case "list literal builds a Cons/Nil chain"
  (check-equal? lst-three (Cons 1 (Cons 2 (Cons 3 Nil)))))

(test-case "nested list literals"
  (check-equal? lst-nested
                (Cons (Cons 1 (Cons 2 Nil))
                      (Cons (Cons 3 (Cons 4 Nil)) Nil))))

(test-case "elements are evaluated"
  (check-equal? lst-eval (Cons 2 (Cons 3 (Cons 4 Nil)))))

(test-case "bracket literal agrees with the (list ..) form"
  (check-equal? lst-three paren-list))

(test-case "a heterogeneous bracket literal is a type error"
  (check-rackton-compile-error
   (define bad [1 "x"])))

;; ===== map literals =================================================

(test-case "empty map literal is empty-map"
  (check-equal? map-empty empty-map))

(test-case "map literal builds the key/value map"
  (check-equal? map-two (map-insert 1 10 (map-insert 2 20 empty-map))))

(test-case "duplicate map key: last write wins"
  (check-equal? map-dup (map-insert 1 "b" empty-map)))

(test-case "an odd-length map literal is rejected"
  (check-rackton-compile-error
   (define bad {1 2 3})))

(test-case "a heterogeneous map literal is a type error"
  (check-rackton-compile-error
   (define bad {1 "a" "b" 2})))

;; ===== set literals =================================================

(test-case "empty set literal is empty-set"
  (check-equal? set-empty empty-set))

(test-case "set literal builds the membership set"
  (check-equal? set-three
                (set-insert 1 (set-insert 2 (set-insert 3 empty-set)))))

(test-case "duplicate set members collapse"
  (check-equal? set-dup (set-insert 1 (set-insert 2 empty-set))))

;; ===== list patterns ================================================

(test-case "bracket list pattern matches a fixed-arity list"
  (check-equal? classify-three "three")
  (check-equal? classify-empty "empty")
  (check-equal? classify-other "other"))

(test-case "bracket list pattern binds its elements"
  (check-equal? sum3-456 15))

(test-case "a map literal is not a pattern"
  (check-rackton-compile-error
   (define (f m) (match m [{1 2} "x"] [_ "y"]))))

(test-case "a set literal is not a pattern"
  (check-rackton-compile-error
   (define (f s) (match s [#{1 2} "x"] [_ "y"]))))

;; ===== parens unchanged =============================================

(test-case "parenthesised (list ..) still builds a list"
  (check-equal? paren-list (Cons 1 (Cons 2 (Cons 3 Nil)))))

(test-case "parenthesised application still applies"
  (check-equal? applied 42))

;; ===== property: [a ..] is exactly (list a ..) ======================

(rackton
  (: lit3  (-> Integer (-> Integer (-> Integer (List Integer)))))
  (define (lit3 a b c)  [a b c])
  (: call3 (-> Integer (-> Integer (-> Integer (List Integer)))))
  (define (call3 a b c) (list a b c)))

(check-property
 (property ([a gen:natural] [b gen:natural] [c gen:natural])
   (check-equal? (((lit3 a) b) c) (((call3 a) b) c))))
