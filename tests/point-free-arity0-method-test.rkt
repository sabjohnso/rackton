#lang racket/base

;; An ARITY-0 return-typed method (mempty, mzero, empty) given point-free
;; as an alias to a top-level def —
;;   (define mempty the-empty)
;; — used to fail at load with "cannot reference an identifier before its
;; definition": the impl define is emitted at the instance phase, ahead of
;; all top-level defs, and an arity-0 value body evaluates eagerly (there
;; is no lambda to defer the reference behind, so the arity>=1 eta-
;; expansion fix does not apply).  Codegen now emits an arity-0 return-
;; typed impl as a memoized deferred thunk, forced at lookup — deferring
;; the reference to call time, by which point every module binding exists.

(require rackunit
         rackcheck
         "../main.rkt")

;; ----- forward reference: mempty aliases a LATER top-level def --------

(rackton
  (data Wrap (MkWrap Integer))

  (instance (Semigroup Wrap)
    (define (mappend a b)
      (match a [(MkWrap x) (match b [(MkWrap y) (MkWrap (+ x y))])])))

  (instance (Monoid Wrap)
    (define mempty the-empty))          ; point-free, arity-0, forward ref

  ;; Emitted AFTER the instance; a naked reference would fail.
  (define the-empty (MkWrap 0))

  (: fwd-left Integer)
  (define fwd-left
    (match (mappend (ann mempty Wrap) (MkWrap 5)) [(MkWrap n) n]))
  (: fwd-right Integer)
  (define fwd-right
    (match (mappend (MkWrap 5) (ann mempty Wrap)) [(MkWrap n) n]))

  (provide fwd-left fwd-right))

(test-case "point-free arity-0 mempty aliasing a later def resolves"
  (check-equal? fwd-left 5)
  (check-equal? fwd-right 5))

;; ----- def fed by one instance's method, aliased by another's mempty --

(rackton
  (data Sum (MkSum Integer))

  (instance (Semigroup Sum)
    (define (mappend a b)
      (match a [(MkSum x) (match b [(MkSum y) (MkSum (+ x y))])])))

  ;; This def USES the Semigroup instance's method and is itself the value
  ;; the Monoid instance's mempty aliases: an instance<->def interleaving
  ;; the fixed phase order cannot satisfy without deferral.
  (define seed (mappend (MkSum 0) (MkSum 0)))

  (instance (Monoid Sum)
    (define mempty seed))

  (: interleaved Integer)
  (define interleaved
    (match (mappend (ann mempty Sum) (MkSum 9)) [(MkSum n) n]))

  (provide interleaved))

(test-case "def fed by an instance method, aliased by a later mempty"
  (check-equal? interleaved 9))

;; ----- Monoid identity law holds for the deferred mempty --------------

(rackton
  (data Tot (MkTot Integer))

  (instance (Semigroup Tot)
    (define (mappend a b)
      (match a [(MkTot x) (match b [(MkTot y) (MkTot (+ x y))])])))

  (instance (Monoid Tot)
    (define mempty tot-empty))

  (define tot-empty (MkTot 0))

  ;; left- and right-identity applied to an arbitrary payload
  (: left-id (-> Integer Integer))
  (define (left-id n)
    (match (mappend (ann mempty Tot) (MkTot n)) [(MkTot r) r]))
  (: right-id (-> Integer Integer))
  (define (right-id n)
    (match (mappend (MkTot n) (ann mempty Tot)) [(MkTot r) r]))

  (provide left-id right-id))

(check-property
 (property monoid-left-identity ([n (gen:integer-in -100000 100000)])
   (= (left-id n) n)))
(check-property
 (property monoid-right-identity ([n (gen:integer-in -100000 100000)])
   (= (right-id n) n)))

;; ----- regression: inline literal form still works --------------------

(rackton
  (data Lit (MkLit Integer))

  (instance (Semigroup Lit)
    (define (mappend a b)
      (match a [(MkLit x) (match b [(MkLit y) (MkLit (+ x y))])])))

  (instance (Monoid Lit)
    (define mempty (MkLit 0)))          ; inline literal, no forward ref

  (: inline-empty Integer)
  (define inline-empty (match (ann mempty Lit) [(MkLit n) n]))

  (provide inline-empty))

(test-case "inline-literal arity-0 mempty still works"
  (check-equal? inline-empty 0))

;; ----- regression: a prelude arity-0 mempty (List) still works --------

(rackton
  (: prelude-empty (List Integer))
  (define prelude-empty (ann mempty (List Integer)))
  (: prelude-empty-len Integer)
  (define prelude-empty-len (length prelude-empty))
  (provide prelude-empty-len))

(test-case "prelude arity-0 mempty (List) still works"
  (check-equal? prelude-empty-len 0))
