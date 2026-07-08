#lang rackton

;; Tests for rackton/linear.  Increment 1: the Tensored + Symmetric layer on
;; the shipped `Lin` arrow, verified extensionally (an arrow has no decidable
;; equality, so compose concrete arrows and compare OUTPUTS), plus a
;; cross-module check that a client can define its OWN arrow with its OWN
;; tensor `data-instance` — the data family `Ten` lives in rackton/linear.

(require "../unit.rkt"
         "../linear.rkt")

(: gi (Gen Integer)) (define gi (int-range -50 50))

;; concrete Lin arrows
(: inc (Lin Integer Integer)) (define inc (lin (lambda (n) (+ n 1))))
(: dbl (Lin Integer Integer)) (define dbl (lin (lambda (n) (* n 2))))
(: neg (Lin Integer Integer)) (define neg (lin (lambda (n) (- 0 n))))

;; compare a Lin tensor by projecting to a Pair (which has Eq)
(: tp (-> (Ten Lin a b) (Pair a b)))
(define (tp t) (match t [(LinTen a b) (Pair a b)]))

(: lin-laws Test)
(define lin-laws
  (group-of "rackton/linear — Lin (Tensored + Symmetric)"
            (list
              (it-prop "par acts componentwise"
                       (for-all (gen-pair gi gi) (lambda (q) (match q [(Pair a c)
                                                                       (== (tp (at (par inc dbl) (LinTen a c))) (Pair (+ a 1) (* c 2)))]))))
              (it-prop "par identity: par id id = id"
                       (for-all (gen-pair gi gi) (lambda (q) (match q [(Pair a c)
                                                                       (== (tp (at (par ident ident) (LinTen a c)))
                                                                           (tp (at ident (LinTen a c))))]))))
              (it-prop "par composition: par (g.f) (k.h) = par g k . par f h"
                       (for-all (gen-pair gi gi) (lambda (q) (match q [(Pair a c)
                                                                       (== (tp (at (par (comp dbl inc) (comp neg dbl)) (LinTen a c)))
                                                                           (tp (at (comp (par dbl neg) (par inc dbl)) (LinTen a c))))]))))
              (it-prop "braid involutive: braid >>> braid = id"
                       (for-all (gen-pair gi gi) (lambda (q) (match q [(Pair a c)
                                                                       (== (tp (at (comp braid braid) (LinTen a c)))
                                                                           (tp (at ident (LinTen a c))))]))))
              (it-prop "braid natural: par f g >>> braid = braid >>> par g f"
                       (for-all (gen-pair gi gi) (lambda (q) (match q [(Pair a c)
                                                                       (== (tp (at (comp braid (par inc dbl)) (LinTen a c)))
                                                                           (tp (at (comp (par dbl inc) braid) (LinTen a c))))])))))))

;; ----- cross-module: a client-defined arrow with its OWN tensor ------
;; `Pf` and its `(data-instance (Ten Pf a b) ...)` live HERE, in a different
;; module from the data family `Ten` (in rackton/linear).  `par` / `braid`
;; dispatched on `Pf` exercise cross-module data-family reduction.

(data (Pf a b) (MkPf (-> a b)))
(: atp (-> (Pf a b) a b))
(define (atp p x) (match p [(MkPf f) (f x)]))

(data-instance (Ten Pf a b) (PfTen a b))

(instance (Category Pf)
  (define ident (MkPf (lambda (x) x)))
  (define (comp g f) (MkPf (lambda (x) (atp g (atp f x))))))
(instance (Tensored Pf)
  (define (par f g)
    (MkPf (lambda (q) (match q [(PfTen a c) (PfTen (atp f a) (atp g c))])))))
(instance (Symmetric Pf)
  (define braid (MkPf (lambda (q) (match q [(PfTen a b) (PfTen b a)])))))

(: pf-tp (-> (Ten Pf a b) (Pair a b)))
(define (pf-tp t) (match t [(PfTen a b) (Pair a b)]))

(: xmod-laws Test)
(define xmod-laws
  (group-of "rackton/linear — cross-module arrow with its own tensor"
            (list
              (it-prop "par works on a client-defined arrow"
                       (for-all (gen-pair gi gi) (lambda (q) (match q [(Pair a c)
                                                                       (== (pf-tp (atp (par (MkPf (lambda (n) (+ n 1)))
                                                                                            (MkPf (lambda (n) (* n 2))))
                                                                                       (PfTen a c)))
                                                                           (Pair (+ a 1) (* c 2)))]))))
              (it-prop "braid involutive on a client-defined arrow"
                       (for-all (gen-pair gi gi) (lambda (q) (match q [(Pair a c)
                                                                       (== (pf-tp (atp (comp braid braid) (PfTen a c))) (Pair a c))])))))))

;; ----- increment 2: the cartesian capabilities on Fn ----------------
(: fn-tp (-> (Ten Fn a b) (Pair a b)))
(define (fn-tp t) (match t [(FnTen a b) (Pair a b)]))

(: cartesian-laws Test)
(define cartesian-laws
  (group-of "rackton/linear — Fn (Cartesian: copy + discard)"
            (list
              (it-prop "dup is the diagonal: at dup x = (x, x)"
                       (for-all gi (lambda (x) (== (fn-tp (at-fn dup x)) (Pair x x)))))
              (it-prop "discard drops to Unit"
                       (for-all gi (lambda (x) (== (at-fn discard x) Unit))))
              (it-prop "copy is cocommutative: braid . dup = dup"
                       (for-all gi (lambda (x)
                                     (== (fn-tp (at-fn (comp braid dup) x)) (fn-tp (at-fn dup x)))))))))

(: suite Test)
(define suite (group-of "rackton/linear" (list lin-laws xmod-laws cartesian-laws)))

(: test-main (IO Unit))
(define test-main (run-suite-tree suite))
