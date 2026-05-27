#lang racket/base

;; Order-invariance of top-level forms — a module body may reference
;; later forms (defs, data types, classes, instances) regardless of
;; source order.

(require rackunit
         "../main.rkt")

;; ----- forward reference between defs ------------------------
(rackton
  (define (forward-1 x) (forward-2 (+ x 1)))
  (define (forward-2 x) (* x 2))

  (: forward-result Integer)
  (define forward-result (forward-1 3))

  (provide forward-result))

(test-case "forward reference between defs"
  (check-equal? forward-result 8))

;; ----- mutual recursion between defs -------------------------
(rackton
  (define (rk-even? n) (if (== n 0) #t (rk-odd? (- n 1))))
  (define (rk-odd?  n) (if (== n 0) #f (rk-even? (- n 1))))

  (: even-7  Boolean)
  (define even-7  (rk-even? 7))
  (: even-10 Boolean)
  (define even-10 (rk-even? 10))

  (provide even-7 even-10))

(test-case "mutually recursive defs"
  (check-equal? even-7  #f)
  (check-equal? even-10 #t))

;; ----- mutually recursive data types -------------------------
(rackton
  (define-data Tr  Lf (Br Frst Frst))
  (define-data Frst Empt (Cns Tr Frst))

  (: leaf Tr)
  (define leaf Lf)

  (: leaf-forest Frst)
  (define leaf-forest (Cns leaf Empt))

  (: branched Tr)
  (define branched (Br Empt leaf-forest))

  (provide leaf leaf-forest branched))

(test-case "mutually recursive data types"
  (check-equal? leaf Lf)
  (check-not-false leaf-forest)
  (check-not-false branched))

;; ----- class used before its declaration ---------------------
(rackton
  (define (greet-int n) (mk-pretty n))

  (define-class (MkPretty a)
    (: mk-pretty (-> a String)))

  (define-instance (MkPretty Integer)
    (define (mk-pretty n) "an int"))

  (: greet-result String)
  (define greet-result (greet-int 42))

  (provide greet-result))

(test-case "class used before its declaration"
  (check-equal? greet-result "an int"))

;; ----- instance declared before its class --------------------
(rackton
  (define-instance (Tagged BBox)
    (define (tag-of b) "BBox"))

  (define-class (Tagged a)
    (: tag-of (-> a String)))

  (define-data BBox MkBBox)

  (: bbox-tag String)
  (define bbox-tag (tag-of MkBBox))

  (provide bbox-tag))

(test-case "instance declared before its class"
  (check-equal? bbox-tag "BBox"))

;; ----- SCC-preserved polymorphism ----------------------------
;; `helper` is independent of `use1`/`use2` (different SCC),
;; so it should be inferred fully polymorphically and reused at
;; two element types.
(rackton
  (: use1 (Maybe Integer))
  (define use1 (helper (Some 3)))

  (: use2 (Maybe String))
  (define use2 (helper (Some "x")))

  (define (helper m)
    (match m
      [(Some v) (Some v)]
      [None     None]))

  (provide use1 use2))

(test-case "SCC-preserved polymorphism: helper used at two types"
  (check-equal? use1 (Some 3))
  (check-equal? use2 (Some "x")))
