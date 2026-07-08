#lang rackton

;; Order-invariance of top-level forms — a module body may reference
;; later forms (defs, data types, classes, instances) regardless of
;; source order.

(require "../unit.rkt")

;; ----- forward reference between defs ------------------------
(define (forward-1 x) (forward-2 (+ x 1)))
(define (forward-2 x) (* x 2))

(: forward-result Integer)
(define forward-result (forward-1 3))

;; ----- mutual recursion between defs -------------------------
(define (rk-even? n) (if (== n 0) #t (rk-odd? (- n 1))))
(define (rk-odd?  n) (if (== n 0) #f (rk-even? (- n 1))))

(: even-7  Boolean)
(define even-7  (rk-even? 7))
(: even-10 Boolean)
(define even-10 (rk-even? 10))

;; ----- mutually recursive data types -------------------------
(data Tr  Lf (Br Frst Frst))
(data Frst Empt (Cns Tr Frst))

(: leaf Tr)
(define leaf Lf)

(: leaf-forest Frst)
(define leaf-forest (Cns leaf Empt))

(: branched Tr)
(define branched (Br Empt leaf-forest))

;; Structural Boolean predicates (these ADTs are mutually recursive,
;; so we check shape via `match` rather than a derived Eq instance).
(: is-leaf? (-> Tr Boolean))
(define (is-leaf? t) (match t [(Lf) #t] [(Br _ _) #f]))

(: leaf-cons-empty? (-> Frst Boolean))
(define (leaf-cons-empty? f)
  (match f
    [(Empt) #f]
    [(Cns t rest)
     (match rest [(Empt) (is-leaf? t)] [(Cns _ _) #f])]))

(: branched-shape? (-> Tr Boolean))
(define (branched-shape? t)
  (match t
    [(Lf) #f]
    [(Br l r) (match l [(Empt) (leaf-cons-empty? r)] [(Cns _ _) #f])]))

;; ----- class used before its declaration ---------------------
(define (greet-int n) (mk-pretty n))

(protocol (MkPretty a)
          (: mk-pretty (-> a String)))

(instance (MkPretty Integer)
  (define (mk-pretty n) "an int"))

(: greet-result String)
(define greet-result (greet-int 42))

;; ----- instance declared before its class --------------------
(instance (Tagged BBox)
  (define (tag-of b) "BBox"))

(protocol (Tagged a)
          (: tag-of (-> a String)))

(data BBox MkBBox)

(: bbox-tag String)
(define bbox-tag (tag-of MkBBox))

;; ----- SCC-preserved polymorphism ----------------------------
(: use1 (Maybe Integer))
(define use1 (helper (Some 3)))

(: use2 (Maybe String))
(define use2 (helper (Some "x")))

(define (helper m)
  (match m
    [(Some v) (Some v)]
    [None     None]))

(: suite (List Test))
(define suite
  (list
    (it "forward reference between defs"
        (check-equal? forward-result 8))
    (it "mutually recursive defs"
        (all-checks
          (list (check-equal? even-7  #f)
                (check-equal? even-10 #t))))
    (it "mutually recursive data types"
        (all-checks
          (list (check-true (is-leaf? leaf))
                (check-true (leaf-cons-empty? leaf-forest))
                (check-true (branched-shape? branched)))))
    (it "class used before its declaration"
        (check-equal? greet-result "an int"))
    (it "instance declared before its class"
        (check-equal? bbox-tag "BBox"))
    (it "SCC-preserved polymorphism: helper used at two types"
        (all-checks
          (list (check-equal? use1 (Some 3))
                (check-equal? use2 (Some "x")))))))

(: test-main (IO Unit))
(define test-main (run-suite "order invariance" suite))
