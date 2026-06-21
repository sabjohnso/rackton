#lang rackton

;; Associated types (type families).
;;
;; A class may declare a `#:type FamilyName`; each instance supplies
;; a concrete rhs via `#:type (FamilyName = T)`.  Type applications
;; like `(FamilyName c)` reduce eagerly to the instance's rhs once
;; `c` is concrete enough for instance selection.

(require "../unit.rkt")

;; ----- 53.A Sized class with associated Index type -----------
(protocol (Sized (c :: *))
  (#:type Index)
  (: size-of (-> c (Index c))))

;; Concrete List instance: Index resolves to Integer.
(instance (Sized (List a))
  (#:type (Index = Integer))
  (define (size-of xs) (length xs)))

(: r-list-size Integer)
(define r-list-size (size-of (Cons 1 (Cons 2 (Cons 3 Nil)))))

;; ----- 53.B distinct instance with distinct Index -----------
(data MyMap (MkMap Integer))

(instance (Sized MyMap)
  (#:type (Index = MyMap))
  (define (size-of m) m))

(: r-map-self MyMap)
(define r-map-self (size-of (MkMap 99)))

;; ----- assertions ------------------------------------------------

(: suite (List Test))
(define suite
  (list
   (it "associated type Index — List instance resolves to Integer"
       (check-equal? r-list-size 3))
   (it "associated type Index — alternate instance resolves correctly"
       (check-equal? (match r-map-self
                       [(MkMap n) n])
                     99))))

(: main Unit)
(define main (run-io (run-suite "associated-types" suite)))
