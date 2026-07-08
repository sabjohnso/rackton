#lang rackton

;; type-families.rkt — closed and open standalone type families.
;;
;; A type family is a type-level function, reduced during type checking.
;; A CLOSED family lists ordered equations; an OPEN family is declared
;; empty and extended by separate `type-instance` equations.  Here a
;; closed family picks a result type from a promoted flag, and an open
;; family names the cell type of each table column.
;;
;; Run it with `racket examples/type-families.rkt`.

;; ----- closed family: choose a type by a promoted Flag --------------

(data Flag On Off)

(type-family (When b t e)
             [On  t e = t]
             [Off t e = e])

;; (When On Integer String) reduces to Integer; (When Off …) to String,
;; so each definition's value must match the reduced type.
(: chosen-int (When On Integer String))
(define chosen-int 42)

(: chosen-str (When Off Integer String))
(define chosen-str "off")

;; ----- open family: each column type names its cell type ------------

(data NameCol)
(data AgeCol)

(type-family (Cell c))
(type-instance (Cell NameCol) = String)
(type-instance (Cell AgeCol)  = Integer)

(: a-name (Cell NameCol))
(define a-name "Ada")

(: an-age (Cell AgeCol))
(define an-age 36)

;; ----- show the reduced values --------------------------------------

(: main (IO Unit))
(define main
  (do [_ <- (println (string-append "When On  ⇒ " (show chosen-int)))]
    [_ <- (println (string-append "When Off ⇒ " chosen-str))]
    [_ <- (println (string-append "Cell NameCol = " a-name))]
    (println (string-append "Cell AgeCol  = " (show an-age)))))
