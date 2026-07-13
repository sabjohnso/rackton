#lang rackton

;; constraint-kinds.rkt — computing constraints.
;;
;; A constraint SYNONYM names a conjunction of constraints; a constraint
;; FAMILY computes one from type structure (higher-order over the
;; constraint it imposes).  Both are expanded during constraint solving.
;;
;; Run it with `racket examples/constraint-kinds.rkt`.

;; ----- a synonym: `Display a` = `Show a` + `Eq a` -------------------

(define-constraint (Display a) (Show a) (Eq a))

;; `tell` needs both components; the `Display a` context provides them.
(: tell ((Display a) => (-> a a String)))
(define (tell x y)
  (if (== x y)
    (string-append "both equal " (show x))
    (string-append (show x) (string-append " vs " (show y)))))

;; ----- a higher-order family: `All c xs` ---------------------------
;; "constraint c holds of every element of the promoted list xs".

(data (TList a) TNil (TCons a (TList a)))

(constraint-family (All c xs)
                   [c TNil         = ]
                   [c (TCons x xs) = (c x) (All c xs)])

(data (Proxy a) MkProxy)

;; `all-showable` is callable only for a list whose every element has a
;; `Show` instance — `(All Show xs)` reduces to one `Show` per element.
(: all-showable ((All Show xs) => (-> (Proxy xs) String)))
(define (all-showable p) "every element is Showable")

(: pr (Proxy (TCons Integer (TCons String TNil))))
(define pr MkProxy)

(: main (IO Unit))
(define main
  (let& ([_ (println (tell 7 7))]
         [_ (println (tell 7 9))])
    (println (all-showable pr))))
