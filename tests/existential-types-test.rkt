#lang rackton

;; Existential types — ctor fields with their own
;; quantifier and constraints, hidden from the outer type.

(require "../unit.rkt")

;; ----- heterogeneous Show ------------------------------

(data ExistsShow
  (PackShow :forall (a) :where (Show a) a))

;; Construct a list of heterogeneous showable values.
(: heterogeneous (List ExistsShow))
(define heterogeneous
  (Cons (PackShow 42)
        (Cons (PackShow "hello")
              (Cons (PackShow #t)
                    Nil))))

;; Render each via the existential's Show instance.
(: render-each (-> (List ExistsShow) (List String)))
(define (render-each xs)
  (match xs
    [(Nil) Nil]
    [(Cons (PackShow x) rest)
     (Cons (show x) (render-each rest))]))

(: rendered (List String))
(define rendered (render-each heterogeneous))

;; ----- unconstrained existential ------------------------
;; No `:where` clause: the hidden type carries no protocol
;; constraint, so the ctor packs both a value and any function
;; it needs applied to it (guide section 9.1's `Anything` example).

(data Anything
  (Wrap :forall (a) a (-> a String)))

(: render-anything (-> Anything String))
(define (render-anything pkg)
  (match pkg
    [(Wrap v to-string) (to-string v)]))

(: described (List String))
(define described
  (Cons (render-anything (Wrap 42 show))
        (Cons (render-anything (Wrap "hi" id))
              Nil)))

;; ----- existential Eq ----------------------------------
;; Pack a value with its own Eq witness; compare against itself.

(data ExistsEq
  (PackEq :forall (a) :where (Eq a) a))

;; Self-comparison: the bound value compared to itself is always
;; true (since the SAME existential witness is in scope).
(: self-eq (-> ExistsEq Boolean))
(define (self-eq e)
  (match e
    [(PackEq x) (== x x)]))

(: self-eq-int Boolean)
(define self-eq-int (self-eq (PackEq 7)))

(: self-eq-str Boolean)
(define self-eq-str (self-eq (PackEq "abc")))

;; ---------- assertions ---------------------------------------

(: suite (List Test))
(define suite
  (list
    (it "heterogeneous Show list via existential"
        ;; Show on String includes quotes (round-trippable), matching
        ;; the existing prelude convention.
        (check-equal? rendered
                      (Cons "42" (Cons "\"hello\"" (Cons "True" Nil)))))
    (it "existential Eq: x == x"
        (all-checks
          (list (check-true self-eq-int)
                (check-true self-eq-str))))
    (it "unconstrained existential packs a value with its own function"
        (check-equal? described (Cons "42" (Cons "hi" Nil))))))

(: test-main (IO Unit))
(define test-main (run-suite "existential-types" suite))
