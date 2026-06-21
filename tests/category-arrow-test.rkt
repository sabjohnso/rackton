#lang rackton

;; End-to-end tests for the Category / Arrow hierarchy and its canonical
;; instance on the function arrow `(->)`.
;;
;; Arrows generalize plain functions; for the `(->)` instance the
;; combinators reduce to ordinary function plumbing:
;;
;;   ident         = the identity function
;;   (comp f g)    = standard composition  (run g, then f)
;;   (arr f)       = f itself             (a function IS its own arrow)
;;   (on-first f)  = map f over the first  half of a Pair
;;   (on-second f) = map f over the second half of a Pair
;;   (split f g)   = run f and g on the two halves of a Pair
;;   (fanout f g)  = feed one input to both f and g, pairing the results
;;
;; Function values cannot be compared for equality, so the algebraic
;; laws are checked by applying both sides at sample points.

(require "../unit.rkt")

(: inc (-> Integer Integer))
(define (inc x) (+ x 1))

(: dbl (-> Integer Integer))
(define (dbl x) (* x 2))

;; ----- Category: ident / comp -------------------------------------

;; comp is standard (right-to-left) composition: the right arrow runs
;; first.  (comp (arr dbl) (arr inc)) = dbl ∘ inc = "inc, then dbl".
(: inc-then-dbl (-> Integer Integer))
(define (inc-then-dbl x) ((comp (arr dbl) (arr inc)) x))

;; ident is the two-sided identity of `comp`.
(: id-int (-> Integer Integer))
(define id-int ident)

;; ----- Arrow combinators over Pair ---------------------------------

(: first-inc (-> (Pair Integer Integer) (Pair Integer Integer)))
(define (first-inc p) ((on-first (arr inc)) p))

(: second-inc (-> (Pair Integer Integer) (Pair Integer Integer)))
(define (second-inc p) ((on-second (arr inc)) p))

(: split-incdbl (-> (Pair Integer Integer) (Pair Integer Integer)))
(define (split-incdbl p) ((split (arr inc) (arr dbl)) p))

(: fanout-incdbl (-> Integer (Pair Integer Integer)))
(define (fanout-incdbl n) ((fanout (arr inc) (arr dbl)) n))

;; ----- ArrowChoice over Either (Left / Right branches) ------------

(: left-inc (-> (Either Integer Integer) (Either Integer Integer)))
(define (left-inc r) ((on-left (arr inc)) r))

(: right-inc (-> (Either Integer Integer) (Either Integer Integer)))
(define (right-inc r) ((on-right (arr inc)) r))

(: fork-incdbl (-> (Either Integer Integer) (Either Integer Integer)))
(define (fork-incdbl r) ((fork (arr inc) (arr dbl)) r))

(: fanin-incdbl (-> (Either Integer Integer) Integer))
(define (fanin-incdbl r) ((fanin (arr inc) (arr dbl)) r))

;; ----- ArrowApply --------------------------------------------------

(: apply-arrow (-> (Pair (-> Integer Integer) Integer) Integer))
(define (apply-arrow p) (arrow-app p))

;; ----- value-level checks ------------------------------------------

(: suite (List Test))
(define suite
  (list
   (it "arr over (->) is the function itself"
       (check-equal? ((arr inc) 4) 5))
   (it "comp is standard composition (the right arrow runs first)"
       (all-checks
        (list (check-equal? (inc-then-dbl 3) 8)     ; inc 3 = 4, dbl 4 = 8
              (check-equal? (inc-then-dbl 0) 2))))
   (it "ident is the identity arrow"
       (check-equal? (id-int 7) 7))
   (it "Category left identity: comp ident f = f"
       (check-equal? ((comp ident (arr inc)) 9) (inc 9)))
   (it "Category right identity: comp f ident = f"
       (check-equal? ((comp (arr inc) ident) 9) (inc 9)))
   (it "Category associativity at a point"
       (check-equal?
        ((comp (comp (arr inc) (arr dbl)) (arr inc)) 3)
        ((comp (arr inc) (comp (arr dbl) (arr inc))) 3)))
   (it "on-first maps the first half of a Pair"
       (check-equal? (first-inc (Pair 3 100)) (Pair 4 100)))
   (it "on-second maps the second half of a Pair"
       (check-equal? (second-inc (Pair 100 3)) (Pair 100 4)))
   (it "split runs both arrows on the two halves"
       (check-equal? (split-incdbl (Pair 3 5)) (Pair 4 10)))
   (it "fanout feeds one input to both arrows"
       (check-equal? (fanout-incdbl 3) (Pair 4 6)))
   (it "arr respects composition: arr (f∘g) = comp (arr f) (arr g)"
       (check-equal?
        ((arr (lambda (x) (inc (dbl x)))) 5)
        ((comp (arr inc) (arr dbl)) 5)))
   ;; ArrowChoice — Left is the Left/active branch, Right the Right.
   (it "on-left maps the Left branch, passes Right through"
       (all-checks
        (list (check-equal? (left-inc (Left 3)) (Left 4))
              (check-equal? (left-inc (Right 3))  (Right 3)))))
   (it "on-right maps the Right branch, passes Left through"
       (all-checks
        (list (check-equal? (right-inc (Right 3))  (Right 4))
              (check-equal? (right-inc (Left 3)) (Left 3)))))
   (it "fork runs one arrow per branch"
       (all-checks
        (list (check-equal? (fork-incdbl (Left 3)) (Left 4))
              (check-equal? (fork-incdbl (Right 3))  (Right 6)))))
   (it "fanin collapses both branches to one result"
       (all-checks
        (list (check-equal? (fanin-incdbl (Left 3)) 4)
              (check-equal? (fanin-incdbl (Right 3))  6))))
   ;; ArrowApply — feeds an arrow and its argument through `arrow-app`.
   (it "arrow-app applies a captured arrow to its argument"
       (check-equal? (apply-arrow (Pair inc 5)) 6))))

(: main Unit)
(define main (run-io (run-suite "category-arrow" suite)))
