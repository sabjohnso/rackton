#lang racket/base

;; tests/doc-examples-test.rkt — exercise the code samples shown in
;; the Guide that we recently corrected, so the docs don't drift back
;; out of sync with the parser / type-checker / runtime.
;;
;; Each `rackton` block below is a near-verbatim copy of a code block
;; from one of the Guide chapters.  If any of them stops parsing,
;; type-checking, or running, that doc page has lied about the
;; language and this test points at it.

(require rackunit
         "../main.rkt")

;; ----- quickstart.scrbl: fact infers (-> Integer Integer) ----------

(rackton
  (define (fact n)
    (if (= n 0) 1 (* n (fact (- n 1)))))
  (: fact-result Integer)
  (define fact-result (fact 5)))

(test-case "quickstart fact example"
  (check-equal? fact-result 120))

;; ----- advanced-types.scrbl: existential with #:forall / #:where ---

(rackton
  (data ExistsShow1
    (PackShow1 #:forall (a) #:where (Show a) a))

  (: exhibit (List ExistsShow1))
  (define exhibit
    (Cons (PackShow1 42) (Cons (PackShow1 "hi") Nil)))

  (: render-all (-> (List ExistsShow1) (List String)))
  (define (render-all xs)
    (match xs
      [(Nil) Nil]
      [(Cons (PackShow1 x) rest)
       (Cons (show x) (render-all rest))]))

  (: rendered (List String))
  (define rendered (render-all exhibit)))

(test-case "advanced-types existential example"
  (check-equal? rendered (Cons "42" (Cons "\"hi\"" Nil))))

;; ----- advanced-types.scrbl: associated type with #:type clauses ---

(rackton
  (protocol (Container1 c)
    (#:type Elem1)
    (: empty1? (-> c Boolean))
    (: head1   (-> c (Maybe (Elem1 c)))))

  (instance (Container1 (List a))
    (#:type (Elem1 = a))
    (define (empty1? xs) (match xs [(Nil) #t] [(Cons _ _) #f]))
    (define (head1   xs) (match xs [(Nil) None] [(Cons h _) (Some h)])))

  (: assoc-head (Maybe Integer))
  (define assoc-head (head1 (Cons 7 (Cons 8 Nil)))))

(test-case "advanced-types associated-type example"
  (check-equal? assoc-head (Some 7)))

;; ----- optics.scrbl: lens deriving + naming ------------------------

(rackton
  (require rackton/data/lens)
  (struct DocPoint
    [x : Integer]
    [y : Integer]
    #:deriving Lens)

  (: p0 DocPoint)
  (define p0 (DocPoint 3 4))

  (: lens-view-x Integer)
  (define lens-view-x (view DocPoint-x-lens p0))

  (: lens-set-x DocPoint)
  (define lens-set-x (set DocPoint-x-lens 99 p0)))

(test-case "optics lens example"
  (check-equal? lens-view-x 3)
  (check-equal? (DocPoint-x lens-set-x) 99)
  (check-equal? (DocPoint-y lens-set-x) 4))

;; ----- optics.scrbl: prism deriving + naming -----------------------

(rackton
  (require rackton/data/lens)
  (data DocOpt
    DocAbsent
    (DocPresent Integer)
    #:deriving Prism)

  (: preview-present (Maybe Integer))
  (define preview-present (preview DocOpt-DocPresent-prism (DocPresent 7)))

  (: preview-absent (Maybe Integer))
  (define preview-absent (preview DocOpt-DocPresent-prism DocAbsent))

  (: review-result DocOpt)
  (define review-result (review DocOpt-DocPresent-prism 42)))

(test-case "optics prism example"
  (check-equal? preview-present (Some 7))
  (check-equal? preview-absent None)
  (check-equal? review-result (DocPresent 42)))

;; ----- do-and-monads.scrbl: concat-map, the standalone List bind -----
;;
;; List is a Monad (flatmap is concatMap); concat-map is the same
;; operation as a named function with list-comprehension argument order.

(rackton
  (require rackton/data/list)
  (: doubled (List Integer))
  (define doubled
    (concat-map (lambda (x) (Cons x (Cons x Nil)))
                (Cons 1 (Cons 2 Nil)))))

(test-case "do-and-monads concat-map substitute"
  (check-equal? doubled (Cons 1 (Cons 1 (Cons 2 (Cons 2 Nil))))))

;; ----- do-and-monads.scrbl: pure picks instance from return type ---

(rackton
  (: greet (IO Unit))
  (define greet (pure Unit))

  (: many (Maybe Integer))
  (define many (pure 3))

  (: ann-pure (Maybe Integer))
  (define ann-pure ((lambda (x) x) (ann (pure 5) (Maybe Integer)))))

(test-case "do-and-monads pure example"
  (check-equal? many (Some 3))
  (check-equal? ann-pure (Some 5)))

;; ----- values.scrbl: flip and compose are typed and usable ---------

(rackton
  (: subtract (-> Integer (-> Integer Integer)))
  (define (subtract x) (lambda (y) (- x y)))

  (: flipped Integer)
  (define flipped (((flip subtract) 3) 10))   ;; (subtract 10 3) = 7

  (: add1 (-> Integer Integer))
  (define (add1 x) (+ x 1))

  (: mul2 (-> Integer Integer))
  (define (mul2 x) (* x 2))

  (: composed Integer)
  (define composed (((compose add1) mul2) 3)))   ;; add1(mul2(3)) = 7

(test-case "values flip and compose are typed prelude bindings"
  (check-equal? flipped  7)
  (check-equal? composed 7))

;; ----- foreign.scrbl: import a host binding with `foreign` ----------

(rackton
  (foreign str-replace (-> String (-> String (-> String String)))
           #:from racket/string #:as string-replace)

  (: slashify (-> String String))
  (define (slashify s) (str-replace s "." "/"))

  (: slashified String)
  (define slashified (slashify "a.b.c")))

(test-case "foreign import (racket/string string-replace) works"
  (check-equal? slashified "a/b/c"))

;; ----- higher-kinded.scrbl: Arrows section -------------------------

(rackton
  (: inc-then-double (-> Integer Integer))
  (define inc-then-double
    ;; comp is right-to-left: the second arrow (+1) runs first, then (*2).
    (comp (arr (lambda (n) (* n 2)))
          (arr (lambda (n) (+ n 1)))))

  (: id2-result Integer)
  (define id2-result (inc-then-double 5))

  (: sum-with-succ (-> Integer Integer))
  (define sum-with-succ
    (proc (x)
      [y <- (feed (arr (lambda (n) (+ n 1))) x)]
      (feed (arr (lambda (p) (match p [(Pair a b) (+ a b)]))) (Pair x y))))

  (: sws-result Integer)
  (define sws-result (sum-with-succ 3)))

(test-case "higher-kinded Arrows examples"
  (check-equal? id2-result 12)    ; (5+1)*2
  (check-equal? sws-result 7))    ; x=3, y=4, x+y=7
;; ----- standard-library.scrbl: Laziness section -------------------

(rackton
  (require rackton/data/lazy)

  (: slow (Lazy Integer))
  (define slow (delay (* 6 7)))

  (: answer Integer)
  (define answer (force slow))

  (: nats (Stream Integer))
  (define nats (stream-iterate (lambda (n) (+ n 1)) 0))

  (: first5 (List Integer))
  (define first5 (stream-take 5 nats)))

(test-case "stdlib Laziness examples"
  (check-equal? answer 42)
  (check-equal? first5 (Cons 0 (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))))))
