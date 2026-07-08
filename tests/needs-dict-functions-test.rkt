#lang rackton

;; Body-rewriting for user-defined needs-dict functions.

(require rackton/data/monoid
         "../unit.rkt")

;; ----- User-defined Monoid fold ------------------------
(: my-concat ((Monoid a) => (-> (List a) a)))
(define (my-concat xs)
  (foldr (lambda (x acc) (mappend x acc)) mempty xs))

(: glued String)
(define glued (my-concat (Cons "a" (Cons "b" (Cons "c" Nil)))))

(: empty-strs String)
(define empty-strs (my-concat (ann Nil (List String))))

(: total Sum)
(define total (my-concat (Cons (Sum 3) (Cons (Sum 5) (Cons (Sum 7) Nil)))))

(: factorial Product)
(define factorial
  (my-concat (Cons (Product 2)
                   (Cons (Product 3)
                         (Cons (Product 4) Nil)))))

;; ----- User-defined Applicative helper -------------------
(: my-pure-pair ((Applicative f) => (-> a (f (Pair a a)))))
(define (my-pure-pair x)
  (pure (Pair x x)))

(: pair-maybe (Maybe (Pair Integer Integer)))
(define pair-maybe (my-pure-pair 42))

(: pair-list (List (Pair String String)))
(define pair-list (my-pure-pair "hi"))

(: pair-result (Either String (Pair Integer Integer)))
(define pair-result (my-pure-pair 7))

(: pair-io (IO (Pair Integer Integer)))
(define pair-io (my-pure-pair 1))

;; ----- Body mixes runtime-dispatch mappend and dict-passed mempty
(: wrap-with-empty ((Monoid a) => (-> a a)))
(define (wrap-with-empty x)
  (mappend (mappend mempty x) mempty))

(: wrapped-str String)
(define wrapped-str (wrap-with-empty "hello"))

(: wrapped-sum Sum)
(define wrapped-sum (wrap-with-empty (Sum 99)))

;; ---------- assertions -----------------------------------

(: suite (List Test))
(define suite
  (list
    (it "user my-concat on String"
        (check-equal? glued "abc"))
    (it "user my-concat on empty list with ascription"
        (check-equal? empty-strs ""))
    (it "user my-concat on Sum"
        (check-equal? (get-sum total) 15))
    (it "user my-concat on Product"
        (check-equal? (get-product factorial) 24))
    (it "user my-pure-pair into Maybe"
        (check-equal? pair-maybe (Some (Pair 42 42))))
    (it "user my-pure-pair into List"
        (check-equal? pair-list (Cons (Pair "hi" "hi") Nil)))
    (it "user my-pure-pair into Either"
        (check-equal? pair-result (Right (Pair 7 7))))
    (it "user my-pure-pair into IO"
        (check-equal? (run-io pair-io) (Pair 1 1)))
    (it "user body mixes runtime mappend and dict-passed mempty (String)"
        (check-equal? wrapped-str "hello"))
    (it "user body mixes runtime mappend and dict-passed mempty (Sum)"
        (check-equal? (get-sum wrapped-sum) 99))))

(: test-main (IO Unit))
(define test-main (run-suite "needs-dict-functions" suite))
