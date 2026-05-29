#lang racket/base

;; Body-rewriting for user-defined needs-dict functions.

(require rackunit
         "../main.rkt")

(rackton
  (require rackton/data/monoid)
  ;; ----- User-defined Monoid fold ------------------------
  (: my-concat ((Monoid a) => (-> (List a) a)))
  (define (my-concat xs)
    (foldr (lambda (x acc) (<> x acc)) mempty xs))

  (: glued String)
  (define glued (my-concat (Cons "a" (Cons "b" (Cons "c" Nil)))))

  (: empty-strs String)
  (define empty-strs (my-concat (ann Nil (List String))))

  (: total Sum)
  (define total (my-concat (Cons (MkSum 3) (Cons (MkSum 5) (Cons (MkSum 7) Nil)))))

  (: factorial Product)
  (define factorial
    (my-concat (Cons (MkProduct 2)
                     (Cons (MkProduct 3)
                           (Cons (MkProduct 4) Nil)))))

  ;; ----- User-defined Applicative helper -------------------
  (: my-pure-pair ((Applicative f) => (-> a (f (Pair a a)))))
  (define (my-pure-pair x)
    (pure (MkPair x x)))

  (: pair-maybe (Maybe (Pair Integer Integer)))
  (define pair-maybe (my-pure-pair 42))

  (: pair-list (List (Pair String String)))
  (define pair-list (my-pure-pair "hi"))

  (: pair-result (Result String (Pair Integer Integer)))
  (define pair-result (my-pure-pair 7))

  (: pair-io (IO (Pair Integer Integer)))
  (define pair-io (my-pure-pair 1))

  ;; ----- Body mixes runtime-dispatch <> and dict-passed mempty
  (: wrap-with-empty ((Monoid a) => (-> a a)))
  (define (wrap-with-empty x)
    (<> (<> mempty x) mempty))

  (: wrapped-str String)
  (define wrapped-str (wrap-with-empty "hello"))

  (: wrapped-sum Sum)
  (define wrapped-sum (wrap-with-empty (MkSum 99))))

;; ---------- assertions -----------------------------------

(test-case "user my-concat on String"
  (check-equal? glued "abc"))

(test-case "user my-concat on empty list with ascription"
  (check-equal? empty-strs ""))

(test-case "user my-concat on Sum"
  (check-equal? total (MkSum 15)))

(test-case "user my-concat on Product"
  (check-equal? factorial (MkProduct 24)))

(test-case "user my-pure-pair into Maybe"
  (check-equal? pair-maybe (Some (MkPair 42 42))))

(test-case "user my-pure-pair into List"
  (check-equal? pair-list (Cons (MkPair "hi" "hi") Nil)))

(test-case "user my-pure-pair into Result"
  (check-equal? pair-result (Ok (MkPair 7 7))))

(test-case "user my-pure-pair into IO"
  (check-equal? (run-io pair-io) (MkPair 1 1)))

(test-case "user body mixes runtime <> and dict-passed mempty (String)"
  (check-equal? wrapped-str "hello"))

(test-case "user body mixes runtime <> and dict-passed mempty (Sum)"
  (check-equal? wrapped-sum (MkSum 99)))
