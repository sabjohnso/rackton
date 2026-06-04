#lang rackton

;; Inferred needs-dict generalization — polymorphic monadic/monoidal
;; *functions* without an explicit signature pick up dict-passing
;; through the same machinery the declared path uses.

(require rackton/data/monoid
         "../unit.rkt")

;; ----- the motivating example: madd over any Monad -------
;; No `(: madd ...)` — inference should generalize and skolemize.
(define (madd mx my)
  (do [x <- mx]
      [y <- my]
    (pure (+ x y))))

(: madd-maybe-some (Maybe Integer))
(define madd-maybe-some (madd (Some 3) (Some 4)))

(: madd-maybe-none (Maybe Integer))
(define madd-maybe-none (madd None (Some 99)))

(: madd-result (Either String Integer))
(define madd-result (madd (Right 10) (Right 32)))

;; ----- the unascribed counterpart of my-pure-pair --------
(define (pair-pure x) (pure (Pair x x)))

(: pair-pure-maybe (Maybe (Pair Integer Integer)))
(define pair-pure-maybe (pair-pure 42))

(: pair-pure-result (Either String (Pair Integer Integer)))
(define pair-pure-result (pair-pure 7))

;; ----- inferred Monoid fold (unascribed my-concat) -------
(define (cat xs) (foldr (lambda (x acc) (mappend x acc)) mempty xs))

(: cat-string String)
(define cat-string (cat (Cons "a" (Cons "b" (Cons "c" Nil)))))

(: cat-sum Sum)
(define cat-sum (cat (Cons (Sum 3) (Cons (Sum 5) Nil))))

;; ----- self-recursion in a needs-dict function -----------
;; replicate-pure n x = a singleton-list-like fold-up using pure/mappend
;; In Maybe, this collapses to `Just x` when n > 0 (combining `Some x`
;; with `Some x` under First-style semantics).
;; Use a Monoid+Applicative pairing on Maybe to validate that the
;; recursive call correctly threads dict args through.
(define (replicate-pure n x)
  (if (== n 0)
      (pure x)
      (do [y <- (pure x)]
          [rest <- (replicate-pure (- n 1) x)]
        (pure y))))

(: rep-maybe (Maybe Integer))
(define rep-maybe (replicate-pure 3 7))

(: rep-result (Either String Integer))
(define rep-result (replicate-pure 4 99))

;; ---------- assertions -----------------------------------

(: suite (List Test))
(define suite
  (list
   (it "inferred madd over Maybe (Some/Some)"
       (check-equal? madd-maybe-some (Some 7)))
   (it "inferred madd over Maybe (None short-circuits)"
       (check-equal? madd-maybe-none None))
   (it "inferred madd over Either (Right/Right)"
       (check-equal? madd-result (Right 42)))
   (it "inferred pair-pure into Maybe"
       (check-equal? pair-pure-maybe (Some (Pair 42 42))))
   (it "inferred pair-pure into Either"
       (check-equal? pair-pure-result (Right (Pair 7 7))))
   (it "inferred Monoid cat on String"
       (check-equal? cat-string "abc"))
   (it "inferred Monoid cat on Sum"
       (check-equal? (get-sum cat-sum) 8))
   (it "recursive needs-dict function over Maybe"
       (check-equal? rep-maybe (Some 7)))
   (it "recursive needs-dict function over Either"
       (check-equal? rep-result (Right 99)))))

(: _ran Unit)
(define _ran (run-io (run-suite "needs-dict-inferred" suite)))
