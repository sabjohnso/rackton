#lang racket/base

;; Inferred needs-dict generalization — polymorphic monadic/monoidal
;; *functions* without an explicit signature pick up dict-passing
;; through the same machinery the declared path uses.

(require rackunit
         "../main.rkt")

(rackton
  (require rackton/data/monoid)
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

  (: madd-result (Result String Integer))
  (define madd-result (madd (Ok 10) (Ok 32)))

  ;; ----- the unascribed counterpart of my-pure-pair --------
  (define (pair-pure x) (pure (MkPair x x)))

  (: pair-pure-maybe (Maybe (Pair Integer Integer)))
  (define pair-pure-maybe (pair-pure 42))

  (: pair-pure-result (Result String (Pair Integer Integer)))
  (define pair-pure-result (pair-pure 7))

  ;; ----- inferred Monoid fold (unascribed my-concat) -------
  (define (cat xs) (foldr (lambda (x acc) (<> x acc)) mempty xs))

  (: cat-string String)
  (define cat-string (cat (Cons "a" (Cons "b" (Cons "c" Nil)))))

  (: cat-sum Sum)
  (define cat-sum (cat (Cons (MkSum 3) (Cons (MkSum 5) Nil))))

  ;; ----- self-recursion in a needs-dict function -----------
  ;; replicate-pure n x = a singleton-list-like fold-up using pure/<>
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

  (: rep-result (Result String Integer))
  (define rep-result (replicate-pure 4 99)))

;; ---------- assertions -----------------------------------

(test-case "inferred madd over Maybe (Some/Some)"
  (check-equal? madd-maybe-some (Some 7)))

(test-case "inferred madd over Maybe (None short-circuits)"
  (check-equal? madd-maybe-none None))

(test-case "inferred madd over Result (Ok/Ok)"
  (check-equal? madd-result (Ok 42)))

(test-case "inferred pair-pure into Maybe"
  (check-equal? pair-pure-maybe (Some (MkPair 42 42))))

(test-case "inferred pair-pure into Result"
  (check-equal? pair-pure-result (Ok (MkPair 7 7))))

(test-case "inferred Monoid cat on String"
  (check-equal? cat-string "abc"))

(test-case "inferred Monoid cat on Sum"
  (check-equal? cat-sum (MkSum 8)))

(test-case "recursive needs-dict function over Maybe"
  (check-equal? rep-maybe (Some 7)))

(test-case "recursive needs-dict function over Result"
  (check-equal? rep-result (Ok 99)))
