#lang rackton

;; Traversable via dict-passing.

(require "../unit.rkt")

;; ----- traverse over Maybe, into Maybe ------------------------
(: parse-positive (-> Integer (Maybe Integer)))
(define (parse-positive n)
  (if (> n 0) (Some n) None))

(: maybe-of-maybe-some (Maybe (Maybe Integer)))
(define maybe-of-maybe-some (traverse parse-positive (Some 5)))

(: maybe-of-maybe-none (Maybe (Maybe Integer)))
(define maybe-of-maybe-none (traverse parse-positive None))

(: maybe-of-maybe-fail (Maybe (Maybe Integer)))
(define maybe-of-maybe-fail (traverse parse-positive (Some -1)))

;; ----- traverse over List, into Maybe -------------------------
(: maybe-of-list-all (Maybe (List Integer)))
(define maybe-of-list-all
  (traverse parse-positive (Cons 1 (Cons 2 (Cons 3 Nil)))))

(: maybe-of-list-fail (Maybe (List Integer)))
(define maybe-of-list-fail
  (traverse parse-positive (Cons 1 (Cons -2 (Cons 3 Nil)))))

(: maybe-of-list-empty (Maybe (List Integer)))
(define maybe-of-list-empty (traverse parse-positive Nil))

;; ----- traverse over List, into Either ------------------------
(: keep-or-err (-> Integer (Either String Integer)))
(define (keep-or-err n)
  (if (> n 0) (Right n) (Left "non-positive")))

(: result-of-list-all (Either String (List Integer)))
(define result-of-list-all
  (traverse keep-or-err (Cons 5 (Cons 6 Nil))))

(: result-of-list-fail (Either String (List Integer)))
(define result-of-list-fail
  (traverse keep-or-err (Cons 5 (Cons -6 Nil))))

(: suite (List Test))
(define suite
  (list
   (it "traverse Maybe into Maybe (Some, success)"
       (check-equal? maybe-of-maybe-some (Some (Some 5))))
   (it "traverse Maybe into Maybe (None preserved)"
       (check-equal? maybe-of-maybe-none (Some None)))
   (it "traverse Maybe into Maybe (short-circuit on inner None)"
       (check-equal? maybe-of-maybe-fail None))
   (it "traverse List into Maybe (all succeed)"
       (check-equal? maybe-of-list-all
                     (Some (Cons 1 (Cons 2 (Cons 3 Nil))))))
   (it "traverse List into Maybe (short-circuit on inner None)"
       (check-equal? maybe-of-list-fail None))
   (it "traverse List into Maybe (empty list -> pure Nil)"
       (check-equal? maybe-of-list-empty (Some Nil)))
   (it "traverse List into Either (all succeed)"
       (check-equal? result-of-list-all (Right (Cons 5 (Cons 6 Nil)))))
   (it "traverse List into Either (short-circuit on Left)"
       (check-equal? result-of-list-fail (Left "non-positive")))))

(: main Unit)
(define main (run-io (run-suite "traversable" suite)))
