#lang rackton

;; rackton/data/either — Data.Either over the prelude's Either type
;; (Left / Right).  The class instances live in the prelude; these are
;; the non-class eliminators, predicates, and collectors.

(require rackton/data/either
         "../unit.rkt")

(: r-either-l String) (define r-either-l
  (either (lambda (e) (mappend "left:" e)) (lambda (a) (mappend "right:" a)) (ann (Left "x") (Either String String))))
(: r-either-r String) (define r-either-r
  (either (lambda (e) (mappend "left:" e)) (lambda (a) (mappend "right:" a)) (ann (Right "y") (Either String String))))

(: r-is-right Boolean) (define r-is-right (is-right (ann (Right 1)  (Either String Integer))))
(: r-is-left  Boolean) (define r-is-left  (is-left  (ann (Left "e") (Either String Integer))))

(: r-from-right  Integer) (define r-from-right  (from-right 0 (ann (Right 5)  (Either String Integer))))
(: r-from-right0 Integer) (define r-from-right0 (from-right 0 (ann (Left "e") (Either String Integer))))
(: r-from-left   String)  (define r-from-left   (from-left "d" (ann (Left "e") (Either String Integer))))

(: r-rights (List Integer)) (define r-rights
  (rights (list (ann (Right 1) (Either String Integer)) (Left "a") (Right 2))))
(: r-lefts  (List String))  (define r-lefts
  (lefts  (list (ann (Right 1) (Either String Integer)) (Left "a") (Right 2) (Left "b"))))

(: r-part (Pair (List String) (List Integer)))
(define r-part
  (partition-eithers (list (ann (Right 1) (Either String Integer)) (Left "a") (Right 2))))

(: r-to-maybe  (Maybe Integer)) (define r-to-maybe  (right->maybe (ann (Right 7) (Either String Integer))))
(: r-to-maybe0 (Maybe Integer)) (define r-to-maybe0 (right->maybe (ann (Left "e") (Either String Integer))))
(: r-from-maybe (Either String Integer))
(define r-from-maybe (maybe->either "none" (Some 9)))

(: suite (List Test))
(define suite
  (list
   (it "either eliminator"
       (all-checks
        (list (check-equal? r-either-r "right:y")
              (check-equal? r-either-l "left:x"))))
   (it "predicates"
       (all-checks
        (list (check-equal? r-is-right #t)
              (check-equal? r-is-left  #t))))
   (it "extraction with default"
       (all-checks
        (list (check-equal? r-from-right  5)
              (check-equal? r-from-right0 0)
              (check-equal? r-from-left   "e"))))
   (it "collecting"
       (all-checks
        (list (check-equal? r-rights (Cons 1 (Cons 2 Nil)))
              (check-equal? r-lefts  (Cons "a" (Cons "b" Nil)))
              (check-equal? r-part (Pair (Cons "a" Nil) (Cons 1 (Cons 2 Nil)))))))
   (it "Maybe interop"
       (all-checks
        (list (check-equal? r-to-maybe   (Some 7))
              (check-equal? r-to-maybe0  None)
              (check-equal? r-from-maybe (Right 9)))))))

(: main Unit)
(define main (run-io (run-suite "rackton/data/either" suite)))
