#lang rackton

;; rackton/data/result — the stdlib Result type (Err/Ok), its
;; Functor/Applicative/Monad/Bifunctor/Eq/Show instances, the Ok/Err
;; helpers, and the result<->either conversions.

(require rackton/data/result
         "../unit.rkt")

;; ----- eliminator / predicates / extraction -----------------------

(: r-elim-ok  String) (define r-elim-ok
  (result (lambda (e) (mappend "err:" e)) (lambda (a) (mappend "ok:" a)) (ann (Ok "x") (Result String String))))
(: r-elim-err String) (define r-elim-err
  (result (lambda (e) (mappend "err:" e)) (lambda (a) (mappend "ok:" a)) (ann (Err "y") (Result String String))))

(: r-is-ok  Boolean) (define r-is-ok  (is-ok  (ann (Ok 1)    (Result String Integer))))
(: r-is-err Boolean) (define r-is-err (is-err (ann (Err "e") (Result String Integer))))

(: r-from-ok  Integer) (define r-from-ok  (from-ok  0 (ann (Ok 5)    (Result String Integer))))
(: r-from-ok0 Integer) (define r-from-ok0 (from-ok  0 (ann (Err "e") (Result String Integer))))
(: r-from-err String)  (define r-from-err (from-err "d" (ann (Err "e") (Result String Integer))))

(: r-oks  (List Integer)) (define r-oks
  (oks  (list (ann (Ok 1) (Result String Integer)) (Err "a") (Ok 2))))
(: r-errs (List String))  (define r-errs
  (errs (list (ann (Ok 1) (Result String Integer)) (Err "a") (Ok 2) (Err "b"))))
(: r-part (Pair (List String) (List Integer)))
(define r-part
  (partition-results (list (ann (Ok 1) (Result String Integer)) (Err "a") (Ok 2))))

(: r-to-maybe  (Maybe Integer)) (define r-to-maybe  (ok->maybe (ann (Ok 7)    (Result String Integer))))
(: r-to-maybe0 (Maybe Integer)) (define r-to-maybe0 (ok->maybe (ann (Err "e") (Result String Integer))))
(: r-from-maybe (Result String Integer)) (define r-from-maybe (maybe->result "none" (Some 9)))

;; ----- class instances --------------------------------------------

(: r-fmap-ok  (Result String Integer)) (define r-fmap-ok  (fmap (lambda (x) (+ x 1)) (ann (Ok 5)    (Result String Integer))))
(: r-fmap-err (Result String Integer)) (define r-fmap-err (fmap (lambda (x) (+ x 1)) (ann (Err "e") (Result String Integer))))

(: r-pure (Result String Integer)) (define r-pure (pure 7))

(: r-bind-ok  (Result String Integer))
(define r-bind-ok  (flatmap (lambda (x) (Ok (* x 2))) (ann (Ok 5) (Result String Integer))))
(: r-bind-err (Result String Integer))
(define r-bind-err (flatmap (lambda (x) (Ok (* x 2))) (ann (Err "e") (Result String Integer))))

(: r-bimap (Result String Integer))
(define r-bimap (bimap (lambda (s) (mappend s "!")) (lambda (x) (+ x 100)) (ann (Err "e") (Result String Integer))))

(: r-eq-yes Boolean) (define r-eq-yes (== (ann (Ok 1) (Result String Integer)) (Ok 1)))
(: r-eq-no  Boolean) (define r-eq-no  (== (ann (Ok 1) (Result String Integer)) (Err "e")))

(: r-show-ok  String) (define r-show-ok  (show (ann (Ok 5)    (Result String Integer))))
(: r-show-err String) (define r-show-err (show (ann (Err "e") (Result String Integer))))

;; ----- conversions to/from Either ---------------------------------

(: r->e-ok  (Either String Integer)) (define r->e-ok  (result->either (ann (Ok 5)    (Result String Integer))))
(: r->e-err (Either String Integer)) (define r->e-err (result->either (ann (Err "e") (Result String Integer))))
(: e->r-rt  (Either String Integer)) (define e->r-rt  (result->either (either->result r->e-ok)))

(: suite (List Test))
(define suite
  (list
   (it "result eliminator"
       (all-checks
        (list (check-equal? r-elim-ok  "ok:x")
              (check-equal? r-elim-err "err:y"))))
   (it "predicates"
       (all-checks
        (list (check-equal? r-is-ok #t)
              (check-equal? r-is-err #t))))
   (it "extraction with default"
       (all-checks
        (list (check-equal? r-from-ok  5)
              (check-equal? r-from-ok0 0)
              (check-equal? r-from-err "e"))))
   (it "collecting"
       (all-checks
        (list (check-equal? r-oks  (Cons 1 (Cons 2 Nil)))
              (check-equal? r-errs (Cons "a" (Cons "b" Nil)))
              (check-equal? r-part (Pair (Cons "a" Nil) (Cons 1 (Cons 2 Nil)))))))
   (it "Maybe interop"
       (all-checks
        (list (check-equal? r-to-maybe  (Some 7))
              (check-equal? r-to-maybe0 None)
              (check-equal? r-from-maybe (Ok 9)))))
   (it "Functor"
       (all-checks
        (list (check-equal? r-fmap-ok  (Ok 6))
              (check-equal? r-fmap-err (Err "e")))))
   (it "Applicative pure"
       (check-equal? r-pure (Ok 7)))
   (it "Monad flatmap"
       (all-checks
        (list (check-equal? r-bind-ok  (Ok 10))
              (check-equal? r-bind-err (Err "e")))))
   (it "Bifunctor bimap"
       (check-equal? r-bimap (Err "e!")))
   (it "Eq"
       (all-checks
        (list (check-equal? r-eq-yes #t)
              (check-equal? r-eq-no  #f))))
   (it "Show"
       (all-checks
        (list (check-equal? r-show-ok  "(Ok 5)")
              (check-equal? r-show-err "(Err \"e\")"))))
   (it "Either conversions"
       (all-checks
        (list (check-equal? r->e-ok  (Right 5))
              (check-equal? r->e-err (Left "e"))
              (check-equal? e->r-rt  (Right 5)))))))

(: main Unit)
(define main (run-io (run-suite "rackton/data/result" suite)))
