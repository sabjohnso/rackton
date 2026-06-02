#lang rackton

;; rackton/data/either — Data.Either over the prelude's Result type
;; (Err = Left, Ok = Right).

(require rackton/data/either
         "../unit.rkt")

(: r-either-ok  String) (define r-either-ok
  (either (lambda (e) (<> "err:" e)) (lambda (a) (<> "ok:" a)) (ann (Ok "x") (Result String String))))
(: r-either-err String) (define r-either-err
  (either (lambda (e) (<> "err:" e)) (lambda (a) (<> "ok:" a)) (ann (Err "y") (Result String String))))

(: r-is-ok  Boolean) (define r-is-ok  (is-ok  (ann (Ok 1) (Result String Integer))))
(: r-is-err Boolean) (define r-is-err (is-err (ann (Err "e") (Result String Integer))))

(: r-from-ok  Integer) (define r-from-ok  (from-ok  0 (ann (Ok 5) (Result String Integer))))
(: r-from-ok0 Integer) (define r-from-ok0 (from-ok  0 (ann (Err "e") (Result String Integer))))
(: r-from-err String)  (define r-from-err (from-err "d" (ann (Err "e") (Result String Integer))))

(: r-oks  (List Integer)) (define r-oks
  (oks  (list (ann (Ok 1) (Result String Integer)) (Err "a") (Ok 2))))
(: r-errs (List String))  (define r-errs
  (errs (list (ann (Ok 1) (Result String Integer)) (Err "a") (Ok 2) (Err "b"))))

(: r-part (Pair (List String) (List Integer)))
(define r-part
  (partition-results (list (ann (Ok 1) (Result String Integer)) (Err "a") (Ok 2))))

(: r-to-maybe   (Maybe Integer)) (define r-to-maybe   (ok->maybe (ann (Ok 7) (Result String Integer))))
(: r-to-maybe0  (Maybe Integer)) (define r-to-maybe0  (ok->maybe (ann (Err "e") (Result String Integer))))
(: r-from-maybe (Result String Integer))
(define r-from-maybe (maybe->result "none" (Some 9)))

(: suite (List Test))
(define suite
  (list
   (it "either eliminator"
       (all-checks
        (list (check-equal? r-either-ok  "ok:x")
              (check-equal? r-either-err "err:y"))))
   (it "predicates"
       (all-checks
        (list (check-equal? r-is-ok  #t)
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
        (list (check-equal? r-to-maybe   (Some 7))
              (check-equal? r-to-maybe0  None)
              (check-equal? r-from-maybe (Ok 9)))))))

(: _ran Unit)
(define _ran (run-io (run-suite "rackton/data/either" suite)))
