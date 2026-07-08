#lang rackton

;; rackton/control/apply — FunctorApply (the "Apply" class): a Functor
;; that supports application (`apply` / `<.>`) but, unlike Applicative,
;; carries no `pure`.  Standalone in the hierarchy: its only superclass
;; is Functor, and it sits parallel to Applicative rather than below it.

(require rackton/control/apply
         "../unit.rkt")

;; --- apply over Maybe (cartesian, matching its Applicative) ----------
(: m-apply (Maybe Integer))
(define m-apply (apply (Some (lambda (x) (+ x 1))) (Some 41)))

(: m-apply-none (Maybe Integer))
(define m-apply-none (apply None (Some 41)))

;; --- liftF2 (derived from apply) ------------------------------------
(: m-lift (Maybe Integer))
(define m-lift (liftF2 (lambda (a) (lambda (b) (+ a b))) (Some 2) (Some 3)))

;; --- apply over List (cartesian) ------------------------------------
(: l-apply (List Integer))
(define l-apply
  (apply (Cons (lambda (x) (+ x 1)) (Cons (lambda (x) (* x 10)) Nil))
         (Cons 4 (Cons 5 Nil))))

;; --- apply over Either (right-biased) -------------------------------
(: e-apply (Either String Integer))
(define e-apply (apply (Right (lambda (x) (+ x 1))) (Right 41)))

;; --- apply over Identity --------------------------------------------
(: i-apply Integer)
(define i-apply
  (run-identity (apply (Identity (lambda (x) (+ x 1))) (Identity 41))))

(: suite (List Test))
(define suite
  (list
    (it "apply over Maybe"
        (all-checks
          (list (check-equal? m-apply (Some 42))
                (check-equal? m-apply-none None))))
    (it "liftF2 derives from apply"
        (check-equal? m-lift (Some 5)))
    (it "apply over List is cartesian"
        (check-equal? l-apply (Cons 5 (Cons 6 (Cons 40 (Cons 50 Nil))))))
    (it "apply over Either is right-biased"
        (check-equal? e-apply (Right 42)))
    (it "apply over Identity"
        (check-equal? i-apply 42))))

(: test-main (IO Unit))
(define test-main (run-suite "rackton/control/apply" suite))
