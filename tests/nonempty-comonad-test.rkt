#lang rackton

;; NonEmpty's Functor / FunctorApply / Comonad / ComonadApply instances.
;; NonEmpty is the canonical non-trivial comonad: `extract` is the head,
;; `duplicate` is the list of suffixes.  Its `FunctorApply` is ZIPPY
;; (positionwise), consistent with the comonad — unlike `List`'s
;; cartesian Applicative.

(require rackton/data/list/nonempty
         rackton/control/comonad
         rackton/control/apply
         "../unit.rkt")

(: ne (NonEmpty Integer)) (define ne (nonempty 1 (list 2 3)))

;; --- Functor --------------------------------------------------------
(: fmapped (List Integer))
(define fmapped (ne-to-list (fmap (lambda (x) (* x 10)) ne)))

;; --- Comonad: extract = head ----------------------------------------
(: ne-extract Integer)
(define ne-extract (extract ne))

;; duplicate = suffixes; the heads of the suffixes recover the list
(: dup-heads (List Integer))
(define dup-heads (ne-to-list (fmap ne-head (duplicate ne))))

;; extend with extract is the identity
(: ext-id (List Integer))
(define ext-id (ne-to-list (extend extract ne)))

;; extend with a genuine co-Kleisli arrow: running-length of each suffix
(: ext-len (List Integer))
(define ext-len (ne-to-list (extend ne-length ne)))

;; --- FunctorApply: zippy --------------------------------------------
(: zapp (List Integer))
(define zapp
  (ne-to-list
    (apply (nonempty (lambda (x) (+ x 1)) (list (lambda (x) (* x 10))))
           (nonempty 4 (list 5)))))

;; --- ComonadApply (defaults to apply) -------------------------------
(: coapp (List Integer))
(define coapp
  (ne-to-list
    (coapply (nonempty (lambda (x) (+ x 1)) (list (lambda (x) (* x 10))))
             (nonempty 4 (list 5)))))

(: suite (List Test))
(define suite
  (list
    (it "Functor NonEmpty"
        (check-equal? fmapped (list 10 20 30)))
    (it "extract is the head"
        (check-equal? ne-extract 1))
    (it "duplicate yields suffixes (heads recover the list)"
        (check-equal? dup-heads (list 1 2 3)))
    (it "extend extract is identity"
        (check-equal? ext-id (list 1 2 3)))
    (it "extend ne-length gives suffix lengths"
        (check-equal? ext-len (list 3 2 1)))
    (it "FunctorApply NonEmpty is zippy"
        (check-equal? zapp (list 5 50)))
    (it "ComonadApply defaults to apply"
        (check-equal? coapp (list 5 50)))))

(: test-main (IO Unit))
(define test-main (run-suite "nonempty comonad/apply" suite))
