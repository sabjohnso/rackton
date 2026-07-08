#lang rackton

;; NonEmpty's CARTESIAN Applicative / Monad — the nonempty analog of the
;; List monad (pure = singleton, flatmap = concatMap, staying nonempty).
;; This is separate from, and contrasts with, the ZIPPY FunctorApply /
;; ComonadApply pinned in nonempty-comonad-test.rkt: NonEmpty carries
;; cartesian <*>/>>= (fapply/flatmap) AND a zippy <@> (apply/coapply),
;; exactly as Haskell separates Applicative from ComonadApply.

(require rackton/data/list/nonempty
         rackton/control/apply
         "../unit.rkt")

(: ne123 (NonEmpty Integer)) (define ne123 (nonempty 1 (list 2 3)))

;; n, n*10 as a nonempty list — for flatmap.
(: twice (-> Integer (NonEmpty Integer)))
(define (twice n) (nonempty n (list (* n 10))))

;; pure is return-typed; the signature pins it to NonEmpty.
(: ne-seven (NonEmpty Integer)) (define ne-seven (pure 7))

(: suite (List Test))
(define suite
  (list
    (it "pure makes a one-element nonempty list"
        (check-equal? (ne-to-list ne-seven) (list 7)))
    (it "flatmap concatenates and stays nonempty"
        (check-equal? (ne-to-list (flatmap twice ne123))
                      (list 1 10 2 20 3 30)))
    (it "fapply is CARTESIAN (contrast the zippy apply)"
        (check-equal? (ne-to-list
                        (fapply (nonempty (lambda (x) (+ x 1))
                                          (list (lambda (x) (* x 10))))
                                ne123))
                      (list 2 3 4 10 20 30)))
    (it "the same operands under zippy apply give a different result"
        (check-equal? (ne-to-list
                        (apply (nonempty (lambda (x) (+ x 1))
                                         (list (lambda (x) (* x 10))))
                               ne123))
                      (list 2 20)))
    (it "monad left identity: flatmap f (pure a) = f a"
        (check-equal? (ne-to-list (flatmap twice (pure 5)))
                      (ne-to-list (twice 5))))
    (it "monad right identity: flatmap pure m = m"
        (check-equal? (ne-to-list (flatmap (lambda (x) (pure x)) ne123))
                      (list 1 2 3)))
    (it "monad associativity"
        (all-checks
          (list
            (check-equal?
              (ne-to-list (flatmap twice (flatmap twice ne123)))
              (ne-to-list (flatmap (lambda (x) (flatmap twice (twice x))) ne123))))))))

(: test-main (IO Unit))
(define test-main (run-suite "nonempty monad" suite))
