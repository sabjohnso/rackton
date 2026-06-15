#lang rackton

;; rackton/data/list/nonempty — Data.List.NonEmpty.  A list guaranteed
;; to have at least one element, so head / tail are total.

(require rackton/control/apply
         rackton/control/comonad)

(provide (all-defined-out))

(data (NonEmpty a) (NonEmpty a (List a)))

;; construct from a head and a (possibly empty) tail.
(: nonempty (-> a (-> (List a) (NonEmpty a))))
(define (nonempty h t) (NonEmpty h t))

(: ne-head (-> (NonEmpty a) a))
(define (ne-head ne) (match ne [(NonEmpty h _) h]))

(: ne-tail (-> (NonEmpty a) (List a)))
(define (ne-tail ne) (match ne [(NonEmpty _ t) t]))

(: ne-to-list (-> (NonEmpty a) (List a)))
(define (ne-to-list ne) (match ne [(NonEmpty h t) (Cons h t)]))

(: ne-from-list (-> (List a) (Maybe (NonEmpty a))))
(define (ne-from-list xs)
  (match xs
    [(Nil)      None]
    [(Cons h t) (Some (NonEmpty h t))]))

(: ne-cons (-> a (-> (NonEmpty a) (NonEmpty a))))
(define (ne-cons x ne) (match ne [(NonEmpty h t) (NonEmpty x (Cons h t))]))

(: ne-map (-> (-> a b) (-> (NonEmpty a) (NonEmpty b))))
(define (ne-map f ne) (match ne [(NonEmpty h t) (NonEmpty (f h) (fmap f t))]))

(: ne-length (-> (NonEmpty a) Integer))
(define (ne-length ne) (match ne [(NonEmpty _ t) (+ 1 (length t))]))

;; --- class instances -------------------------------------------------
;;
;; NonEmpty is a Functor (map over every element), a ZIPPY FunctorApply
;; (positionwise application — NOT the cartesian product `List` uses), and
;; the canonical non-trivial Comonad (`extract` = head, `duplicate` =
;; suffixes).  The zippy `apply` is what makes `ComonadApply` consistent
;; with the comonad, so `coapply` is just `apply`.
;;
;; Every method is written out explicitly rather than leaning on the
;; classes' default cycles: those defaults are defined inside
;; rackton/control/apply and rackton/control/comonad and DO NOT cross the
;; module boundary (the scheme-codec sidecar drops a class's default
;; bodies).  An instance of an imported class must therefore supply a
;; complete method set.

(instance (Functor NonEmpty)
  (define (fmap f ne) (match ne [(NonEmpty h t) (NonEmpty (f h) (fmap f t))])))

(instance (FunctorApply NonEmpty)
  ;; zip the heads and the tails; the tail zip truncates to the shorter,
  ;; matching ZipList semantics.
  (define (apply ff fx)
    (match ff
      [(NonEmpty f fs)
       (match fx
         [(NonEmpty x xs)
          (letrec ([zapp (lambda (gs ys)
                           (match gs
                             [(Nil) Nil]
                             [(Cons g gt)
                              (match ys
                                [(Nil)       Nil]
                                [(Cons y yt) (Cons (g y) (zapp gt yt))])]))])
            (NonEmpty (f x) (zapp fs xs)))])]))
  (define (liftF2 g fa fb) (apply (fmap g fa) fb)))

(instance (Comonad NonEmpty)
  (define (extract ne) (match ne [(NonEmpty h _) h]))
  ;; duplicate = the non-empty list of non-empty suffixes.
  (define (duplicate ne)
    (match ne
      [(NonEmpty h t)
       (letrec ([suffixes (lambda (xs)
                            (match xs
                              [(Nil)      Nil]
                              [(Cons y ys) (Cons (NonEmpty y ys) (suffixes ys))]))])
         (NonEmpty (NonEmpty h t) (suffixes t)))]))
  (define (extend f w) (fmap f (duplicate w))))

(instance (ComonadApply NonEmpty)
  (define (coapply ff fx) (apply ff fx)))
