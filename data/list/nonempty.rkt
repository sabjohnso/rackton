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

;; Map `f` over the list and concatenate the resulting nonempty lists —
;; the CARTESIAN concatMap, staying nonempty (the head of the first keeps
;; it so).  Both the cartesian Applicative and Monad instances delegate
;; here, so neither calls the other's class method.  The tail elements'
;; results are flattened through the ordinary `List` `flatmap`.
(: ne-flatmap (-> (-> a (NonEmpty b)) (-> (NonEmpty a) (NonEmpty b))))
(define (ne-flatmap f ne)
  (match ne
    [(NonEmpty h t)
     (match (f h)
       [(NonEmpty fh ft)
        (NonEmpty fh (append ft (flatmap (lambda (x) (ne-to-list (f x))) t)))])]))

;; --- class instances -------------------------------------------------
;;
;; NonEmpty is a Functor (map over every element); the CARTESIAN
;; Applicative/Monad (the nonempty analog of the `List` monad — `pure` is
;; a singleton, `flatmap` is concatMap staying nonempty); a ZIPPY
;; FunctorApply (positionwise application — NOT the cartesian product);
;; and the canonical non-trivial Comonad (`extract` = head, `duplicate` =
;; suffixes).
;;
;; The cartesian `fapply` (`<*>`) and the zippy `apply` (`<@>`) coexist
;; deliberately, exactly as Haskell separates `Applicative` from
;; `ComonadApply`: the Monad law forces `<*> = ap` (cartesian), while the
;; zippy `apply` is what makes `ComonadApply` consistent with the comonad
;; (`coapply` is just `apply`).  So the two application operators differ
;; here, and that is intended.
;;
;; Every method is written out explicitly rather than leaning on the
;; classes' default cycles: those defaults are defined inside
;; rackton/control/apply and rackton/control/comonad and DO NOT cross the
;; module boundary (the scheme-codec sidecar drops a class's default
;; bodies).  An instance of an imported class must therefore supply a
;; complete method set.

(instance (Functor NonEmpty)
  (define (fmap f ne) (match ne [(NonEmpty h t) (NonEmpty (f h) (fmap f t))])))

(instance (Applicative NonEmpty)
  ;; cartesian: `pure` is a singleton, `fapply` is the cross product —
  ;; every function meets every argument, in order (so `<*> = ap`).
  (define (pure x) (NonEmpty x Nil))
  (define (fapply sf sx) (ne-flatmap (lambda (f) (ne-map f sx)) sf)))

(instance (Monad NonEmpty)
  (define (flatmap f ne) (ne-flatmap f ne)))

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
