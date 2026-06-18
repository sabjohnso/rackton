#lang rackton

;; rackton/data/array — type-class instances for the built-in fixed-size
;; `Array`.  The array surface forms and operations (array / aref /
;; build-array / array-map / array-rotate / …) are always available; this
;; module adds the instances that need a `* -> *` view of `(Array n)`.
;;
;;   Functor (Array n)        — fmap = array-map, for arrays of ANY size.
;;   Comonad (Array (+ n 1))  — the cyclic comonad over NON-EMPTY arrays:
;;     extract  = element 0,
;;     extend f = the stencil  i ↦ f (rotate i w)   (one pass, indexed),
;;     duplicate (derived) = the array of all rotations.
;;
;; Non-emptiness is in the index: `(+ n 1)` excludes size 0, so `extract`
;; is total.  The more-general `Functor (Array n)` (all sizes) discharges
;; the `Comonad` superclass for `(Array (+ n 1))`.

(require rackton/control/comonad)

(provide (all-defined-out))

;; Size-preserving map makes every array a Functor (any size).
(instance (Functor (Array n))
  (define (fmap f w) (array-map f w)))

;; The cyclic comonad over non-empty arrays.  We give `extract` and
;; `extend`; `duplicate` derives from the protocol's default
;; (`duplicate = extend id`), which now crosses the module boundary.
(instance (Comonad (Array (+ n 1)))
  ;; extract the focused (index-0) element; well-defined because the
  ;; (+ n 1) index guarantees at least one element.
  (define (extract w) (aref w 0))
  ;; extend a co-Kleisli arrow over every cyclic position: position i
  ;; sees the array rotated to bring element i to the front.
  (define (extend f w)
    (array-imap (lambda (i _x) (f (array-rotate i w))) w)))
