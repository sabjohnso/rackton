#lang racket/base

;; Regression for ISSUES.org "Imported promoted constructor loses its
;; kind".  A DataKinds-promoted constructor keeps its kind across a
;; module boundary, so the importer's kind checker enforces a promoted
;; index instead of treating it as a fresh (anything-goes) kind.
;; Promotion is computed once in the defining module (promote-data) and
;; transported via the rackton-schemes sidecar's `rackton-promoted`
;; table; the importer folds it back in.

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

(define-syntax-rule (check-rackton-compile-error form ...)
  (check-exn
   exn:fail?
   (lambda ()
     (eval #'(rackton form ...)
           (variable-reference->namespace (#%variable-reference))))))

;; A legitimate promoted-index program: g is inferred to kind Stack from
;; (Mem g TInt), so (TypedVar g) gets kind Stack -> *.  Reaching the test
;; below at all means this block kind-checked.
(rackton
  (require "promoted-kinds-cross-module-lib.rkt")
  (data (TypedVar g)
    (TVInt  : (-> (Mem g TInt)  (TypedVar g)))
    (TVBool : (-> (Mem g TBool) (TypedVar g)))))

(test-case "imported promoted index: a legitimate use compiles"
  (check-true #t))

;; Ill-kinded: TInt :: Ty, but Mem's first parameter is Stack.  In Mem's
;; own module this is a kind error; with the promotion transported it is
;; now caught across the module boundary too (previously accepted — the
;; soundness hole this fix closes).
(test-case "imported promoted index: an ill-kinded use is a compile error"
  (check-rackton-compile-error
   (require "promoted-kinds-cross-module-lib.rkt")
   (data (Bad g)
     (B : (-> (Mem TInt g) (Bad g))))))

;; A PROMOTED PARAMETERISED datatype crosses the module boundary: g is
;; inferred to kind (PList Ty) from (PStack (PCons TInt g)), so the
;; importer must have recovered PCons's kind-polymorphic scheme (which
;; carries a `kapp`) from the sidecar.
(rackton
  (require "promoted-kinds-cross-module-lib.rkt")
  (data (UsesP g)
    (U : (-> (PStack (PCons TInt g)) (UsesP g)))))

(test-case "imported promoted parameterised datatype: a legitimate use compiles"
  (check-true #t))

;; Ill-kinded across the boundary: PCons's tail must be a (PList k), but
;; TInt has kind Ty.
(test-case "imported promoted parameterised datatype: ill-kinded use is a compile error"
  (check-rackton-compile-error
   (require "promoted-kinds-cross-module-lib.rkt")
   (data (BadP g)
     (BP : (-> (PStack (PCons TInt TInt)) (BadP g))))))
