#lang rackton

;; Regression: an inherited DEFAULT method whose body calls a
;; RETURN-TYPED method over the class's own (abstract) carrier must
;; resolve that call per-instance, exactly as an explicitly-written
;; instance method already does.
;;
;; Before the fix, the inherited copy reused the default's
;; protocol-scope (abstract-carrier) method-use recording instead of
;; re-resolving it in the adopting instance's concrete context, so
;; codegen emitted a bare, unbound reference to the called method
;; (`make: unbound identifier`).  See ISSUES.org "Inherited default
;; methods don't resolve return-typed method calls".

(require "../unit.rkt")

(data (Box a) (MkBox a))
(data (Wrap a) (MkWrap a))

(define (unbox b) (match b [(MkBox x) x]))
(define (unwrap w) (match w [(MkWrap x) x]))

;; `make` is return-typed: the carrier `f` appears only in the result,
;; so resolution depends on the concrete result type at the call site.
(protocol (Make (f :: (-> * *)))
          (: make (-> a (f a))))

(instance (Make Box)
  (define (make x) (MkBox x)))

(instance (Make Wrap)
  (define (make x) (MkWrap x)))

;; `Use`'s default for `use` calls the return-typed superclass method
;; `make` over the still-abstract carrier `f`.
(protocol (Use (f :: (-> * *)))
          (:requires (Make f))
          (: use (-> a (f a)))
          (define (use x) (make x)))

;; Both instances OMIT `use` and inherit the default.  Each must
;; resolve `make` to its own `Make` impl.
(instance (Use Box))
(instance (Use Wrap))

(: boxed (Box Integer))
(define boxed (use 42))

(: wrapped (Wrap Integer))
(define wrapped (use 7))

;; ----- suite -------------------------------------------------------

(: suite (List Test))
(define suite
  (list
    (it "inherited default resolves return-typed make for Box"
        (check-equal? (unbox boxed) 42))
    (it "inherited default resolves return-typed make for Wrap"
        (check-equal? (unwrap wrapped) 7))))

(: test-main (IO Unit))
(define test-main (run-suite "inherited-default-return-typed" suite))
