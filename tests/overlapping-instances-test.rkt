#lang racket/base

;; Overlapping/specific instance support.  When multiple
;; instances of a class match a target, the most-specific one wins;
;; incomparable matches raise an "overlapping instances" error.
;; Same-head registrations are rejected at compile time.

(require rackunit
         "../main.rkt")

(rackton
  ;; ----- 37.A specific overrides generic ----------------------

  (data (Box a) (MkBox a))

  ;; A generic `Show (Box a)` (requires Show a too) and a specific
  ;; `Show (Box Integer)` that ignores the inner Show and emits a
  ;; sentinel string.  Lookup at (Box Integer) must pick the
  ;; specific; lookup at (Box String) must fall back to the generic.

  (instance ((Show a) => (Show (Box a)))
    (define (show b)
      (match b
        [(MkBox v) (<> "generic-box(" (<> (show v) ")"))])))

  (instance (Show (Box Integer))
    (define (show b)
      (match b
        [(MkBox _) "specific-integer-box"])))

  (: show-int-box String)
  (define show-int-box (show (MkBox 99)))

  (: show-str-box String)
  (define show-str-box (show (MkBox "hi")))

  ;; ----- 37.B nested specificity ------------------------------
  ;; Declare a generic `Show (Seq a)` and a specific `Show (Seq Char)`
  ;; on a local container type.  Show (Seq Char) takes precedence at
  ;; concrete Char, while the generic still applies at other types.
  ;; (A custom type rather than List, since the prelude now ships a
  ;; `Show (List a)` that this generic instance would otherwise
  ;; duplicate — a same-head registration error.)

  (data (Seq a) SNil (SCons a (Seq a)))

  (instance ((Show a) => (Show (Seq a)))
    (define (show xs)
      (match xs
        [(SNil)      "seq[]"]
        [(SCons h _) (<> "seq[" (<> (show h) "]"))])))

  (instance (Show (Seq Char))
    (define (show _) "<chars>"))

  (: show-chars  String)
  (define show-chars  (show (SCons #\a (SCons #\b SNil))))

  (: show-ints   String)
  (define show-ints   (show (SCons 1 (SCons 2 SNil)))))

;; ---------- assertions ---------------------------------------

(test-case "specific (Show (Box Integer)) overrides generic (Show (Box a))"
  (check-equal? show-int-box "specific-integer-box")
  (check-equal? show-str-box "generic-box(\"hi\")"))

(test-case "specific (Show (Seq Char)) overrides generic (Show (Seq a))"
  (check-equal? show-chars "<chars>")
  (check-equal? show-ints "seq[1]"))

;; ----- 37.C duplicate instance registration --------------------

(define-syntax-rule (check-rackton-compile-error form ...)
  (check-exn
   exn:fail?
   (lambda ()
     (eval #'(rackton form ...)
           (variable-reference->namespace (#%variable-reference))))))

(test-case "registering two instances with the same head is rejected"
  (check-rackton-compile-error
   (data Foo MkFoo)
   (instance (Show Foo)
     (define (show _) "foo-a"))
   (instance (Show Foo)
     (define (show _) "foo-b"))))

(test-case "overlapping instances with no most-specific raise at lookup"
  (check-rackton-compile-error
   (data (P2 a b) (MkP2 a b))
   (instance ((Show b) => (Show (P2 Integer b)))
     (define (show _) "leftP2"))
   (instance ((Show a) => (Show (P2 a Integer)))
     (define (show _) "rightP2"))
   ;; Both instances match (P2 Integer Integer) — incomparable.
   (define ambiguous (show (MkP2 1 2)))))
