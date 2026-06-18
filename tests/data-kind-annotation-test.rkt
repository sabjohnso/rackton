#lang racket/base

;; Kind annotations on `data` / `newtype` headers: a parameter may be
;; written `(v :: k)` to STATE its kind directly rather than have it
;; inferred — mirroring `protocol`'s `[v :: k]`.  `k` is `*`, `Nat`, an
;; arrow `(-> k …)`, or a promoted datatype name (e.g. `Stack`).  The
;; declared kind seeds inference, so constructor usage is checked against
;; it, and a PHANTOM parameter (one no constructor mentions) keeps the
;; declared kind instead of defaulting to `*`.

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

(define-syntax-rule (kind-error-message form ...)
  (with-handlers ([exn:fail? exn-message])
    (eval #'(rackton form ...)
          (variable-reference->namespace (#%variable-reference)))
    (fail "expected a kind error but the program compiled")))

;; ----- phantom parameter with a promoted kind ----------------------
;; `a` appears in no constructor, so inference alone would default it to
;; `*`.  The annotation pins it to the promoted kind `Stack`.
(rackton
  (data Stack SEmpty (SCons Stack))
  ;; `a` is phantom (no constructor mentions it); the payload is Integer.
  (data (Phantom (a :: Stack)) (MkPhantom Integer))
  (: payload (-> (Phantom SEmpty) Integer))     ; (Phantom SEmpty): a := SEmpty :: Stack
  (define (payload p) (match p [(MkPhantom n) n]))
  (: built (Phantom SEmpty))
  (define built (MkPhantom 42))
  (: got Integer)
  (define got (payload built)))

(test-case "phantom data parameter keeps its declared promoted kind"
  ;; Reaching here means the block above kind-checked; confirm it also runs.
  (check-equal? got 42))

(test-case "an index of the wrong promoted kind is rejected"
  ;; Integer :: * where the phantom slot wants Stack.
  (check-regexp-match
   #rx"kind error"
   (kind-error-message
    (data Stack SEmpty (SCons Stack))
    (data (Phantom (a :: Stack)) MkPhantom)
    (data Bad (B : (-> (Phantom Integer) Bad))))))

;; ----- higher-kind annotation --------------------------------------
(rackton
  (data (Proxy (f :: (-> * *))) MkProxy)
  (data UsesProxy (UseQ : (-> (Proxy Maybe) UsesProxy))))

(test-case "a data parameter may be annotated with a higher kind"
  (check-true #t))

(test-case "an index of the wrong higher kind is rejected"
  (check-regexp-match
   #rx"kind error"
   (kind-error-message
    (data (Proxy (f :: (-> * *))) MkProxy)
    (data Bad (B : (-> (Proxy Integer) Bad))))))

;; ----- annotation checked against constructor usage ----------------
(test-case "an annotation contradicting constructor usage is a kind error"
  (check-regexp-match
   #rx"kind error"
   (kind-error-message
    (data Ty TInt TBool)
    (data Stack SEmpty (SCons Ty Stack))
    ;; SCons forces g :: Stack, but the annotation says Ty.
    (data (Mem (g :: Ty) a)
      (MZ : (Mem (SCons a g) a))))))

;; ----- a non-lowercase parameter name is rejected ------------------
(test-case "a data parameter must be a lowercase identifier"
  (check-exn
   exn:fail?
   (lambda ()
     (eval #'(rackton (data (Foo (A :: Nat)) MkFoo))
           (variable-reference->namespace (#%variable-reference))))))
