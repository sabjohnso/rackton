#lang racket/base

;; The kind checker: ill-kinded type expressions are rejected at
;; compile time, while every well-kinded program — including
;; higher-kinded signatures and the monad-transformer stack — is
;; accepted.

(require rackunit
         (for-syntax racket/base)
         racket/port
         "../main.rkt")

(define-syntax-rule (kind-error-message form ...)
  ;; Compile the block; return the error message, or fail if it compiled.
  (with-handlers ([exn:fail? exn-message])
    (eval #'(rackton form ...)
          (variable-reference->namespace (#%variable-reference)))
    (fail "expected a kind error but the program compiled")))

(define-syntax-rule (check-accepts form ...)
  (check-not-exn
   (lambda ()
     (eval #'(rackton form ...)
           (variable-reference->namespace (#%variable-reference))))))

;; ----- rejection: over-application and applying a `*` -----------------

(test-case "an over-applied type constructor is a kind error"
  (define msg (kind-error-message
               (: x (List Integer Integer))
               (define x Nil)))
  (check-regexp-match #rx"kind error" msg)
  (check-regexp-match #rx"List" msg)
  (check-regexp-match #rx"applied to 2" msg))

(test-case "applying a *-kinded type is a kind error"
  (define msg (kind-error-message
               (: f (-> (Integer Boolean) Integer))
               (define (f z) 0)))
  (check-regexp-match #rx"kind error" msg)
  (check-regexp-match #rx"Integer" msg)
  (check-regexp-match #rx"cannot be applied" msg))

(test-case "a kind error fires even when no value contradicts the type"
  ;; The argument is unused, so unification never catches it — only the
  ;; kind checker does.
  (check-regexp-match
   #rx"kind error"
   (kind-error-message
    (data (Box a) (MkBox a))
    (: g (-> (Box Integer Integer) Integer))
    (define (g z) 0))))

(test-case "a nested ill-kinded type blames the exact sub-expression"
  ;; The bad part is buried inside an arrow type; blame must be the
  ;; inner (List Integer Integer) node, not the enclosing signature.
  (define blamed
    (with-handlers ([exn:fail:syntax?
                     (lambda (e)
                       (and (pair? (exn:fail:syntax-exprs e))
                            (syntax->datum (car (exn:fail:syntax-exprs e)))))])
      (eval #'(rackton
               (: f (-> Integer (-> Boolean (List Integer Integer))))
               (define (f a b) Nil))
            (variable-reference->namespace (#%variable-reference)))
      #f))
  (check-equal? blamed '(List Integer Integer)))

;; ----- rejection: wrong-kinded class argument ------------------------

(test-case "an instance whose argument has the wrong kind is rejected"
  ;; Functor's parameter is * -> *; Integer is *.
  (check-regexp-match
   #rx"kind error"
   (kind-error-message
    (instance (Functor Integer)
      (define (fmap f x) x)))))

(test-case "a wrong-kinded argument in a signature constraint is rejected"
  ;; (Monad Integer) in a signature: Monad wants * -> *.
  (check-regexp-match
   #rx"kind error"
   (kind-error-message
    (: bad ((Monad Integer) => Integer))
    (define bad 0))))

;; ----- acceptance: higher-kinded signatures --------------------------

(test-case "a higher-kinded function signature is accepted"
  ;; The signature exercises the kind checker; the body is a bottom stub
  ;; (`panic`, fully polymorphic) so the value side lines up trivially and
  ;; the signature is not a dangling declaration.
  (check-accepts
   (: ap (-> (-> a b) (-> (f a) (f b))))
   (define (ap g) (panic "kind-only"))))

(test-case "the traverse shape (two * -> * variables) is accepted"
  (check-accepts
   (: trav (-> (-> a (f b)) (-> (t a) (f (t b)))))
   (define (trav g) (panic "kind-only"))))

;; ----- acceptance: data types and the transformer stack --------------

(test-case "a StateT-shaped data type and its use are accepted"
  (check-accepts
   (data (Pair a b) (MkPair a b))
   (data (StateT s m a) (MkStateT (-> s (m (Pair s a)))))
   (: run (-> (StateT s m a) (-> s (m (Pair s a)))))
   (define (run st) (match st [(MkStateT f) f]))))

(test-case "a higher-kinded class declared without an annotation is accepted"
  ;; s is used applied in the method, so its kind * -> * is inferred.
  (check-accepts
   (data (Cap a) (MkCap a))
   (protocol (Shape s)
     (: smap (-> (-> a b) (-> (s a) (s b)))))
   (instance (Shape Cap)
     (define (smap f x) (match x [(MkCap v) (MkCap (f v))])))))

(test-case "prelude higher-kinded classes and instances still compile"
  (check-accepts
   (data (Box a) (MkBox a))
   (instance (Functor Box)
     (define (fmap f x) (match x [(MkBox v) (MkBox (f v))])))
   (: mapped (Box Integer))
   (define mapped (fmap (lambda (x) (+ x 1)) (MkBox 41)))))
