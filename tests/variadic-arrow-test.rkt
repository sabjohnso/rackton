#lang rackton

;; Variadic `->` sugar: `(-> A B C)` parses as `(-> A (-> B C))`, etc.
;; The core type AST stays binary, so inference and codegen are unchanged;
;; this test exercises the pipeline end-to-end to confirm a user signature
;; written in the variadic form behaves identically to the nested form.

(require "../unit.rkt")

;; ----- type signatures written in variadic form ------------------
(: add3 (-> Integer Integer Integer Integer))
(define (add3 a b c) (+ a (+ b c)))

(: const2 (-> Integer Integer Integer))
(define (const2 a b) a)

;; Mixed with higher-order: `apply-twice` takes a unary fn and a value.
(: apply-twice (-> (-> Integer Integer) Integer Integer))
(define (apply-twice f x) (f (f x)))

;; Partial application across a variadic-form signature.
(: add3-curried (-> Integer (-> Integer Integer)))
(define add3-curried (add3 10))

;; A class method whose signature uses the variadic form.
(protocol (TriOp a)
  (: combine (-> a a a a)))

(instance (TriOp Integer)
  (define (combine x y z) (+ x (+ y z))))

(: tri-int Integer)
(define tri-int (combine 1 2 3))

;; 0-arg-fn encoding still works alongside the variadic form.
(: thunk (-> Integer))
(define (thunk) 99)

(: thunk-val Integer)
(define thunk-val (thunk))

(: suite (List Test))
(define suite
  (list
   (it "variadic -> in signature: ternary fn type-checks and runs"
       (check-equal? (add3 1 2 3) 6))
   (it "variadic -> in signature: binary fn type-checks and runs"
       (check-equal? (const2 5 7) 5))
   (it "variadic -> mixed with higher-order arg"
       (check-equal? (apply-twice (+ 1) 10) 12))
   (it "partial application across a variadic-form signature"
       (check-equal? ((add3-curried 20) 30) 60))
   (it "variadic -> in a class method signature"
       (check-equal? tri-int 6))
   (it "1-arg `(-> T)` (0-arg fn encoding) still works"
       (check-equal? thunk-val 99))))

(: main Unit)
(define main (run-io (run-suite "variadic arrow" suite)))
