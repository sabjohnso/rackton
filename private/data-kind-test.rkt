#lang racket/base

;; Tests for data-type kind inference (Phase A2.5, infer-data-kinds).
;; After inference, each data tcon's stored kind must reflect how its
;; parameters are used in its constructors — not merely its arity.

(module+ test
  (require rackunit
           racket/match
           "types.rkt"
           "env.rkt"
           "surface.rkt"
           "infer.rkt")

  (define (program-env forms)
    (infer-program (for/list ([f (in-list forms)])
                     (parse-top (datum->syntax #f f)))
                   initial-env))

  (define (kind-of forms name)
    (kind->datum (tcon-info-kind (env-ref-tcon (program-env forms) name))))

  ;; ----- ordinary ADTs: all-* kinds ------------------------------------

  (check-equal? (kind-of '((data (Box a) (MkBox a))) 'Box)
                '(-> * *))
  (check-equal? (kind-of '((data (Two a b) (MkTwo a b))) 'Two)
                '(-> * (-> * *)))
  (check-equal? (kind-of '((data Flag T F)) 'Flag)
                '*
                "a nullary data type has kind *")

  ;; ----- higher-kinded parameter: the discriminating case --------------
  ;; A `* -> *` parameter applied to another parameter. Arity alone
  ;; (the old placeholder) would give `* -> * -> *`; real inference must
  ;; give `(* -> *) -> * -> *`.

  (check-equal? (kind-of '((data (Fix f a) (MkFix (f a)))) 'Fix)
                '(-> (-> * *) (-> * *))
                "f is used applied, so f : * -> *")

  ;; A StateT-shaped type: `(-> s (m (Pair s a)))`. m must be `* -> *`.
  (check-equal? (kind-of '((data (Pair a b) (MkPair a b))
                           (data (StateT s m a)
                             (MkStateT (-> s (m (Pair s a))))))
                         'StateT)
                '(-> * (-> (-> * *) (-> * *)))
                "StateT : * -> (* -> *) -> * -> *")

  ;; ----- self / mutual recursion ---------------------------------------

  (check-equal? (kind-of '((data (Tree a) (Leaf a) (Node (Tree a) (Tree a))))
                         'Tree)
                '(-> * *)
                "a self-recursive type infers its kind from the shared seed")

  ;; Mutually recursive rose-tree-ish pair.
  (let ([env (program-env '((data (Forest a) (MkForest (Rose a)))
                            (data (Rose a) (MkRose a (Forest a)))))])
    (check-equal? (kind->datum (tcon-info-kind (env-ref-tcon env 'Forest)))
                  '(-> * *))
    (check-equal? (kind->datum (tcon-info-kind (env-ref-tcon env 'Rose)))
                  '(-> * *)))

  ;; ----- GADT result types constrain the parameter kind ----------------

  (check-equal? (kind-of '((data (Expr a)
                             (Lit  : (-> Integer (Expr Integer)))
                             (BVal : (-> Boolean (Expr Boolean)))))
                         'Expr)
                '(-> * *))

  ;; ----- phantom parameter defaults to * -------------------------------

  (check-equal? (kind-of '((data (Tagged a) (MkTagged Integer))) 'Tagged)
                '(-> * *)
                "a parameter unused in any field defaults to kind *"))
