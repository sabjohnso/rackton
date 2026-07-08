#lang rackton

;; thih.rkt — a Hindley–Milner type inferencer for a small lambda
;; calculus, written in Rackton.  In the spirit of Mark Jones's
;; "Typing Haskell in Haskell" (THIH): Rackton is itself an Algorithm-W
;; language with let-polymorphism, so here it types a miniature language
;; of its own.  An HM type checker, expressed in an HM-typed language.
;;
;; The object language:
;;   x                  variable
;;   1, 2, …            integer literal
;;   #t, #f             boolean literal
;;   (\x. e)            lambda
;;   (f a)              application
;;   (let x e1 e2)      let-binding (the source of polymorphism)
;;   (if c t e)         conditional
;;
;; The inferencer follows Algorithm W using Jones's stateful `TI` monad:
;; a single "current substitution" threaded through inference alongside
;; a fresh-name counter, so `infer` stays flat — each `unify` extends
;; the global substitution rather than returning one to thread by hand.
;;
;; Run it with `racket examples/thih.rkt`.

(require rackton/data/map
         rackton/data/set
         rackton/data/list
         rackton/data/result)

;; ===== The object language =========================================

;; Monotypes: variables, nullary constructors (Int, Bool), and arrows.
(data Type
  (TVar String)
  (TCon String)
  (TFun Type Type))

;; A type scheme quantifies a monotype over a list of variables —
;; `(Forall (a) (TFun (TVar a) (TVar a)))` is `forall a. a -> a`.
(data Scheme (Forall (List String) Type))

;; Literals and the expressions we infer types for.
(data Lit
  (LInt Integer)
  (LBool Boolean))

(data Expr
  (EVar String)
  (ELit Lit)
  (ELam String Expr)
  (EApp Expr Expr)
  (ELet String Expr Expr)
  (EIf Expr Expr Expr))

;; A substitution maps type-variable names to types; a typing
;; environment maps program variables to their schemes.
(define-alias Subst   (Map String Type))
(define-alias TypeEnv (Map String Scheme))

;; ===== The inference monad =========================================
;;
;; `St` is the inference state: the current substitution plus the next
;; fresh-variable number.  `Infer a` is a state-and-error computation
;; over it — Jones's `TI` monad, here built from scratch so the rest of
;; the file can use `do`-notation.

(data St (St Subst Integer))

(newtype (Infer a)
         (Infer (-> St (Result String (Pair St a)))))

(: run-infer-fn (-> (Infer a) (-> St (Result String (Pair St a)))))
(define (run-infer-fn m) (match m [(Infer f) f]))

(instance (Functor Infer)
  (define (fmap f m)
    (Infer (lambda (st)
             (match ((run-infer-fn m) st)
               [(Err e)          (Err e)]
               [(Ok (Pair s a))  (Ok (Pair s (f a)))])))))

(instance (Applicative Infer)
  (define (pure x) (Infer (lambda (st) (Ok (Pair st x)))))
  (define (fapply mf mx)
    (Infer (lambda (st)
             (match ((run-infer-fn mf) st)
               [(Err e)           (Err e)]
               [(Ok (Pair s1 f))
                (match ((run-infer-fn mx) s1)
                  [(Err e)           (Err e)]
                  [(Ok (Pair s2 x))  (Ok (Pair s2 (f x)))])])))))

(instance (Monad Infer)
  (define (flatmap k m)
    (Infer (lambda (st)
             (match ((run-infer-fn m) st)
               [(Err e)           (Err e)]
               [(Ok (Pair s1 a))  ((run-infer-fn (k a)) s1)])))))

;; Read the current substitution.
(: get-subst (Infer Subst))
(define get-subst
  (Infer (lambda (st) (match st [(St sub n) (Ok (Pair (St sub n) sub))]))))

;; Compose a freshly-found substitution onto the current one.
(: ext-subst (-> Subst (Infer Unit)))
(define (ext-subst u)
  (Infer (lambda (st)
           (match st [(St sub n) (Ok (Pair (St (compose-subst u sub) n) Unit))]))))

;; A fresh type variable, named "t0", "t1", … from the counter.
(: fresh (Infer Type))
(define fresh
  (Infer (lambda (st)
           (match st
             [(St sub n)
              (Ok (Pair (St sub (+ n 1))
                        (TVar (string-append "t" (integer->string n)))))]))))

;; Abort inference with a message.
(: throw (-> String (Infer a)))
(define (throw msg) (Infer (lambda (_) (Err msg))))

;; Run a computation from the empty substitution and counter 0.
(: run-infer (-> (Infer a) (Result String a)))
(define (run-infer m)
  (match ((run-infer-fn m) (St empty-map 0))
    [(Err e)          (Err e)]
    [(Ok (Pair _ a))  (Ok a)]))

;; ===== Free type variables =========================================

(: ftv-type (-> Type (Set String)))
(define (ftv-type t)
  (match t
    [(TVar a)    (set-singleton a)]
    [(TCon _)    empty-set]
    [(TFun x y)  (set-union (ftv-type x) (ftv-type y))]))

(: ftv-scheme (-> Scheme (Set String)))
(define (ftv-scheme sch)
  (match sch
    [(Forall vars t) (set-difference (ftv-type t) (set-from-list vars))]))

(: ftv-env (-> TypeEnv (Set String)))
(define (ftv-env env)
  (foldr (lambda (sch acc) (set-union (ftv-scheme sch) acc))
         empty-set
         (map-values env)))

;; ===== Applying substitutions ======================================

(: apply-type (-> Subst (-> Type Type)))
(define (apply-type s t)
  (match t
    [(TVar a)    (match (map-lookup a s) [(Some t2) t2] [(None) t])]
    [(TCon _)    t]
    [(TFun x y)  (TFun (apply-type s x) (apply-type s y))]))

(: apply-scheme (-> Subst (-> Scheme Scheme)))
(define (apply-scheme s sch)
  (match sch
    ;; The quantified variables are bound here, so they are shielded
    ;; from the substitution.
    [(Forall vars t) (Forall vars (apply-type (foldr map-delete s vars) t))]))

(: apply-env (-> Subst (-> TypeEnv TypeEnv)))
(define (apply-env s env) (map-map (apply-scheme s) env))

;; `(compose-subst s1 s2)` is "s1 after s2": apply s1 across s2's range,
;; then add s1's own bindings (s2's mapped entries win on shared keys).
(: compose-subst (-> Subst (-> Subst Subst)))
(define (compose-subst s1 s2)
  (map-union (map-map (apply-type s1) s2) s1))

;; ===== Unification =================================================

;; Bind a variable to a type, guarding against the trivial self-bind
;; and against an occurs-check failure (which would build an infinite
;; type).
(: var-bind (-> String (-> Type (Infer Subst))))
(define (var-bind name t)
  (match t
    [(TVar other)
     (if (== name other) (pure empty-map) (pure (map-singleton name t)))]
    [_
      (if (set-member? name (ftv-type t))
        (throw (string-append "occurs check: "
                              (string-append name
                                             (string-append " occurs in " (type->string t)))))
        (pure (map-singleton name t)))]))

(: unify-error (-> Type (-> Type (Infer a))))
(define (unify-error t1 t2)
  (throw (string-append "cannot unify "
                        (string-append (type->string t1)
                                       (string-append " with " (type->string t2))))))

;; Most general unifier of two monotypes.
(: mgu (-> Type (-> Type (Infer Subst))))
(define (mgu t1 t2)
  (match (Pair t1 t2)
    [(Pair (TFun a1 r1) (TFun a2 r2))
     (do [s1 <- (mgu a1 a2)]
       [s2 <- (mgu (apply-type s1 r1) (apply-type s1 r2))]
       (pure (compose-subst s2 s1)))]
    [(Pair (TVar a) _)  (var-bind a t2)]
    [(Pair _ (TVar a))  (var-bind a t1)]
    [(Pair (TCon a) (TCon b))
     (if (== a b) (pure empty-map) (unify-error t1 t2))]
    [_ (unify-error t1 t2)]))

;; Unify under the current substitution and record the result.
(: unify (-> Type (-> Type (Infer Unit))))
(define (unify t1 t2)
  (do [s <- get-subst]
    [u <- (mgu (apply-type s t1) (apply-type s t2))]
    (ext-subst u)))

;; ===== Generalize / instantiate ====================================

;; A monadic map: run `f` over each element, collecting the results.
(: infer-mapM (-> (-> a (Infer b)) (-> (List a) (Infer (List b)))))
(define (infer-mapM f xs)
  (match xs
    [(Nil) (pure Nil)]
    [(Cons h t)
     (do [y  <- (f h)]
       [ys <- (infer-mapM f t)]
       (pure (Cons y ys)))]))

;; Replace a scheme's quantified variables with fresh ones.
(: instantiate (-> Scheme (Infer Type)))
(define (instantiate sch)
  (match sch
    [(Forall vars t)
     (do [fresh-tys <- (infer-mapM (lambda (_) fresh) vars)]
       (pure (apply-type (map-from-list (zip vars fresh-tys)) t)))]))

;; Quantify over the type's free variables that are not also free in
;; the environment — this is where let-polymorphism comes from.
(: generalize (-> TypeEnv (-> Type Scheme)))
(define (generalize env t)
  (Forall (set-to-list (set-difference (ftv-type t) (ftv-env env))) t))

;; ===== Algorithm W =================================================

(: infer (-> TypeEnv (-> Expr (Infer Type))))
(define (infer env e)
  (match e
    [(EVar x)
     (match (map-lookup x env)
       [(None)     (throw (string-append "unbound variable: " x))]
       [(Some sch) (instantiate sch)])]
    [(ELit (LInt _))  (pure (TCon "Int"))]
    [(ELit (LBool _)) (pure (TCon "Bool"))]
    [(ELam x body)
     (do [tv <- fresh]
       [tb <- (infer (map-insert x (Forall Nil tv) env) body)]
       [s  <- get-subst]
       (pure (apply-type s (TFun tv tb))))]
    [(EApp f a)
     (do [tf <- (infer env f)]
       [ta <- (infer env a)]
       [tv <- fresh]
       [_  <- (unify tf (TFun ta tv))]
       [s  <- get-subst]
       (pure (apply-type s tv)))]
    [(ELet x e1 e2)
     (do [t1 <- (infer env e1)]
       [s  <- get-subst]
       ;; Generalize e1's type under the solved substitution, then bind
       ;; the resulting scheme while inferring the body.
       (infer (map-insert x (generalize (apply-env s env) (apply-type s t1)) env)
              e2))]
    [(EIf c th el)
     (do [tc <- (infer env c)]
       [_  <- (unify tc (TCon "Bool"))]
       [tt <- (infer env th)]
       [te <- (infer env el)]
       [_  <- (unify tt te)]
       [s  <- get-subst]
       (pure (apply-type s tt)))]))

;; Infer a closed expression's principal type scheme.
(: type-of (-> Expr (Result String Scheme)))
(define (type-of e)
  (run-infer
    (do [t <- (infer empty-map e)]
      [s <- get-subst]
      (pure (generalize empty-map (apply-type s t))))))

;; ===== Pretty-printing =============================================

;; Arrows are right-associative, so the left operand is parenthesized
;; only when it is itself an arrow.
(: type->string (-> Type String))
(define (type->string t)
  (match t
    [(TVar a)   a]
    [(TCon c)   c]
    [(TFun a b) (string-append (paren-arrow a)
                               (string-append " -> " (type->string b)))]))

(: paren-arrow (-> Type String))
(define (paren-arrow t)
  (match t
    [(TFun _ _) (string-append "(" (string-append (type->string t) ")"))]
    [_          (type->string t)]))

(: scheme->string (-> Scheme String))
(define (scheme->string sch)
  (match sch
    [(Forall vars t)
     (match vars
       [(Nil) (type->string t)]
       [_     (string-append "forall " (string-append (string-join " " vars)
                                                      (string-append ". " (type->string t))))])]))

;; Rename a scheme's variables to a, b, c, … in order of first
;; appearance, so output reads `forall a. a -> a` rather than `t3`.
(: alphabet (List String))
(define alphabet (fmap char->string (string->chars "abcdefghijklmnopqrstuvwxyz")))

(: vars-in-order (-> Type (List String)))
(define (vars-in-order t)
  (match t
    [(TVar a)   (Cons a Nil)]
    [(TCon _)   Nil]
    [(TFun x y) (append (vars-in-order x) (vars-in-order y))]))

(: normalize-scheme (-> Scheme Scheme))
(define (normalize-scheme sch)
  (match sch
    [(Forall _ t)
     (let ([vars (nub (vars-in-order t))])
       (let ([names (take (length vars) alphabet)])
         (let ([sub (map-from-list (zip vars (fmap TVar names)))])
           (Forall names (apply-type sub t)))))]))

;; ===== Demo ========================================================

;; Print one term's inferred type (or the type error it provokes).
(: report (-> String (-> Expr (IO Unit))))
(define (report label e)
  (println (string-append label
                          (string-append " :: "
                                         (match (type-of e)
                                           [(Ok sch)  (scheme->string (normalize-scheme sch))]
                                           [(Err msg) (string-append "TYPE ERROR — " msg)])))))

;; --- Well-typed terms ---------------------------------------------

;; \x. x
(define e-id (ELam "x" (EVar "x")))

;; \x. \y. x
(define e-const (ELam "x" (ELam "y" (EVar "x"))))

;; \f. \g. \x. f (g x)
(define e-compose
  (ELam "f" (ELam "g" (ELam "x"
                            (EApp (EVar "f") (EApp (EVar "g") (EVar "x")))))))

;; let id = \x. x in if (id #t) then (id 1) else 2
;; id is used at both Bool and Int — let-polymorphism in action.
(define e-let-poly
  (ELet "id" (ELam "x" (EVar "x"))
        (EIf (EApp (EVar "id") (ELit (LBool #t)))
             (EApp (EVar "id") (ELit (LInt 1)))
             (ELit (LInt 2)))))

;; let id = \x. x in id id
(define e-id-id
  (ELet "id" (ELam "x" (EVar "x"))
        (EApp (EVar "id") (EVar "id"))))

;; if #t then 1 else 0
(define e-if
  (EIf (ELit (LBool #t)) (ELit (LInt 1)) (ELit (LInt 0))))

;; --- Ill-typed terms ----------------------------------------------

;; (1 2) — applying a non-function
(define e-apply-int (EApp (ELit (LInt 1)) (ELit (LInt 2))))

;; if #t then 1 else #f — branches disagree
(define e-bad-if
  (EIf (ELit (LBool #t)) (ELit (LInt 1)) (ELit (LBool #f))))

;; \x. x x — self-application, fails the occurs check
(define e-omega (ELam "x" (EApp (EVar "x") (EVar "x"))))

(: main (IO Unit))
(define main (do [_ <- (println "Hindley–Milner inference for a small lambda calculus:")]
               [_ <- (println "")]
               [_ <- (println "well-typed:")]
               [_ <- (report "  \\x. x                       " e-id)]
               [_ <- (report "  \\x. \\y. x                    " e-const)]
               [_ <- (report "  \\f. \\g. \\x. f (g x)          " e-compose)]
               [_ <- (report "  let id=\\x.x in id id        " e-id-id)]
               [_ <- (report "  let id=.. in if id#t..      " e-let-poly)]
               [_ <- (report "  if #t then 1 else 0         " e-if)]
               [_ <- (println "")]
               [_ <- (println "ill-typed:")]
               [_ <- (report "  (1 2)                       " e-apply-int)]
               [_ <- (report "  if #t then 1 else #f        " e-bad-if)]
               [_ <- (report "  \\x. x x                      " e-omega)]
               [_ <- (println "")]
               (println "done.")))
