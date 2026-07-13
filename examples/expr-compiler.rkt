#lang rackton

;; expr-compiler.rkt — a *type-safe* compiler from a small integer
;; expression language to a stack machine, in Rackton.
;;
;; Surface language:
;;   integer literal   ⇒ that integer
;;   (Add a b)         ⇒ a + b
;;   (Sub a b)         ⇒ a − b
;;   (Mul a b)         ⇒ a × b
;;
;; The interesting part is the *types*.  Machine code is a GADT indexed
;; by the shape of the operand stack — a type-level list of tags built
;; from DataKinds-promoted datatypes (`Ty`, `Stack`).  Because the
;; indices are kind-checked, the compiler can only ever emit
;; stack-balanced code: `compile` has type
;;
;;     Expr → Code (SPush TInt s) t → Code s t
;;
;; which reads "compiling an expression turns a continuation that
;; expects one integer already on the stack into code that produces it"
;; — so every arithmetic op is guaranteed two operands, and the whole
;; program nets exactly one result.  A mis-wired instruction sequence
;; would not typecheck.
;;
;; Run it with `racket examples/expr-compiler.rkt`.

;; ----- Source language -------------------------------------------

(data Expr
  (Lit Integer)
  (Add Expr Expr)
  (Sub Expr Expr)
  (Mul Expr Expr))

;; ----- Stack shapes, at the type level (DataKinds) ---------------
;; `Ty` and `Stack` are ordinary monomorphic datatypes; Rackton
;; promotes them to kinds, so their constructors can index `Code`
;; below.  An integer machine only ever stacks `TInt` tags.

(data Ty    TInt)
(data Stack SEmpty (SPush Ty Stack))

;; ----- Typed machine code ----------------------------------------
;; `(Code s t)` is code that transforms a stack of shape `s` into one
;; of shape `t`.  Its kind, `Stack → Stack → *`, is inferred from the
;; constructors' use of the promoted `SPush`/`TInt`.  Each instruction
;; threads a continuation, so a value of type `Code s t` is a complete
;; program fragment (this is the defunctionalized-continuation form).

(data (Code s t)
  ;; stop: the stack is already in its final shape
  (HALT  : (Code s s))
  ;; push a literal, then run the continuation
  (PUSHI : (-> Integer (Code (SPush TInt s) t) (Code s t)))
  ;; pop two ints, push their sum / difference / product, then continue
  (IADD  : (-> (Code (SPush TInt s) t)
               (Code (SPush TInt (SPush TInt s)) t)))
  (ISUB  : (-> (Code (SPush TInt s) t)
               (Code (SPush TInt (SPush TInt s)) t)))
  (IMUL  : (-> (Code (SPush TInt s) t)
               (Code (SPush TInt (SPush TInt s)) t))))

;; ----- The compiler ----------------------------------------------
;; Continuation-passing: `k` is the code to run once this expression's
;; value sits on top of the stack.  The signature's `s` is universally
;; quantified, so the recursive calls below specialise it freely
;; (polymorphic recursion, admitted because the type is declared).

(: compile (-> Expr (-> (Code (SPush TInt s) t) (Code s t))))
(define (compile e k)
  (match e
    [(Lit n)   (PUSHI n k)]
    [(Add a b) (compile a (compile b (IADD k)))]
    [(Sub a b) (compile a (compile b (ISUB k)))]
    [(Mul a b) (compile a (compile b (IMUL k)))]))

;; ----- The machine -----------------------------------------------
;; Types are erased at runtime, so the operand stack is a plain
;; `List Integer`.  The `Code`'s type already proved every op has its
;; operands, so the underflow arms below are unreachable for compiled
;; code — they exist only to keep the runtime match total.

(: binop (-> (-> Integer (-> Integer Integer))
             (-> (List Integer) (List Integer))))
(define (binop f stack)
  (match stack
    [(Cons x (Cons y rest)) (Cons (f y x) rest)]
    [_ (panic "stack underflow — unreachable for compiled code")]))

(: exec (-> (Code s t) (-> (List Integer) (List Integer))))
(define (exec c stack)
  (match c
    [(HALT)      stack]
    [(PUSHI n k) (exec k (Cons n stack))]
    [(IADD k)    (exec k (binop (lambda (x y) (+ x y)) stack))]
    [(ISUB k)    (exec k (binop (lambda (x y) (- x y)) stack))]
    [(IMUL k)    (exec k (binop (lambda (x y) (* x y)) stack))]))

;; Run the code from the empty stack and read back the single result.
(: top-result (-> (Code s t) Integer))
(define (top-result code)
  (match (exec code Nil)
    [(Cons result _) result]
    [(Nil) (panic "no result on the stack — unreachable")]))

;; ----- Disassembly -----------------------------------------------
;; Render the compiled code as one mnemonic per instruction.

(: disasm (-> (Code s t) (List String)))
(define (disasm c)
  (match c
    [(HALT)      Nil]
    [(PUSHI n k) (Cons (string-append "push " (integer->string n)) (disasm k))]
    [(IADD k)    (Cons "add" (disasm k))]
    [(ISUB k)    (Cons "sub" (disasm k))]
    [(IMUL k)    (Cons "mul" (disasm k))]))

;; Print each instruction on its own indented line (IO over a List).
(: print-instrs (-> (List String) (IO Unit)))
(define (print-instrs instrs)
  (match instrs
    [(Nil)         (pure Unit)]
    [(Cons i rest) (let& ([_ (println (string-append "    " i))])
                     (print-instrs rest))]))

;; ----- Demo ------------------------------------------------------

;; (2 + 3) * 4 = 20
(: e1 Expr)
(define e1 (Mul (Add (Lit 2) (Lit 3)) (Lit 4)))

;; 10 - (2 * 3) = 4
(: e2 Expr)
(define e2 (Sub (Lit 10) (Mul (Lit 2) (Lit 3))))

;; (7 - 2) * (1 + 4) = 25
(: e3 Expr)
(define e3 (Mul (Sub (Lit 7) (Lit 2)) (Add (Lit 1) (Lit 4))))

;; Compile once, then print the result and the instruction listing.
(: demo (-> String (-> Expr (IO Unit))))
(define (demo label e)
  (let ([code (compile e HALT)])
    (let& ([_ (println (string-append label
                                      (string-append " = " (integer->string (top-result code)))))]
           [_ (print-instrs (disasm code))])
      (println ""))))

(: main (IO Unit))
(define main
  (let& ([_ (println "compiling expressions to a typed stack machine:")]
         [_ (demo "(2 + 3) * 4    " e1)]
         [_ (demo "10 - (2 * 3)   " e2)]
         [_ (demo "(7 - 2)*(1 + 4)" e3)])
    (println "done.")))
