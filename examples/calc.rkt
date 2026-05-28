#lang rackton

;; calc.rkt — a small expression interpreter in Rackton.
;;
;; Surface language:
;;   integer literal        ⇒ that integer
;;   identifier             ⇒ the value bound to it
;;   (+ a b) (- a b) (* a b)⇒ arithmetic
;;   (let x v body)         ⇒ bind `x` to (eval v) while evaluating body
;;
;; Run it with `racket examples/calc.rkt`; it loops over stdin until
;; an empty line is entered.  Lines are read with Racket's `read`
;; through the host-language escape, then walked into a typed AST
;; before evaluation.

;; ----- Sexpr: result of a single `read` -------------------------

(data Sexpr
  (SInt Integer)
  (SSym String)
  (SList (List Sexpr)))

;; ----- Expr: the typed AST after the surface parser -------------

(data Expr
  (EInt Integer)
  (EVar String)
  (EAdd Expr Expr)
  (ESub Expr Expr)
  (EMul Expr Expr)
  (ELet String Expr Expr))

;; ----- Reading: String → Sexpr via the host escape -------------

(: parse-line (-> String Sexpr))
(define (parse-line line)
  (racket Sexpr (line)
    ;; Use Racket's `read` to lex+parse one s-expression from the
    ;; line.  Convert the resulting Racket value into our Sexpr ADT.
    (define raw
      (let ([port (open-input-string line)])
        (read port)))
    (let walk ([x raw])
      (cond
        [(exact-integer? x) (SInt x)]
        [(symbol? x)        (SSym (symbol->string x))]
        [(pair? x)
         (SList
          (let to-rackton ([ys x])
            (cond
              [(null? ys) Nil]
              [else (Cons (walk (car ys)) (to-rackton (cdr ys)))])))]
        [(null? x) (SList Nil)]
        [else (panic "unsupported atom in source")]))))

;; ----- Sexpr → Expr parser -------------------------------------
;; All parser signatures declared up front so the mutually-recursive
;; defines below can forward-reference one another.

(: parse-expr   (-> Sexpr (Result String Expr)))
(: parse-list   (-> (List Sexpr) (Result String Expr)))
(: parse-form   (-> String (-> (List Sexpr) (Result String Expr))))
(: parse-binary (-> (-> Expr (-> Expr Expr))
                    (-> (List Sexpr) (-> String (Result String Expr)))))
(: parse-let    (-> (List Sexpr) (Result String Expr)))

(define (parse-expr s)
  (match s
    [(SInt n)   (Ok (EInt n))]
    [(SSym x)   (Ok (EVar x))]
    [(SList xs) (parse-list xs)]))

(define (parse-list xs)
  (match xs
    [(Nil)
     (Err "empty form")]
    [(Cons head rest)
     (match head
       [(SSym name) (parse-form name rest)]
       [_           (Err "expected operator at head of form")])]))

(define (parse-form op rest)
  (if (== op "+")   (parse-binary EAdd rest "+")
  (if (== op "-")   (parse-binary ESub rest "-")
  (if (== op "*")   (parse-binary EMul rest "*")
  (if (== op "let") (parse-let rest)
                    (Err (string-append "unknown operator: " op)))))))

(define (parse-binary mk rest op)
  (match rest
    [(Cons a (Cons b (Nil)))
     (flatmap (lambda (ea)
                (flatmap (lambda (eb) (Ok (mk ea eb)))
                         (parse-expr b)))
              (parse-expr a))]
    [_ (Err (string-append op ": expected two operands"))]))

(define (parse-let rest)
  (match rest
    [(Cons (SSym name) (Cons val (Cons body (Nil))))
     (flatmap (lambda (ev)
                (flatmap (lambda (eb) (Ok (ELet name ev eb)))
                         (parse-expr body)))
              (parse-expr val))]
    [_ (Err "let: expected (let name value body)")]))

;; ----- Evaluator -----------------------------------------------

(: eval-expr   (-> (Map String Integer) (-> Expr (Result String Integer))))
(: eval-binary (-> (Map String Integer)
                   (-> Expr
                       (-> Expr
                           (-> (-> Integer (-> Integer Integer))
                               (Result String Integer))))))

(define (eval-expr env e)
  (match e
    [(EInt n) (Ok n)]
    [(EVar x)
     (match (map-lookup x env)
       [(None)   (Err (string-append "unbound variable: " x))]
       [(Some v) (Ok v)])]
    [(EAdd a b) (eval-binary env a b (lambda (x y) (+ x y)))]
    [(ESub a b) (eval-binary env a b (lambda (x y) (- x y)))]
    [(EMul a b) (eval-binary env a b (lambda (x y) (* x y)))]
    [(ELet name val body)
     (flatmap (lambda (v) (eval-expr (map-insert name v env) body))
              (eval-expr env val))]))

(define (eval-binary env a b f)
  (flatmap (lambda (x)
             (flatmap (lambda (y) (Ok (f x y)))
                      (eval-expr env b)))
           (eval-expr env a)))

;; ----- Pretty -----------------------------------------------

(: result->string (-> (Result String Integer) String))
(define (result->string r)
  (match r
    [(Err msg) (string-append "error: " msg)]
    [(Ok n)    (integer->string n)]))

;; Read → parse → evaluate, all packaged.
(: process (-> String String))
(define (process line)
  (result->string (flatmap (lambda (e) (eval-expr empty-map e))
                           (parse-expr (parse-line line)))))

;; ----- REPL loop in IO --------------------------------------

(: repl-step (IO Unit))
(define repl-step
  (do [_  <- (print "calc> ")]
      [ln <- read-line]
    (if (== ln "")
        (println "bye!")
        (do [_ <- (println (process ln))]
          repl-step))))

;; Top-level: run the loop on module load (interactive use).
(: main Unit)
(define main (run-io repl-step))
