#lang rackton

;; rackton/cmdline/argval — argument constructors (cmdliner's
;; Arg.flag/opt/…) and the cardinality adapters (value / required /
;; non-empty / last) that turn an Arg into a Term.
;;
;; An `Arg a` is a single argument's declaration plus an extractor that
;; reads its value out of the parsed command line.  A cardinality
;; adapter applied exactly once turns it into a `Term a`, fixing the
;; absence policy.
;;
(require rackton/cmdline/arg          ; ArgInfo, ArgInfo-names
         rackton/cmdline/conv         ; Conv, conv-parse
         rackton/cmdline/parsed       ; ParseCtx, EvalError, lookups
         rackton/cmdline/term         ; Term
         rackton/data/result)         ; Result, Ok, Err

(provide (data-out Arg)
         arg-decls arg-run
         flag flag-all opt opt-all
         pos pos-all pos-left pos-right
         vflag vflag-all
         value required non-empty last)

(data (Arg a)
  (Arg (List ArgDecl) (-> ParseCtx (Result EvalError a))))

(: arg-decls (-> (Arg a) (List ArgDecl)))
(define (arg-decls a) (match a [(Arg d _) d]))

;; declare one argument of a given kind.
(: decl1 (-> ArgInfo ArgKind (List ArgDecl)))
(define (decl1 info kind) (list (ArgDecl info kind)))

(: arg-run (-> (Arg a) ParseCtx (Result EvalError a)))
(define (arg-run a ctx) (match a [(Arg _ f) (f ctx)]))

;; run a converter, tagging a parse failure as an EvalError.
(: run-conv (-> (Conv a) String (Result EvalError a)))
(define (run-conv c s)
  (match (conv-parse c s) [(Ok x) (Ok x)] [(Err m) (Err (EvErr m))]))

;; parse every string, failing on the first bad one.
(: run-conv-all (-> (Conv a) (List String) (Result EvalError (List a))))
(define (run-conv-all c xs)
  (match xs
    [(Nil) (Ok Nil)]
    [(Cons s rest)
     (match (run-conv c s)
       [(Err e) (Err e)]
       [(Ok v)  (match (run-conv-all c rest)
                  [(Err e)  (Err e)]
                  [(Ok vs)  (Ok (Cons v vs))])])]))

;; one copy of x per count (a flag's occurrences).
(: repeat (-> Integer a (List a)))
(define (repeat n x) (if (<= n 0) Nil (Cons x (repeat (- n 1) x))))

;; ----- constructors ------------------------------------------------

(: flag (-> ArgInfo (Arg Boolean)))
(define (flag info)
  (Arg (decl1 info KFlag)
       (lambda (ctx) (Ok (> (ctx-flag-count ctx (ArgInfo-names info)) 0)))))

(: flag-all (-> ArgInfo (Arg (List Boolean))))
(define (flag-all info)
  (Arg (decl1 info KFlag)
       (lambda (ctx) (Ok (repeat (ctx-flag-count ctx (ArgInfo-names info)) #t)))))

(: opt (-> (Conv a) a ArgInfo (Arg a)))
(define (opt c default info)
  (Arg (decl1 info KOpt)
       (lambda (ctx)
         (match (reverse (ctx-opt-values ctx (ArgInfo-names info)))
           [(Cons v _) (run-conv c v)]      ; last occurrence wins
           [(Nil)      (Ok default)]))))

(: opt-all (-> (Conv a) (List a) ArgInfo (Arg (List a))))
(define (opt-all c defaults info)
  (Arg (decl1 info KOpt)
       (lambda (ctx)
         (match (ctx-opt-values ctx (ArgInfo-names info))
           [(Nil) (Ok defaults)]
           [vals  (run-conv-all c vals)]))))

;; ----- list helpers for positionals --------------------------------

;; the element at index i, if any.
(: nth (-> (List a) Integer (Maybe a)))
(define (nth xs i)
  (match xs
    [(Nil)         None]
    [(Cons x rest) (if (<= i 0) (Some x) (nth rest (- i 1)))]))

;; the first n elements (indices 0 .. n-1).
(: take-n (-> Integer (List a) (List a)))
(define (take-n n xs)
  (if (<= n 0) Nil
      (match xs [(Nil) Nil] [(Cons x rest) (Cons x (take-n (- n 1) rest))])))

;; all but the first n elements.
(: drop-n (-> Integer (List a) (List a)))
(define (drop-n n xs)
  (if (<= n 0) xs
      (match xs [(Nil) Nil] [(Cons _ rest) (drop-n (- n 1) rest)])))

;; ----- positional constructors -------------------------------------

(: pos (-> Integer (Conv a) a ArgInfo (Arg a)))
(define (pos n c default info)
  (Arg (decl1 info KPos)
       (lambda (ctx)
         (match (nth (ctx-positionals ctx) n)
           [(Some s) (run-conv c s)]
           [(None)   (Ok default)]))))

(: pos-all (-> (Conv a) (List a) ArgInfo (Arg (List a))))
(define (pos-all c defaults info)
  (Arg (decl1 info KPos)
       (lambda (ctx)
         (match (ctx-positionals ctx)
           [(Nil) (Ok defaults)]
           [ps    (run-conv-all c ps)]))))

;; positionals strictly left of index n (cmdliner pos_left).
(: pos-left (-> Integer (Conv a) (List a) ArgInfo (Arg (List a))))
(define (pos-left n c defaults info)
  (Arg (decl1 info KPos)
       (lambda (ctx)
         (match (take-n n (ctx-positionals ctx))
           [(Nil) (Ok defaults)]
           [ps    (run-conv-all c ps)]))))

;; positionals strictly right of index n (cmdliner pos_right).
(: pos-right (-> Integer (Conv a) (List a) ArgInfo (Arg (List a))))
(define (pos-right n c defaults info)
  (Arg (decl1 info KPos)
       (lambda (ctx)
         (match (drop-n (+ n 1) (ctx-positionals ctx))
           [(Nil) (Ok defaults)]
           [ps    (run-conv-all c ps)]))))

;; ----- enumerated flags --------------------------------------------

;; the alternative values whose flag occurred (declaration order).
(: present-vals (-> ParseCtx (List (Pair a ArgInfo)) (List a)))
(define (present-vals ctx alts)
  (match alts
    [(Nil) Nil]
    [(Cons (Pair v info) rest)
     (if (> (ctx-flag-count ctx (ArgInfo-names info)) 0)
         (Cons v (present-vals ctx rest))
         (present-vals ctx rest))]))

;; one copy of each alternative's value per occurrence (declaration order).
(: present-vals-all (-> ParseCtx (List (Pair a ArgInfo)) (List a)))
(define (present-vals-all ctx alts)
  (match alts
    [(Nil) Nil]
    [(Cons (Pair v info) rest)
     (append (repeat (ctx-flag-count ctx (ArgInfo-names info)) v)
             (present-vals-all ctx rest))]))

(: vflag (-> a (List (Pair a ArgInfo)) (Arg a)))
(define (vflag default alts)
  (Arg (fmap (lambda (i) (ArgDecl i KFlag)) (fmap snd alts))
       (lambda (ctx)
         (match (present-vals ctx alts)
           [(Nil)          (Ok default)]
           [(Cons v (Nil)) (Ok v)]
           [_              (Err (EvErr "these flags are mutually exclusive"))]))))

(: vflag-all (-> (List a) (List (Pair a ArgInfo)) (Arg (List a))))
(define (vflag-all defaults alts)
  (Arg (fmap (lambda (i) (ArgDecl i KFlag)) (fmap snd alts))
       (lambda (ctx)
         (match (present-vals-all ctx alts)
           [(Nil) (Ok defaults)]
           [vs    (Ok vs)]))))

;; ----- cardinality adapters (real) ---------------------------------

(: value (-> (Arg a) (Term a)))
(define (value a) (match a [(Arg infos f) (Term infos f)]))

(: required (-> (Arg (Maybe a)) (Term a)))
(define (required a)
  (match a
    [(Arg infos f)
     (Term infos
           (lambda (ctx)
             (match (f ctx)
               [(Err e)       (Err e)]
               [(Ok (Some x)) (Ok x)]
               [(Ok (None))   (Err (EvErr "required argument is missing"))])))]))

(: non-empty (-> (Arg (List a)) (Term (List a))))
(define (non-empty a)
  (match a
    [(Arg infos f)
     (Term infos
           (lambda (ctx)
             (match (f ctx)
               [(Err e)    (Err e)]
               [(Ok (Nil)) (Err (EvErr "missing a required argument"))]
               [(Ok xs)    (Ok xs)])))]))

(: last (-> (Arg (List a)) (Term a)))
(define (last a)
  (match a
    [(Arg infos f)
     (Term infos
           (lambda (ctx)
             (match (f ctx)
               [(Err e)    (Err e)]
               [(Ok (Nil)) (Err (EvErr "missing a required argument"))]
               [(Ok xs)    (match (reverse xs)
                             [(Cons x _) (Ok x)]
                             [(Nil)      (Err (EvErr "unreachable"))])])))]))
