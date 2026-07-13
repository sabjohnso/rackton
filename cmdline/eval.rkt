#lang rackton

;; rackton/cmdline/eval — commands and evaluation (cmdliner's
;; Cmdliner.Cmd + Cmd.eval*).
;;
;; A Cmd is a leaf (info + term) or a group (info + optional default
;; term + subcommands, recursive to any depth).  `eval-core` is the
;; PURE evaluator: given the environment map and argv it dispatches
;; subcommands, runs the leaf's term, and produces an EvalOutcome
;; (value / help text / version / failure).  The IO wrappers read argv
;; and the declared environment variables, then call eval-core.
;;
(require rackton/cmdline/arg            ; ArgDecl, ArgInfo, EnvInfo
         rackton/cmdline/term           ; Term, term-args, term-run
         rackton/cmdline/parser         ; parse-argv
         rackton/cmdline/parsed         ; ParseCtx, EvalError, EvErr
         rackton/cmdline/manpage        ; sections, render-plain, Subst
         rackton/cmdline/run            ; term->specs
         rackton/data/result            ; Result, Ok, Err
         rackton/system/environment)    ; argv, getenv

(provide (struct-out CmdInfo)
         (data-out Cmd)
         (data-out EvalOutcome)
         cmd-info cmd-doc cmd-version cmd-v cmd-group cmd-name
         eval-core eval-value eval)

;; ----- command metadata + tree -------------------------------------

(struct CmdInfo
  [name    : String]
  [doc     : String]
  [version : (Maybe String)])

(data (Cmd a)
  (CmdLeaf  CmdInfo (Term a))
  (CmdGroup CmdInfo (Maybe (Term a)) (List (Cmd a))))

(data (EvalOutcome a)
  (OkValue a)
  (HelpText String)
  (VersionText String)
  (EvalFail EvalError))

;; ----- builders ----------------------------------------------------

(: cmd-info (-> String CmdInfo))
(define (cmd-info name) (CmdInfo name "" None))

(: cmd-doc (-> String CmdInfo CmdInfo))
(define (cmd-doc d i) (match i [(CmdInfo n _ v) (CmdInfo n d v)]))

(: cmd-version (-> String CmdInfo CmdInfo))
(define (cmd-version v i) (match i [(CmdInfo n d _) (CmdInfo n d (Some v))]))

(: cmd-v (-> CmdInfo (Term a) (Cmd a)))
(define (cmd-v info t) (CmdLeaf info t))

(: cmd-group (-> CmdInfo (Maybe (Term a)) (List (Cmd a)) (Cmd a)))
(define (cmd-group info default subs) (CmdGroup info default subs))

(: cmd-name (-> (Cmd a) String))
(define (cmd-name c)
  (match c [(CmdLeaf i _) (CmdInfo-name i)] [(CmdGroup i _ _) (CmdInfo-name i)]))

;; ----- small helpers -----------------------------------------------

(: cat-maybes (-> (List (Maybe a)) (List a)))
(define (cat-maybes xs)
  (match xs
    [(Nil)                Nil]
    [(Cons (Some x) rest) (Cons x (cat-maybes rest))]
    [(Cons (None) rest)   (cat-maybes rest)]))

(: has-token? (-> String (List String) Boolean))
(define (has-token? tok argv)
  (match argv [(Nil) #f] [(Cons t rest) (if (== t tok) #t (has-token? tok rest))]))

(: find-sub (-> (List (Cmd a)) String (Maybe (Cmd a))))
(define (find-sub subs name)
  (match subs
    [(Nil) None]
    [(Cons c rest) (if (== (cmd-name c) name) (Some c) (find-sub rest name))]))

(: cmd-doc-of (-> (Cmd a) String))
(define (cmd-doc-of c)
  (match c [(CmdLeaf i _) (CmdInfo-doc i)] [(CmdGroup i _ _) (CmdInfo-doc i)]))

;; ----- help rendering ----------------------------------------------

(: app2 (-> String String String))
(define (app2 a b) (string-append a b))

;; ArgInfos of the named (flag/opt) declarations, for the OPTIONS list.
(: named-infos (-> (List ArgDecl) (List ArgInfo)))
(define (named-infos decls)
  (cat-maybes
   (fmap (lambda (d) (match d [(ArgDecl i (KPos)) None] [(ArgDecl i _) (Some i)])) decls)))

(: default-exits (List (Pair Integer String)))
(define default-exits (list (Pair 0 "on success") (Pair 124 "on a command-line error")))

(: leaf-help (-> CmdInfo (Term a) String))
(define (leaf-help info t)
  (let ([name (CmdInfo-name info)])
    (render-plain
     (append (name-section name (CmdInfo-doc info))
             (append (synopsis-section name "[OPTION]...")
                     (append (options-section (named-infos (term-args t)))
                             (exit-section default-exits))))
     (Subst name name))))

(: command-items (-> (List (Cmd a)) (List ManBlock)))
(define (command-items subs)
  (fmap (lambda (c) (ManI (cmd-name c) (cmd-doc-of c))) subs))

(: group-help (-> CmdInfo (List (Cmd a)) String))
(define (group-help info subs)
  (let ([name (CmdInfo-name info)])
    (render-plain
     (append (name-section name (CmdInfo-doc info))
             (Cons (ManS "COMMANDS") (command-items subs)))
     (Subst name name))))

(: version-outcome (-> CmdInfo (EvalOutcome a)))
(define (version-outcome info)
  (match (CmdInfo-version info)
    [(Some v) (VersionText v)]
    [(None)   (EvalFail (EvErr "no version information"))]))

;; ----- environment seeding -----------------------------------------

(: lookup-env (-> (List (Pair String String)) String (Maybe String)))
(define (lookup-env env var)
  (match env
    [(Nil) None]
    [(Cons (Pair k v) rest) (if (== k var) (Some v) (lookup-env rest var))]))

(: add-occ (-> ParseCtx String String ParseCtx))
(define (add-occ ctx name v)
  (match ctx [(ParseCtx named pos) (ParseCtx (append named (list (Pair name (list v)))) pos)]))

;; For each named declaration with an env var that is ABSENT from argv,
;; seed an occurrence from the environment (lowest precedence).
(: seed-one (-> ArgDecl (List (Pair String String)) ParseCtx ParseCtx))
(define (seed-one d envmap ctx)
  (match d
    [(ArgDecl info kind)
     (match kind
       [(KPos) ctx]
       [_ (match (ArgInfo-env info)
            [(None) ctx]
            [(Some env)
             (if (> (ctx-flag-count ctx (ArgInfo-names info)) 0)
                 ctx
                 (match (lookup-env envmap (EnvInfo-var env))
                   [(None)   ctx]
                   [(Some v) (add-occ ctx (canonical info) v)]))])])]))

(: canonical (-> ArgInfo String))
(define (canonical info) (match (ArgInfo-names info) [(Cons n _) n] [(Nil) ""]))

(: seed-env (-> (List ArgDecl) (List (Pair String String)) ParseCtx ParseCtx))
(define (seed-env decls envmap ctx)
  (foldr (lambda (d acc) (seed-one d envmap acc)) ctx decls))

;; ----- the pure evaluator ------------------------------------------

(: eval-leaf (-> CmdInfo (Term a) (List (Pair String String)) (List String) (EvalOutcome a)))
(define (eval-leaf info t envmap argv)
  (cond
    [(has-token? "--help" argv)    (HelpText (leaf-help info t))]
    [(has-token? "--version" argv) (version-outcome info)]
    [else
     (match (parse-argv (term->specs t) argv)
       [(Err e)  (EvalFail e)]
       [(Ok ctx)
        (match (term-run t (seed-env (term-args t) envmap ctx))
          [(Ok v)  (OkValue v)]
          [(Err e) (EvalFail e)])])]))

(: eval-core (-> (Cmd a) (List (Pair String String)) (List String) (EvalOutcome a)))
(define (eval-core cmd envmap argv)
  (match cmd
    [(CmdLeaf info t) (eval-leaf info t envmap argv)]
    [(CmdGroup info default subs)
     (match argv
       [(Nil) (match default
                [(Some t) (eval-leaf info t envmap Nil)]
                [(None)   (HelpText (group-help info subs))])]
       [(Cons tok rest)
        (cond
          [(== tok "--help")    (HelpText (group-help info subs))]
          [(== tok "--version") (version-outcome info)]
          [else (match (find-sub subs tok)
                  [(Some sub) (eval-core sub envmap rest)]
                  [(None)     (EvalFail (EvErr (app2 "unknown command: " tok)))])])])]))

;; ----- IO wrappers -------------------------------------------------

;; environment-variable names declared by a term's options.
(: decls-env-vars (-> (List ArgDecl) (List String)))
(define (decls-env-vars decls)
  (cat-maybes
   (fmap (lambda (d)
           (match d
             [(ArgDecl info _)
              (match (ArgInfo-env info) [(Some e) (Some (EnvInfo-var e))] [(None) None])]))
         decls)))

;; every environment variable mentioned anywhere in the command tree.
(: collect-env-vars (-> (Cmd a) (List String)))
(define (collect-env-vars cmd)
  (match cmd
    [(CmdLeaf _ t) (decls-env-vars (term-args t))]
    [(CmdGroup _ d subs)
     (append (match d [(Some t) (decls-env-vars (term-args t))] [(None) Nil])
             (foldr (lambda (c acc) (append (collect-env-vars c) acc)) Nil subs))]))

;; read the given variables into an (name . value) map.
(: read-envmap (-> (List String) (IO (List (Pair String String)))))
(define (read-envmap vars)
  (match vars
    [(Nil) (pure Nil)]
    [(Cons v rest)
     (let& ([mv   (getenv v)]
            [more (read-envmap rest)])
       (pure (match mv [(Some val) (Cons (Pair v val) more)] [(None) more])))]))

(: eval-error-message (-> EvalError String))
(define (eval-error-message e) (match e [(EvErr m) m]))

;; Get the parsed outcome: read argv + declared env vars, run eval-core.
(: eval-value (-> (Cmd a) (IO (EvalOutcome a))))
(define (eval-value cmd)
  (let& ([args argv]
         [env  (read-envmap (collect-env-vars cmd))])
    (pure (eval-core cmd env args))))

;; Run a command whose term yields an IO action; print help/version/
;; errors and return a conventional exit code.
(: eval (-> (Cmd (IO Unit)) (IO Integer)))
(define (eval cmd)
  (let& ([outcome (eval-value cmd)])
    (match outcome
      [(OkValue act)    (let& ([_ act]) (pure 0))]
      [(HelpText t)     (let& ([_ (println t)]) (pure 0))]
      [(VersionText v)  (let& ([_ (println v)]) (pure 0))]
      [(EvalFail e)     (let& ([_ (println (eval-error-message e))]) (pure 124))])))
