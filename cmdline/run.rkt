#lang rackton

;; rackton/cmdline/run — the pure bridge from a Term to a parsed value.
;;
;; `term->specs` reads each declaration's kind to build the parser's
;; option specs; `run-term` parses argv against those specs and runs the
;; Term over the resulting context.  No IO — eval (Step 6b) wraps this
;; with argv/env/help/exit.
;;
(require rackton/cmdline/arg          ; ArgDecl, ArgInfo, ArgKind
         rackton/cmdline/term         ; Term, term-args, term-run
         rackton/cmdline/parser       ; OptSpec, parse-argv
         rackton/cmdline/parsed       ; ParseCtx, EvalError
         rackton/data/result)         ; Result, Ok, Err

(provide term->specs run-term)

;; keep the Some elements of a list of Maybe.
(: cat-maybes (-> (List (Maybe a)) (List a)))
(define (cat-maybes xs)
  (match xs
    [(Nil)                Nil]
    [(Cons (Some x) rest) (Cons x (cat-maybes rest))]
    [(Cons (None) rest)   (cat-maybes rest)]))

;; A named declaration (flag/opt) becomes an OptSpec; positionals are
;; handled generically by the parser and contribute no spec.
(: decl->spec (-> ArgDecl (Maybe OptSpec)))
(define (decl->spec d)
  (match d
    [(ArgDecl info kind)
     (match kind
       [(KFlag) (Some (OptSpec (ArgInfo-names info) #f))]
       [(KOpt)  (Some (OptSpec (ArgInfo-names info) #t))]
       [(KPos)  None])]))

(: term->specs (-> (Term a) (List OptSpec)))
(define (term->specs t) (cat-maybes (fmap decl->spec (term-args t))))

(: run-term (-> (Term a) (List String) (Result EvalError a)))
(define (run-term t argv)
  (match (parse-argv (term->specs t) argv)
    [(Err e)  (Err e)]
    [(Ok ctx) (term-run t ctx)]))
