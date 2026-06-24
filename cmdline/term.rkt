#lang rackton

;; rackton/cmdline/term — the applicative Term (cmdliner's Cmdliner.Term).
;;
;; A `Term a` carries two things at once:
;;   1. the static list of ArgInfo it declares (for help / usage / the
;;      synopsis and unknown-option detection), and
;;   2. a function that, given the parsed command line, produces the
;;      value (or an error).
;;
;; Term is an APPLICATIVE, not a Monad — deliberately, so the full set
;; of arguments is known before parsing.  `pure` declares nothing;
;; `fapply` UNIONS the two argument lists and threads the parse.
;;
;; STATUS: red.  `fapply` is stubbed (drops the right operand's args
;; and always errors), so term-test.rkt fails; the Functor instance is
;; derived through `fapply`, so it is red too.  The next commit (green)
;; supplies the real `fapply`.

(require rackton/cmdline/arg          ; ArgInfo
         rackton/cmdline/parsed       ; ParseCtx / EvalError / EvErr
         rackton/data/result)         ; Result / Ok / Err

(provide (data-out Term)
         term-args term-run term-app)

(data (Term a)
  (Term (List ArgDecl)                       ; declared arguments
        (-> ParseCtx (Result EvalError a)))) ; assemble the value

(: term-args (-> (Term a) (List ArgDecl)))
(define (term-args t) (match t [(Term i _) i]))

(: term-run (-> (Term a) ParseCtx (Result EvalError a)))
(define (term-run t c) (match t [(Term _ f) (f c)]))

;; ----- the applicative ---------------------------------------------

;; #:derive-supers synthesizes the Functor Term superclass from the
;; prelude derivation `fmap f = fapply (pure f)`, so we supply only the
;; two irreducible methods.
(instance (Applicative Term) #:derive-supers
  ;; pure declares nothing and ignores the parsed command line.
  (define (pure x) (Term Nil (lambda (c) (Ok x))))

  ;; fapply unions the declared arguments (left-to-right, so the
  ;; synopsis order is stable) and threads the parsed command line to
  ;; both sides, propagating the first Err.
  (define (fapply tf ta)
    (match* (tf ta)
      [((Term i1 f) (Term i2 g))
       (Term (append i1 i2)
             (lambda (c)
               (match (f c)
                 [(Err e)  (Err e)]
                 [(Ok fn)
                  (match (g c)
                    [(Err e) (Err e)]
                    [(Ok x)  (Ok (fn x))])])))])))

;; cmdliner's `const f $ a` apply, by name.
(: term-app (-> (Term (-> a b)) (Term a) (Term b)))
(define (term-app tf ta) (fapply tf ta))
