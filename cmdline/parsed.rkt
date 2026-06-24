#lang rackton

;; rackton/cmdline/parsed — the parsed command line that a Term reads
;; from, plus the evaluation-error type.
;;
;; Step 4 builds the argv -> ParseCtx parser; this module is the shared
;; target it produces and that the argument extractors in argval.rkt
;; consume.  Keeping it separate breaks the arg <-> term dependency
;; cycle (term references ParseCtx/EvalError; argval references Term).

(provide (struct-out ParseCtx)
         (data-out EvalError)
         empty-ctx mk-ctx
         ctx-occurrences ctx-flag-count ctx-opt-values ctx-positionals)

;; Placeholder error; Step 6 refines the variants (EvParse / EvTerm /
;; EvExn).  Term's type signature stays stable across that refinement.
(data EvalError (EvErr String))

;; named:       each option NAME mapped to the value strings recorded for
;;              its occurrences, earliest first.  A flag records one entry
;;              per occurrence (the value is irrelevant — only the count
;;              matters).
;; positionals: the positional arguments, left to right.
(struct ParseCtx
  [named       : (List (Pair String (List String)))]
  [positionals : (List String)])

(: empty-ctx ParseCtx)
(define empty-ctx (ParseCtx Nil Nil))

(: mk-ctx (-> (List (Pair String (List String))) (List String) ParseCtx))
(define (mk-ctx named positionals) (ParseCtx named positionals))

;; does k match any of names?
(: any-name? (-> String (List String) Boolean))
(define (any-name? k names)
  (match names
    [(Nil)         #f]
    [(Cons n rest) (if (== k n) #t (any-name? k rest))]))

;; all recorded value-strings for any of `names`, concatenated in the
;; ctx's stored (earliest-first) order.
(: ctx-occurrences (-> ParseCtx (List String) (List String)))
(define (ctx-occurrences ctx names)
  (match ctx
    [(ParseCtx named _)
     (foldr (lambda (entry acc)
              (match entry
                [(Pair k vs) (if (any-name? k names) (append vs acc) acc)]))
            Nil
            named)]))

;; how many times any of `names` occurred (a flag's occurrence count).
(: ctx-flag-count (-> ParseCtx (List String) Integer))
(define (ctx-flag-count ctx names) (length (ctx-occurrences ctx names)))

;; the value strings given for any of `names` (a valued option).
(: ctx-opt-values (-> ParseCtx (List String) (List String)))
(define (ctx-opt-values ctx names) (ctx-occurrences ctx names))

;; the positional arguments, left to right.
(: ctx-positionals (-> ParseCtx (List String)))
(define (ctx-positionals ctx) (match ctx [(ParseCtx _ ps) ps]))
