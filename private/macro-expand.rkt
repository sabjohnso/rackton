#lang racket/base

;; Phase-0 user-macro helpers shared by the analyzer and the REPL.
;;
;; A Rackton macro is a real Racket transformer.  Both the REPL kernel
;; (`repl.rkt`) and the analysis layer (`analyze.rkt`) process Rackton
;; forms one at a time at *runtime* (phase 0), so neither can reuse the
;; elaborator's phase-1 macro pass (`expand-user-macros` in
;; `elaborate.rkt`, which depends on `syntax-local-*`).  Instead they
;; bind transformers into a live namespace and single-step-expand uses.
;; These pure helpers — recognizing macro-definition forms, naming the
;; identifiers they bind, and walking a form to expand registered-macro
;; uses — are common to both; the namespace each caller supplies (a REPL
;; session namespace, or the analyzer's throwaway one) is the only part
;; that differs, so it stays at the call site.
;;
;; Tenets: separation of concerns (the macro-walk knows nothing about
;; which namespace it runs in — that is `current-namespace`); small,
;; focused interface depended on by `analyze.rkt` and `repl.rkt`.
;;
;; Recognizing a macro-definition form is deliberately NOT here: the
;; analyzer holds forms as syntax, the REPL as data, so each keeps its
;; own one-line `macro-def-form?` over its own representation.

(provide macro-def-names
         head-macro?
         expand-macro-walk)

(require racket/match)

;; The macro name(s) a macro-definition form introduces.  `form` is a
;; datum (the `syntax->datum` of a macro-definition form).
(define (macro-def-names form)
  (match form
    [(list 'define-syntax (cons name _) _ ...)      (list name)] ; (define-syntax (m . args) body)
    [(list 'define-syntax (? symbol? name) _ ...)   (list name)] ; (define-syntax m expr)
    [(list 'define-syntax-rule (cons name _) _ ...) (list name)] ; (define-syntax-rule (m . pat) tmpl)
    [(list 'define-syntaxes (list names ...) _ ...) names]       ; (define-syntaxes (m ...) expr)
    [_ '()]))

;; Does `head-stx` name one of the `names` registered as user macros?
(define (head-macro? head-stx names)
  (and (identifier? head-stx)
       (and (memq (syntax-e head-stx) names) #t)))

;; One structural pass over `stx`: while the head names a registered
;; macro, take a single expansion step with `expand-once` (which fires
;; the transformer without lowering the result into Racket core syntax
;; the way a full `expand` would), then recurse into every sub-form.
;; The `names` guard is essential — `expand-once` on a plain application
;; like `(+ 1 2)` would lower it to `(#%app + 1 2)`, which `parse-top`
;; cannot read; guarding on registered-macro heads keeps ordinary forms
;; untouched.  Expansion resolves transformers against the caller's
;; `current-namespace`.
(define (expand-macro-walk stx names)
  (define l (syntax->list stx))
  (cond
    [(or (not l) (null? l)) stx]
    [(head-macro? (car l) names)
     (expand-macro-walk (expand-once stx) names)]
    [else
     (datum->syntax stx
                    (map (lambda (s) (expand-macro-walk s names)) l)
                    stx stx)]))
