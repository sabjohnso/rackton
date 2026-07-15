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
     (expand-macro-walk (neutralize-argument-scopes stx (expand-once stx)) names)]
    [else
     (datum->syntax stx
                    (map (lambda (s) (expand-macro-walk s names)) l)
                    stx stx)]))

;; `expand-once` runs a transformer through the macro expander, which stamps
;; the macro's ARGUMENTS with a fresh use-site scope (and the namespace's
;; scopes).  An identifier the macro forwards from an argument therefore
;; carries scopes the surrounding, un-expanded code lacks — and once a session
;; has defined any macro the surface parser resolves a local binder by
;; `bound-identifier=?` (exact scope-set match).  A local binder written
;; OUTSIDE the macro call (a function parameter passed into the macro, say)
;; then fails to match the forwarded reference, and inference reports it
;; unbound.  (A top-level binding is unaffected — it resolves by name; a binder
;; the macro itself introduces keeps a distinct macro-introduction scope, which
;; is the hygiene we must preserve.)
;;
;; Undo exactly the expander's argument stamping: find an argument identifier
;; the expansion forwarded — one whose symbol came from the use form and that
;; carries no macro-introduction scope — and remove the scopes it gained from
;; the whole expansion.  Forwarded arguments return to their use-site scopes
;; (matching outside binders); macro-introduced identifiers never carried the
;; argument scopes, so they keep their macro-introduction scope and stay
;; hygienically distinct.  With nothing forwarded there is nothing to undo.
(define (neutralize-argument-scopes use-stx expanded)
  (define anchor (forwarded-anchor use-stx expanded))
  (cond
    [anchor
     ((make-syntax-delta-introducer (car anchor) (cdr anchor)) expanded 'remove)]
    [else expanded]))

;; The first forwarded-argument identifier in `expanded` that gained scopes,
;; paired with its use-site original: `(cons forwarded-occurrence use-arg)`, or
;; #f when the expansion forwarded no argument.  A forwarded occurrence shares
;; a symbol with a use-form argument, carries no macro-introduction scope (so
;; it is an argument, not a template identifier), and actually gained scopes
;; over the original.  All arguments of one `expand-once` gain the same scope
;; set, so this one pair's delta neutralizes every forwarded argument.
(define (forwarded-anchor use-stx expanded)
  (define arg-ids (collect-identifiers use-stx))
  (for/or ([o (in-list (collect-identifiers expanded))])
    (define base
      (for/or ([a (in-list arg-ids)])
        (and (eq? (syntax-e a) (syntax-e o)) a)))
    (and base
         (not (introduced-identifier? o))
         (not (bound-identifier=? o base))
         (cons o base))))

;; Every identifier occurring anywhere in `stx`, outermost first.
(define (collect-identifiers stx)
  (let loop ([s stx] [acc '()])
    (cond
      [(identifier? s) (cons s acc)]
      [(syntax? s)     (loop (syntax-e s) acc)]
      [(pair? s)       (loop (car s) (loop (cdr s) acc))]
      [else            acc])))

;; True when `id` carries a macro-introduction scope — i.e. it came from a
;; macro template rather than the use site.  `syntax-debug-info`'s 'context
;; describes each scope; a macro-introduction scope is tagged `macro` (this
;; shape is stable on Racket 8.x/9.x; `introduced-identifier?`'s test pins it,
;; so a future format drift surfaces as a targeted failure here).
(define (introduced-identifier? id)
  (for/or ([s (in-list (hash-ref (syntax-debug-info id) 'context '()))])
    (and (vector? s) (and (memq 'macro (cdr (vector->list s))) #t))))

(module+ test
  (require rackunit
           rackcheck
           racket/list)

  ;; Run `(m arg …)` through the phase-0 walk in a fresh namespace that binds
  ;; `m` as `defn` (a `define-syntax-rule` datum).  Returns the expansion.
  (define (expand-use defn use-datum)
    (parameterize ([current-namespace (make-base-namespace)])
      (eval defn)
      (expand-macro-walk (datum->syntax #f use-datum) '(m))))

  ;; ----- introduced-identifier? contract (pins the syntax-debug-info shape) -
  (test-case "introduced-identifier?: template id yes, use-site arg no"
    (define out (expand-use '(define-syntax-rule (m a) (let ([g 0]) a))
                            '(m here)))
    (define ids  (collect-identifiers out))
    (define g    (findf (lambda (i) (eq? (syntax-e i) 'g))    ids))
    (define here (findf (lambda (i) (eq? (syntax-e i) 'here)) ids))
    (check-true  (introduced-identifier? g)     "template-introduced binder")
    (check-false (introduced-identifier? here)  "forwarded use-site argument"))

  ;; ----- the forwarding invariant, over generated macros ------------------
  ;; Laws (stated on syntax, independent of inference): after the walk,
  ;;  1. every forwarded argument occurrence is bound-identifier=? to its
  ;;     use-site binder; and
  ;;  2. a template-introduced binder is bound-identifier=? to NO use-site
  ;;     argument — even when their symbols collide.
  (define params '(p1 p2 p3))
  (define use-syms '(x1 x2 x3))
  (check-property
   (make-config #:tests 300)
   (property forwarding-preserves-binding-and-hygiene
             ([k      (gen:integer-in 1 3)]
              [intro? gen:boolean])
     (define ps  (take params k))
     (define us  (take use-syms k))
     (define collide (car us))                 ; introduced binder collides with x1
     (define body `(list ,@ps))
     (define tmpl (if intro? `(let ([,collide 0]) ,body) body))
     (define use-stx (datum->syntax #f (cons 'm us)))
     (define use-ids (cdr (syntax->list use-stx)))
     (define out (parameterize ([current-namespace (make-base-namespace)])
                   (eval `(define-syntax-rule (m ,@ps) ,tmpl))
                   (expand-macro-walk use-stx '(m))))
     (define out-ids (collect-identifiers out))
     ;; law 1: each argument is forwarded to a reference that binds to it
     (define law1
       (for/and ([u (in-list use-ids)])
         (for/or ([o (in-list out-ids)]) (bound-identifier=? o u))))
     ;; law 2: the introduced binder stays distinct from every use-site arg
     (define law2
       (or (not intro?)
           (for/or ([o (in-list out-ids)])
             (and (eq? (syntax-e o) collide)
                  (for/and ([u (in-list use-ids)]) (not (bound-identifier=? o u)))))))
     (and law1 law2))))
