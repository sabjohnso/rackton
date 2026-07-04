#lang racket/base

;; Rackton — surface parser.
;;
;; Translates syntax objects from the surface language into a typed-core
;; source AST.  Used by both the `(rackton ...)` macro and the
;; `#lang rackton` reader; both feed the same elaboration pipeline.
;;
;; Lexical convention
;;   - Identifiers whose first character is a lowercase letter are
;;     "lowercase".  In type position they introduce a fresh type
;;     variable; in pattern position they introduce a fresh pattern
;;     variable; in expression position they are an ordinary
;;     reference to a value binding.
;;   - Every other identifier (uppercase letters, symbols like ->,
;;     punctuation) is "non-lowercase".  It is always a reference to
;;     an already-bound name, never a fresh binding: a type
;;     constructor / class name in a type, a data constructor as a
;;     pattern head, and a value / function / class method / data
;;     constructor in an expression.
;;
;; The AST exported below carries the originating syntax object in a
;; trailing `stx` slot so that downstream stages can produce sourcemap-
;; aware errors.

;; The AST data vocabulary now lives in ast.rkt and is re-exported here so
;; existing importers of surface.rkt (infer, codegen, …) see it unchanged.
(provide (all-from-out "ast.rkt")
         parse-kind-stx
         parse-expr
         parse-type
         parse-pattern
         parse-top
         parse-toplevel-list

         current-hygiene?
         emit-id
         lowercase-id?
         classify-id
         keyword-id?
         keyword-id->name
         qualified-id?
         qualified-id-prefix
         qualified-id-name)

(require syntax/parse
         racket/match
         racket/list
         racket/set
         "ast.rkt"
         "deriving.rkt")
;; ----- lexical classification ---------------------------------------

(define (lowercase-id? sym)
  (and (symbol? sym)
       (let ([s (symbol->string sym)])
         (and (positive? (string-length s))
              (let ([c (string-ref s 0)])
                (and (char-alphabetic? c)
                     (char-lower-case? c)))))))

;; A class name (and a type/data-constructor name) is conventionally a
;; non-lowercase identifier, but the reserved type/kind operators
;; (`->` `*` `=>` `::`) are also non-lowercase without being class
;; names.  `uppercase-id?` is the tighter predicate for *class* names:
;; the first character must be an uppercase letter.  It rejects an
;; operator written where a class was expected — e.g. the kind
;; expression `(-> * *)` slipped in after `=>` instead of `::`.
(define (uppercase-id? sym)
  (and (symbol? sym)
       (let ([s (symbol->string sym)])
         (and (positive? (string-length s))
              (let ([c (string-ref s 0)])
                (and (char-alphabetic? c)
                     (char-upper-case? c)))))))

;; A valid constraint head: a class name, or the built-in type-equality
;; predicate `~` (which is not a class but is a legitimate constraint).
(define (constraint-head-id? sym)
  (or (uppercase-id? sym) (eq? sym '~)))

(define (wildcard-symbol? sym) (eq? sym '_))

;; ----- colon classification -----------------------------------------
;; A whitespace-delimited identifier token now carries structure in its
;; colons.  `classify-id` reports which of the five shapes a symbol has:
;;
;;   (plain      sym)            no colon — an ordinary unqualified name
;;   (operator   sym)            the reserved `:` / `::` annotation/kind ops
;;   (keyword    name)          `:name`  — a label / option marker
;;   (qualified  prefix name)   `prefix:name` — a namespaced reference
;;   (illegal    sym)            trailing or repeated colons — rejected
;;
;; The reader still produces ordinary symbols; this is purely a spelling
;; interpretation, mirroring `lowercase-id?`.
(define (classify-id sym)
  (if (not (symbol? sym))
      (list 'illegal sym)
      (let* ([s (symbol->string sym)]
             [n (string-length s)]
             [colons (for/list ([c (in-string s)] [i (in-naturals)]
                                #:when (char=? c #\:))
                       i)]
             [k (length colons)])
        (cond
          [(zero? k) (list 'plain sym)]
          ;; all-colon tokens: only `:` and `::` are reserved operators.
          [(= k n) (if (or (= n 1) (= n 2))
                       (list 'operator sym)
                       (list 'illegal sym))]
          ;; leading colon → keyword: exactly one colon, nonempty rest.
          [(char=? (string-ref s 0) #\:)
           (if (= k 1)
               (list 'keyword (string->symbol (substring s 1)))
               (list 'illegal sym))]
          ;; one internal colon, not leading, not trailing → qualified.
          [(and (= k 1)
                (let ([p (car colons)]) (and (> p 0) (< p (sub1 n)))))
           (define p (car colons))
           (list 'qualified
                 (string->symbol (substring s 0 p))
                 (string->symbol (substring s (add1 p))))]
          [else (list 'illegal sym)]))))

(define (illegal-colon-id? sym) (eq? (car (classify-id sym)) 'illegal))
;; Shared message for a malformed colon name (trailing or repeated
;; colons): a colon may only lead a keyword or separate one qualifier.
(define malformed-colon-message
  "malformed name: a colon may only lead a keyword (:name) or separate a single qualifier (mod:name)")
(define (keyword-id? sym)     (eq? (car (classify-id sym)) 'keyword))
(define (keyword-id->name sym) (cadr (classify-id sym)))
(define (qualified-id? sym)   (eq? (car (classify-id sym)) 'qualified))
(define (qualified-id-prefix sym) (cadr (classify-id sym)))
(define (qualified-id-name sym)   (caddr (classify-id sym)))

;; ----- expressions --------------------------------------------------

;; A prelude reference emitted by literal sugar (bracket / dotted-pair /
;; map / set literal, quoted data): tagged so it resolves to the prelude
;; binding even where the module shadows the name (see `mark-sugar-ref`
;; in ast.rkt).
(define (sugar-ref name stx) (e:var name (mark-sugar-ref stx)))
(define (list-cons-ref stx) (sugar-ref 'Cons stx))
(define (list-nil-ref  stx) (sugar-ref 'Nil  stx))

;; Build the Cons/Nil list AST from a list of already-parsed element
;; expressions.  Used by the variadic `describe`/`context` desugaring.
(define (build-list-ast elems stx)
  (cond
    [(null? elems) (list-nil-ref stx)]
    [else (e:app (list-cons-ref stx)
                 (list (car elems) (build-list-ast (cdr elems) stx))
                 stx)]))

;; ----- bracket / brace literal syntax -------------------------------
;;
;; The reader records the original bracket of each form in the
;; `paren-shape` syntax property (`#\[` for `[..]`, `#\{` for `{..}` and
;; `#{..}`; `#f` for ordinary `(..)`).  In *expression* and *pattern*
;; position that lets three additive literal forms be told apart with no
;; custom readtable:
;;
;;   [v1 v2 ...]   list literal   (List a)   — paren-shape #\[, datum a list
;;   {k1 v1 ...}   map literal     (Map k v)  — paren-shape #\{, datum a list
;;   #{m1 m2 ...}  set literal     (Set a)    — paren-shape #\{, datum a vector
;;
;; Parens keep every current meaning.  Structural bracket clauses (the
;; `[..]` in let / cond / match / do / update) never reach here: their
;; special forms destructure them, so only a bracket/brace in genuine
;; expression or pattern position is read as a literal.

;; Classify a form by its reader paren-shape: 'list, 'pair, 'map, 'set, or
;; #f (an ordinary form to be handled by the usual syntax-parse dispatch).
;; A square-bracket form is a list literal when proper ([a b c]) and a
;; dotted-pair literal when improper ([a . b]).
(define (bracket-literal-kind stx)
  (case (syntax-property stx 'paren-shape)
    [(#\[) (if (improper-pair-syntax? stx) 'pair 'list)]
    [(#\{) (if (vector? (syntax-e stx)) 'set 'map)]
    [else  #f]))

;; #t when the form is an improper pair — a dotted tail, [a . b] / (a . b).
;; (`syntax->list` returns #f exactly for an improper list.)
(define (improper-pair-syntax? stx)
  (and (pair? (syntax-e stx)) (not (syntax->list stx))))

;; [a . b] => (Pair a b), evaluating both positions — the Pair analogue of
;; the [a b c] list literal.  The dotted tail must read as a single element
;; ([a . b]); a longer dotted form ([a b . c]) is a located error.
(define (parse-pair-literal stx)
  (define e (syntax-e stx))
  (define cdr-stx (cdr e))
  (when (pair? (syntax-e cdr-stx))
    (raise-syntax-error 'rackton
      "dotted pair literal must have the form [a . b]" stx))
  (e:app (sugar-ref 'Pair stx)
         (list (parse-expr (car e)) (parse-expr cdr-stx))
         stx))

;; {k1 v1 ... kn vn} => nested map-insert with the first pair innermost,
;; so a later duplicate key overwrites an earlier one (last write wins).
;; An odd number of forms is a located error.
(define (parse-map-literal stx)
  (define elems (syntax->list stx))
  (unless (even? (length elems))
    (raise-syntax-error 'rackton
      "map literal {..} needs an even number of forms: {key value ...}"
      stx))
  (let loop ([elems elems] [acc (sugar-ref 'empty-map stx)])
    (cond
      [(null? elems) acc]
      [else
       (loop (cddr elems)
             (e:app (sugar-ref 'map-insert stx)
                    (list (parse-expr (car elems))
                          (parse-expr (cadr elems))
                          acc)
                    stx))])))

;; #{m1 ... mn} => nested set-insert; duplicate members collapse.
(define (parse-set-literal stx)
  (let loop ([elems (vector->list (syntax-e stx))]
             [acc (sugar-ref 'empty-set stx)])
    (cond
      [(null? elems) acc]
      [else
       (loop (cdr elems)
             (e:app (sugar-ref 'set-insert stx)
                    (list (parse-expr (car elems)) acc)
                    stx))])))

;; ----- quotation ----------------------------------------------------
;;
;; `'datum` and `` `datum `` build *typed homogeneous list literals*.  A
;; quoted atom becomes the matching literal (numbers → Integer, strings →
;; String, identifiers → Symbol, …); a quoted list becomes a Cons/Nil
;; chain, so ordinary inference types it as `(List a)` and rejects a
;; heterogeneous list by unification.  Quasiquote additionally honours
;; `,e` (unquote: evaluate the Rackton expression `e`) and `,@e`
;; (unquote-splicing: concatenate the list value of `e` via the prelude's
;; `append`).
;;
;; A single integer `level` carries the quasiquote nesting depth: 0 means
;; not inside any quasiquote, so a *direct* unquote / unquote-splicing
;; there is an error (the user-facing rule: escapes are legal only inside
;; a quasiquote); 1 means inside one quasiquote, where an unquote escapes;
;; deeper levels keep the unquote as data with the level decremented, and
;; a nested quasiquote increments it — Racket-standard level tracking.
;; `quote` enters at level 0, `quasiquote` at level 1.

;; A two-element data list `(sym inner)`, i.e. `(Cons 'sym (Cons inner
;; Nil))` — how a non-escaping unquote / quasiquote keyword survives as
;; data at a deeper nesting level.
(define (quote-data-form sym inner stx)
  (e:app (list-cons-ref stx)
         (list (e:literal sym stx)
               (e:app (list-cons-ref stx)
                      (list inner (list-nil-ref stx))
                      stx))
         stx))

;; {k1 v1 ... kn vn} under quote/quasiquote => nested map-insert, the first
;; pair innermost (last write wins), recursing through `quote->ast` at the
;; current level so a `,` escape evaluates and a bare key stays a Symbol.
;; An odd number of forms is a located error.
(define (quote-map elems level stx)
  (unless (even? (length elems))
    (raise-syntax-error 'rackton
      "quoted map literal {..} needs an even number of forms: {key value ...}"
      stx))
  (let loop ([elems elems] [acc (sugar-ref 'empty-map stx)])
    (cond
      [(null? elems) acc]
      [else
       (loop (cddr elems)
             (e:app (sugar-ref 'map-insert stx)
                    (list (quote->ast (car elems) level stx)
                          (quote->ast (cadr elems) level stx)
                          acc)
                    stx))])))

;; #{m1 ... mn} under quote/quasiquote => nested set-insert, recursing
;; through `quote->ast` at the current level; duplicate members collapse.
(define (quote-set elems level stx)
  (let loop ([elems elems] [acc (sugar-ref 'empty-set stx)])
    (cond
      [(null? elems) acc]
      [else
       (loop (cdr elems)
             (e:app (sugar-ref 'set-insert stx)
                    (list (quote->ast (car elems) level stx) acc)
                    stx))])))

;; Convert a quoted/quasiquoted datum to a core AST node.  A curly-brace
;; datum builds a Map/Set literal (paren-shape #\{, list => map, vector =>
;; set), extending {..}/#{..} to quotation exactly as '[..] extends [..].
(define (quote->ast d level stx)
  (case (bracket-literal-kind d)
    [(map) (quote-map (syntax->list d) level stx)]
    [(set) (quote-set (vector->list (syntax-e d)) level stx)]
    [else (quote->ast/form d level stx)]))

(define (quote->ast/form d level stx)
  (syntax-parse d
    #:datum-literals (quasiquote unquote unquote-splicing)
    [(unquote e)
     (cond
       [(= level 0)
        (raise-syntax-error 'rackton "unquote not in quasiquote" stx d)]
       [(= level 1) (parse-expr #'e)]
       [else (quote-data-form 'unquote (quote->ast #'e (sub1 level) stx) stx)])]
    [(unquote-splicing _)
     ;; A splice is only meaningful in list position (handled by
     ;; quote-list); reaching it here is a bare `,@e`.
     (if (= level 0)
         (raise-syntax-error 'rackton "unquote-splicing not in quasiquote" stx d)
         (raise-syntax-error 'rackton "unquote-splicing not in a list" stx d))]
    [(quasiquote e)
     (quote-data-form 'quasiquote (quote->ast #'e (add1 level) stx) stx)]
    [n:number  (e:literal (syntax->datum #'n) stx)]
    [b:boolean (e:literal (syntax->datum #'b) stx)]
    [s:string  (e:literal (syntax->datum #'s) stx)]
    [c:char    (e:literal (syntax->datum #'c) stx)]
    [by:bytes  (e:literal (syntax->datum #'by) stx)]
    [x:id      (e:literal (syntax->datum #'x) stx)]
    [(elem ...) (quote-list (syntax->list #'(elem ...)) level stx)]
    ;; A dotted pair (proper lists are matched above): '(a . b) builds a
    ;; Pair of the quoted parts; a head unquote still escapes.
    [(a . b)   (quote-pair #'a #'b level stx)]
    [_ (raise-syntax-error 'rackton
                           "unsupported quoted form (improper list?)" stx d)]))

;; '(a . b) => (Pair a b) over the quoted parts.  The dotted tail must read
;; as a single element; a longer dotted form ((a b . c)) is an error.
(define (quote-pair a-stx b-stx level stx)
  (when (pair? (syntax-e b-stx))
    (raise-syntax-error 'rackton
      "a dotted pair must have the form (a . b)" stx b-stx))
  (e:app (sugar-ref 'Pair stx)
         (list (quote->ast a-stx level stx) (quote->ast b-stx level stx))
         stx))

;; Build a quoted list, splicing `,@e` elements positionally.
(define (quote-list elems level stx)
  (cond
    [(null? elems) (list-nil-ref stx)]
    [else
     (syntax-parse (car elems)
       #:datum-literals (unquote-splicing)
       [(unquote-splicing e)
        (cond
          [(= level 0)
           (raise-syntax-error 'rackton "unquote-splicing not in quasiquote"
                               stx (car elems))]
          [(= level 1)
           (e:app (sugar-ref 'append stx)
                  (list (parse-expr #'e) (quote-list (cdr elems) level stx))
                  stx)]
          [else
           (e:app (list-cons-ref stx)
                  (list (quote-data-form 'unquote-splicing
                                         (quote->ast #'e (sub1 level) stx) stx)
                        (quote-list (cdr elems) level stx))
                  stx)])]
       [_
        (e:app (list-cons-ref stx)
               (list (quote->ast (car elems) level stx)
                     (quote-list (cdr elems) level stx))
               stx)])]))

;; ----- local-binding hygiene ----------------------------------------
;;
;; The parser emits symbols; lexical scoping is resolved later, by symbol,
;; in inference.  That is fine until a macro is involved: a macro-introduced
;; binder and a user binder may share a symbol yet must stay distinct
;; (hygiene), and a macro's reference to a top-level name must not be
;; captured by a use-site binder of the same symbol (referential
;; transparency).  When a Rackton block defines macros, `current-hygiene?`
;; is on and every LOCAL binder is α-renamed to a fresh symbol keyed by
;; identifier identity (`bound-identifier=?`); references resolve against the
;; same rename environment, falling through to the bare symbol — and thus to
;; the top-level/prelude environment — when they match no local binder.
;;
;; When off (no macros in the block) this is a no-op: binders keep their
;; written symbol and references are a plain `syntax->datum`, exactly as
;; before, so non-macro programs are entirely unaffected.
(define current-hygiene? (make-parameter #f))
;; The rename environment in scope: a list of (identifier . fresh-symbol).
(define current-rename-env (make-parameter '()))

;; Resolve an identifier occurrence (binder or reference) to the symbol the
;; AST should carry: its fresh rename if a local binder of the same identity
;; is in scope, else its bare symbol.
(define (resolve-id id-stx)
  (cond
    [(current-hygiene?)
     (let loop ([env (current-rename-env)])
       (cond
         [(null? env)                              (syntax->datum id-stx)]
         [(bound-identifier=? (caar env) id-stx)   (cdar env)]
         [else                                     (loop (cdr env))]))]
    [else (syntax->datum id-stx)]))

;; Extend the current rename environment with a fresh symbol for each binder
;; identifier (a no-op when hygiene is off).  Returns the extended env, to be
;; installed via `parameterize` around the scope the binders cover.
(define (extend-rename-env binder-id-stxs)
  (cond
    [(current-hygiene?)
     (for/fold ([env (current-rename-env)]) ([id (in-list binder-id-stxs)])
       (cons (cons id (make-hygiene-rename id)) env))]
    [else (current-rename-env)]))

;; A hygiene α-rename: a fresh uninterned symbol standing in for a local
;; binder of `id`.  Tagged with `hygiene-rename-prefix` so codegen
;; (`emit-id`) can tell a rename apart from a parser-internal placeholder
;; (`$arg`, `_delay`) — only the former is emitted scope-free.  The tag is
;; internal and surfaces only in generated code, never in `,source` (which
;; replays the original form) or in inferred types.
(define hygiene-rename-prefix "hyg~")

(define (make-hygiene-rename id)
  (gensym (string-append hygiene-rename-prefix (symbol->string (syntax-e id)))))

(define (hygiene-rename-symbol? name)
  (and (symbol? name)
       (not (symbol-interned? name))
       (let ([s (symbol->string name)])
         (and (>= (string-length s) (string-length hygiene-rename-prefix))
              (string=? (substring s 0 (string-length hygiene-rename-prefix))
                        hygiene-rename-prefix)))))

;; Lower an identifier symbol to a syntax object for code generation,
;; preserving `src`'s source location.  A hygiene α-rename (a local binder
;; or one of its references, made by `make-hygiene-rename`) must carry its
;; binding identity in the SYMBOL alone — so emit it scope-free (an empty
;; lexical context).  Otherwise a binder and a reference, which draw their
;; source syntax from different forms (the enclosing `let` vs. the use-site
;; occurrence), keep different scope sets after macro expansion and stop
;; being `bound-identifier=?`, leaving the binder unreachable.  Every other
;; name — user identifiers, prelude references, constructor names that must
;; resolve a match expander, and parser-internal placeholders (`$arg`,
;; `_delay`) whose binder and references already share one context — keeps
;; `src`'s context, so non-macro code generation is byte-for-byte unchanged.
(define (emit-id name src)
  (datum->syntax (if (hygiene-rename-symbol? name) #f src) name src))

;; The binder identifiers introduced by a pattern (lowercase, non-wildcard
;; variables), recurring through constructor patterns.  Used to extend the
;; rename environment before a binding form's body is parsed.
(define (pattern-binder-ids pat-stx)
  (syntax-parse pat-stx
    #:datum-literals (quote quasiquote)
    ;; A plain quote is all-literal, so it binds nothing.  A quasiquote
    ;; binds only the variables under its `,` escapes.  `(list …)` and
    ;; other parenthesised patterns fall through to the ctor branch, whose
    ;; recursion already collects their binders (and skips the `...` token,
    ;; which is not a lowercase identifier).
    [(quote _)         '()]
    [(quasiquote d)    (quasi-pattern-binder-ids #'d 1)]
    [x:id
     (let ([name (syntax->datum #'x)])
       (if (or (wildcard-symbol? name) (not (lowercase-id? name)))
           '()
           (list #'x)))]
    [(ctor:id arg ...) (append-map pattern-binder-ids (syntax->list #'(arg ...)))]
    [_                 '()]))

;; Binders introduced by the `,` escapes inside a quasiquoted pattern, with
;; Racket-standard nesting levels (an unquote escapes only at level 1).
(define (quasi-pattern-binder-ids d level)
  (syntax-parse d
    #:datum-literals (quasiquote unquote unquote-splicing)
    [(unquote e)
     (if (= level 1)
         (pattern-binder-ids #'e)
         (quasi-pattern-binder-ids #'e (sub1 level)))]
    [(unquote-splicing _) '()]
    [(quasiquote e) (quasi-pattern-binder-ids #'e (add1 level))]
    [(elem ...)
     (append-map (lambda (e) (quasi-pattern-binder-ids e level))
                 (syntax->list #'(elem ...)))]
    ;; a dotted pair (proper lists matched above): collect from both parts
    [(a . b)
     (append (quasi-pattern-binder-ids #'a level)
             (quasi-pattern-binder-ids #'b level))]
    [_ '()]))

;; ----- infix notation -----------------------------------------------
;;
;; A quasiquoted identifier in operator position turns an application into
;; infix notation:
;;
;;   (a `op b)            => (op a b)
;;   (a `op b `op c ...)  => (op a b c ...)
;;   (`op b ...)          => (lambda (x) (op x b ...))   right section
;;   (a `op)              => (lambda (x) (op a x))        left section
;;
;; The operator must be HOMOGENEOUS across a chain: mixing `+ and `* in one
;; expression is exactly the precedence ambiguity infix notation invites,
;; so it is a located error rather than a silent reading.  Because Rackton
;; is curried with over-application, a homogeneous chain of N terms is a
;; single application of the operator to N arguments and a section is an
;; ordinary one-argument lambda.  The design mirrors spork's `#%app`-based
;; notation, but is realised here in the surface parser since Rackton owns
;; its own application reading.

;; A quasiquoted identifier: `op reads as (quasiquote op).
(define-syntax-class infix-operator
  #:datum-literals (quasiquote)
  (pattern (quasiquote name:id)))

;; Compatible operators: an #f tail-operator marks the end of a chain and
;; is compatible with anything; otherwise the two must be the same symbol.
(define (uniform-operator? head-op tail-op)
  (or (not (syntax->datum tail-op))
      (eq? (syntax->datum head-op) (syntax->datum tail-op))))

;; The leading `term `op` of an infix expression.
(define-splicing-syntax-class infix-head
  #:attributes (term operator)
  (pattern (~seq term:expr op:infix-operator)
           #:with operator #'op.name))

;; `term `op term `op ... term` — terms separated by a homogeneous
;; operator, ending ON A TERM.  `operator` is #f for a lone term.
(define-splicing-syntax-class infix-tail
  #:attributes (operator (terms 1))
  (pattern (~seq term:expr op:infix-operator tail:infix-tail)
           #:when (uniform-operator? #'op.name #'tail.operator)
           #:with operator #'op.name
           #:with (terms ...) #'(term tail.terms ...))
  (pattern (~seq term:expr)
           #:with operator #f
           #:with (terms ...) #'(term)))

;; `\`op term ... \`op` — the tail of a left section: a homogeneous
;; operator separating terms but ending ON AN OPERATOR (no final term).
(define-splicing-syntax-class infix-left-tail
  #:attributes (operator (terms 1))
  (pattern (~seq op:infix-operator term:expr tail:infix-left-tail)
           #:when (uniform-operator? #'op.name #'tail.operator)
           #:with operator #'op.name
           #:with (terms ...) #'(term tail.terms ...))
  (pattern (~seq op:infix-operator)
           #:with operator #'op.name
           #:with (terms ...) #'()))

;; The infix operator symbols appearing in `stx`, for the inhomogeneous
;; error message.
(define (infix-operator-names stx)
  (filter values
          (for/list ([e (in-list (syntax->list stx))])
            (syntax-parse e
              #:datum-literals (quasiquote)
              [(quasiquote name:id) (syntax->datum #'name)]
              [_ #f]))))

(define (parse-expr stx)
  ;; Bracket/brace literals win unconditionally in expression position
  ;; (decided once, by paren-shape) before the keyword dispatch below.
  (case (bracket-literal-kind stx)
    [(list) (build-list-ast (map parse-expr (syntax->list stx)) stx)]
    [(pair) (parse-pair-literal stx)]
    [(map)  (parse-map-literal stx)]
    [(set)  (parse-set-literal stx)]
    [else (parse-expr/form stx)]))

(define (parse-expr/form stx)
  (syntax-parse stx
    #:datum-literals (lambda λ case-lambda case-λ let let& let% let+ letrec let* if cond else ann open match match* racket do proc delay <- update handle return describe context list tuple tref array build-array aref array-take array-drop array-split-at bits -> quote quasiquote unquote unquote-splicing)
    [n:number  (e:literal (syntax->datum #'n) stx)]
    [b:boolean (e:literal (syntax->datum #'b) stx)]
    [s:string  (e:literal (syntax->datum #'s) stx)]
    [c:char    (e:literal (syntax->datum #'c) stx)]
    [by:bytes  (e:literal (syntax->datum #'by) stx)]
    ;; Quotation builds typed list literals (see `quote->ast`): `'foo` is
    ;; the Symbol 'foo, `'(1 2 3)` is a `(List Integer)`, and quasiquote
    ;; adds `,`/`,@` escapes.  An unquote outside a quasiquote is an error.
    [(quote datum)      (quote->ast #'datum 0 stx)]
    [(quasiquote datum) (quote->ast #'datum 1 stx)]
    [(unquote _)
     (raise-syntax-error 'rackton "unquote not in quasiquote" stx stx)]
    [(unquote-splicing _)
     (raise-syntax-error 'rackton "unquote-splicing not in quasiquote" stx stx)]

    ;; `lambda` / `λ` — each parameter is a bare identifier or a
    ;; constructor pattern like `(Pair x y)`, desugared via the same
    ;; single-clause, irrefutable mechanism as `define` function heads
    ;; (see `parse-fn-params+body`).  The fn-clauses record is for the
    ;; multi-clause `define` combiner only, so it is suppressed here.
    [(lambda (param ...) body)
     (parameterize ([current-fn-clauses-record #f])
       (define-values (names wrapped)
         (parse-fn-params+body #'(param ...) #'body stx))
       (e:lam names wrapped stx))]
    [(λ (param ...) body)
     (parameterize ([current-fn-clauses-record #f])
       (define-values (names wrapped)
         (parse-fn-params+body #'(param ...) #'body stx))
       (e:lam names wrapped stx))]
    ;; Any other `lambda` / `λ` shape is malformed.  Diagnose it by name
    ;; instead of letting it fall through to a function-application
    ;; reading and an "unbound identifier: lambda" error (mirrors the
    ;; let&/let%/let+ guards below).
    [(lambda . _) (raise-bad-lambda 'lambda stx)]
    [(λ . _)      (raise-bad-lambda 'λ stx)]

    ;; `case-lambda` / `case-λ` — an anonymous function that pattern
    ;; matches on *all* of its arguments at once.  Each clause's
    ;; parenthesized pattern list fixes the arity; the form desugars
    ;; into a lambda over fresh argument names whose body is an
    ;; `e:match*` (the same shape the multi-clause `define` combiner
    ;; emits, see `combine-fn-clauses`), so inference and codegen need
    ;; no new cases.
    [(case-lambda cl ...+) (parse-case-lambda 'case-lambda (syntax->list #'(cl ...)) stx)]
    [(case-λ      cl ...+) (parse-case-lambda 'case-λ      (syntax->list #'(cl ...)) stx)]
    [(case-lambda . _) (raise-bad-case-lambda 'case-lambda stx)]
    [(case-λ      . _) (raise-bad-case-lambda 'case-λ      stx)]

    ;; `match*` — match several scrutinees at once.  The honest N-ary
    ;; generalization of `match`: the parenthesized scrutinee list fixes
    ;; the arity, and each clause leads with a parenthesized list of that
    ;; many patterns.  It builds the same `e:match*` core node that
    ;; `case-lambda` desugars into, but over real scrutinee expressions
    ;; rather than fresh argument names, so no lambda wrapper is added.
    [(match* (scrut ...) cl ...+)
     (parse-match* (syntax->list #'(scrut ...)) (syntax->list #'(cl ...)) stx)]
    [(match* . _) (raise-bad-match* stx)]

    ;; Named (loop) let — Scheme-style: `loop` is a recursive procedure
    ;; bound in the body, seeded by the initial RHS.  Matched before the
    ;; plain `let` because the head after `let` is an id here, a binding
    ;; group there, so the two never overlap.
    [(let loop:id ([x:id rhs] ...+) body)
     ;; Initial RHSs evaluate in the outer scope; the loop name and params
     ;; are in scope for the body (and the recursive call).
     (define rhss (map parse-expr (syntax->list #'(rhs ...))))
     (define ids  (syntax->list #'(x ...)))
     (parameterize ([current-rename-env (extend-rename-env (cons #'loop ids))])
       (build-named-let (resolve-id #'loop)
                        (for/list ([id (in-list ids)] [r (in-list rhss)])
                          (cons (resolve-id id) r))
                        (parse-expr #'body)
                        stx))]

    ;; Plain `let` — parallel binding, with destructuring.  Each LHS is a
    ;; pattern: a bare identifier is an ordinary (let-polymorphic) binding;
    ;; a constructor/wildcard/literal pattern destructures its RHS via an
    ;; irrefutable match.  All RHSs are evaluated in the surrounding scope
    ;; (parallel), so a binding cannot see another's pattern variables.
    [(let ([lhs rhs] ...) body)
     (build-destructuring-let (syntax->list #'(lhs ...))
                              (syntax->list #'(rhs ...))
                              #'body
                              stx)]

    ;; let& — sequential monad bind (deps allowed); nested flatmap, same
    ;; family as `do`.  Body is the final monadic expression.
    [(let& ([x:id rhs] ...+) body)
     ;; Sequential: each RHS sees the binders before it; the body sees all.
     (let loop ([ids (syntax->list #'(x ...))]
                [rhss (syntax->list #'(rhs ...))]
                [acc '()])
       (cond
         [(null? ids)
          (build-sequential-let (reverse acc) (parse-expr #'body) stx)]
         [else
          (define rhs-ast (parse-expr (car rhss)))
          (parameterize ([current-rename-env (extend-rename-env (list (car ids)))])
            (loop (cdr ids) (cdr rhss)
                  (cons (cons (resolve-id (car ids)) rhs-ast) acc)))]))]

    ;; let% (named) — monadic loop: loop params are the monadic values,
    ;; combined per iteration via the gathered-product engine.
    [(let% loop:id ([x:id rhs] ...+) body)
     ;; Seeds evaluate in the enclosing scope; loop name and params scope the body.
     (define ids   (syntax->list #'(x ...)))
     (define seeds (map parse-expr (syntax->list #'(rhs ...))))
     (parameterize ([current-rename-env (extend-rename-env (cons #'loop ids))])
       (build-named-monadic-let
        (resolve-id #'loop)
        (for/list ([id (in-list ids)] [r (in-list seeds)]) (cons (resolve-id id) r))
        (parse-expr #'body)
        stx))]

    ;; let% — parallel/independent monad bind: gather via `product`,
    ;; then `flatmap` into a monadic body.
    [(let% ([x:id rhs] ...+) body)
     (parse-gathered-let 'flatmap #'(x ...) #'(rhs ...) #'body stx)]

    ;; let+ — applicative bind: gather via `product`, then `fmap`.  The
    ;; body is a PURE expression; the result is wrapped by the functor.
    [(let+ ([x:id rhs] ...+) body)
     (parse-gathered-let 'fmap #'(x ...) #'(rhs ...) #'body stx)]

    ;; Malformed monadic / parallel let — the binding group did not match
    ;; `([var expr] ...+)`.  Diagnose the offending clause here instead of
    ;; letting the form fall through to a function-application reading and
    ;; an "unbound identifier: let&" error.  These come AFTER every
    ;; well-formed let&/let%/let+ clause above, so only ill-formed uses
    ;; reach them.
    [(let& . _) (raise-bad-monadic-let 'let& stx)]
    [(let% . _) (raise-bad-monadic-let 'let% stx)]
    [(let+ . _) (raise-bad-monadic-let 'let+ stx)]

    [(letrec ([x:id rhs] ...) body)
     ;; Recursive: every binder is in scope in all RHSs and the body, so
     ;; introduce them all before parsing either.
     (define ids (syntax->list #'(x ...)))
     (parameterize ([current-rename-env (extend-rename-env ids)])
       (e:letrec (for/list ([id (in-list ids)]
                            [r  (in-list (syntax->list #'(rhs ...)))])
                   (cons (resolve-id id) (parse-expr r)))
                 (parse-expr #'body)
                 stx))]

    ;; (list e ...) — list-literal sugar.  Desugars to a Cons/Nil
    ;; chain; (list) is Nil.  Purely a parser rewrite, so the result is
    ;; an ordinary (List a).
    [(list elem ...)
     (build-list-ast
      (map parse-expr (syntax->list #'(elem ...))) stx)]

    ;; (tuple e ...) — variadic heterogeneous product constructor.
    [(tuple elem ...)
     (e:tuple (map parse-expr (syntax->list #'(elem ...))) stx)]

    ;; (bits seg ...) — Erlang-style binary constructor; builds a
    ;; Bitstring from its segment clauses.
    [(bits seg ...)
     (e:bits (parse-bit-segments (syntax->list #'(seg ...)) parse-expr) stx)]

    ;; (tref t n) — indexed tuple access.  `n` MUST be a non-negative
    ;; integer literal so the index is known (and bounds-checkable)
    ;; statically; a non-literal index is rejected here, before
    ;; inference, with a located error.
    [(tref t idx:nat)
     (e:tref (parse-expr #'t) (syntax->datum #'idx) stx)]
    [(tref _ _)
     (raise-syntax-error 'rackton
       "tref index must be a non-negative integer literal" stx)]

    ;; (array e ...) — fixed-size array listing; size = element count.
    [(array elem ...)
     (e:array (map parse-expr (syntax->list #'(elem ...))) stx)]

    ;; (build-array n f) — sized builder.  `n` MUST be a non-negative
    ;; integer literal so it fixes the type-level size; `f` is applied to
    ;; each index 0..n-1.
    [(build-array n:nat f)
     (e:build-array (syntax->datum #'n) (parse-expr #'f) stx)]
    [(build-array _ _)
     (raise-syntax-error 'rackton
       "build-array size must be a non-negative integer literal" stx)]

    ;; (aref arr n) — indexed element read; `n` MUST be a literal.
    [(aref arr idx:nat)
     (e:aref (parse-expr #'arr) (syntax->datum #'idx) stx)]
    [(aref _ _)
     (raise-syntax-error 'rackton
       "aref index must be a non-negative integer literal" stx)]

    ;; (array-take k arr) / (array-drop k arr) / (array-split-at k arr)
    ;; — concrete-size slices; `k` MUST be a non-negative integer literal.
    [(array-take k:nat arr)      (e:array-slice 'take  (syntax->datum #'k) (parse-expr #'arr) stx)]
    [(array-drop k:nat arr)      (e:array-slice 'drop  (syntax->datum #'k) (parse-expr #'arr) stx)]
    [(array-split-at k:nat arr)  (e:array-slice 'split (syntax->datum #'k) (parse-expr #'arr) stx)]
    [((~or array-take array-drop array-split-at) _ _)
     (raise-syntax-error 'rackton
       "array-take / array-drop / array-split-at point must be a non-negative integer literal" stx)]

    ;; (describe NAME child ...) / (context NAME child ...) — the test
    ;; framework's grouping forms, made variadic so children need no
    ;; explicit list wrapper.  Desugars to a call to the library
    ;; function `group-of` (resolved in the user's env, like `do`
    ;; resolves `flatmap`); the children are gathered into a List via
    ;; Cons/Nil.  Both forms are aliases.
    [(describe name child ...)
     (e:app (e:var 'group-of stx)
            (list (parse-expr #'name)
                  (build-list-ast
                   (map parse-expr (syntax->list #'(child ...))) stx))
            stx)]
    [(context name child ...)
     (e:app (e:var 'group-of stx)
            (list (parse-expr #'name)
                  (build-list-ast
                   (map parse-expr (syntax->list #'(child ...))) stx))
            stx)]

    ;; (let* ([lhs expr] ...) body) — sequential local bindings, with
    ;; destructuring (Lisp/Scheme let*).  Each binding sees the ones
    ;; before it (nested singleton lets); a non-variable LHS pattern
    ;; destructures its RHS via an irrefutable match.
    [(let* ([lhs rhs] ...+) body)
     (build-destructuring-let* (syntax->list #'(lhs ...))
                               (syntax->list #'(rhs ...))
                               #'body
                               stx)]

    [(if c t e)
     (e:if (parse-expr #'c) (parse-expr #'t) (parse-expr #'e) stx)]

    ;; (cond [c1 b1] [c2 b2] ... [else bN])
    ;; Desugars to nested ifs.  The final clause MUST be `[else b]`
    ;; — without it there's no value to fall through to and the
    ;; resulting expression would be ill-typed.
    [(cond clause ...+)
     (parse-cond-clauses (syntax->list #'(clause ...)) stx)]

    [(ann e t)
     (e:ann (parse-expr #'e) (parse-type #'t) stx)]

    ;; Existential elimination: `(open e (a … x) body)` binds the hidden
    ;; type variable(s) `a …` and the witness value `x` (the LAST id) for
    ;; `body`.
    [(open e (tv:id ... x:id) body)
     (e:open (parse-expr #'e)
             (map syntax->datum (syntax->list #'(tv ...)))
             (syntax->datum #'x)
             (parse-expr #'body)
             stx)]

    [(match scrut cl ...+)
     (e:match (parse-expr #'scrut)
              (for/list ([c-stx (in-list (syntax->list #'(cl ...)))])
                (parse-match-clause c-stx))
              #f stx)]

    ;; (racket τ (var ...) body ...+) — host-language escape.
    ;; Multiple body forms are wrapped in `begin` so users can write
    ;; sequences and inner `define`s naturally.
    [(racket τ (v:id ...) body ...+)
     (define body-list (syntax->list #'(body ...)))
     (define body-stx
       (cond
         [(= (length body-list) 1) (car body-list)]
         [else (datum->syntax stx (cons 'begin body-list) stx)]))
     (e:escape (parse-type #'τ)
               (map syntax->datum (syntax->list #'(v ...)))
               body-stx
               stx)]

    ;; (do [x <- m1] [y <- m2] ... body)  desugars to nested flatmap
    ;; calls.  A statement is `[var <- expr]`; each binds the un-wrapped
    ;; value for the rest of the chain.  The trailing `body` is the
    ;; final computation.
    [(do stmt ...+ body)
     (parse-do (syntax->list #'(stmt ...)) #'body stx)]

    ;; (proc (pat) cmd ...+)  — arrow notation, desugared at parse time
    ;; into Category/Arrow combinator calls (the analogue of `do` for
    ;; Arrows).  `pat` binds the proc's input; the commands form a
    ;; point-free pipeline and the last command is the output.  See
    ;; `parse-proc`.
    [(proc (pat) cmd ...+)
     (parse-proc #'pat (syntax->list #'(cmd ...)) stx)]

    ;; (delay e) — defer `e` (call-by-need).  Like `do`, this desugars at
    ;; parse time: it cannot be a function (that would evaluate `e`
    ;; eagerly), so it wraps `e` in a thunk and hands it to `make-lazy`
    ;; (from rackton/data/lazy).  The lambda parameter is a fresh ignored
    ;; name typed at Unit; `force` calls the thunk with Unit at most once.
    [(delay e)
     (e:app (e:var 'make-lazy stx)
            (list (e:lam (list (gensym '_delay)) (parse-expr #'e) stx))
            stx)]

    ;; (handle EXPR [op (args ...) k -> body] ... [return v -> body])
    ;; EXPR is run in a context where the named ops
    ;; are dispatched to the listed clauses; if EXPR finishes
    ;; normally, its value flows through the return clause.
    [(handle expr cl ...+)
     (define cls (syntax->list #'(cl ...)))
     (define-values (op-clauses ret-clause)
       (parse-handle-clauses cls stx))
     (e:handle (parse-expr #'expr) op-clauses ret-clause stx)]

    ;; (update RECORD [field val] ...) — functional record update.
    ;; Each [field val] replaces the named field of the recordrm
    ;; result; the rest are preserved.  Field names must match the
    ;; struct's declared fields, checked at inference time.
    [(update record (~and upd [_ _]) ...+)
     (e:update (parse-expr #'record)
               (for/list ([u (in-list (syntax->list #'(upd ...)))])
                 (syntax-parse u
                   [[name:id v]
                    (cons (syntax->datum #'name) (parse-expr #'v))]))
               stx)]

    ;; A bare keyword `:foo` is a syntactic marker, never a value.
    [x:id
     #:when (keyword-id? (syntax->datum #'x))
     (raise-syntax-error 'rackton
       (format "keyword :~a is not an expression; it can only label a constructor argument"
               (keyword-id->name (syntax->datum #'x)))
       #'x)]
    [x:id
     #:when (illegal-colon-id? (syntax->datum #'x))
     (raise-syntax-error 'rackton malformed-colon-message #'x)]

    [x:id  (e:var (resolve-id #'x) stx)]

    ;; ----- infix notation (see the syntax classes above) -----
    ;; (a `op b `op ... z)  =>  (op a b ... z)   homogeneous operator
    [(h:infix-head t:infix-tail)
     #:when (uniform-operator? #'h.operator #'t.operator)
     (e:app (e:var (resolve-id #'h.operator) stx)
            (map parse-expr (cons #'h.term (syntax->list #'(t.terms ...))))
            stx)]
    ;; (`op b ...)  =>  right section  (lambda (x) (op x b ...))
    [(op:infix-operator t:infix-tail)
     #:when (uniform-operator? #'op.name #'t.operator)
     (let ([arg (gensym '$infix)])
       (e:lam (list arg)
              (e:app (e:var (resolve-id #'op.name) stx)
                     (cons (e:var arg stx)
                           (map parse-expr (syntax->list #'(t.terms ...))))
                     stx)
              stx))]
    ;; (a `op)  =>  left section  (lambda (x) (op a ... x))
    [(term:expr t:infix-left-tail)
     (let ([arg (gensym '$infix)])
       (e:lam (list arg)
              (e:app (e:var (resolve-id #'t.operator) stx)
                     (append (map parse-expr
                                  (cons #'term (syntax->list #'(t.terms ...))))
                             (list (e:var arg stx)))
                     stx)
              stx))]
    ;; A leading `term `op` whose chain is not homogeneous: mixed operators
    ;; (`+ and `*) are an ambiguity we refuse to silently disambiguate.
    [(h:infix-head t:expr ...+)
     (raise-syntax-error 'rackton
       (format "infix operators must be homogeneous in one expression, but got ~a"
               (infix-operator-names stx))
       stx)]

    ;; Also accept zero-arg applications `(f)`.  These
    ;; are passed an implicit Unit so the typing of 0-arg ops
    ;; as `(-> Unit T)` lines up — saves users from spelling out
    ;; the dummy at every effect call site.
    [(head)
     (e:app (parse-expr #'head)
            (list (e:var 'Unit stx))
            stx)]
    ;; Keyword construction `(C :f1 v1 :f2 v2 …)`.  Recognised when the
    ;; first argument is a keyword token; every argument must then be a
    ;; `:label value` pair (no mixing with positional).  Keyword fields
    ;; must appear in declared order, so the values are already the
    ;; positional argument sequence: desugar to an `e:app`, recording the
    ;; labels as a property for inference to verify against the head
    ;; constructor's declared fields.
    [(head karg ...+)
     #:when (keyword-token? (car (syntax->list #'(karg ...))))
     (define labels+vals (parse-keyword-args (syntax->list #'(karg ...))))
     (e:app (parse-expr #'head)
            (map cdr labels+vals)
            (syntax-property stx 'rackton:kw-labels (map car labels+vals)))]
    [(head arg ...+)
     (e:app (parse-expr #'head)
            (for/list ([a (in-list (syntax->list #'(arg ...)))])
              (parse-expr a))
            stx)]))

;; Is `stx` a keyword token `:label`?
(define (keyword-token? stx)
  (and (identifier? stx) (keyword-id? (syntax->datum stx))))

;; Parse a `:label value …` sequence into a list of (field-name . expr).
;; Enforces strict label/value alternation: a positional argument in a
;; label position, or a trailing label with no value, is an error.
(define (parse-keyword-args args)
  (let loop ([args args] [acc '()])
    (cond
      [(null? args) (reverse acc)]
      [(not (keyword-token? (car args)))
       (raise-syntax-error 'rackton
         "keyword construction must use :label value pairs throughout"
         (car args))]
      [(null? (cdr args))
       (raise-syntax-error 'rackton
         (format "keyword :~a is missing its value"
                 (keyword-id->name (syntax->datum (car args))))
         (car args))]
      [else
       (loop (cddr args)
             (cons (cons (keyword-id->name (syntax->datum (car args)))
                         (parse-expr (cadr args)))
                   acc))])))

;; Parse a sequence of cond clauses, desugaring to nested if forms.
;; The final clause is required to be `[else body]`.
(define (parse-cond-clauses clauses stx)
  (syntax-parse clauses
    #:datum-literals (else)
    [([else body]) (parse-expr #'body)]
    [([test body] more ...)
     (e:if (parse-expr #'test)
           (parse-expr #'body)
           (parse-cond-clauses (syntax->list #'(more ...)) stx)
           stx)]
    [()
     (raise-syntax-error 'parse-cond
       "cond must end with an [else …] clause"
       stx)]))

;; Parse one `match` clause.  Two shapes are accepted:
;;   [pat body]
;;   [pat :when guard body]
;; The guard, when present, is a Boolean-typed expression evaluated
;; after the pattern's variable bindings are in scope.  If the guard
;; fails, the next clause is tried.
(define (parse-match-clause stx)
  (syntax-parse stx
    [[pat (~datum :when) guard body]
     (parameterize ([current-rename-env
                     (extend-rename-env (pattern-binder-ids #'pat))])
       (clause (parse-pattern #'pat)
               (parse-expr #'guard)
               (parse-expr #'body)
               stx))]
    [[pat body]
     (parameterize ([current-rename-env
                     (extend-rename-env (pattern-binder-ids #'pat))])
       (clause (parse-pattern #'pat)
               #f
               (parse-expr #'body)
               stx))]))

(define (parse-handle-clauses cls outer-stx)
  ;; Split clauses into op-clauses and a single return-clause.  The
  ;; return clause must be present and must be last (or anywhere —
  ;; we pick the unique one, regardless of position).
  (define parsed
    (for/list ([c (in-list cls)])
      (syntax-parse c
        #:datum-literals (return ->)
        [[return v:id -> body]
         (parameterize ([current-rename-env (extend-rename-env (list #'v))])
           (handle-return (resolve-id #'v)
                          (parse-expr #'body) c))]
        [[op:id (p:id ...) k:id -> body]
         ;; `op` names the effect operation (a reference); the argument
         ;; binders and the continuation `k` are in scope for the body.
         (parameterize ([current-rename-env
                         (extend-rename-env (cons #'k (syntax->list #'(p ...))))])
           (handle-clause (syntax->datum #'op)
                          (map resolve-id (syntax->list #'(p ...)))
                          (resolve-id #'k)
                          (parse-expr #'body) c))])))
  (define-values (rets ops)
    (partition handle-return? parsed))
  (unless (= (length rets) 1)
    (raise-syntax-error 'parse-handle
      "expected exactly one [return v -> body] clause" outer-stx))
  (values ops (car rets)))

;; ----- monadic / applicative let desugarings -----------------------
;;
;; The monadic / parallel let forms (let&, let%, let+) all bind with the
;; shape `([var expr] ...+)`.  When that pattern fails to match — say the
;; user wrote `[_ <> (println …)]` with a stray token between the
;; variable and its expression — the bare syntax-parse fallthrough would
;; mis-read `(let& …)` as a function application and report an unhelpful
;; "unbound identifier: let&".  This procedure is invoked from an
;; explicit error clause so the diagnosis names the form and points at
;; the offending binding clause.  `who` is the form's keyword symbol.
;; A `lambda` / `λ` form whose parameter list or body is malformed.  The
;; well-formed clauses in `parse-expr` accept `(lambda (param ...) body)`
;; with one body and each param an identifier or a constructor pattern;
;; anything else lands here.  Without this, the form would be read as a
;; function application and report the unhelpful "unbound identifier:
;; lambda".  `who` is the form's keyword symbol.
(define (raise-bad-lambda who full-stx)
  (syntax-parse full-stx
    [(_ params . _)
     #:when (not (syntax->list #'params))
     (raise-syntax-error
      who
      (format
       "malformed ~a: the parameter list ~s must be a parenthesized list of identifiers and/or patterns, as in (~a (x (Pair a b)) body)"
       who (syntax->datum #'params) who)
      full-stx #'params)]
    [_
     (raise-syntax-error
      who
      (format
       "malformed ~a: expected (~a (param ...) body) with a single body expression; each param is an identifier or a constructor pattern like (Pair x y)"
       who who)
      full-stx)]))

;; `case-lambda` / `case-λ` desugaring.  Each clause is `[(pat ...) body]`
;; or `[(pat ...) #:when guard body]`; the parenthesized pattern list
;; gives the argument patterns, and every clause must share one arity.
;; The form becomes `(λ (a ...) (match* (a ...) clause ...))` over fresh
;; argument names, reusing the existing `e:match*` core node.  `who` is
;; the form's keyword symbol (for diagnostics).
(define (parse-case-lambda who clause-stxs stx)
  (define clauses (map (lambda (c) (parse-case-lambda-clause who c)) clause-stxs))
  (define arity (length (clause*-patterns (car clauses))))
  (for ([cl (in-list clauses)] [c-stx (in-list clause-stxs)])
    (unless (= (length (clause*-patterns cl)) arity)
      (raise-syntax-error
       who
       (format
        "all ~a clauses must take the same number of arguments; expected ~a pattern(s) but got ~a"
        who arity (length (clause*-patterns cl)))
       stx c-stx)))
  (define fresh-names
    (for/list ([_ (in-range arity)]) (gensym '$arg)))
  (define scrutinees
    (for/list ([n (in-list fresh-names)]) (e:var n stx)))
  (e:lam fresh-names
         (e:match* scrutinees clauses #f stx)
         stx))

;; Parse one `case-lambda` clause into a `clause*` (multi-scrutinee
;; clause).  Mirrors `parse-match-clause`, but the pattern position is a
;; parenthesized list of patterns rather than a single pattern.
(define (parse-case-lambda-clause who stx)
  (syntax-parse stx
    [[(pat ...) (~datum :when) guard body]
     (parameterize ([current-rename-env
                     (extend-rename-env
                      (append-map pattern-binder-ids (syntax->list #'(pat ...))))])
       (clause* (map parse-pattern (syntax->list #'(pat ...)))
                (parse-expr #'guard)
                (parse-expr #'body)
                stx))]
    [[(pat ...) body]
     (parameterize ([current-rename-env
                     (extend-rename-env
                      (append-map pattern-binder-ids (syntax->list #'(pat ...))))])
       (clause* (map parse-pattern (syntax->list #'(pat ...)))
                #f
                (parse-expr #'body)
                stx))]
    [_
     (raise-syntax-error
      who
      (format
       "malformed ~a clause: expected [(pat ...) body] or [(pat ...) :when guard body]"
       who)
      stx)]))

;; A `case-lambda` / `case-λ` form with no clauses or a non-clause shape.
;; The well-formed `parse-expr` branches require at least one clause, so
;; anything else lands here named instead of falling through to an
;; "unbound identifier: case-lambda" application reading.
(define (raise-bad-case-lambda who full-stx)
  (raise-syntax-error
   who
   (format
    "malformed ~a: expected (~a [(pat ...) body] ...+) with at least one clause; each clause matches all arguments at once"
    who who)
   full-stx))

;; `match*` desugaring.  Each clause is `[(pat ...) body]` or
;; `[(pat ...) #:when guard body]`, sharing the clause grammar with
;; `case-lambda` (so `parse-case-lambda-clause` is reused).  The arity is
;; fixed by the scrutinee list; every clause must supply that many
;; patterns.  Unlike `case-lambda`, the scrutinees are real expressions
;; matched in place, so no lambda wrapper is built — the result is the
;; bare `e:match*` core node.
(define (parse-match* scrut-stxs clause-stxs stx)
  (define arity (length scrut-stxs))
  (define clauses (map (lambda (c) (parse-case-lambda-clause 'match* c)) clause-stxs))
  (for ([cl (in-list clauses)] [c-stx (in-list clause-stxs)])
    (unless (= (length (clause*-patterns cl)) arity)
      (raise-syntax-error
       'match*
       (format
        "each match* clause must have one pattern per scrutinee; expected ~a pattern(s) but got ~a"
        arity (length (clause*-patterns cl)))
       stx c-stx)))
  (e:match* (map parse-expr scrut-stxs) clauses #f stx))

;; A `match*` form whose shape is not `(match* (scrut ...) clause ...+)`.
;; Lands here named instead of falling through to an "unbound
;; identifier: match*" application reading.
(define (raise-bad-match* full-stx)
  (raise-syntax-error
   'match*
   "malformed match*: expected (match* (scrut ...) [(pat ...) body] ...+) with at least one clause; each clause supplies one pattern per scrutinee"
   full-stx))

(define (raise-bad-monadic-let who full-stx)
  (define (generic)
    (raise-syntax-error
     who
     (format "expected (~a ([var expr] ...+) body)" who)
     full-stx))
  (syntax-parse full-stx
    [(_ (clause ...) . _)
     (for ([c (in-list (syntax->list #'(clause ...)))])
       (syntax-parse c
         [[_x:id _rhs] (void)]                         ;; well-formed clause
         [[x:id mid rest ...]
          ;; e.g. [_ <> (println …)] — stray token(s) after the variable.
          (raise-syntax-error
           who
           (format
            "malformed binding clause ~s: a ~a binding has the form [var expr]; the `~a` after the variable `~a` is not allowed."
            (syntax->datum c) who (syntax->datum #'mid) (syntax->datum #'x))
           full-stx c)]
         [_
          ;; A clause that is not even [var …]: empty, [var] (missing the
          ;; expression), or a non-identifier on the left.
          (raise-syntax-error
           who
           (format "malformed binding clause ~s: a ~a binding has the form [var expr]"
                   (syntax->datum c) who)
           full-stx c)]))
     ;; Every clause was individually well-formed, so the failure lay
     ;; elsewhere (e.g. a missing or extra body).
     (generic)]
    [_ (generic)]))

;; ----- destructuring let / where -----------------------------------
;;
;; A binding LHS is a pattern.  A bare-identifier pattern is an ordinary
;; binding; any other pattern destructures its RHS via an IRREFUTABLE
;; match (a failure panics), exactly as the retired `match-let` did.

;; Parallel `let`: every RHS is evaluated in the surrounding scope, so the
;; RHSs go into one `e:let` (preserving let-polymorphism for the plain
;; bindings) and the non-variable patterns destructure their bound temp
;; around the body — no binding can see another's pattern variables.
(define (build-destructuring-let lhs-stxs rhs-stxs body-stx stx)
  ;; Parallel `let`: every RHS is evaluated in the surrounding scope, so
  ;; parse the RHSs BEFORE the binders are introduced into the rename env.
  (define rhss (map parse-expr rhs-stxs))
  ;; Introduce the binders (hygienic α-rename when active) for the body and
  ;; for re-parsing the LHS patterns, so a binder and its references in the
  ;; body share the same fresh symbol.
  (parameterize ([current-rename-env
                  (extend-rename-env (append-map pattern-binder-ids lhs-stxs))])
    (define body (parse-expr body-stx))
    (define-values (binds destructures)
      (for/fold ([binds '()] [ds '()]
                 #:result (values (reverse binds) (reverse ds)))
                ([l (in-list lhs-stxs)] [rhs (in-list rhss)] [i (in-naturals)])
        (define pat (parse-pattern l))
        (cond
          [(p:var? pat)
           (values (cons (cons (p:var-name pat) rhs) binds) ds)]
          [else
           (define tmp (string->symbol (format "$let.~a" i)))
           (values (cons (cons tmp rhs) binds)
                   (cons (list tmp pat l) ds))])))
    (define body*
      (for/foldr ([acc body]) ([d (in-list destructures)])
        (e:match (e:var (car d) stx)
                 (list (clause (cadr d) #f acc (caddr d)))
                 #t stx)))
    (e:let binds body* stx)))

;; Sequential `let*`: each binding nests inside the next, so a later RHS
;; sees the earlier pattern variables.  A plain identifier is a singleton
;; `e:let`; any other pattern is a singleton irrefutable match.
(define (build-destructuring-let* lhs-stxs rhs-stxs body-stx stx)
  (let loop ([ls lhs-stxs] [rs rhs-stxs])
    (cond
      [(null? ls) (parse-expr body-stx)]
      [else
       ;; Sequential: this RHS sees the bindings before it (current env);
       ;; this binding's own binders cover the remaining bindings and body.
       (define rhs (parse-expr (car rs)))
       (parameterize ([current-rename-env
                       (extend-rename-env (pattern-binder-ids (car ls)))])
         (define pat   (parse-pattern (car ls)))
         (define inner (loop (cdr ls) (cdr rs)))
         (cond
           [(p:var? pat)
            (e:let (list (cons (p:var-name pat) rhs)) inner stx)]
           [else
            (e:match rhs (list (clause pat #f inner (car ls))) #t stx)]))])))

;; Gather independent binding RHS into one applicative value via
;; right-associated `product`, with the matching nested `Pair`
;; destructuring pattern.  A single binding has no product: the value is
;; the lone RHS and the pattern is a plain variable.
;;
;;   [(a . m1) (b . m2) (c . m3)]
;;     value   = (product m1 (product m2 m3))
;;     pattern = (Pair a (Pair b c))
(define (gather-product binds stx)
  (define name (car (car binds)))
  (define rhs  (cdr (car binds)))
  (cond
    [(null? (cdr binds))
     (values rhs (p:var name stx))]
    [else
     (define-values (rest-ast rest-pat) (gather-product (cdr binds) stx))
     (values (e:app (sugar-ref 'product stx) (list rhs rest-ast) stx)
             ;; `product` returns a prelude Pair, so its destructuring
             ;; pattern is the prelude Pair too (shadow-proof).
             (sugar-pat 'Pair (list (p:var name stx) rest-pat) stx))]))

;; Build `(combiner (lambda (p) (match p [pattern body])) gathered)` for
;; let% (combiner = 'flatmap) and let+ (combiner = 'fmap).  A single
;; binding takes the shortcut `(combiner (lambda (name) body) rhs)`.
(define (build-gathered-let combiner binds body-ast stx)
  (cond
    [(null? (cdr binds))
     (define name (car (car binds)))
     (define rhs  (cdr (car binds)))
     (e:app (sugar-ref combiner stx)
            (list (e:lam (list name) body-ast stx) rhs)
            stx)]
    [else
     (define-values (gathered pat) (gather-product binds stx))
     (define p (gensym '$let))
     (e:app (sugar-ref combiner stx)
            (list (e:lam (list p)
                         (e:match (e:var p stx)
                                  (list (clause pat #f body-ast stx))
                                  #t stx)
                         stx)
                  gathered)
            stx)]))

;; Parse a parallel monadic/applicative let (let%, let+): the RHSs evaluate in
;; the enclosing scope; the binders are introduced (hygienically α-renamed when
;; active) for the body; then the gathered-product engine assembles the call.
(define (parse-gathered-let combiner ids-stx rhss-stx body-stx stx)
  (define ids  (syntax->list ids-stx))
  (define rhss (map parse-expr (syntax->list rhss-stx)))
  (parameterize ([current-rename-env (extend-rename-env ids)])
    (build-gathered-let combiner
                        (for/list ([id (in-list ids)] [r (in-list rhss)])
                          (cons (resolve-id id) r))
                        (parse-expr body-stx)
                        stx)))

;; let& — sequential nested flatmap.  Each later binding's RHS sits
;; inside the earlier bindings' lambdas, so it sees them in scope.
(define (build-sequential-let binds body-ast stx)
  (cond
    [(null? binds) body-ast]
    [else
     (define name (car (car binds)))
     (define rhs  (cdr (car binds)))
     (e:app (sugar-ref 'flatmap stx)
            (list (e:lam (list name)
                         (build-sequential-let (cdr binds) body-ast stx)
                         stx)
                  rhs)
            stx)]))

;; Named pure let — `(letrec ([loop (lambda (x ...) body)]) (loop i ...))`.
(define (build-named-let loop-name binds body-ast stx)
  (e:letrec (list (cons loop-name
                        (e:lam (map car binds) body-ast stx)))
            (e:app (e:var loop-name stx) (map cdr binds) stx)
            stx))

;; Named monadic let% — the loop's parameters are the monadic values;
;; each entry combines them via the gathered-product engine (flatmap)
;; and binds the names; the body is monadic and may recurse with fresh
;; monadic values.  The initial RHS are the seeds.
(define (build-named-monadic-let loop-name binds body-ast stx)
  (define names  (map car binds))
  (define seeds  (map cdr binds))
  (define params (map (lambda (_) (gensym '$arg)) binds))
  (define param-binds
    (for/list ([n (in-list names)] [p (in-list params)])
      (cons n (e:var p stx))))
  (define loop-body (build-gathered-let 'flatmap param-binds body-ast stx))
  (e:letrec (list (cons loop-name (e:lam params loop-body stx)))
            (e:app (e:var loop-name stx) seeds stx)
            stx))

(define (parse-do stmts body-stx stx)
  (cond
    [(null? stmts) (parse-expr body-stx)]
    [else
     (define s (car stmts))
     (syntax-parse s
       #:datum-literals (<-)
       [[v:id <- expr]
        ;; The RHS is in the enclosing scope; `v` binds the unwrapped value
        ;; for the rest of the chain and the body.
        (define rhs (parse-expr #'expr))
        (parameterize ([current-rename-env (extend-rename-env (list #'v))])
          (e:app (sugar-ref 'flatmap stx)
                 (list (e:lam (list (resolve-id #'v))
                              (parse-do (cdr stmts) body-stx stx)
                              stx)
                       rhs)
                 stx))]
       [expr
        ;; Bare-expression clause: sequence `expr` for its monadic
        ;; effect, discard the result, then continue.  Desugars to the
        ;; same shape as `[_fresh <- expr]` but with a fresh
        ;; identifier so the wildcard isn't a binder.
        (define fresh (gensym '_do))
        (e:app (sugar-ref 'flatmap stx)
               (list (e:lam (list fresh)
                            (parse-do (cdr stmts) body-stx stx)
                            stx)
                     (parse-expr #'expr))
               stx)])]))

;; ----- proc / arrow notation ---------------------------------------
;;
;; A `proc (pat) cmd …` desugars to a single arrow built from the
;; Category/Arrow combinators, following the environment-passing
;; (Paterson) translation.  The "environment" is a runtime value
;; carrying every in-scope variable; at each point we track `env-pat`,
;; a pattern that destructures that value into the variables bound so
;; far.  A command translates to an arrow `env ~> output`.
;;
;; Combinator shorthands used by the translation:
;;   arr g            lift a plain function into the arrow
;;   comp a b         run a, then b (forward composition)
;;   ident            the identity arrow
;;   fanout a b       env ~> (Pair (a env) (b env))   (used to keep env)
;;   fanin  a b       (Either x y) ~> out             (used by if/match)
;;   arrow-app        run a fed-in arrow on its argument (feed-apply)
;;
;; Sequencing a command and binding its result to `p` keeps the old
;; environment alongside the new output:
;;   comp (fanout ⟦cmd⟧ ident) ⟦rest⟧      with env' = (Pair p env)

(define (parse-proc pat-stx cmd-stxs stx)
  (translate-proc-seq cmd-stxs (parse-pattern pat-stx) stx))

;; Arrow-combinator constructors (kept tiny and local).  Each method
;; reference gets a FRESH syntax object: the infer→codegen method-
;; resolution table is keyed by the `e:var`'s `stx` identity, so reusing
;; one `stx` across the many method call sites a single `proc` emits
;; would make their resolutions collide.  `fresh-op-stx` mints a distinct
;; object per reference while preserving the source location for errors.
(define (fresh-op-stx ctx) (datum->syntax ctx 'arrow-op ctx))
(define (arrow-arr g stx)      (e:app (sugar-ref 'arr (fresh-op-stx stx))   (list g) stx))
;; `comp` is standard (right-to-left) composition: `(comp g f)` runs `f`
;; then `g`.  A `proc` pipeline reads left-to-right ("do `a`, then `b`"),
;; so the two arrows are passed to `comp` in reverse: `(comp b a)`.
(define (arrow-comp a b stx)   (e:app (sugar-ref 'comp (fresh-op-stx stx))  (list b a) stx))
(define (arrow-fanout a b stx) (e:app (sugar-ref 'fanout (fresh-op-stx stx))(list a b) stx))
(define (arrow-fanin a b stx)  (e:app (sugar-ref 'fanin (fresh-op-stx stx)) (list a b) stx))
(define (arrow-ident stx)      (sugar-ref 'ident (fresh-op-stx stx)))
(define (arrow-app-ref stx)    (sugar-ref 'arrow-app (fresh-op-stx stx)))
(define (arrow-loop a stx)     (e:app (sugar-ref 'arrow-loop (fresh-op-stx stx)) (list a) stx))
;; Product / coproduct intro + projection, expressed through the
;; tensor-class methods (Prod / Coprod) rather than the strict
;; `Pair` / `Either` constructors.  This keeps the whole proc translation
;; polymorphic in the arrow's product and coproduct: over `(->)` these
;; resolve to `Pair` / `Either` (identical behavior), and over a lazy
;; arrow they resolve to its lazy product / coproduct.
(define (mk-prod-e a b stx)    (e:app (sugar-ref 'mk-prod (fresh-op-stx stx))   (list a b) stx))
(define (prod-fst-e v stx)     (e:app (sugar-ref 'prod-fst (fresh-op-stx stx))  (list v) stx))
(define (prod-snd-e v stx)     (e:app (sugar-ref 'prod-snd (fresh-op-stx stx))  (list v) stx))
(define (inj-left-e v stx)     (e:app (sugar-ref 'inj-left (fresh-op-stx stx))  (list v) stx))
(define (inj-right-e v stx)    (e:app (sugar-ref 'inj-right (fresh-op-stx stx)) (list v) stx))

;; Right-nest a non-empty list of items into binary pairs via `mk`:
;; [a b c] → (mk a (mk b c)).  Used to build the recursive-binding tuple
;; (and its destructuring pattern) for `rec`.
(define (nest-right items mk)
  (let loop ([xs items])
    (if (null? (cdr xs)) (car xs) (mk (car xs) (loop (cdr xs))))))

;; Bind every variable in `env-pat` from the product value `val-expr`,
;; then evaluate `body`.  The pattern's `Pair` spine — the bookkeeping
;; tuple the proc translation builds with `mk-prod` — is taken apart by
;; `prod-fst` / `prod-snd` projections rather than a constructor match, so
;; the carried environment can be ANY arrow's product (strict `Pair`, a
;; lazy `LPair`, …).  A non-`Pair` leaf (a user pattern bound by `[p <- c]`
;; or a `match` branch) is matched ordinarily and irrefutably: the value
;; was placed there by an earlier binding that already matched it.
(define (bind-env-pat env-pat val-expr body stx)
  (cond
    [(p:wild? env-pat) body]
    [(p:var? env-pat)
     (e:let (list (cons (p:var-name env-pat) val-expr)) body stx)]
    [(and (p:ctor? env-pat)
          (eq? (p:ctor-name env-pat) 'Pair)
          (= 2 (length (p:ctor-args env-pat))))
     (define g (gensym '$prod))
     (e:let (list (cons g val-expr))
            (bind-env-pat (car (p:ctor-args env-pat))
                          (prod-fst-e (e:var g stx) stx)
                          (bind-env-pat (cadr (p:ctor-args env-pat))
                                        (prod-snd-e (e:var g stx) stx)
                                        body stx)
                          stx)
            stx)]
    [else
     (e:match val-expr (list (clause env-pat #f body stx)) #t stx)]))

;; Build `arr (λ g. ⟦bind env-pat from g⟧ body)` — a pure arrow that
;; projects the environment value and computes `body`.
(define (env-arr env-pat body stx)
  (define g (gensym '$env))
  (arrow-arr
   (e:lam (list g) (bind-env-pat env-pat (e:var g stx) body stx) stx)
   stx))

;; Translate a command sequence in environment `env-pat`.  The final
;; command is the output; earlier ones bind into the environment.
(define (translate-proc-seq cmds env-pat stx)
  (cond
    [(null? (cdr cmds)) (translate-proc-cmd (car cmds) env-pat stx)]
    [else
     (define s (car cmds))
     (syntax-parse s
       #:datum-literals (<- let rec)
       ;; (rec [p <- c] …) — mutually-recursive bindings: every p is in
       ;; scope in every c (and downstream).  Translated via ArrowLoop:
       ;; the binding tuple R is fed back as the loop's recursive
       ;; channel, so each command sees (Pair env R).  No `(->)` instance
       ;; exists, so `rec` only resolves for a loop-capable user arrow.
       [(rec [p <- c] ...+)
        (define ps (map parse-pattern (syntax->list #'(p ...))))
        (define cs (syntax->list #'(c ...)))
        (define r-pat (nest-right ps (lambda (a b) (sugar-pat 'Pair (list a b) stx))))
        ;; inside the loop each command sees env extended with R.
        (define inner-pat (sugar-pat 'Pair (list env-pat r-pat) stx))
        (define branch-arrs
          (for/list ([ci (in-list cs)]) (translate-proc-cmd ci inner-pat stx)))
        (define fanout-tree
          (nest-right branch-arrs (lambda (a b) (arrow-fanout a b stx))))
        ;; loopbody : (Pair env R) ~> (Pair R R) — produce R as output and
        ;; feedback both.
        (define r (gensym '$rec))
        (define dup (arrow-arr (e:lam (list r) (mk-prod-e (e:var r stx) (e:var r stx) stx) stx) stx))
        (define loopbody (arrow-comp fanout-tree dup stx))
        (define looped (arrow-loop loopbody stx))
        ;; extend the outer env with the produced bindings, then continue.
        (define env-pat* (sugar-pat 'Pair (list r-pat env-pat) stx))
        (arrow-comp (arrow-fanout looped (arrow-ident stx) stx)
                    (translate-proc-seq (cdr cmds) env-pat* stx)
                    stx)]
       ;; [p <- cmd] — run cmd, bind its output to pattern p, continue.
       [[p <- c]
        (define c-arr (translate-proc-cmd #'c env-pat stx))
        (define env-pat* (sugar-pat 'Pair (list (parse-pattern #'p) env-pat) stx))
        (arrow-comp (arrow-fanout c-arr (arrow-ident stx) stx)
                    (translate-proc-seq (cdr cmds) env-pat* stx)
                    stx)]
       ;; (let ([v e] …) — pure bindings that extend the environment for
       ;; the remaining commands.  Each binding is prepended with
       ;; @racket[fanout], NOT a single @racket[mk-prod] tuple: a
       ;; @racket[fanout] output has the arrow's own product type @racket[p]
       ;; (pinned by the fundep @racket[cat -> p]), whereas a bare
       ;; @racket[mk-prod] inside an @racket[arr] makes an ambiguous
       ;; product that never reaches a @racket[p]-typed position.  Every
       ;; binding's rhs reads the @emph{original} env (each @racket[fanout]
       ;; feeds the same input to both sides), so the bindings are mutually
       ;; independent — parallel @racket[let], as before.
       [(let ([v:id e] ...+))
        (define vs (map syntax->datum (syntax->list #'(v ...))))
        (define es (syntax->list #'(e ...)))
        (define ext-arr
          (foldr (lambda (e acc)
                   (arrow-fanout (env-arr env-pat (parse-expr e) stx) acc stx))
                 (arrow-ident stx)
                 es))
        (define env-pat*
          (foldr (lambda (v acc) (sugar-pat 'Pair (list (p:var v stx) acc) stx))
                 env-pat vs))
        (arrow-comp ext-arr (translate-proc-seq (cdr cmds) env-pat* stx) stx)]
       ;; bare command — run it for effect, discard its output, continue.
       [_
        (define c-arr (translate-proc-cmd s env-pat stx))
        (define env-pat* (sugar-pat 'Pair (list (p:wild stx) env-pat) stx))
        (arrow-comp (arrow-fanout c-arr (arrow-ident stx) stx)
                    (translate-proc-seq (cdr cmds) env-pat* stx)
                    stx)])]))

;; Translate one command to an arrow `env ~> output`.
(define (translate-proc-cmd cmd env-pat stx)
  (syntax-parse cmd
    #:datum-literals (feed feed-apply if match via)
    ;; (feed arrow e) — `arrow -< e`: arrow is static, e reads the env.
    [(feed f e)
     (arrow-comp (env-arr env-pat (parse-expr #'e) stx)
                 (parse-expr #'f)
                 stx)]
    ;; (feed-apply af e) — `af -<< e`: the arrow itself reads the env.
    [(feed-apply f e)
     (arrow-comp (env-arr env-pat (mk-prod-e (parse-expr #'f) (parse-expr #'e) stx) stx)
                 (arrow-app-ref stx)
                 stx)]
    ;; (if test c1 c2) — choose a command by a Boolean test.  Route the
    ;; whole env into the coproduct's left (inj-left) or right (inj-right),
    ;; then fan the branches back in with `fanin`.
    [(if test c1 c2)
     (define g (gensym '$env))
     (define route
       (arrow-arr
        (e:lam (list g)
               (bind-env-pat env-pat (e:var g stx)
                             (e:if (parse-expr #'test)
                                   (inj-left-e  (e:var g stx) stx)
                                   (inj-right-e (e:var g stx) stx)
                                   stx)
                             stx)
               stx)
        stx))
     (arrow-comp route
                 (arrow-fanin (translate-proc-cmd #'c1 env-pat stx)
                              (translate-proc-cmd #'c2 env-pat stx)
                              stx)
                 stx)]
    ;; (match e [pat c] …) — choose a command by matching `e` (read from
    ;; the env).  Each branch runs in the env extended by its pattern's
    ;; bindings.  Routing tags the (env-carrying) value into a right-
    ;; nested `Either`, then a right-nested `fanin` dispatches the
    ;; branches.  See `translate-proc-case`.
    [(match e [pat c] ...+)
     (translate-proc-case #'e
                          (syntax->list #'(pat ...))
                          (syntax->list #'(c ...))
                          env-pat stx)]
    ;; (via opExpr c …) — banana brackets: apply an arrow-combining
    ;; expression to the translated sub-commands.
    [(via op c ...+)
     (e:app (parse-expr #'op)
            (for/list ([ci (in-list (syntax->list #'(c ...)))])
              (translate-proc-cmd ci env-pat stx))
            stx)]))

;; Build a coproduct-nesting wrapper for branch `i` of `n` (0-based) to
;; feed a right-nested `fanin` consumer
;; `(fanin c0 (fanin c1 (… c_{n-1})))`: branch i is `i` right-injections
;; deep, then a left-injection (the fanin's left/active side) for every
;; branch except the last, which sits on the final right-injection.
;; n = 1 needs no tag at all.  `inj-left`/`inj-right` are the Coproduct
;; methods, so this is polymorphic in the arrow's coproduct (over `(->)`
;; they are `Left`/`Right`).
(define (case-wrap i n v stx)
  (define inner
    (if (= i (sub1 n))
        v
        (inj-left-e v stx)))
  (let loop ([k i] [acc inner])
    (if (= k 0)
        acc
        (loop (sub1 k) (inj-right-e acc stx)))))

;; Translate `(match e [pat c] …)` to a routing arrow followed by a
;; right-nested fanin over the branch arrows.
(define (translate-proc-case e-stx pat-stxs cmd-stxs env-pat stx)
  (define n (length pat-stxs))
  (define pats (map parse-pattern pat-stxs))
  (define g (gensym '$case))
  ;; First pair the scrutinee `e` with the env using @racket[fanout], so
  ;; the carried @racket[(p ev env)] product has the arrow's own product
  ;; type @racket[p] (pinned by the fundep) rather than an ambiguous
  ;; @racket[mk-prod] product — the same reason @racket[let] uses
  ;; @racket[fanout].  @racket[pre : env ~> (p ev env)].
  (define pre
    (arrow-fanout (env-arr env-pat (parse-expr e-stx) stx)
                  (arrow-ident stx) stx))
  ;; Route on the scrutinee half (its @racket[prod-fst]), injecting the
  ;; whole @racket[(p ev env)] product into the nested coproduct for the
  ;; matching branch.
  (define route-clauses
    (for/list ([p (in-list pats)] [i (in-naturals)])
      (clause p #f (case-wrap i n (e:var g stx) stx) stx)))
  (define route
    (arrow-arr
     (e:lam (list g)
            (e:match (prod-fst-e (e:var g stx) stx) route-clauses #f stx)
            stx)
     stx))
  ;; Each branch consumes (p ev env) with its pattern re-bound on the
  ;; scrutinee half.
  (define branch-arrs
    (for/list ([p (in-list pats)] [c (in-list cmd-stxs)])
      (translate-proc-cmd c (sugar-pat 'Pair (list p env-pat) stx) stx)))
  ;; Single branch: no choice, just the routing + the one branch.
  ;; Otherwise fold a right-nested fanin over the branch arrows.
  (define consumer
    (let loop ([bs branch-arrs])
      (cond
        [(null? (cdr bs)) (car bs)]
        [else (arrow-fanin (car bs) (loop (cdr bs)) stx)])))
  (arrow-comp pre (arrow-comp route consumer stx) stx))

;; ----- patterns -----------------------------------------------------

;; Marked prelude-List constructor PATTERNS — the pattern dual of
;; `list-cons-ref` / `list-nil-ref`, so list-pattern sugar matches the
;; prelude List even where the module shadows those constructor names.
(define (sugar-pat name args stx) (p:ctor name args (mark-sugar-ref stx)))
(define (list-cons-pat args stx) (sugar-pat 'Cons args stx))
(define (list-nil-pat  stx)      (sugar-pat 'Nil  '()  stx))

;; A list pattern is Cons/Nil, the dual of `build-list-ast`: fold `pats`
;; into nested `(Cons p rest)` constructor patterns, ending at `tail`
;; (`Nil` for a fixed-arity list, or a rest-binder for a `… ...` tail).
(define (build-cons-pattern pats tail stx)
  (cond
    [(null? pats) tail]
    [else (list-cons-pat
           (list (car pats) (build-cons-pattern (cdr pats) tail stx))
           stx)]))

;; The literal-data form of a deep-nested quasiquote keyword in a pattern:
;; `(sym inner)` as a Cons/Nil pattern.  Mirrors `quote-data-form`.
(define (quote-data-pattern sym inner stx)
  (list-cons-pat
   (list (p:lit sym stx)
         (list-cons-pat (list inner (list-nil-pat stx)) stx))
   stx))

;; The pattern dual of `quote->ast`: convert a quoted/quasiquoted datum to
;; a pattern.  `level` tracks quasiquote nesting exactly as on the
;; expression side — 0 = not inside a quasiquote (a direct unquote is an
;; error), 1 = unquote escapes to an ordinary sub-pattern, deeper levels
;; keep it as data.  A quoted list is always fixed-arity (Nil-terminated);
;; `,@` (splice) has no pattern meaning and is rejected.
(define (quote->pattern d level stx)
  ;; Curly-brace literals have no pattern meaning (as in unquoted position),
  ;; so a quoted/quasiquoted map or set is a located error rather than a
  ;; silently-different list pattern.
  (case (bracket-literal-kind d)
    [(map set)
     (raise-syntax-error 'rackton
       "map/set literals ({..} / #{..}) are not patterns" stx)]
    [else (quote->pattern/form d level stx)]))

(define (quote->pattern/form d level stx)
  (syntax-parse d
    #:datum-literals (quasiquote unquote unquote-splicing)
    [(unquote e)
     (cond
       [(= level 0)
        (raise-syntax-error 'rackton "unquote not in quasiquote" stx d)]
       [(= level 1) (parse-pattern #'e)]
       [else (quote-data-pattern 'unquote
                                 (quote->pattern #'e (sub1 level) stx) stx)])]
    [(unquote-splicing _)
     (raise-syntax-error 'rackton
                         "unquote-splicing is not supported in patterns" stx d)]
    [(quasiquote e)
     (quote-data-pattern 'quasiquote (quote->pattern #'e (add1 level) stx) stx)]
    [n:number  (p:lit (syntax->datum #'n) stx)]
    [b:boolean (p:lit (syntax->datum #'b) stx)]
    [s:string  (p:lit (syntax->datum #'s) stx)]
    [c:char    (p:lit (syntax->datum #'c) stx)]
    [by:bytes  (p:lit (syntax->datum #'by) stx)]
    [x:id      (p:lit (syntax->datum #'x) stx)]
    [(elem ...)
     (build-cons-pattern
      (for/list ([e (in-list (syntax->list #'(elem ...)))])
        (quote->pattern e level stx))
      (list-nil-pat stx) stx)]
    ;; '(a . b) as a pattern (proper lists matched above) => Pair pattern.
    [(a . b)
     (when (pair? (syntax-e #'b))
       (raise-syntax-error 'rackton
         "a dotted pair must have the form (a . b)" stx #'b))
     (sugar-pat 'Pair
                (list (quote->pattern #'a level stx) (quote->pattern #'b level stx))
                stx)]
    [_ (raise-syntax-error 'rackton "unsupported quoted pattern" stx d)]))

;; A literal `...` token (the ellipsis identifier).
(define (ellipsis-id? s) (and (identifier? s) (eq? (syntax-e s) '...)))

;; `(list p …)` pattern.  A plain list is fixed-arity (matches exactly
;; that many elements, Nil-terminated).  A single trailing `<pat> ...`
;; makes `<pat>` a rest-binder over the remaining elements — desugaring to
;; the tail position of the Cons chain — so it must be a variable or `_`.
(define (parse-list-pattern elems stx)
  (cond
    [(and (pair? elems) (ellipsis-id? (last elems)))
     (define body (drop-right elems 1))    ;; without the trailing `...`
     (when (null? body)
       (raise-syntax-error 'rackton
                           "`...` must follow a pattern in a list pattern" stx))
     (when (ormap ellipsis-id? body)
       (raise-syntax-error 'rackton
                           "`...` must be the final element of a list pattern" stx))
     (define rest-stx (last body))
     (define rest-pat (parse-pattern rest-stx))
     (unless (or (p:var? rest-pat) (p:wild? rest-pat))
       (raise-syntax-error 'rackton
         "the pattern before `...` must be a variable or `_` (it binds the rest of the list)"
         stx rest-stx))
     (build-cons-pattern (map parse-pattern (drop-right body 1)) rest-pat stx)]
    [(ormap ellipsis-id? elems)
     (raise-syntax-error 'rackton
                         "`...` must be the final element of a list pattern" stx)]
    [else
     (build-cons-pattern (map parse-pattern elems) (list-nil-pat stx) stx)]))

(define (parse-pattern stx)
  ;; A `[p ...]` list pattern parallels the `[v ...]` expression literal,
  ;; and `[a . b]` a Pair pattern parallels the dotted-pair literal;
  ;; map/set literals have no pattern meaning and are a located error.
  (case (bracket-literal-kind stx)
    [(list) (parse-list-pattern (syntax->list stx) stx)]
    [(pair) (parse-pair-pattern stx)]
    [(map set)
     (raise-syntax-error 'rackton
       "map/set literals ({..} / #{..}) are not patterns" stx)]
    [else (parse-pattern/form stx)]))

;; [a . b] as a pattern => (Pair a b).  The dotted tail must be a single
;; element ([a . b]); a longer dotted form ([a b . c]) is a located error.
(define (parse-pair-pattern stx)
  (define e (syntax-e stx))
  (define cdr-stx (cdr e))
  (when (pair? (syntax-e cdr-stx))
    (raise-syntax-error 'rackton
      "dotted pair pattern must have the form [a . b]" stx))
  (sugar-pat 'Pair (list (parse-pattern (car e)) (parse-pattern cdr-stx)) stx))

(define (parse-pattern/form stx)
  (syntax-parse stx
    #:datum-literals (quote quasiquote unquote unquote-splicing list tuple bits)
    [n:number  (p:lit (syntax->datum #'n) stx)]
    [b:boolean (p:lit (syntax->datum #'b) stx)]
    [s:string  (p:lit (syntax->datum #'s) stx)]
    ;; `(tuple p …)` — destructure a tuple of matching arity.
    [(tuple elem ...)
     (p:tuple (map parse-pattern (syntax->list #'(elem ...))) stx)]
    ;; `(bits seg …)` — Erlang-style binary pattern; destructures a
    ;; Bitstring segment by segment.
    [(bits seg ...)
     (p:bits (parse-bit-segments (syntax->list #'(seg ...)) parse-pattern) stx)]
    ;; Quotation builds list patterns (see `quote->pattern`): `'foo` is the
    ;; Symbol 'foo, `'(1 2 3)` matches that literal list, and quasiquote
    ;; adds `,sub` escapes.  An unquote outside a quasiquote is an error.
    [(quote datum)      (quote->pattern #'datum 0 stx)]
    [(quasiquote datum) (quote->pattern #'datum 1 stx)]
    [(unquote _)
     (raise-syntax-error 'rackton "unquote not in quasiquote" stx stx)]
    [(unquote-splicing _)
     (raise-syntax-error 'rackton "unquote-splicing not in quasiquote" stx stx)]
    ;; `(list p …)` — fixed-arity or `<var> ...` rest-binder list pattern.
    [(list elem ...) (parse-list-pattern (syntax->list #'(elem ...)) stx)]
    [x:id
     (define name (syntax->datum #'x))
     (cond
       [(wildcard-symbol? name) (p:wild stx)]
       [(illegal-colon-id? name)
        (raise-syntax-error 'rackton malformed-colon-message #'x)]
       ;; A qualified reference `mod:Ctor` is a (nullary) constructor,
       ;; never a fresh pattern variable, even if lowercase-initial.
       [(qualified-id? name)    (p:ctor name '() stx)]
       [(lowercase-id? name)    (p:var (resolve-id #'x) stx)]
       [else                    (p:ctor name '() stx)])]
    [(ctor:id arg ...)
     #:fail-unless (or (qualified-id? (syntax->datum #'ctor))
                       (not (lowercase-id? (syntax->datum #'ctor))))
     "constructor pattern head must be a constructor or qualified name"
     (define args (syntax->list #'(arg ...)))
     (cond
       ;; Keyword pattern `(C :f1 p1 :f2 p2 …)`: like keyword
       ;; construction, fields must be in declared order, so the
       ;; sub-patterns are already positional.  Desugar to a positional
       ;; p:ctor, recording the labels for inference to verify.
       [(and (pair? args) (keyword-token? (car args)))
        (define labels+pats (parse-keyword-patterns args))
        (p:ctor (syntax->datum #'ctor)
                (map cdr labels+pats)
                (syntax-property stx 'rackton:kw-labels (map car labels+pats)))]
       [else
        (p:ctor (syntax->datum #'ctor)
                (map parse-pattern args)
                stx)])]))

;; Parse a `:label pattern …` sequence into a list of (field-name .
;; pattern).  Mirrors `parse-keyword-args` for pattern position.
(define (parse-keyword-patterns args)
  (let loop ([args args] [acc '()])
    (cond
      [(null? args) (reverse acc)]
      [(not (keyword-token? (car args)))
       (raise-syntax-error 'rackton
         "keyword pattern must use :label pattern pairs throughout"
         (car args))]
      [(null? (cdr args))
       (raise-syntax-error 'rackton
         (format "keyword :~a is missing its pattern"
                 (keyword-id->name (syntax->datum (car args))))
         (car args))]
      [else
       (loop (cddr args)
             (cons (cons (keyword-id->name (syntax->datum (car args)))
                         (parse-pattern (cadr args)))
                   acc))])))

;; ----- bit syntax ---------------------------------------------------
;;
;; A `(bits seg …)` form, in either expression or pattern position, is a
;; sequence of segment clauses `[subject size? type? flag…]` (see
;; BitSyntax.org).  `parse-bit-segments` is shared by both positions; the
;; only difference is `parse-subject` (parse-expr vs parse-pattern).

(define bit-type-keywords '(integer float binary bitstring utf8 utf16 utf32))
(define bit-flag-keywords '(signed unsigned big little))

(define (parse-bit-segments seg-stxs parse-subject)
  (define segs
    (for/list ([seg-stx (in-list seg-stxs)])
      (parse-one-bit-segment seg-stx parse-subject)))
  ;; A `_` (rest) size may appear only on the final segment.
  (let loop ([ss segs])
    (cond
      [(or (null? ss) (null? (cdr ss))) (void)]
      [(eq? (bit-seg-size (car ss)) 'rest)
       (raise-syntax-error 'rackton
         "a `_` (rest) bits segment may appear only as the last segment"
         (bit-seg-stx (car ss)))]
      [else (loop (cdr ss))]))
  segs)

(define (parse-one-bit-segment seg-stx parse-subject)
  (define parts (syntax->list seg-stx))
  (unless (and parts (pair? parts))
    (raise-syntax-error 'rackton
      "a bits segment must be a `[subject size? type? flag…]` clause" seg-stx))
  (define subject (parse-subject (car parts)))
  ;; size — a nat literal (bits / bytes), `_` (rest), or a variable name
  ;; (a dependent width); absent when the next token is a type/flag.
  (define-values (size after-size)
    (let ([tail (cdr parts)])
      (cond
        [(null? tail) (values #f '())]
        [else
         (define d (syntax->datum (car tail)))
         (cond
           [(exact-nonnegative-integer? d) (values d (cdr tail))]
           [(eq? d '_)                     (values 'rest (cdr tail))]
           [(and (symbol? d) (or (memq d bit-type-keywords)
                                 (memq d bit-flag-keywords)))
            (values #f tail)]
           [(symbol? d)                    (values d (cdr tail))]
           [else (raise-syntax-error 'rackton
                   "bits segment size must be a literal, a variable, or `_`"
                   (car tail))])])))
  ;; type + flags
  (let loop ([toks after-size] [type #f] [signed? #f] [endian #f])
    (cond
      [(null? toks)
       (finish-bit-segment subject size type signed? endian seg-stx)]
      [else
       (define tok (car toks))
       (case (syntax->datum tok)
         [(integer binary bitstring)
          (when type
            (raise-syntax-error 'rackton "bits segment has two types" tok))
          (loop (cdr toks) (syntax->datum tok) signed? endian)]
         [(float utf8 utf16 utf32)
          (raise-syntax-error 'rackton
            "float/utf bits segments are not yet supported (phase 2)" tok)]
         [(signed)   (loop (cdr toks) type #t endian)]
         [(unsigned) (loop (cdr toks) type #f endian)]
         [(big)      (loop (cdr toks) type signed? 'big)]
         [(little)
          (raise-syntax-error 'rackton
            "little-endian bits segments are not yet supported (phase 2)" tok)]
         [else (raise-syntax-error 'rackton
                 "unknown bits segment specifier" tok)])])))

;; Resolve segment defaults and reject contradictions.
(define (finish-bit-segment subject size type signed? endian seg-stx)
  (define ty (or type 'integer))
  (define sz
    (cond
      [size size]
      [(eq? ty 'integer) 8]     ; default integer width
      [else 'rest]))            ; binary/bitstring with no size = the rest
  (when (and (eq? ty 'integer) (eq? sz 'rest))
    (raise-syntax-error 'rackton
      "an integer bits segment needs a width" seg-stx))
  (bit-seg subject sz ty (and signed? #t) (or endian 'big) seg-stx))

;; Side channel: every `(define (f params…) body)` form parsed via
;; `parse-fn-params+body` deposits its original `(params-stx body-stx)`
;; here, keyed by the def's source stx.  The post-processor in
;; `parse-toplevel-list` consults this when grouping consecutive
;; same-name function defs into a single multi-clause `e:match*`,
;; reparsing the parameter list under the multi-clause rule (bare
;; uppercase identifiers become 0-arg ctor patterns rather than plain
;; param names).  Outside `parse-toplevel-list` the parameter is #f
;; and recording is skipped — `parse-top` then behaves exactly as
;; before for the surface tests.
(define current-fn-clauses-record (make-parameter #f))

;; Parse a function's parameter list together with its body.  Each
;; parameter is either a bare identifier (binds a plain parameter
;; — preserves the pre-feature behaviour for every existing
;; identifier shape, including `_x` and uppercase names) or a
;; parenthesized pattern like `(Point x y)` or `(Cons x xs)`.
;; Pattern parameters desugar to a synthetic identifier plus an
;; irrefutable `match` wrapping the body.  Returns
;; `(values param-names body-ast)`.
;;
;; Source order is preserved in the wrap: matching against the
;; first parameter is outermost in the resulting expression, so a
;; refutable pattern in the first parameter raises its mismatch
;; before later parameters are even examined.
;;
;; `irrefutable?` is flagged on every generated `e:match` so the
;; exhaustiveness checker skips it — the caller is explicitly
;; asserting the pattern fits.  Sum-type cases that need
;; fall-through across constructors should use the multi-clause
;; `define` mechanism instead of a single-clause refutable pattern.
(define (parse-fn-params+body params-stx body-stx ctx-stx)
  (define params-list (syntax->list params-stx))
  ;; Bare-identifier params bind directly; parenthesized params are patterns.
  ;; Introduce all param binders into the rename env (hygienic α-rename when
  ;; active) before parsing the body, so the body's references to a param and
  ;; the param binder share one fresh symbol.
  (parameterize ([current-rename-env
                  (extend-rename-env
                   (append-map (lambda (p)
                                 (if (identifier? p)
                                     (list p)
                                     (pattern-binder-ids p)))
                               params-list))])
    (define-values (names-rev wrappers-rev)
      (for/fold ([names '()] [wrappers '()])
                ([p (in-list params-list)])
        (cond
          [(identifier? p)
           ;; Bare identifier: use directly as the lambda's
           ;; parameter name.  No pattern parsing — `Nil` here is a
           ;; parameter literally named `Nil`, not a 0-arg ctor
           ;; pattern.  To pattern-match against a 0-arg ctor, wrap
           ;; in parens: `(Nil)`.
           (values (cons (resolve-id p) names) wrappers)]
          [else
           (define pat (parse-pattern p))
           (define fresh (gensym '$arg))
           (values (cons fresh names)
                   (cons (cons fresh pat) wrappers))])))
    (define names    (reverse names-rev))
    (define wrappers (reverse wrappers-rev))
    (define body-ast (parse-expr body-stx))
    (define wrapped
      (foldr (lambda (w body)
               (e:match (e:var (car w) ctx-stx)
                        (list (clause (cdr w) #f body body-stx))
                        #t
                        ctx-stx))
             body-ast
             wrappers))
    ;; Stash the original params/body for the multi-clause combiner
    ;; that may run in `parse-toplevel-list`.  Outside that scope the
    ;; record parameter is #f and this is a no-op.
    (let ([rec (current-fn-clauses-record)])
      (when rec
        (hash-set! rec ctx-stx (cons params-stx body-stx))))
    (values names wrapped)))

;; ----- types --------------------------------------------------------

(define (parse-type stx)
  (syntax-parse stx
    #:datum-literals (All Exists => ->)
    ;; A non-negative integer literal in type position is a type-level
    ;; natural (kind `Nat`) — e.g. the `3` in `(Array 3 a)`.
    [n:nat (ty:nat (syntax->datum #'n) stx)]
    [(All (v:id ...) body)
     (ty:forall (map syntax->datum (syntax->list #'(v ...)))
                (parse-type #'body)
                stx)]
    ;; First-class existential: the dual of `All`.  `(Exists (a …) body)`,
    ;; where `body` is usually a qualified type `(=> ctx τ)`.
    [(Exists (v:id ...) body)
     (ty:exists (map syntax->datum (syntax->list #'(v ...)))
                (parse-type #'body)
                stx)]
    ;; Qualified type: (constraint ...+ => body)
    [(c ...+ => body)
     (ty:qual (for/list ([cstx (in-list (syntax->list #'(c ...)))])
                (parse-constraint cstx))
              (parse-type #'body)
              stx)]
    ;; Bare function arrow as a referenceable type constructor.  Either the
    ;; standalone literal `->` or the parenthesized zero-arg `(->)` denotes
    ;; the arrow tycon unapplied, so it can sit in an instance head such as
    ;; `(Arrow (->))`.  These clauses sit above the variadic arrow form and
    ;; match only the zero-argument cases, leaving `(-> A B …)` untouched.
    [->    (ty:con '-> stx)]
    [(->)  (ty:con '-> stx)]
    ;; Arrow type — variadic in the surface, binary in the core AST.
    ;;   `(-> T)`         → `(-> Unit T)` (0-arg fn returning T)
    ;;   `(-> A B)`       → standard binary arrow
    ;;   `(-> A B C …)`   → right-associates: `(-> A (-> B (-> C …)))`
    [(-> arg ...+)
     (build-arrow-type (syntax->list #'(arg ...)) stx)]
    [x:id
     (define name (syntax->datum #'x))
     (cond
       [(illegal-colon-id? name)
        (raise-syntax-error 'rackton malformed-colon-message #'x)]
       ;; A qualified reference `mod:Name` is always a type constructor,
       ;; even when its name part is lowercase — qualification overrides
       ;; the lowercase-means-type-variable rule.
       [(qualified-id? name) (ty:con name stx)]
       [(lowercase-id? name) (ty:var name stx)]
       [else                 (ty:con name stx)])]
    [(head arg ...+)
     (ty:app (parse-type #'head)
             (for/list ([a (in-list (syntax->list #'(arg ...)))])
               (parse-type a))
             stx)]))

;; Right-fold a variadic `->` form into binary arrow applications so the
;; core type AST stays binary (downstream stages only ever see `(-> A B)`).
(define (build-arrow-type arg-stxs stx)
  (cond
    [(null? (cdr arg-stxs))
     ;; `(-> T)` — 0-arg fn encoding.
     (ty:app (ty:con '-> stx)
             (list (ty:con 'Unit stx) (parse-type (car arg-stxs)))
             stx)]
    [(null? (cddr arg-stxs))
     ;; `(-> A B)` — terminal binary arrow.
     (ty:app (ty:con '-> stx)
             (list (parse-type (car arg-stxs)) (parse-type (cadr arg-stxs)))
             stx)]
    [else
     ;; `(-> A B C …)` — right-associate.
     (ty:app (ty:con '-> stx)
             (list (parse-type (car arg-stxs))
                   (build-arrow-type (cdr arg-stxs) stx))
             stx)]))

;; A variadic arrow `(-> fixed … rep ... result)` carries the literal
;; ellipsis token `...` immediately after the repeated argument type and
;; immediately before the (single) result type.  When `ty-stx` has that
;; shape, return two values: the desugared BINARY core type — the
;; repeated type wrapped as `(List rep)` in final argument position, so
;; `(-> A C ... R)` becomes `(-> A (-> (List C) R))` — and the count of
;; FIXED parameters preceding the rest-list.  Otherwise return
;; `(values #f #f)`, leaving ordinary arrows to `parse-type`.
(define (parse-variadic-arrow ty-stx)
  (syntax-parse ty-stx
    #:datum-literals (All => ->)
    ;; Peel a leading quantifier / qualifier and rewrap the desugared
    ;; core, so `(All (a) (-> a ... a))` and `((Ord a) => (-> a ... a))`
    ;; are recognized as variadic too.
    [(All (v:id ...) body)
     (define-values (core k) (parse-variadic-arrow #'body))
     (if core
         (values (ty:forall (map syntax->datum (syntax->list #'(v ...))) core ty-stx) k)
         (values #f #f))]
    [(c ...+ => body)
     (define-values (core k) (parse-variadic-arrow #'body))
     (if core
         (values (ty:qual (map parse-constraint (syntax->list #'(c ...))) core ty-stx) k)
         (values #f #f))]
    [(-> a ...+)
     (define args (syntax->list #'(a ...)))
     (define dots-positions
       (for/list ([x (in-list args)] [i (in-naturals)]
                  #:when (eq? (syntax->datum x) '...))
         i))
     (cond
       [(null? dots-positions) (values #f #f)]
       [(not (null? (cdr dots-positions)))
        (raise-syntax-error 'rackton
          "a variadic arrow type may contain only one `...`" ty-stx)]
       [else
        (define dots-idx (car dots-positions))
        (define len (length args))
        (unless (and (>= dots-idx 1) (= dots-idx (- len 2)))
          (raise-syntax-error 'rackton
            (string-append "`...` must follow the repeated argument type "
                           "and precede a single result type")
            ty-stx))
        (define k          (sub1 dots-idx))      ; number of fixed params
        (define fixed-tys  (map parse-type (take args k)))
        (define rep-ty     (parse-type (list-ref args (sub1 dots-idx))))
        (define res-ty     (parse-type (list-ref args (sub1 len))))
        (define list-ty    (ty:app (ty:con 'List ty-stx) (list rep-ty) ty-stx))
        (define core
          (foldr (lambda (a acc) (ty:app (ty:con '-> ty-stx) (list a acc) ty-stx))
                 res-ty
                 (append fixed-tys (list list-ty))))
        (values core k)])]
    [_ (values #f #f)]))

;; Parse a constraint expression like `(Eq a)` or `(Foo (Maybe a))`.
;; The head must be a non-lowercase identifier (a class name); every
;; argument is a plain type.  (Kind-annotated class parameters appear
;; only in a `protocol` head, which `parse-class-head` handles — not
;; through this entry point.)
(define (parse-constraint stx)
  (syntax-parse stx
    [(name:id arg ...+)
     #:fail-unless (constraint-head-id? (syntax->datum #'name))
     "protocol name in a constraint must begin with an uppercase letter"
     (constraint (syntax->datum #'name)
                 (for/list ([a (in-list (syntax->list #'(arg ...)))])
                   (parse-type a))
                 stx)]))

;; Parse a kind expression: `*` or `(-> k1 k2 …)`.  Like the type arrow,
;; the kind arrow is variadic in surface syntax and right-associates into
;; binary `k:arr` nodes in the core kind AST.
(define (parse-kind-stx stx)
  (syntax-parse stx
    #:datum-literals (* -> Nat)
    [* (k:star)]
    [Nat (k:nat)]
    [(-> k1 k2 ks ...)
     (build-arrow-kind (cons #'k1 (cons #'k2 (syntax->list #'(ks ...)))))]
    ;; An applied promoted kind constructor, e.g. `(List Ty)` — a
    ;; parameterised datatype promoted to the kind level (PolyKinds).
    [(head:id a0 a ...)
     #:fail-unless (uppercase-id? (syntax->datum #'head))
     "a promoted kind constructor must begin with an uppercase letter"
     (k:app (syntax->datum #'head)
            (map parse-kind-stx (syntax->list #'(a0 a ...))))]
    ;; A DataKinds-promoted datatype name used as a kind, e.g. `Stack`.
    [k:id
     #:fail-unless (uppercase-id? (syntax->datum #'k))
     "a kind is `*`, `Nat`, an arrow `(-> k …)`, a promoted datatype name (uppercase), or `(K k …)`"
     (k:con (syntax->datum #'k))]))

;; Parse one closed-type-family clause `[pat … = rhs]`: the patterns
;; before `=` are the per-parameter LHS types, the form after it the rhs.
(define (parse-tyfam-clause stx)
  (syntax-parse stx
    #:datum-literals (=)
    [(pat ... = rhs)
     (tyfam-clause (map parse-type (syntax->list #'(pat ...)))
                   (parse-type #'rhs)
                   stx)]))

;; Parse one constraint-family clause `[pat … = constraint …]`: the LHS
;; type patterns, then a (possibly empty) RHS constraint list.
(define (parse-cfam-clause stx)
  (syntax-parse stx
    #:datum-literals (=)
    [(pat ... = c ...)
     (cfam-clause (map parse-type (syntax->list #'(pat ...)))
                  (map parse-cfam-constraint (syntax->list #'(c ...)))
                  stx)]))

;; Like `parse-constraint`, but the head may be a lowercase parameter —
;; a higher-order constraint application `(c x)` whose head is bound to a
;; concrete class only when the family is reduced.
(define (parse-cfam-constraint stx)
  (syntax-parse stx
    [(name:id arg ...+)
     (constraint (syntax->datum #'name)
                 (for/list ([a (in-list (syntax->list #'(arg ...)))]) (parse-type a))
                 stx)]))

;; Right-fold a variadic `->` kind form into binary `k:arr` nodes.  Expects
;; at least two kind syntax objects (enforced by the caller's pattern).
(define (build-arrow-kind kind-stxs)
  (cond
    [(null? (cdr kind-stxs))
     (parse-kind-stx (car kind-stxs))]
    [else
     (k:arr (parse-kind-stx (car kind-stxs))
            (build-arrow-kind (cdr kind-stxs)))]))

;; ----- foreign-c helpers --------------------------------------------

;; Split a C signature datum `(cty ... -> cty)` into (values arg-tags
;; result-tag) on the single `->`.  `(-> int)` yields no args.
(define (split-c-sig parts stx)
  (unless (list? parts)
    (raise-syntax-error 'foreign-c ":sig must be a list (cty ... -> cty)" stx))
  (let loop ([xs parts] [args '()])
    (cond
      [(null? xs)
       (raise-syntax-error 'foreign-c ":sig must contain ->" stx)]
      [(eq? (car xs) '->)
       (define after (cdr xs))
       (unless (and (pair? after) (null? (cdr after)))
         (raise-syntax-error 'foreign-c
                             ":sig must have exactly one result type after ->" stx))
       (values (reverse args) (car after))]
      [else (loop (cdr xs) (cons (car xs) args))])))

;; Is the source type `t`, after peeling `n` argument arrows, headed by
;; IO?  Used to decide whether a foreign-c binding is a pure function or
;; an IO action.
(define (foreign-c-io-headed? t)
  (and (ty:app? t)
       (let ([h (ty:app-head t)])
         (and (ty:con? h) (eq? (ty:con-name h) 'IO)))))

(define (type-result-io? t n)
  (if (<= n 0)
      (foreign-c-io-headed? t)
      (and (ty:app? t)
           (let ([h (ty:app-head t)]
                 [args (ty:app-args t)])
             (and (ty:con? h) (eq? (ty:con-name h) '->) (= 2 (length args))
                  (type-result-io? (cadr args) (sub1 n)))))))

;; ----- top-level forms ----------------------------------------------

(define (parse-top stx)
  (syntax-parse stx
    #:datum-literals (define data newtype struct protocol instance define-alias define-constraint constraint-family define-effect require provide foreign foreign-c type-family type-instance data-family data-instance : => :: =)
    [(require spec ...)
     (top:require (syntax->list #'(spec ...)) stx)]

    ;; (type-family (F p …) [pat … = rhs] …)   — closed (ordered clauses)
    ;; (type-family (F p …))                    — open (extended below)
    ;; (type-family (F p …) :: kind …)          — optional kind annotation
    [(type-family (fname:id p:id ...) (~optional (~seq :: k)) clause ...)
     #:fail-unless (not (lowercase-id? (syntax->datum #'fname)))
     "type family name must be a non-lowercase identifier"
     (top:type-family (syntax->datum #'fname)
                      (map syntax->datum (syntax->list #'(p ...)))
                      (and (attribute k) (parse-kind-stx #'k))
                      (map parse-tyfam-clause (syntax->list #'(clause ...)))
                      stx)]
    ;; (type-instance (F T …) = U)             — an open-family equation
    [(type-instance (fname:id arg ...) = rhs)
     #:fail-unless (not (lowercase-id? (syntax->datum #'fname)))
     "type family name must be a non-lowercase identifier"
     (top:type-instance (syntax->datum #'fname)
                        (map parse-type (syntax->list #'(arg ...)))
                        (parse-type #'rhs)
                        stx)]

    ;; (data-family (F p …) [:: k])            — a data family declaration
    [(data-family (fname:id p:id ...) (~optional (~seq :: k)))
     #:fail-unless (not (lowercase-id? (syntax->datum #'fname)))
     "data family name must be a non-lowercase identifier"
     (top:data-family (syntax->datum #'fname)
                      (map syntax->datum (syntax->list #'(p ...)))
                      (and (attribute k) (parse-kind-stx #'k))
                      stx)]
    ;; (data-instance (F T …) ctor …)          — constructors for one instance
    [(data-instance (fname:id arg ...) ctor ...)
     #:fail-unless (not (lowercase-id? (syntax->datum #'fname)))
     "data family name must be a non-lowercase identifier"
     (top:data-instance (syntax->datum #'fname)
                        (map parse-type (syntax->list #'(arg ...)))
                        (map parse-data-ctor (syntax->list #'(ctor ...)))
                        stx)]

    ;; (foreign name τ :from M)            — racket-id = name
    ;; (foreign name τ :from M :as rkt-id) — renamed
    ;; M is a module path (collection id like racket/string, or a
    ;; relative "file.rkt" string).
    [(foreign name:id ty (~datum :from) mod (~datum :as) rkt:id)
     (top:foreign (syntax->datum #'name) (parse-type #'ty)
                  (syntax->datum #'mod) (syntax->datum #'rkt) stx)]
    [(foreign name:id ty (~datum :from) mod)
     (top:foreign (syntax->datum #'name) (parse-type #'ty)
                  (syntax->datum #'mod) (syntax->datum #'name) stx)]

    ;; (foreign-c name τ :lib L :symbol S :sig (cty ... -> cty))
    ;; Inline C-function import.  L is a string or #f; S a string; the
    ;; sig is C type keywords with a single `->` splitting args / result.
    [(foreign-c name:id ty (~datum :lib) lib (~datum :symbol) sym (~datum :sig) sig)
     (let*-values ([(t)             (parse-type #'ty)]
                   [(arg-tags res)  (split-c-sig (syntax->datum #'sig) #'sig)])
       (top:foreign-c (syntax->datum #'name) t
                      (syntax->datum #'lib) (syntax->datum #'sym)
                      arg-tags res
                      (type-result-io? t (length arg-tags))
                      stx))]

    [(provide spec ...)
     (top:provide (syntax->list #'(spec ...)) stx)]

    [(define-alias (aname:id aparam:id ...) target)
     #:fail-unless (not (lowercase-id? (syntax->datum #'aname)))
     "type alias name must be a non-lowercase identifier"
     (top:alias (syntax->datum #'aname)
                (map syntax->datum (syntax->list #'(aparam ...)))
                (parse-type #'target)
                stx)]
    [(define-alias aname:id target)
     #:fail-unless (not (lowercase-id? (syntax->datum #'aname)))
     "type alias name must be a non-lowercase identifier"
     (top:alias (syntax->datum #'aname) '() (parse-type #'target) stx)]

    ;; (define-constraint (C p …) constraint …) — a constraint synonym
    [(define-constraint (cname:id p:id ...) c ...+)
     #:fail-unless (not (lowercase-id? (syntax->datum #'cname)))
     "constraint synonym name must be a non-lowercase identifier"
     (top:constraint-syn (syntax->datum #'cname)
                         (map syntax->datum (syntax->list #'(p ...)))
                         (map parse-constraint (syntax->list #'(c ...)))
                         stx)]

    ;; (constraint-family (F p …) [pat … = constraint …] …)
    [(constraint-family (fname:id p:id ...) clause ...)
     #:fail-unless (not (lowercase-id? (syntax->datum #'fname)))
     "constraint family name must be a non-lowercase identifier"
     (top:constraint-fam (syntax->datum #'fname)
                         (map syntax->datum (syntax->list #'(p ...)))
                         (map parse-cfam-clause (syntax->list #'(clause ...)))
                         stx)]
    [(: name:id ty)
     ;; A `...` in the arrow makes this a variadic declaration: emit the
     ;; desugared binary core type as the dec, plus a `top:variadic`
     ;; marker recording the fixed-arg count.
     (define-values (vcore k) (parse-variadic-arrow #'ty))
     (if vcore
         (list (top:dec (syntax->datum #'name) vcore stx)
               (top:variadic (syntax->datum #'name) k stx))
         (top:dec (syntax->datum #'name) (parse-type #'ty) stx))]

    [(define (f:id pre ... . rest:id) body)
     ;; A dotted rest parameter: `(f a b . xs)` binds the gathered
     ;; trailing arguments as the list `xs`.  Desugar to an ordinary
     ;; final parameter (so codegen and inference see a plain function
     ;; whose last argument is a list) plus a `top:variadic` marker
     ;; recording the count of fixed parameters before the rest-list.
     (define fixed-stxs (syntax->list #'(pre ...)))
     (define params-stx
       (datum->syntax #'body (append fixed-stxs (list #'rest)) #'body))
     (define-values (names body-ast)
       (parse-fn-params+body params-stx #'body stx))
     (list (top:def (syntax->datum #'f) (e:lam names body-ast stx) stx)
           (top:variadic (syntax->datum #'f) (length fixed-stxs) stx))]

    [(define (f:id arg ...) body)
     ;; A 0-arg define `(define (f) body)` desugars to a
     ;; lambda with one ignored Unit parameter, matching the 0-arg
     ;; call-site convention.  Without this `(f)` would call a
     ;; non-function value.  Each `arg` may be a plain identifier
     ;; (the historical case) or any pattern; pattern parameters
     ;; desugar via `parse-fn-params+body` into a synthetic
     ;; identifier plus an irrefutable match in the body.
     (define arg-list (syntax->list #'(arg ...)))
     (cond
       [(null? arg-list)
        (top:def (syntax->datum #'f)
                 (e:lam '($unit-arg) (parse-expr #'body) stx)
                 stx)]
       [else
        (define-values (names body-ast)
          (parse-fn-params+body #'(arg ...) #'body stx))
        (top:def (syntax->datum #'f)
                 (e:lam names body-ast stx)
                 stx)])]
    [(define x:id e)
     (top:def (syntax->datum #'x) (parse-expr #'e) stx)]

    [(data (tname:id binder ...) item ...)
     #:fail-unless (not (lowercase-id? (syntax->datum #'tname)))
     "data type name must be a non-lowercase identifier"
     (define-values (tparams param-kinds)
       (parse-data-params (syntax->list #'(binder ...))))
     (parse-data-form (syntax->datum #'tname)
                      tparams
                      (syntax->list #'(item ...))
                      stx
                      #'tname
                      param-kinds)]
    [(data tname:id item ...)
     #:fail-unless (not (lowercase-id? (syntax->datum #'tname)))
     "data type name must be a non-lowercase identifier"
     (parse-data-form (syntax->datum #'tname)
                      '()
                      (syntax->list #'(item ...))
                      stx
                      #'tname)]

    ;; (newtype Name (Wrap T) [:deriving Cls ...])
    ;; (newtype (Name a ...) (Wrap T) [:deriving Cls ...])
    ;; Sugar over data for the common "one ctor, one field"
    ;; case.  A nominal wrapper around an existing type.  At runtime
    ;; the wrapper is a plain ADT — the "zero-cost" of a newtype is
    ;; documentary, not a perf optimization.  A trailing
    ;; `:deriving Cls ...` flows through to parse-data-form.
    [(newtype (tname:id binder ...) (ctor:id ftype) rest ...)
     #:fail-unless (not (lowercase-id? (syntax->datum #'tname)))
     "newtype name must be a non-lowercase identifier"
     #:fail-unless (not (lowercase-id? (syntax->datum #'ctor)))
     "newtype constructor name must be a non-lowercase identifier"
     #:fail-unless (newtype-rest-ok? #'(rest ...))
     "newtype must declare exactly one constructor with one field — for multiple ctors or multiple fields use data"
     (define-values (tparams param-kinds)
       (parse-data-params (syntax->list #'(binder ...))))
     (parse-data-form (syntax->datum #'tname)
                      tparams
                      (cons #'(ctor ftype) (syntax->list #'(rest ...)))
                      stx
                      #'tname
                      param-kinds)]
    [(newtype tname:id (ctor:id ftype) rest ...)
     #:fail-unless (not (lowercase-id? (syntax->datum #'tname)))
     "newtype name must be a non-lowercase identifier"
     #:fail-unless (not (lowercase-id? (syntax->datum #'ctor)))
     "newtype constructor name must be a non-lowercase identifier"
     #:fail-unless (newtype-rest-ok? #'(rest ...))
     "newtype must declare exactly one constructor with one field — for multiple ctors or multiple fields use data"
     (parse-data-form (syntax->datum #'tname)
                      '()
                      (cons #'(ctor ftype) (syntax->list #'(rest ...)))
                      stx
                      #'tname)]
    [(newtype _ _ ...)
     (raise-syntax-error
      'newtype
      "newtype must declare exactly one constructor with one field — for multiple ctors or multiple fields use data"
      stx)]

    ;; (struct (Name a b ...) [field : type] ...) and the bare
    ;; non-parameterised variant.  Desugars to a single-constructor
    ;; data plus one accessor function per field.
    [(struct (sname:id sparam:id ...) field ...)
     #:fail-unless (not (lowercase-id? (syntax->datum #'sname)))
     "struct name must be a non-lowercase identifier"
     (parse-struct-form (syntax->datum #'sname)
                        (map syntax->datum (syntax->list #'(sparam ...)))
                        (syntax->list #'(field ...))
                        stx
                        #'sname)]
    [(struct sname:id field ...)
     #:fail-unless (not (lowercase-id? (syntax->datum #'sname)))
     "struct name must be a non-lowercase identifier"
     (parse-struct-form (syntax->datum #'sname)
                        '()
                        (syntax->list #'(field ...))
                        stx
                        #'sname)]

    ;; A protocol: `(Name binder …)`, with superclasses expressed as
    ;; per-parameter `=>` bounds on the binders and/or `(:requires …)`
    ;; clauses in the body.  The prefix form `((Super x) … => (Name …))`
    ;; has been retired.
    [(protocol head body ...)
     (define-values (head-constraint bound-supers)
       (parse-class-head #'head))
     (define-values (req-supers methods)
       (parse-class-body (syntax->list #'(body ...))))
     (top:class (append bound-supers req-supers)
                head-constraint
                methods
                stx)]

    ;; Opt-in cross-class derivation.  `:derive-supers` directly
    ;; after the head bundles the irreducible primitives; the compiler
    ;; synthesizes the missing superclass instances.  These two clauses
    ;; must precede the plain instance clauses so the keyword is consumed
    ;; here rather than captured as the first body item.
    [(instance (ctx ...+ => head) (~datum :derive-supers) body ...)
     (top:derive-instance (for/list ([c (in-list (syntax->list #'(ctx ...)))])
                            (parse-constraint c))
                          (parse-constraint #'head)
                          (for/list ([m (in-list (syntax->list #'(body ...)))])
                            (parse-instance-method m))
                          stx)]
    [(instance head (~datum :derive-supers) body ...)
     (top:derive-instance '()
                          (parse-constraint #'head)
                          (for/list ([m (in-list (syntax->list #'(body ...)))])
                            (parse-instance-method m))
                          stx)]

    ;; Instance with context: ((Eq a) ... => (Eq (Maybe a)))
    [(instance (ctx ...+ => head) body ...)
     (top:instance (for/list ([c (in-list (syntax->list #'(ctx ...)))])
                     (parse-constraint c))
                   (parse-constraint #'head)
                   (for/list ([m (in-list (syntax->list #'(body ...)))])
                     (parse-instance-method m))
                   stx)]
    [(instance head body ...)
     (top:instance '()
                   (parse-constraint #'head)
                   (for/list ([m (in-list (syntax->list #'(body ...)))])
                     (parse-instance-method m))
                   stx)]

    ;; (define-effect Name (op argType ... -> resultType) ...)
    [(define-effect name:id op ...)
     #:fail-unless (not (lowercase-id? (syntax->datum #'name)))
     "effect name must be a non-lowercase identifier"
     (top:effect (syntax->datum #'name)
                 (for/list ([o (in-list (syntax->list #'(op ...)))])
                   (parse-effect-op o))
                 stx)]))

(define (parse-effect-op stx)
  (syntax-parse stx
    #:datum-literals (->)
    [(name:id arg ... -> result)
     (effect-op (syntax->datum #'name)
                (for/list ([a (in-list (syntax->list #'(arg ...)))])
                  (parse-type a))
                (parse-type #'result)
                stx)]))

;; ----- protocol head -----------------------------------------------
;;
;; A `protocol` head is `(Name binder …)`.  Each binder is one of:
;;   v            — a plain parameter (kind inferred / defaults to *)
;;   [v :: k]     — an explicit kind annotation, no superclass
;;   [v => B …]   — one or more superclass BOUNDS on `v`
;; A bound `B` is a class head missing its final argument; `v` supplies
;; it.  So `[v => Functor]` ⇒ `(Functor v)` and `[v => (Pairing f)]` ⇒
;; `(Pairing f v)`.  The head desugars to a plain class constraint plus
;; a list of superclass constraints, matching the representation the old
;; prefix form `((Super x) … => (Name …))` produced.

;; Desugar one bound entry `B` for parameter `var` into a superclass
;; constraint, appending `var` as the final argument.
(define (bound->constraint stx var)
  (syntax-parse stx
    [c:id
     #:fail-unless (uppercase-id? (syntax->datum #'c))
     "superprotocol in a bound must begin with an uppercase letter (did you mean :: for a kind?)"
     (constraint (syntax->datum #'c) (list var) stx)]
    [(c:id arg ...+)
     #:fail-unless (uppercase-id? (syntax->datum #'c))
     "superprotocol head in a bound must begin with an uppercase letter (did you mean :: for a kind?)"
     (constraint (syntax->datum #'c)
                 (append (for/list ([a (in-list (syntax->list #'(arg ...)))])
                           (parse-type a))
                         (list var))
                 stx)]))

;; Parse one head binder.  Returns (values head-tyvar bound-constraints).
;; The head-tyvar carries an explicit kind on its stx as 'rackton:kind
;; when written `[v :: k]`; otherwise the kind is left to inference.
(define (parse-class-param stx)
  (syntax-parse stx
    #:datum-literals (:: =>)
    [v:id
     #:fail-unless (lowercase-id? (syntax->datum #'v))
     "protocol parameter must be a lowercase identifier"
     (values (ty:var (syntax->datum #'v) #'v) '())]
    [(v:id :: k)
     #:fail-unless (lowercase-id? (syntax->datum #'v))
     "protocol parameter must be a lowercase identifier"
     (values (ty:var (syntax->datum #'v)
                     (syntax-property #'v 'rackton:kind (parse-kind-stx #'k)))
             '())]
    [(v:id => b ...+)
     #:fail-unless (lowercase-id? (syntax->datum #'v))
     "protocol parameter must be a lowercase identifier"
     (define var (ty:var (syntax->datum #'v) #'v))
     (values var
             (for/list ([b (in-list (syntax->list #'(b ...)))])
               (bound->constraint b var)))]))

;; Parse a protocol head `(Name binder …)` into (values head-constraint
;; bound-supers), where bound-supers are the superclass constraints
;; contributed by `=>` bounds on the binders.
(define (parse-class-head stx)
  (syntax-parse stx
    [(name:id binder ...+)
     #:fail-unless (uppercase-id? (syntax->datum #'name))
     "protocol name must begin with an uppercase letter"
     (define-values (vars super-lists)
       (for/lists (vs ss)
                  ([b (in-list (syntax->list #'(binder ...)))])
         (parse-class-param b)))
     (values (constraint (syntax->datum #'name) vars stx)
             (append* super-lists))]))

;; Split a protocol body into superclass constraints contributed by
;; `(:requires c …)` clauses and the remaining method items.  Keyword
;; clauses self-quote in syntax-parse, like `:fundep` / `:type`.
;;
;; A bare `:derive` keyword is special: it consumes the *following* item,
;; a parenthesized list of `[Super (define …) …]` derivation clauses, and
;; turns each into a `class-super-derive` node kept inside the methods list
;; (the class handler in infer.rkt separates them from real methods).
(define (parse-class-body items)
  (let loop ([items items] [supers '()] [methods '()])
    (cond
      [(null? items) (values (reverse supers) (reverse methods))]
      [(derive-keyword? (car items))
       (when (null? (cdr items))
         (raise-syntax-error
          #f ":derive must be followed by a list of [Super (define …) …] clauses"
          (car items)))
       (loop (cddr items)
             supers
             (append (reverse (parse-derive-list (cadr items))) methods))]
      [(laws-keyword? (car items))
       (when (null? (cdr items))
         (raise-syntax-error
          #f ":laws must be followed by a list of [name (All …)] clauses"
          (car items)))
       (loop (cddr items)
             supers
             (append (reverse (parse-law-list (cadr items))) methods))]
      [else
       (define item (car items))
       (syntax-parse item
         [((~datum :requires) c ...+)
          (loop (cdr items)
                (append (reverse (for/list ([cs (in-list (syntax->list #'(c ...)))])
                                   (parse-constraint cs)))
                        supers)
                methods)]
         [_ (loop (cdr items) supers (cons (parse-class-method item) methods))])])))

;; Is `stx` the bare `:laws` keyword?
(define (laws-keyword? stx)
  (eq? (syntax-e stx) ':laws))

;; Parse the parenthesized list following a bare `:laws` keyword: one or
;; more `[name (All …)]` clauses, each producing a `class-law`.
(define (parse-law-list stx)
  (syntax-parse stx
    [(clause ...+)
     (for/list ([c (in-list (syntax->list #'(clause ...)))])
       (parse-class-law c))]
    [_ (raise-syntax-error
        #f ":laws expects a non-empty list of [name (All …)] clauses" stx)]))

;; Parse one law clause from a `:laws` list into a `class-law`.  A law
;; is `[law-name (All ([v : t] …) body)]`, optionally prefixed with a
;; `=>` constraint context assumed only while checking the law:
;; `[law-name ((Eq a) … => (All …))]`.  The context lets the equation
;; compare results (via the assumed `Eq`) without that `Eq` becoming a
;; superprotocol requirement on instances.
(define (parse-class-law stx)
  (syntax-parse stx
    #:datum-literals (=>)
    [(name:id (ctx ...+ => quantified))
     (parse-law-quantified (syntax->datum #'name)
                           (for/list ([c (in-list (syntax->list #'(ctx ...)))])
                             (parse-constraint c))
                           #'quantified
                           stx)]
    [(name:id quantified)
     (parse-law-quantified (syntax->datum #'name) '() #'quantified stx)]
    [_ (raise-syntax-error
        #f "a law must have the shape [name (All ([v : t] …) body)] or [name (ctx … => (All …))]"
        stx)]))

;; Parse the `(All ([v : t] …) body)` part of a law, given the already
;; parsed name and `=>` context.  The quantifier may be written `All` or
;; the mathematical `∀`; both bind per-binder–annotated variables over
;; the Boolean-typed body (checked later in inference).
(define (parse-law-quantified name context q-stx stx)
  (syntax-parse q-stx
    #:datum-literals (All ∀)
    [(q (binder ...) body)
     #:fail-unless (memq (syntax-e #'q) '(All ∀))
     "a law must be quantified with `All` or `∀`"
     #:fail-when (null? (syntax->list #'(binder ...)))
     "a law must bind at least one variable"
     (class-law name
                context
                (for/list ([b (in-list (syntax->list #'(binder ...)))])
                  (parse-law-binder b))
                (parse-expr #'body)
                stx)]
    [_ (raise-syntax-error
        #f "a law body must be quantified as (All ([v : t] …) body)" q-stx)]))

;; Parse one quantifier binder `[v : t]` into a `law-binder`.  The
;; annotation is mandatory: a law quantifies over an explicitly typed
;; domain so the equation can be checked without inferring the binder's
;; type from its uses.
(define (parse-law-binder stx)
  (syntax-parse stx
    #:datum-literals (:)
    [(v:id : t)
     #:fail-unless (lowercase-id? (syntax->datum #'v))
     "a law binder must be a lowercase identifier"
     (law-binder (syntax->datum #'v) (parse-type #'t) stx)]
    [_ (raise-syntax-error
        #f "a law binder must be annotated as [variable : type]" stx)]))

;; Is `stx` the bare `:derive` keyword?
(define (derive-keyword? stx)
  (eq? (syntax-e stx) ':derive))

;; Parse the list following a bare `:derive` keyword: one or more
;; `[Super (define …) …]` clauses, each producing a `class-super-derive`.
(define (parse-derive-list stx)
  (syntax-parse stx
    [(clause ...+)
     (for/list ([c (in-list (syntax->list #'(clause ...)))])
       (parse-derive-clause c))]
    [_ (raise-syntax-error
        #f ":derive expects a non-empty list of [Super (define …) …] clauses"
        stx)]))

;; A single `[Super (define …) …]` derivation clause: canonical bodies that
;; fill superclass `Super`'s methods in terms of this class's own methods.
(define (parse-derive-clause stx)
  (syntax-parse stx
    [(super:id m ...+)
     #:fail-unless (uppercase-id? (syntax->datum #'super))
     "superprotocol in :derive must begin with an uppercase letter"
     (class-super-derive
      (syntax->datum #'super)
      (for/list ([md (in-list (syntax->list #'(m ...)))])
        (parse-class-method md))
      stx)]))

;; A method form inside `protocol`: either a `(: name type)` signature,
;; a `(define ...)` providing a default implementation, or a functional
;; dependency `(:fundep lhs … -> rhs …)`.
(define (parse-class-method stx)
  (syntax-parse stx
    #:datum-literals (: define ->)
    [(: name:id ty)
     (method-sig (syntax->datum #'name) (parse-type #'ty) stx)]
    [(define (f:id arg ...) body)
     (define arg-list (syntax->list #'(arg ...)))
     (cond
       [(null? arg-list)
        (method-default (syntax->datum #'f)
                        (e:lam '($unit-arg) (parse-expr #'body) stx)
                        stx)]
       [else
        (define-values (names body-ast)
          (parse-fn-params+body #'(arg ...) #'body stx))
        (method-default (syntax->datum #'f)
                        (e:lam names body-ast stx)
                        stx)])]
    [(define x:id e)
     (method-default (syntax->datum #'x) (parse-expr #'e) stx)]
    [((~datum :fundep) lhs:id ...+ -> rhs:id ...+)
     (class-fundep (map syntax->datum (syntax->list #'(lhs ...)))
                   (map syntax->datum (syntax->list #'(rhs ...)))
                   stx)]
    [((~datum :type) name:id)
     #:fail-unless (not (lowercase-id? (syntax->datum #'name)))
     "associated-type name must be a non-lowercase identifier"
     (class-type-fam (syntax->datum #'name) stx)]))

;; An instance method must be a `define`, or a `:type` binding for
;; an associated type declared by the class.
(define (parse-instance-method stx)
  (syntax-parse stx
    #:datum-literals (define =)
    [((~datum :type) (name:id = ty))
     #:fail-unless (not (lowercase-id? (syntax->datum #'name)))
     "associated-type name must be a non-lowercase identifier"
     (inst-type-fam (syntax->datum #'name) (parse-type #'ty) stx)]
    [(define (f:id arg ...) body)
     (define arg-list (syntax->list #'(arg ...)))
     (cond
       [(null? arg-list)
        (top:def (syntax->datum #'f)
                 (e:lam '($unit-arg) (parse-expr #'body) stx)
                 stx)]
       [else
        (define-values (names body-ast)
          (parse-fn-params+body #'(arg ...) #'body stx))
        (top:def (syntax->datum #'f)
                 (e:lam names body-ast stx)
                 stx)])]
    [(define x:id e)
     (top:def (syntax->datum #'x) (parse-expr #'e) stx)]))

;; Split a data body into constructor specs and an optional
;; `:deriving Cls ...` tail.  Returns a list of top-level forms:
;; the top:data and any synthesized top:instance entries.
;;
;; For the synthesized instances we use the syntax handle of the *first
;; constructor spec* as the lexical-context anchor — that handle is an
;; actual identifier from the user's source and so carries the same
;; scope set as anything else the user wrote.  Using the whole-form's
;; syntax instead leaves the synthesised identifiers missing scopes
;; that show up only on individual identifier-leaf syntax objects.
;; Parse one `data` / `newtype` head parameter.  Returns (values name
;; surface-kind-or-#f): a bare `v` leaves the kind to inference, while
;; `(v :: k)` states it explicitly (mirroring `protocol`'s `[v :: k]`).
(define (parse-data-param stx)
  (syntax-parse stx
    #:datum-literals (::)
    [v:id (values (syntax->datum #'v) #f)]
    [(v:id :: k)
     #:fail-unless (lowercase-id? (syntax->datum #'v))
     "a data type parameter must be a lowercase identifier"
     (values (syntax->datum #'v) (parse-kind-stx #'k))]))

;; Parse a head's parameter binders into (values names kinds-alist),
;; where `kinds-alist` carries only the explicitly-annotated params
;; (name . surface-kind) — empty when none are annotated.
(define (parse-data-params binder-stxs)
  (define-values (names kinds)
    (for/lists (ns ks) ([b (in-list binder-stxs)]) (parse-data-param b)))
  (values names
          (for/list ([n (in-list names)] [k (in-list kinds)] #:when k)
            (cons n k))))

;; Enforce that, across a data type's constructors, field naming is
;; uniform: every constructor that has fields either names all of them
;; or none of them.  Nullary constructors carry no fields and are skipped.
(define (check-uniform-field-naming ctors stx)
  (define fielded
    (filter (lambda (c) (pair? (data-ctor-field-types c))) ctors))
  (define named   (filter data-ctor-field-names fielded))
  (define unnamed (filter (lambda (c) (not (data-ctor-field-names c))) fielded))
  (when (and (pair? named) (pair? unnamed))
    (raise-syntax-error
     'data
     "a data type's constructors must either all name their fields or all be positional"
     stx)))

(define (parse-data-form tname tparams items stx
                         [tname-stx #f] [param-kinds '()])
  ;; Peel off the `:abstract` flag (it may appear
  ;; alongside `:deriving` in any order before the ctor list ends).
  (define-values (items-1 abstract?) (split-abstract items))
  (define-values (items-2 runtime-tag) (split-runtime-tag items-1))
  (define-values (ctor-stxs deriving-classes)
    (split-deriving items-2))
  (define ctors
    (for/list ([c (in-list ctor-stxs)]) (parse-data-ctor c)))
  ;; A data type's fielded constructors must agree: either every
  ;; constructor with fields names them, or none does.  Nullary ctors
  ;; (no fields) are exempt and need no names.
  (check-uniform-field-naming ctors stx)
  ;; Stash any explicit param-kind annotations on the form's stx (the
  ;; same channel `protocol` uses for `'rackton:kind`); the kind checker
  ;; reads them to seed the declared kinds.
  (define stx*
    (if (null? param-kinds)
        stx
        (syntax-property stx 'rackton:data-param-kinds param-kinds)))
  (define data-form (top:data tname tparams ctors stx* abstract? runtime-tag))
  (cond
    [(null? deriving-classes) data-form]
    [else
     (define ctx-stx
       (cond
         [tname-stx tname-stx]
         [(pair? ctor-stxs) (car ctor-stxs)]
         [else stx]))
     (cons data-form
           (synthesize-deriving deriving-classes
                                tname tparams ctors ctx-stx stx
                                'data))]))

;; A newtype's `rest` after `(ctor ftype)` is permitted only if it
;; is either empty or starts with the `:deriving` keyword.  Reject
;; anything else (extra ctor specs, stray items) at parse time.
(define (newtype-rest-ok? rest-stx)
  (define rest (syntax->list rest-stx))
  (or (null? rest)
      (eq? (syntax->datum (car rest)) ':deriving)))

;; Peel off a `:abstract` flag anywhere it appears in a
;; data/struct body items list.  Returns (values rest abstract?)
;; where abstract? is #t iff the keyword was found.
(define (split-abstract items)
  (let loop ([rem items] [acc '()] [abs? #f])
    (cond
      [(null? rem) (values (reverse acc) abs?)]
      [(eq? (syntax->datum (car rem)) ':abstract)
       (loop (cdr rem) acc #t)]
      [else (loop (cdr rem) (cons (car rem) acc) abs?)])))

;; Peel off a `:runtime-tag tag` pair from a data body items list.
;; Returns (values rest tag-symbol-or-#f).  The tag names the dispatch
;; tag the type's opaque runtime values carry (see tcon-info runtime-tag
;; in env.rkt); used for foreign-backed opaque types with instances.
(define (split-runtime-tag items)
  (let loop ([rem items] [acc '()])
    (cond
      [(null? rem) (values (reverse acc) #f)]
      [(eq? (syntax->datum (car rem)) ':runtime-tag)
       (when (null? (cdr rem))
         (raise-syntax-error 'data ":runtime-tag must be followed by a tag"
                             (car rem)))
       (values (append (reverse acc) (cddr rem))
               (syntax->datum (cadr rem)))]
      [else (loop (cdr rem) (cons (car rem) acc))])))

;; Split the trailing `:deriving Cls ...` clause off a list of body
;; items.  Returns (values items-before deriving-classes).  Shared
;; by data, newtype, and struct so all three
;; honor the same deriving menu.
(define (split-deriving items)
  (let loop ([rem items] [acc '()])
    (cond
      [(null? rem) (values (reverse acc) '())]
      [(eq? (syntax->datum (car rem)) ':deriving)
       (values (reverse acc)
               (for/list ([c (in-list (cdr rem))])
                 (syntax->datum c)))]
      [else (loop (cdr rem) (cons (car rem) acc))])))

;; Synthesize the per-class instance forms for a data type's
;; deriving list.  `kind-tag` is the surface keyword raising the
;; error so messages stay accurate ('data / 'struct).
(define (synthesize-deriving classes tname tparams ctors ctx-stx err-stx kind-tag)
  ;; Deriving Ord implies deriving Eq (Ord has Eq as a superclass
  ;; and our `<` calls `==`); same for Foldable's required Functor
  ;; superclass when the user asks for Foldable without Functor.
  (define classes-needing-eq
    (cond
      [(and (member 'Ord classes) (not (member 'Eq classes)))
       (cons 'Eq classes)]
      [else classes]))
  ;; Deriving Monoid implies deriving Semigroup since
  ;; Monoid's class declaration lists Semigroup as a superclass.
  (define classes-needing-semigroup
    (cond
      [(and (member 'Monoid classes-needing-eq)
            (not (member 'Semigroup classes-needing-eq)))
       (cons 'Semigroup classes-needing-eq)]
      [else classes-needing-eq]))
  (apply append
         (for/list ([cls (in-list classes-needing-semigroup)])
           (case cls
             [(Eq)   (list (synthesize-eq-instance      tname tparams ctors ctx-stx))]
             [(Show) (list (synthesize-show-instance    tname tparams ctors ctx-stx))]
             [(Ord)  (list (synthesize-ord-instance     tname tparams ctors ctx-stx))]
             [(Functor)
              (cond
                [(null? tparams)
                 (raise-syntax-error kind-tag
                   "cannot derive Functor for a type with no type parameters"
                   err-stx)]
                [else
                 (list (synthesize-functor-instance tname tparams ctors ctx-stx))])]
             [(Foldable)
              (cond
                [(null? tparams)
                 (raise-syntax-error kind-tag
                   "cannot derive Foldable for a type with no type parameters"
                   err-stx)]
                [else
                 (list (synthesize-foldable-instance tname tparams ctors ctx-stx))])]
             [(Traversable)
              (list (synthesize-traversable-instance tname tparams ctors ctx-stx))]
             [(Bifunctor)
              (list (synthesize-bifunctor-instance tname tparams ctors ctx-stx))]
             [(Semigroup)
              (list (synthesize-semigroup-instance tname tparams ctors ctx-stx))]
             [(Monoid)
              (list (synthesize-monoid-instance tname tparams ctors ctx-stx))]
             [(Prism)
              (cond
                [(eq? kind-tag 'struct)
                 (raise-syntax-error kind-tag
                   "cannot derive Prism for struct (single-ctor record) — use Lens instead"
                   err-stx)]
                [else
                 (synthesize-prism-defs tname tparams ctors ctx-stx)])]
             [else
              (raise-syntax-error kind-tag
                (format "cannot derive ~s — supported: Eq, Ord, Show, Functor, Foldable, Traversable, Bifunctor, Semigroup, Monoid, Prism" cls)
                err-stx)]))))

;; ----- records: struct ---------------------------------------

(define (parse-struct-form name tparams field-stxs stx tname-stx)
  ;; Split off a trailing `:deriving Cls ...` clause before
  ;; parsing field specs — the deriving classes are routed through the
  ;; shared synthesize-deriving helper.
  ;; Also peel off `:abstract` from anywhere in the body.
  (define-values (field-stxs-1 abstract?) (split-abstract field-stxs))
  (define-values (field-only-stxs deriving-classes)
    (split-deriving field-stxs-1))
  (define field-pairs
    (for/list ([fs (in-list field-only-stxs)]) (parse-field-spec fs)))
  (define field-names (map car field-pairs))
  (define field-types (map cdr field-pairs))
  ;; A struct's fields are always named, so its constructor carries the
  ;; names — enabling keyword construction `(S :f v …)` while positional
  ;; `(S v …)` keeps working.
  (define ctor (data-ctor-named name field-pairs stx))
  (define data-form (top:data name tparams (list ctor) stx abstract? #f))
  (define accessor-defs
    (for/list ([fname (in-list field-names)]
               [i (in-naturals)])
      (synthesize-accessor name (length field-names) fname i tname-stx)))
  ;; Lens-deriving needs field-NAMES (to name the lenses
  ;; and generate the accessor / re-builder), which the generic
  ;; synthesize-deriving doesn't have.  Peel `Lens` out and handle
  ;; it here; pass the rest through to synthesize-deriving normally.
  (define-values (lens? other-deriving-classes)
    (partition-by-eq 'Lens deriving-classes))
  (define lens-defs
    (cond
      [lens? (synthesize-lens-defs name tparams field-names tname-stx)]
      [else '()]))
  (define derived
    (cond
      [(null? other-deriving-classes) '()]
      [else
       (synthesize-deriving other-deriving-classes
                            name tparams (list ctor)
                            tname-stx stx 'struct)]))
  (append (list data-form
                (top:struct-fields name field-names stx))
          accessor-defs derived lens-defs))

;; Peel a class symbol out of the deriving list.  Returns
;; (values present? rest).
(define (partition-by-eq sym xs)
  (cond
    [(member sym xs) (values #t (filter (lambda (x) (not (eq? x sym))) xs))]
    [else            (values #f xs)]))

;; Emit per-field lens defs `Tname-fname-lens` for a
;; single-ctor struct.  Each lens reuses the existing
;; accessor `Tname-fname` as the getter and rebuilds the struct
;; with `(Tname ...)` for the setter.
(define (synthesize-lens-defs tname tparams field-names ctx-stx)
  (define arity (length field-names))
  (for/list ([fname (in-list field-names)] [idx (in-naturals)])
    (define lens-name
      (string->symbol (format "~a-~a-lens" tname fname)))
    (define accessor-name
      (string->symbol (format "~a-~a" tname fname)))
    ;; Getter:  (lambda (p) (Tname-fname p))
    (define getter
      (e:lam '(p)
             (e:app (e:var accessor-name (fresh-stx ctx-stx))
                    (list (e:var 'p (fresh-stx ctx-stx)))
                    (fresh-stx ctx-stx))
             (fresh-stx ctx-stx)))
    ;; Setter:  (lambda (p) (lambda (v) (Tname f0 ... v ... fn)))
    ;; where f_j = (Tname-f_j p) for j != idx, and v at slot idx.
    (define ctor-args
      (for/list ([f-other (in-list field-names)] [j (in-naturals)])
        (cond
          [(= j idx)
           (e:var 'v (fresh-stx ctx-stx))]
          [else
           (define other-accessor
             (string->symbol (format "~a-~a" tname f-other)))
           (e:app (e:var other-accessor (fresh-stx ctx-stx))
                  (list (e:var 'p (fresh-stx ctx-stx)))
                  (fresh-stx ctx-stx))])))
    (define setter
      (e:lam '(p)
             (e:lam '(v)
                    (e:app (e:var tname (fresh-stx ctx-stx))
                           ctor-args
                           (fresh-stx ctx-stx))
                    (fresh-stx ctx-stx))
             (fresh-stx ctx-stx)))
    (define lens-body
      (e:app (e:var 'Lens (fresh-stx ctx-stx))
             (list getter setter)
             (fresh-stx ctx-stx)))
    (top:def lens-name lens-body ctx-stx)))

(define (parse-field-spec stx)
  (syntax-parse stx
    #:datum-literals (:)
    [(fname:id : t)
     #:fail-unless (lowercase-id? (syntax->datum #'fname))
     "struct field name must be a lowercase identifier"
     (cons (syntax->datum #'fname) (parse-type #'t))]))

;; Build `(define (Name-fname r) (match r [(Name _ … v … _) v]))`.
(define (synthesize-accessor struct-name arity field-name idx ctx-stx)
  (define accessor-name
    (string->symbol (format "~a-~a" struct-name field-name)))
  (define pat-vars
    (for/list ([j (in-range arity)])
      (if (= j idx) (p:var 'v ctx-stx) (p:wild ctx-stx))))
  (define pat (p:ctor struct-name pat-vars ctx-stx))
  (define body
    (e:match (e:var 'r ctx-stx)
             (list (clause pat #f (e:var 'v ctx-stx) ctx-stx))
             #f ctx-stx))
  (top:def accessor-name (e:lam '(r) body ctx-stx) ctx-stx))

;; Split a GADT constructor's `: SIG` type into (values field-stxs result-stx).
;; An arrow `(-> a₁ … aₙ)` with n ≥ 2 yields fields a₁…aₙ₋₁ and result aₙ.
;; A single-element arrow `(-> r)` (a 0-arg function) and any non-arrow type
;; both yield no fields and result = the type itself (a nullary constructor).
(define (split-ctor-signature sig)
  (syntax-parse sig
    #:datum-literals (->)
    [(-> a ...+)
     (define args (syntax->list #'(a ...)))
     (cond
       [(null? (cdr args)) (values '() (car args))]
       [else
        (define rev (reverse args))
        (values (reverse (cdr rev)) (car rev))])]
    [_ (values '() sig)]))

(define (parse-data-ctor stx)
  (syntax-parse stx
    [name:id
     #:fail-unless (not (lowercase-id? (syntax->datum #'name)))
     "data constructor name must be a non-lowercase identifier"
     (data-ctor-plain (syntax->datum #'name) '() stx)]
    ;; Existential ctor with :forall and :where clauses.
    [(name:id (~datum :forall) (tv:id ...+)
              (~datum :where) ctx ...
              ft ...+)
     #:fail-unless (not (lowercase-id? (syntax->datum #'name)))
     "data constructor name must be a non-lowercase identifier"
     (data-ctor (syntax->datum #'name)
                (for/list ([t (in-list (syntax->list #'(ft ...)))])
                  (parse-type t))
                stx
                (map syntax->datum (syntax->list #'(tv ...)))
                (for/list ([c (in-list (syntax->list #'(ctx ...)))])
                  (parse-constraint c))
                #f
                #f)]
    ;; GADT ctor with `: SIG` clause giving the ctor's full type
    ;; signature.  When SIG is an arrow `(-> ft … RT)` the leading
    ;; types are the fields and the final type is the refined result;
    ;; a non-arrow SIG is a nullary ctor whose result type is SIG.
    [(name:id (~datum :) sig)
     #:fail-unless (not (lowercase-id? (syntax->datum #'name)))
     "data constructor name must be a non-lowercase identifier"
     (define-values (field-stxs result-stx) (split-ctor-signature #'sig))
     (data-ctor (syntax->datum #'name)
                (for/list ([t (in-list field-stxs)]) (parse-type t))
                stx
                '()
                '()
                (parse-type result-stx)
                #f)]
    ;; Positional `(C t …)` or named `(C [f : t] …)` fields.  Fields
    ;; must be uniformly named or uniformly positional within a ctor.
    [(name:id ft ...+)
     #:fail-unless (not (lowercase-id? (syntax->datum #'name)))
     "data constructor name must be a non-lowercase identifier"
     (parse-ctor-fields (syntax->datum #'name)
                        (syntax->list #'(ft ...)) stx)]))

;; Classify one ctor field syntax: a named field `[f : t]` yields
;; (cons name-sym type-ast); a positional field `t` yields
;; (cons #f type-ast).  A field-spec is recognised by the `id : type`
;; shape with a lowercase field name (which no type expression has).
(define (parse-ctor-field f)
  (syntax-parse f
    #:datum-literals (:)
    [(fname:id : t)
     #:when (lowercase-id? (syntax->datum #'fname))
     (cons (syntax->datum #'fname) (parse-type #'t))]
    [_ (cons #f (parse-type f))]))

;; Build a data-ctor from its field syntaxes, enforcing the all-named /
;; all-positional rule within the constructor.
(define (parse-ctor-fields name field-stxs stx)
  (define parsed (map parse-ctor-field field-stxs))
  (define names (map car parsed))
  (cond
    [(andmap values names)
     (data-ctor-named name parsed stx)]
    [(andmap not names)
     (data-ctor-plain name (map cdr parsed) stx)]
    [else
     (raise-syntax-error
      'data
      "constructor fields must be either all named `[f : t]` or all positional"
      stx)]))

(define (parse-toplevel-list stx-or-list)
  (define forms
    (cond
      [(syntax? stx-or-list) (syntax->list stx-or-list)]
      [(list? stx-or-list)   stx-or-list]
      [else (raise-argument-error 'parse-toplevel-list
                                  "syntax or list" stx-or-list)]))
  (define record (make-hasheq))
  (define raw
    (parameterize ([current-fn-clauses-record record])
      ;; A single surface form may parse to multiple AST entries (e.g.
      ;; `(data … :deriving Eq Show)` desugars to the data plus
      ;; the two synthesized instances).  Flatten if so.
      (apply append
             (for/list ([f (in-list forms)])
               (define result (parse-top f))
               (if (list? result) result (list result))))))
  (combine-multi-clause-defs raw record))

;; Walk the parsed top-form list, then descend into every nested
;; method list (instances, derived instances, protocol defaults, and
;; `:derive` superclass blocks) and combine multi-clause function
;; definitions there too.  A `define` is a `define` wherever it sits:
;; equational, Haskell-style definitions work in any of these scopes,
;; not just at the top level.
(define (combine-multi-clause-defs parsed record)
  (map (lambda (f) (combine-in-form f record))
       (combine-clause-list parsed record
                            top:def? top:def-name top:def-expr top:def-stx
                            top:def)))

;; Recurse into a single combined top-form, combining the multi-clause
;; definitions held in any method lists it carries.  Method lists hold
;; `top:def`s (instances) or `method-default`s (protocol defaults);
;; `:derive` clauses (`class-super-derive`) nest a further default
;; list.  Non-method forms pass through untouched.
(define (combine-in-form f record)
  (cond
    [(top:instance? f)
     (struct-copy top:instance f
       [methods (combine-method-defs (top:instance-methods f) record)])]
    [(top:derive-instance? f)
     (struct-copy top:derive-instance f
       [methods (combine-method-defs (top:derive-instance-methods f) record)])]
    [(top:class? f)
     (struct-copy top:class f
       [methods (combine-class-methods (top:class-methods f) record)])]
    [else f]))

;; Instance / derived-instance method lists carry method bodies as
;; `top:def`s (`inst-type-fam` entries pass through as non-clause forms).
(define (combine-method-defs methods record)
  (combine-clause-list methods record
                       top:def? top:def-name top:def-expr top:def-stx
                       top:def))

;; Protocol method lists mix signatures, fundeps, associated types,
;; `class-super-derive` blocks, and `method-default`s.  Combine the
;; defaults at this level, then recurse into each `:derive` block's
;; own default list.
(define (combine-class-methods methods record)
  (define combined
    (combine-clause-list methods record
                         method-default? method-default-name
                         method-default-expr method-default-stx
                         method-default))
  (for/list ([m (in-list combined)])
    (cond
      [(class-super-derive? m)
       (struct-copy class-super-derive m
         [methods (combine-class-methods (class-super-derive-methods m) record)])]
      [else m])))

;; Group consecutive same-named function-clause forms in `forms` into a
;; single combined form.  A form participates iff `clause?` holds AND its
;; stx was recorded as a function-form `(define (f …) …)` in `record`;
;; value-form definitions (`(define x e)`) and non-definitions pass
;; through.  `get-name` / `get-expr` / `get-stx` read the participating
;; struct; `make` rebuilds it from `(name lam-expr stx)`.  Singletons are
;; returned unchanged (already desugared by `parse-top`); a name with
;; multiple clauses becomes one form whose body is an `e:match*` over
;; fresh argument identifiers, each clause carrying its parsed-as-pattern
;; parameter list (so bare uppercase identifiers dispatch as 0-arg ctor
;; patterns rather than naming a plain parameter).
(define (combine-clause-list forms record clause? get-name get-expr get-stx make)
  (define (clause-form? f) (and (clause? f) (hash-has-key? record (get-stx f))))
  (define groups (make-hasheq))     ; name → (list-of form in reverse src order)
  (define non-fn (make-hasheq))     ; name → stx of conflicting value-form
  (for ([f (in-list forms)])
    (cond
      [(clause-form? f)
       (hash-update! groups (get-name f) (lambda (xs) (cons f xs)) '())]
      [(clause? f)
       (hash-set! non-fn (get-name f) (get-stx f))]
      [else (void)]))
  ;; Validate: every name must be EITHER a single value def OR a
  ;; group of function clauses with matching arity.
  (for ([(name fns) (in-hash groups)])
    (define arities
      (for/seteq ([d (in-list fns)])
        (length (e:lam-params (get-expr d)))))
    (when (> (set-count arities) 1)
      (raise-syntax-error 'rackton
        (format "definition ~s has clauses with different arities: ~s"
                name (sort (set->list arities) <))
        (get-stx (car (reverse fns)))))
    (when (hash-has-key? non-fn name)
      (raise-syntax-error 'rackton
        (format "definition ~s mixes function-form (define (~s …)) and value-form (define ~s …)"
                name name name)
        (get-stx (car (reverse fns))))))
  ;; Emit: replace each first-occurrence clause-bearing form with the
  ;; combined form; skip subsequent occurrences for the same name.  All
  ;; other forms pass through.
  (define emitted (mutable-seteq))
  (apply append
         (for/list ([f (in-list forms)])
           (cond
             [(clause-form? f)
              (define name (get-name f))
              (cond
                [(set-member? emitted name) '()]
                [else
                 (set-add! emitted name)
                 (define clauses (reverse (hash-ref groups name)))
                 (list (combine-fn-clauses name clauses record
                                           get-expr get-stx make))])]
             [else (list f)]))))

;; If `clause-forms` is a single clause, return it unchanged — parse-top
;; already produced the desugared singleton form.  Otherwise synthesize
;; one form whose body is an `e:match*` over fresh arg names, with each
;; clause's parameter list reparsed under the multi-clause rule.
(define (combine-fn-clauses name clause-forms record get-expr get-stx make)
  (cond
    [(null? (cdr clause-forms)) (car clause-forms)]
    [else
     (define first (car clause-forms))
     (define stx (get-stx first))
     (define arity (length (e:lam-params (get-expr first))))
     (define fresh-names
       (for/list ([_ (in-range arity)]) (gensym '$arg)))
     (define clauses
       (for/list ([d (in-list clause-forms)])
         (define rec (hash-ref record (get-stx d)))
         (define params-stx (car rec))
         (define body-stx   (cdr rec))
         (define pats
           (for/list ([p-stx (in-list (syntax->list params-stx))])
             (parse-pattern p-stx)))
         (clause* pats #f
                  (parse-expr body-stx)
                  (get-stx d))))
     (define scrutinees
       (for/list ([n (in-list fresh-names)]) (e:var n stx)))
     (make name
           (e:lam fresh-names
                  (e:match* scrutinees clauses #f stx)
                  stx)
           stx)]))
