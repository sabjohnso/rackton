#lang racket/base

;; Rackton — type AST, free type variables, and substitution.
;;
;; Tenets:
;;   - Encapsulation: only the public API surface below escapes this module.
;;     Other modules treat types abstractly through these operations.
;;   - Algebra-driven: substitution composition is associative and applying
;;     `(subst-compose s2 s1)` is the same as piping through s1 then s2.
;;     These laws are checked in private/types-test.rkt.
;;
;; Representation
;;   types     := (tvar α)
;;             |  (tcon C)
;;             |  (tapp head (listof type))
;;   schemes   := (scheme (listof α) body)
;;   substitutions are immutable eq-keyed hash tables from α to type.

(provide (struct-out tvar)
         (struct-out tcon)
         (struct-out tapp)
         (struct-out tforall)
         (struct-out scheme)
         (struct-out qual)
         (struct-out pred)
         (struct-out kind-star)
         (struct-out kind-arr)
         kstar
         k->
         kind->datum

         type?
         type-vars
         scheme-free-vars
         pred-vars
         qual-body-type

         empty-subst
         subst?
         subst-singleton
         subst-extend
         subst-compose
         subst-domain
         subst-ref
         apply-subst
         apply-subst/scheme

         ;; smart constructors
         mqual

         ;; readability helpers
         type->datum
         scheme->datum
         pred->datum

         ;; diagnostic pretty-printing (display only — not serialization)
         type->pretty-datum
         pred->pretty-datum
         format-pretty-datum
         format-types
         format-type
         format-scheme
         format-preds
         format-pred

         ;; pre-built primitive types
         t-int t-bool t-string t-symbol t-unit t-float t-char t-bytes
         t-rational t-complex t-complex-exact
         t-arrow t-list

         ;; arrows and other shape predicates
         make-arrow
         arrow?
         arrow-dom
         arrow-cod
         make-tapp)

(require racket/set
         racket/match
         racket/pretty)

;; ----- Type AST ------------------------------------------------------

(struct tvar   (name)       #:transparent)
(struct tcon   (name)       #:transparent)
(struct tapp   (head args)  #:transparent)
(struct scheme (vars body)  #:transparent)
;; A polymorphic type embedded inside a larger type.
;; `(tforall vars body)` reads "∀vars. body" and may appear in any
;; type-level position (most usefully a function's argument type for
;; rank-N).  This is distinct from `scheme`, which only lives at the
;; top of env entries and never embeds.
(struct tforall (vars body) #:transparent)

;; A class predicate, e.g. (Eq a) or (Ord Integer).
(struct pred   (class args) #:transparent)

;; ----- Kinds ---------------------------------------------------------
;; `*` is the kind of ordinary types.  `(kind-arr a b)` is the kind of a
;; type constructor that takes a type of kind `a` and returns a type of
;; kind `b`.  Kinds are explicit; no kind inference is performed.

(struct kind-star ()        #:transparent)
(struct kind-arr  (dom cod) #:transparent)

(define kstar (kind-star))
(define (k-> a b) (kind-arr a b))

(define (kind->datum k)
  (match k
    [(kind-star)      '*]
    [(kind-arr a b)   `(-> ,(kind->datum a) ,(kind->datum b))]))

;; A qualified type: `(qual (pred ...) body)` reads "ρ ::= π ... => τ".
;; Smart constructor `mqual` collapses an empty context to the bare body.
(struct qual   (constraints body) #:transparent)

(define (type? v)
  (or (tvar? v) (tcon? v) (tapp? v) (tforall? v)))

(define (mqual constraints body)
  (cond
    [(null? constraints) body]
    [else (qual constraints body)]))

(define (qual-body-type t)
  (if (qual? t) (qual-body t) t))

;; ----- Pre-built primitive constructors ------------------------------

(define t-int    (tcon 'Integer))
(define t-bool   (tcon 'Boolean))
(define t-string (tcon 'String))
(define t-symbol (tcon 'Symbol))
(define t-unit   (tcon 'Unit))
(define t-float  (tcon 'Float))
(define t-char   (tcon 'Char))
(define t-bytes  (tcon 'Bytes))
(define t-rational (tcon 'Rational))
(define t-complex  (tcon 'Complex))
(define t-complex-exact (tcon 'ComplexExact))
(define t-arrow  (tcon '->))
(define t-list   (tcon 'List))

(define (make-arrow dom cod)
  (tapp t-arrow (list dom cod)))

(define (arrow? t)
  (match t
    [(tapp (tcon '->) (list _ _)) #t]
    [_ #f]))

(define (arrow-dom t)
  (match t [(tapp (tcon '->) (list d _)) d]))

(define (arrow-cod t)
  (match t [(tapp (tcon '->) (list _ c)) c]))

;; (make-tapp head args) collapses (tapp head '()) into head so that
;; nullary applications stay normalized, and flattens nested tapps so
;; partial applications combine.  This matters for higher-kinded type
;; variables: after substituting `f ↦ (Either e)` into `(f a)`, we get
;; a single `(tapp (tcon Either) (list e a))` rather than a nested form.
(define (make-tapp head args)
  (cond
    [(null? args) head]
    [(tapp? head)
     (tapp (tapp-head head)
           (append (tapp-args head) args))]
    [else (tapp head args)]))

;; ----- Free type variables -------------------------------------------

(define (type-vars t)
  (match t
    [(tvar a)        (seteq a)]
    [(tcon _)        (seteq)]
    [(tapp h args)   (for/fold ([acc (type-vars h)]) ([a (in-list args)])
                       (set-union acc (type-vars a)))]
    [(pred _ args)   (for/fold ([acc (seteq)]) ([a (in-list args)])
                       (set-union acc (type-vars a)))]
    [(qual cs body)  (for/fold ([acc (type-vars body)]) ([c (in-list cs)])
                       (set-union acc (type-vars c)))]
    [(tforall vs body)
     ;; A tforall's quantified vars are bound there and
     ;; don't count as free in the surrounding type.
     (set-subtract (type-vars body) (list->seteq vs))]))

;; The free type variables of a single predicate, exposed for callers
;; that don't want to dispatch through `type-vars`.
(define (pred-vars p)
  (type-vars p))

(define (scheme-free-vars sch)
  (match sch
    [(scheme vs body) (set-subtract (type-vars body) (list->seteq vs))]))

;; ----- Substitutions -------------------------------------------------

;; Represented as an immutable eq-keyed hash table:  α-symbol → type.
;; We only ever extend or compose, never mutate.

(define empty-subst (hasheq))

(define (subst? v)
  (and (hash? v) (hash-eq? v)))

(define (subst-singleton var ty)
  (hasheq var ty))

(define (subst-extend s var ty)
  (hash-set s var ty))

(define (subst-domain s)
  (list->seteq (hash-keys s)))

(define (subst-ref s var [default #f])
  (hash-ref s var default))

(define (apply-subst s t)
  (cond
    [(hash-empty? s) t]
    [else
     (match t
       [(tvar a)      (hash-ref s a t)]
       [(tcon _)      t]
       [(tapp h args) (make-tapp (apply-subst s h)
                                 (for/list ([a (in-list args)])
                                   (apply-subst s a)))]
       [(pred c args) (pred c (for/list ([a (in-list args)])
                                (apply-subst s a)))]
       [(qual cs body)
        (mqual (for/list ([c (in-list cs)]) (apply-subst s c))
               (apply-subst s body))]
       [(tforall vs body)
        ;; A tforall's bound vars shadow `s`.  Drop the
        ;; bound vars from `s` before recursing, matching the way
        ;; `scheme` handles substitution.
        (define s*
          (for/fold ([acc s]) ([v (in-list vs)])
            (hash-remove acc v)))
        (tforall vs (apply-subst s* body))])]))

(define (apply-subst/scheme s sch)
  (match sch
    [(scheme vs body)
     ;; Bound type variables shadow s.  Drop them before recursing.
     (define s*
       (for/fold ([acc s]) ([v (in-list vs)])
         (hash-remove acc v)))
     (scheme vs (apply-subst s* body))]))

;; (subst-compose s2 s1) is the substitution θ such that
;;   apply-subst θ t  =  apply-subst s2 (apply-subst s1 t)
;; for every type t.  Standard definition:
;;   θ(α) = apply-subst s2 (s1(α))      when α ∈ dom(s1)
;;   θ(α) = s2(α)                       when α ∈ dom(s2) \ dom(s1)
(define (subst-compose s2 s1)
  (define from-s1
    (for/fold ([acc empty-subst]) ([(v t) (in-hash s1)])
      (hash-set acc v (apply-subst s2 t))))
  (for/fold ([acc from-s1]) ([(v t) (in-hash s2)])
    (if (hash-has-key? s1 v) acc (hash-set acc v t))))

;; ----- Human-readable printing ---------------------------------------

(define (type->datum t)
  (match t
    [(tvar a) a]
    [(tcon n) n]
    [(tapp (tcon '->) (list d c))
     `(-> ,(type->datum d) ,(type->datum c))]
    [(tapp h args)
     `(,(type->datum h) ,@(map type->datum args))]
    [(qual cs body)
     `(,@(map pred->datum cs) => ,(type->datum body))]
    [(tforall vs body)
     `(All ,vs ,(type->datum body))]))

(define (pred->datum p)
  (match p
    [(pred c args) `(,c ,@(map type->datum args))]))

(define (scheme->datum sch)
  (match sch
    [(scheme '() body) (type->datum body)]
    [(scheme vs body)  `(All ,vs ,(type->datum body))]))

;; ----- Diagnostic pretty-printing ------------------------------------
;;
;; `type->datum` mirrors the *internal* binary-arrow representation
;; exactly (the scheme codec depends on that shape).  The functions
;; below are for human-facing diagnostics only: they flatten curried
;; arrows back into the n-ary surface form `(-> A B C)`, rename internal
;; fresh tvars to nice single letters, and wrap wide types across lines.

;; Like `type->datum`, but collapse a right-nested chain of binary `->`
;; applications into a single n-ary `(-> a b … r)` — the form the user
;; actually wrote.
(define (type->pretty-datum t)
  (match t
    [(tvar a) a]
    [(tcon n) n]
    [(tapp (tcon '->) (list d c))
     (let loop ([acc (list (type->pretty-datum d))] [rest c])
       (match rest
         [(tapp (tcon '->) (list d2 c2))
          (loop (cons (type->pretty-datum d2) acc) c2)]
         [_ `(-> ,@(reverse (cons (type->pretty-datum rest) acc)))]))]
    [(tapp h args)
     `(,(type->pretty-datum h) ,@(map type->pretty-datum args))]
    [(qual cs body)
     `(,@(map pred->pretty-datum cs) => ,(type->pretty-datum body))]
    [(tforall vs body)
     `(All ,vs ,(type->pretty-datum body))]))

(define (pred->pretty-datum p)
  (match p
    [(pred c args) `(,c ,@(map type->pretty-datum args))]))

;; Render a datum to a (possibly multi-line) string, wrapping wide forms
;; across lines.  `racket/pretty` does the layout; we only strip the
;; trailing newline it appends.
;;
;; The width is the budget *after* the diagnostic's label column
;; (`"  expected: "` is 12 chars); 66 + 12 keeps a wrapped line inside a
;; standard 79-column terminal.  Treating `->` / `=>` / `All` like
;; `lambda` keeps the head together with its first operand on a wrapped
;; type, instead of orphaning the arrow on its own line.
(define pretty-type-columns 66)
(define pretty-type-style-table
  (pretty-print-extend-style-table #f '(-> => All) '(lambda lambda lambda)))
(define (format-pretty-datum d)
  (define s
    (parameterize ([pretty-print-current-style-table pretty-type-style-table])
      (pretty-format d pretty-type-columns #:mode 'write)))
  (if (and (positive? (string-length s))
           (char=? (string-ref s (sub1 (string-length s))) #\newline))
      (substring s 0 (sub1 (string-length s)))
      s))

;; The i-th display name: a, b, …, z, a1, b1, ….
(define (display-tvar-name i)
  (if (< i 26)
      (string->symbol (string (integer->char (+ (char->integer #\a) i))))
      (string->symbol
       (format "~a~a"
               (integer->char (+ (char->integer #\a) (modulo i 26)))
               (quotient i 26)))))

;; Free type-variable names across `ts`, in left-to-right first-occurrence
;; order.  Encounter order (rather than sorting by internal name) gives
;; contiguous letters that start at `a` in the first type, while a
;; variable shared between types still maps to a single letter.  Bound
;; tvars are excluded (they are not in the free set computed by
;; `type-vars`).
(define (ordered-free-vars ts)
  (define freeset
    (for/fold ([acc (seteq)]) ([t (in-list ts)])
      (set-union acc (type-vars t))))
  (define seen (make-hasheq))
  (define out '())
  (define (walk t)
    (match t
      [(tvar a)
       (when (and (set-member? freeset a) (not (hash-has-key? seen a)))
         (hash-set! seen a #t)
         (set! out (cons a out)))]
      [(tcon _) (void)]
      [(tapp h args) (walk h) (for-each walk args)]
      [(qual cs body) (for-each walk cs) (walk body)]
      [(tforall _ body) (walk body)]
      [(pred _ args) (for-each walk args)]))
  (for-each walk ts)
  (reverse out))

;; A skolem constant is represented as a `tcon` whose name is a gensym of
;; the form `$<flavour>skolem.<origin>.<n>` (see the skolemize helpers in
;; infer.rkt).  Such names are internal — they must read as ordinary
;; type variables in diagnostics, not as concrete type constructors.
(define (skolem-tcon-name? n)
  (define s (symbol->string n))
  (and (> (string-length s) 0)
       (char=? (string-ref s 0) #\$)
       (regexp-match? #rx"skolem" s)))

;; Renameable display names across `ts` in first-occurrence order: free
;; type variables plus skolem tcons, sharing one letter sequence so a
;; rigid skolem and the flexible variables around it read as distinct
;; consecutive letters (a, b, c, …) rather than as `$gen-skolem.a0.42`.
(define (ordered-display-names ts)
  (define freeset
    (for/fold ([acc (seteq)]) ([t (in-list ts)])
      (set-union acc (type-vars t))))
  (define seen (make-hasheq))
  (define out '())
  (define (note! a)
    (unless (hash-has-key? seen a)
      (hash-set! seen a #t)
      (set! out (cons a out))))
  (define (walk t)
    (match t
      [(tvar a) (when (set-member? freeset a) (note! a))]
      [(tcon n) (when (skolem-tcon-name? n) (note! n))]
      [(tapp h args) (walk h) (for-each walk args)]
      [(qual cs body) (for-each walk cs) (walk body)]
      [(tforall _ body) (walk body)]
      [(pred _ args) (for-each walk args)]))
  (for-each walk ts)
  (reverse out))

;; Like `type->pretty-datum`, but additionally rewrite any skolem tcon
;; whose name is in `skmap` to its assigned display symbol.
(define (type->pretty-datum/sk t skmap)
  (let recur ([t t])
    (match t
      [(tvar a) a]
      [(tcon n) (hash-ref skmap n n)]
      [(tapp (tcon '->) (list d c))
       (let loop ([acc (list (recur d))] [rest c])
         (match rest
           [(tapp (tcon '->) (list d2 c2))
            (loop (cons (recur d2) acc) c2)]
           [_ `(-> ,@(reverse (cons (recur rest) acc)))]))]
      [(tapp h args)
       `(,(recur h) ,@(map recur args))]
      [(qual cs body)
       `(,@(map (lambda (p) (pred->pretty-datum/sk p skmap)) cs)
         => ,(recur body))]
      [(tforall vs body)
       `(All ,vs ,(recur body))])))

(define (pred->pretty-datum/sk p skmap)
  (match p
    [(pred c args)
     `(,c ,@(map (lambda (a) (type->pretty-datum/sk a skmap)) args))]))

;; Build the display substitution: i-th free var (in order) → i-th letter.
(define (display-subst ordered-vars)
  (for/fold ([s empty-subst]) ([old (in-list ordered-vars)] [i (in-naturals)])
    (subst-extend s old (tvar (display-tvar-name i)))))

;; Rename the free tvars of every type in `ts` with ONE shared
;; substitution, then flatten + format each.  Sharing matters for an
;; expected/got pair: a variable common to both reads as the same letter
;; on both sides.
(define (format-types ts)
  ;; Assign one shared letter sequence to free tvars AND skolem tcons in
  ;; first-occurrence order; tvars are renamed by substitution, skolems
  ;; by a name→letter map applied while building the pretty datum.
  (define-values (σ skmap)
    (for/fold ([σ empty-subst] [skmap (hasheq)])
              ([old (in-list (ordered-display-names ts))]
               [i (in-naturals)])
      (define nice (display-tvar-name i))
      (if (skolem-tcon-name? old)
          (values σ (hash-set skmap old nice))
          (values (subst-extend σ old (tvar nice)) skmap))))
  (for/list ([t (in-list ts)])
    (format-pretty-datum (type->pretty-datum/sk (apply-subst σ t) skmap))))

(define (format-type t)
  (car (format-types (list t))))

;; Render a type scheme for diagnostics, mirroring `scheme->datum`'s
;; `(All …)` wrapper but with nice names + flattened arrows.  The
;; quantified binders are renamed in lock-step with the body.
(define (format-scheme sch)
  (match sch
    [(scheme '() body) (format-type body)]
    [(scheme vs body)
     (define order (ordered-free-vars (list body)))
     (define σ (display-subst order))
     (define (renamed v)
       (cond [(subst-ref σ v) => tvar-name] [else v]))
     (format-pretty-datum
      `(All ,(map renamed vs) ,(type->pretty-datum (apply-subst σ body))))]))

;; Same shared renaming, for class-constraint predicates.
(define (format-preds ps)
  (define σ (display-subst (ordered-free-vars ps)))
  (for/list ([p (in-list ps)])
    (format-pretty-datum (pred->pretty-datum (apply-subst σ p)))))

(define (format-pred p)
  (car (format-preds (list p))))
