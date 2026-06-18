#lang racket/base

;; Rackton — codec for schemes / types / preds.
;;
;; Used to ferry type information across .rkt module boundaries.
;; `scheme->sexp` is just `scheme->datum`; `sexp->scheme` is the inverse,
;; reconstructing the structured representation from the datum form.
;; The textual representation must round-trip exactly.

(require racket/match
         racket/list
         "types.rkt"
         "env.rkt"
         "ast.rkt"
         "surface.rkt")

(provide scheme->sexp
         sexp->scheme
         sexp->type
         sexp->pred
         pred->sexp
         encode-data-info
         decode-data-info
         encode-tcon-info
         decode-tcon-info
         encode-kind
         decode-kind
         encode-class-info
         decode-class-info
         encode-instance-info
         decode-instance-info)

(define (scheme->sexp s) (scheme->datum s))

(define (sexp->scheme datum)
  (match datum
    [(list 'All vars body)
     (scheme vars (sexp->type body))]
    [body
     (scheme '() (sexp->type body))]))

(define (sexp->type datum)
  (cond
    ;; A bare non-negative integer is a type-level natural literal.
    [(exact-nonnegative-integer? datum) (tnat datum)]
    [(symbol? datum)
     (if (lowercase-id? datum) (tvar datum) (tcon datum))]
    [(and (list? datum) (memq '=> datum))
     (define i (index-of datum '=>))
     (define preds-syntax (take datum i))
     (define after-arrow (drop datum (add1 i)))
     ;; `(p1 p2 ... => body)` — after `=>` is exactly one body.
     (mqual (map sexp->pred preds-syntax)
            (sexp->type (car after-arrow)))]
    [(and (list? datum)
          (eq? '-> (car datum))
          (= (length datum) 3))
     (make-arrow (sexp->type (cadr datum)) (sexp->type (caddr datum)))]
    [(list? datum)
     (make-tapp (sexp->type (car datum))
                (map sexp->type (cdr datum)))]
    [else
     (error 'sexp->type "cannot decode: ~v" datum)]))

(define (sexp->pred datum)
  (match datum
    [(cons name args) (pred name (map sexp->type args))]))

;; ----- data-info and tcon-info codecs -------------------------

(define (encode-data-info di)
  (list (data-info-type-name di)
        (data-info-ctor-name di)
        (data-info-arity di)
        (scheme->sexp (data-info-scheme di))
        (data-info-ex-tvars di)))

(define (decode-data-info datum)
  (match datum
    [(list type-name ctor-name arity scheme-sexp)
     ;; Backwards-compatible with older sidecars: missing
     ;; ex-tvars defaults to empty.
     (data-info type-name ctor-name arity
                (sexp->scheme scheme-sexp) '())]
    [(list type-name ctor-name arity scheme-sexp ex-tvars)
     (data-info type-name ctor-name arity
                (sexp->scheme scheme-sexp) ex-tvars)]))

(define (encode-tcon-info ti)
  (list (tcon-info-name ti)
        (tcon-info-arity ti)
        (tcon-info-ctors ti)
        ;; Abstract? flag, present for newly-emitted sidecars;
        ;; old sidecars decode with #f.
        (tcon-info-abstract? ti)
        ;; Opaque runtime dispatch tag (or #f); lets an importer that
        ;; defines a new instance for the type register it correctly.
        (tcon-info-runtime-tag ti)
        ;; The inferred kind.  Newest field; legacy sidecars without it
        ;; fall back to the all-`*` arity kind on decode.
        (encode-kind (tcon-info-kind ti))))

(define (decode-tcon-info datum)
  (match datum
    [(list name arity ctors abstract? runtime-tag kind)
     (tcon-info name arity (decode-kind kind) ctors abstract? runtime-tag)]
    [(list name arity ctors abstract? runtime-tag)
     (tcon-info name arity (arity->star-kind arity) ctors abstract? runtime-tag)]
    [(list name arity ctors abstract?)
     (tcon-info name arity (arity->star-kind arity) ctors abstract? #f)]
    [(list name arity ctors)
     (tcon-info name arity (arity->star-kind arity) ctors #f #f)]))

;; ----- kinds, classes, instances ------------------------------

(define (encode-kind k)
  (match k
    [(kind-star)     '*]
    [(kind-arr a b)  `(-> ,(encode-kind a) ,(encode-kind b))]
    [(kind-con n)    `(con ,n)]
    [(kind-nat)      'Nat]))

(define (decode-kind datum)
  (cond
    [(eq? datum '*) (kind-star)]
    [(eq? datum 'Nat) (kind-nat)]
    [(and (list? datum) (eq? (car datum) '->))
     (kind-arr (decode-kind (cadr datum)) (decode-kind (caddr datum)))]
    [(and (list? datum) (eq? (car datum) 'con))
     (kind-con (cadr datum))]
    [else (error 'decode-kind "bad kind: ~v" datum)]))

(define (pred->sexp p) (pred->datum p))

;; ----- surface-AST codec (for class defaults) ----------------------
;; A class's default method bodies are surface-AST expressions.  They
;; must cross module boundaries so an instance written in an importing
;; module can fall back on the protocol's defaults exactly like one in
;; the protocol's own module.  We serialize the node set that
;; `remap-ast-stx` (ast.rkt) relocates — authoritatively "what can appear
;; in a relocatable default" — dropping each node's `stx`: the importer
;; re-anchors every handle with `freshen-ast` before inference, so the
;; placeholder a decoded node carries is always replaced.

;; A single synthetic handle for decoded nodes; freshen-ast overwrites it.
(define ast-placeholder-stx (datum->syntax #f 'rackton-default))
(define ph ast-placeholder-stx)

(define (expr->sexp e)
  (match e
    [(e:literal v _)       (list 'lit v)]
    [(e:var n _)           (list 'var n)]
    [(e:lam ps body _)     (list 'lam ps (expr->sexp body))]
    [(e:app h args _)      (list 'app (expr->sexp h) (map expr->sexp args))]
    [(e:let bs body _)     (list 'let (binds->sexp bs) (expr->sexp body))]
    [(e:letrec bs body _)  (list 'letrec (binds->sexp bs) (expr->sexp body))]
    [(e:if a b c _)        (list 'if (expr->sexp a) (expr->sexp b) (expr->sexp c))]
    [(e:ann ex t _)        (list 'ann (expr->sexp ex) (tyast->sexp t))]
    [(e:escape t vs body _) (list 'escape (tyast->sexp t) vs (syntax->datum body))]
    [(e:match s cs irr? _) (list 'match (expr->sexp s) (map clause->sexp cs) irr?)]
    [(e:match* ss cs irr? _) (list 'match* (map expr->sexp ss)
                                   (map clause*->sexp cs) irr?)]
    [(e:tuple es _)        (list 'tuple (map expr->sexp es))]
    [(e:tref t i _)        (list 'tref (expr->sexp t) i)]
    [(e:array es _)        (list 'array (map expr->sexp es))]
    [(e:build-array n p _) (list 'build-array n (expr->sexp p))]
    [(e:aref a i _)        (list 'aref (expr->sexp a) i)]
    [(e:array-slice op i a _) (list 'aslice op i (expr->sexp a))]
    [(e:update r us _)     (list 'update (expr->sexp r)
                                 (for/list ([u (in-list us)])
                                   (cons (car u) (expr->sexp (cdr u)))))]))

(define (binds->sexp bs)
  (for/list ([b (in-list bs)]) (cons (car b) (expr->sexp (cdr b)))))

(define (clause->sexp c)
  (match c
    [(clause pat guard body _)
     (list (pat->sexp pat)
           (and guard (expr->sexp guard))
           (expr->sexp body))]))

(define (clause*->sexp c)
  (match c
    [(clause* pats guard body _)
     (list (map pat->sexp pats)
           (and guard (expr->sexp guard))
           (expr->sexp body))]))

(define (pat->sexp p)
  (match p
    [(p:wild _)        (list 'pw)]
    [(p:var n _)       (list 'pv n)]
    [(p:lit v _)       (list 'pl v)]
    [(p:ctor n args _) (list 'pc n (map pat->sexp args))]
    [(p:tuple ps _)    (list 'pt (map pat->sexp ps))]))

(define (tyast->sexp t)
  (match t
    [(ty:var n _)       (list 'tv n)]
    [(ty:con n _)       (list 'tc n)]
    [(ty:nat v _)       (list 'tn v)]
    [(ty:app h args _)  (list 'ta (tyast->sexp h) (map tyast->sexp args))]
    [(ty:forall vs b _) (list 'tall vs (tyast->sexp b))]
    [(ty:qual cs b _)   (list 'tq (map constraint->sexp cs) (tyast->sexp b))]))

(define (constraint->sexp c)
  (match c
    [(constraint cls args _) (list cls (map tyast->sexp args))]))

(define (sexp->expr d)
  (match d
    [(list 'lit v)            (e:literal v ph)]
    [(list 'var n)            (e:var n ph)]
    [(list 'lam ps body)      (e:lam ps (sexp->expr body) ph)]
    [(list 'app h args)       (e:app (sexp->expr h) (map sexp->expr args) ph)]
    [(list 'let bs body)      (e:let (sexp->binds bs) (sexp->expr body) ph)]
    [(list 'letrec bs body)   (e:letrec (sexp->binds bs) (sexp->expr body) ph)]
    [(list 'if a b c)         (e:if (sexp->expr a) (sexp->expr b) (sexp->expr c) ph)]
    [(list 'ann ex t)         (e:ann (sexp->expr ex) (sexp->tyast t) ph)]
    [(list 'escape t vs body) (e:escape (sexp->tyast t) vs (datum->syntax ph body) ph)]
    [(list 'match s cs irr?)  (e:match (sexp->expr s) (map sexp->clause cs) irr? ph)]
    [(list 'match* ss cs irr?) (e:match* (map sexp->expr ss)
                                         (map sexp->clause* cs) irr? ph)]
    [(list 'tuple es)         (e:tuple (map sexp->expr es) ph)]
    [(list 'tref t i)         (e:tref (sexp->expr t) i ph)]
    [(list 'array es)         (e:array (map sexp->expr es) ph)]
    [(list 'build-array n p)  (e:build-array n (sexp->expr p) ph)]
    [(list 'aref a i)         (e:aref (sexp->expr a) i ph)]
    [(list 'aslice op i a)    (e:array-slice op i (sexp->expr a) ph)]
    [(list 'update r us)      (e:update (sexp->expr r)
                                        (for/list ([u (in-list us)])
                                          (cons (car u) (sexp->expr (cdr u))))
                                        ph)]))

(define (sexp->binds bs)
  (for/list ([b (in-list bs)]) (cons (car b) (sexp->expr (cdr b)))))

(define (sexp->clause d)
  (match d
    [(list pat guard body)
     (clause (sexp->pat pat) (and guard (sexp->expr guard)) (sexp->expr body) ph)]))

(define (sexp->clause* d)
  (match d
    [(list pats guard body)
     (clause* (map sexp->pat pats) (and guard (sexp->expr guard))
              (sexp->expr body) ph)]))

(define (sexp->pat d)
  (match d
    [(list 'pw)        (p:wild ph)]
    [(list 'pv n)      (p:var n ph)]
    [(list 'pl v)      (p:lit v ph)]
    [(list 'pc n args) (p:ctor n (map sexp->pat args) ph)]
    [(list 'pt ps)     (p:tuple (map sexp->pat ps) ph)]))

(define (sexp->tyast d)
  (match d
    [(list 'tv n)       (ty:var n ph)]
    [(list 'tc n)       (ty:con n ph)]
    [(list 'tn v)       (ty:nat v ph)]
    [(list 'ta h args)  (ty:app (sexp->tyast h) (map sexp->tyast args) ph)]
    [(list 'tall vs b)  (ty:forall vs (sexp->tyast b) ph)]
    [(list 'tq cs b)    (ty:qual (map sexp->constraint cs) (sexp->tyast b) ph)]))

(define (sexp->constraint d)
  (match d
    [(list cls args) (constraint cls (map sexp->tyast args) ph)]))

(define (encode-defaults defaults)
  (for/list ([(name expr) (in-hash defaults)])
    (list name (expr->sexp expr))))

(define (decode-defaults sexp)
  (for/hasheq ([entry (in-list sexp)])
    (values (car entry) (sexp->expr (cadr entry)))))

(define (encode-class-info ci)
  (list (class-info-name ci)
        (class-info-params ci)
        (for/list ([(k v) (in-hash (class-info-kinds ci))])
          (list k (encode-kind v)))
        (map pred->sexp (class-info-supers ci))
        (for/list ([(m s) (in-hash (class-info-methods ci))])
          (list m (scheme->sexp s)))
        (for/list ([(m p) (in-hash (class-info-dispatchpos ci))])
          (list m p))
        (for/list ([fd (in-list (class-info-fundeps ci))])
          (list (car fd) (cdr fd)))
        (for/list ([(m cs) (in-hash (class-info-dictreqs ci))])
          (list m cs))
        ;; List of associated-type names.
        (class-info-type-families ci)
        ;; Default method bodies (surface AST), so an importing module's
        ;; instance can fall back on them just like an in-module one.
        (encode-defaults (class-info-defaults ci))))

;; `defaults` now cross modules (decoded into the `defaults` field below);
;; super-derives still does not (it holds the `#:derive` cross-class
;; bodies) — every decoded class gets an empty super-derives table, so an
;; imported user class's `#:derive` clauses don't cross modules.  The
;; prelude monad stack lives in prelude-env, available everywhere, so
;; deriving the standard superclasses works regardless.
(define (decode-class-info datum)
  (match datum
    ;; Current shape: trailing defaults list (after type-families).
    [(list name params kinds-list supers-list methods-list dispatchpos-list
           fundeps-list dictreqs-list type-families-list defaults-list)
     (class-info name
                 params
                 (for/hasheq ([entry (in-list kinds-list)])
                   (values (car entry) (decode-kind (cadr entry))))
                 (map sexp->pred supers-list)
                 (for/hasheq ([entry (in-list methods-list)])
                   (values (car entry) (sexp->scheme (cadr entry))))
                 (decode-defaults defaults-list)
                 (for/hasheq ([entry (in-list dispatchpos-list)])
                   (values (car entry) (cadr entry)))
                 (for/list ([entry (in-list fundeps-list)])
                   (cons (car entry) (cadr entry)))
                 (for/hasheq ([entry (in-list dictreqs-list)])
                   (values (car entry) (cadr entry)))
                 type-families-list
                 (hasheq)
                 '())]
    ;; Older shape: trailing type-families list, no defaults.
    [(list name params kinds-list supers-list methods-list dispatchpos-list
           fundeps-list dictreqs-list type-families-list)
     (class-info name
                 params
                 (for/hasheq ([entry (in-list kinds-list)])
                   (values (car entry) (decode-kind (cadr entry))))
                 (map sexp->pred supers-list)
                 (for/hasheq ([entry (in-list methods-list)])
                   (values (car entry) (sexp->scheme (cadr entry))))
                 (hasheq)
                 (for/hasheq ([entry (in-list dispatchpos-list)])
                   (values (car entry) (cadr entry)))
                 (for/list ([entry (in-list fundeps-list)])
                   (cons (car entry) (cadr entry)))
                 (for/hasheq ([entry (in-list dictreqs-list)])
                   (values (car entry) (cadr entry)))
                 type-families-list
                 (hasheq)
                 '())]
    [(list name params kinds-list supers-list methods-list dispatchpos-list
           fundeps-list dictreqs-list)
     (class-info name
                 params
                 (for/hasheq ([entry (in-list kinds-list)])
                   (values (car entry) (decode-kind (cadr entry))))
                 (map sexp->pred supers-list)
                 (for/hasheq ([entry (in-list methods-list)])
                   (values (car entry) (sexp->scheme (cadr entry))))
                 (hasheq)
                 (for/hasheq ([entry (in-list dispatchpos-list)])
                   (values (car entry) (cadr entry)))
                 (for/list ([entry (in-list fundeps-list)])
                   (cons (car entry) (cadr entry)))
                 (for/hasheq ([entry (in-list dictreqs-list)])
                   (values (car entry) (cadr entry)))
                 '()
                 (hasheq)
                 '())]
    [(list name params kinds-list supers-list methods-list dispatchpos-list
           fundeps-list)
     (class-info name
                 params
                 (for/hasheq ([entry (in-list kinds-list)])
                   (values (car entry) (decode-kind (cadr entry))))
                 (map sexp->pred supers-list)
                 (for/hasheq ([entry (in-list methods-list)])
                   (values (car entry) (sexp->scheme (cadr entry))))
                 (hasheq)
                 (for/hasheq ([entry (in-list dispatchpos-list)])
                   (values (car entry) (cadr entry)))
                 (for/list ([entry (in-list fundeps-list)])
                   (cons (car entry) (cadr entry)))
                 (hasheq)
                 '()
                 (hasheq)
                 '())]
    [(list name params kinds-list supers-list methods-list dispatchpos-list)
     (class-info name
                 params
                 (for/hasheq ([entry (in-list kinds-list)])
                   (values (car entry) (decode-kind (cadr entry))))
                 (map sexp->pred supers-list)
                 (for/hasheq ([entry (in-list methods-list)])
                   (values (car entry) (sexp->scheme (cadr entry))))
                 (hasheq)
                 (for/hasheq ([entry (in-list dispatchpos-list)])
                   (values (car entry) (cadr entry)))
                 '()
                 (hasheq)
                 '()
                 (hasheq)
                 '())]))

;; Instance info is encoded with its owning class name as the first
;; element so we know where to install it on decode.  Method bodies
;; are not transmitted — the runtime side reaches the importer via
;; standard Racket module loading.
(define (encode-instance-info class-name ii)
  (list class-name
        (pred->sexp (instance-info-head ii))
        (map pred->sexp (instance-info-context ii))
        ;; Emit the type-family bindings as (name . type)
        ;; sexps so the importer can normalize associated types
        ;; against this instance.
        (for/list ([(name ty) (in-hash (instance-info-type-family-bindings ii))])
          (list name (type->datum ty)))
        ;; Originating module identity, preserved across re-export so a
        ;; diamond import of one instance can be deduped (see
        ;; instance-info origin field in env.rkt).
        (instance-info-origin ii)))

(define (decode-instance-info datum)
  (match datum
    [(list class-name head-sexp ctx-list bindings-list origin)
     (cons class-name
           (instance-info (sexp->pred head-sexp)
                          (map sexp->pred ctx-list)
                          (hasheq)
                          (for/hasheq ([entry (in-list bindings-list)])
                            (values (car entry) (sexp->type (cadr entry))))
                          origin
                          #f))]
    ;; Back-compat: sidecars compiled before the origin field carried
    ;; only four / three elements.  Decode them with origin = #f.
    [(list class-name head-sexp ctx-list bindings-list)
     (cons class-name
           (instance-info (sexp->pred head-sexp)
                          (map sexp->pred ctx-list)
                          (hasheq)
                          (for/hasheq ([entry (in-list bindings-list)])
                            (values (car entry) (sexp->type (cadr entry))))
                          #f
                          #f))]
    [(list class-name head-sexp ctx-list)
     (cons class-name
           (instance-info (sexp->pred head-sexp)
                          (map sexp->pred ctx-list)
                          (hasheq)
                          (hasheq)
                          #f
                          #f))]))
