#lang racket/base

;; Rackton — pattern compilation.
;;
;; (compile-pattern p) → syntax-object
;;
;; Translates a surface pattern AST node into a Racket match pattern.
;; Constructor patterns are emitted by name so that the match expanders
;; created by `define-data-ctor` (private/adt.rkt) handle the actual
;; struct-shape match.  The original syntax object's lexical context is
;; preserved so that those match expanders resolve correctly.
;;
;; Pattern correspondence
;;   p:wild  → _
;;   p:var x → x
;;   p:lit v → v   (literal datum, comparison by equal?)
;;                 — except a symbol literal lowers to (quote v); a bare
;;                   symbol in a Racket match pattern is a wildcard binding.
;;   p:ctor C (p ...) → (C compiled-p ...)

(provide compile-pattern)

(require racket/match
         racket/list
         "surface.rkt"
         ;; The inline reader closure a `bits` pattern lowers to is plain
         ;; runtime code (lambda / and / let / arithmetic and the leaf bit
         ;; operations).  Those identifiers must resolve at the GENERATED
         ;; code's phase, not at this module's phase, so they are imported
         ;; for-template — exactly as codegen.rkt does for racket/base.
         (for-template racket/base
                       "bitstring-runtime.rkt"
                       ;; The prelude List/Pair match-expanders, so a
                       ;; literal-sugar pattern (`[a b]`, `[a . b]`, quoted
                       ;; data) matches the prelude constructor even where
                       ;; the user module shadows the name (see the
                       ;; `sugar-ref-marked?` branch below).
                       (only-in "prelude-runtime.rkt" Cons Nil Pair)))

;; Prelude-scoped match-expander identifiers for the constructors that
;; literal-sugar patterns reference.  Written as literals so they carry
;; the for-template prelude-runtime context.
(define sugar-ctor-ids (hasheq 'Cons #'Cons 'Nil #'Nil 'Pair #'Pair))

(define (compile-pattern p)
  (match p
    [(p:wild stx)
     (datum->syntax stx '_ stx)]
    [(p:var name stx)
     (emit-id name stx)]
    [(p:lit v stx)
     #:when (symbol? v)
     (datum->syntax stx (list 'quote v) stx)]
    [(p:lit v stx)
     (datum->syntax stx v stx)]
    [(p:ctor name args stx)
     ;; A literal-sugar constructor pattern resolves through the
     ;; prelude-scoped match-expander (imported for-template above), so it
     ;; matches the prelude constructor regardless of a user shadow.
     (define sugar-id
       (and (sugar-ref-marked? stx) (hash-ref sugar-ctor-ids name #f)))
     (define name-stx
       (if sugar-id
           (datum->syntax sugar-id (syntax-e sugar-id) stx)
           (datum->syntax stx name stx)))
     (define arg-stxs (map compile-pattern args))
     (datum->syntax stx (cons name-stx arg-stxs) stx)]
    ;; A tuple pattern matches the tuple's hidden representation.  The
    ;; representation (a vector — see prelude-runtime's rackton-tuple-make
    ;; / rackton-tuple-ref) is matched through racket/match's own `vector`
    ;; pattern, emitted in THIS module's context so it resolves regardless
    ;; of the user module's bindings.
    [(p:tuple args _)
     (with-syntax ([(a ...) (map compile-pattern args)])
       #'(vector a ...))]
    ;; A `bits` pattern lowers to a real Racket match pattern: an `app`
    ;; over an inline reader closure that walks a bit cursor over the
    ;; scrutinee.  The reader returns the list of raw field values, or #f
    ;; when the binary is too short / mis-aligned — and #f never matches a
    ;; `(list …)` pattern, so the clause falls through to the next one,
    ;; the ordinary Racket mechanism.  The field positions are the user's
    ;; own sub-patterns, so they bind into the clause body normally.
    [(p:bits segs stx)
     (compile-bits-pattern segs stx)]))

;; Build `(app <reader-lambda> (list <field-pat> …))` for a bits pattern.
;;
;; A dependent width like `[payload len binary]` cannot read `len` from a
;; sibling match variable (stock match does not scope pattern variables
;; into an `app` expression), so the reader resolves it INSIDE the
;; closure: it binds each segment's value to a fresh temporary and a
;; later segment's width refers to that temporary.  The user's `len` is
;; bound only afterwards, by the outer `(list …)` pattern.
(define (compile-bits-pattern segs stx)
  (define n (length segs))
  (define bs   (car (generate-temporaries '(bits-bs))))
  (define Lid  (car (generate-temporaries '(bits-len))))
  (define offs (generate-temporaries (build-list (add1 n) (lambda (_) 'bits-off))))
  (define flds (generate-temporaries (build-list n (lambda (_) 'bits-field))))
  ;; segment-bound variable name → its field temporary (for dependent widths)
  (define var->fld
    (for/hash ([sg (in-list segs)] [f (in-list flds)]
               #:when (p:var? (bit-seg-subject sg)))
      (values (p:var-name (bit-seg-subject sg)) f)))
  ;; A segment's width, in BITS, as a syntax object.  For `binary` the
  ;; size counts bytes (unit 8); for `integer` / `bitstring`, bits.
  (define (width-syntax sg off-id)
    (define sz (bit-seg-size sg))
    (define unit (if (eq? (bit-seg-type sg) 'binary) 8 1))
    (cond
      [(eq? sz 'rest) #`(- #,Lid #,off-id)]
      [(exact-nonnegative-integer? sz) (* unit sz)]
      [(symbol? sz)
       (define f (hash-ref var->fld sz #f))
       (unless f
         (raise-syntax-error 'rackton
           (format "bits segment width `~a` must be bound by an earlier segment" sz)
           stx))
       (if (= unit 1) f #`(* 8 #,f))]
      [else (raise-syntax-error 'rackton "malformed bits segment size" stx)]))
  ;; Read one segment's value at `off-id` with width `W-id`.
  (define (read-syntax sg off-id W-id)
    (case (bit-seg-type sg)
      [(integer)   #`(bitstring-read #,bs #,off-id #,W-id #,(bit-seg-signed? sg))]
      [(bitstring) #`(bitstring-slice #,bs #,off-id #,W-id)]
      [(binary)    #`(bitstring->bytes-exact (bitstring-slice #,bs #,off-id #,W-id))]
      [else (raise-syntax-error 'rackton "unsupported bits segment type" stx)]))
  ;; Reader body, built inside-out.  Innermost: require exact consumption
  ;; (no bits left over) and return the field list.
  (define reader-body
    (let loop ([i 0])
      (cond
        [(= i n)
         #`(and (= #,(list-ref offs n) #,Lid) (list #,@flds))]
        [else
         (define sg (list-ref segs i))
         (define off-in (list-ref offs i))
         (define off-out (list-ref offs (add1 i)))
         (define f (list-ref flds i))
         (define W (car (generate-temporaries '(bits-w))))
         (define cont (loop (add1 i)))
         (define rd (read-syntax sg off-in W))
         ;; A `binary` read is #f when the slice is not byte-aligned;
         ;; guard on it so a mis-aligned binary falls the clause through.
         (define inner
           (if (eq? (bit-seg-type sg) 'binary)
               #`(let ([#,f #,rd])
                   (and #,f (let ([#,off-out (+ #,off-in #,W)]) #,cont)))
               #`(let ([#,f #,rd] [#,off-out (+ #,off-in #,W)]) #,cont)))
         #`(let ([#,W #,(width-syntax sg off-in)])
             (and (<= (+ #,off-in #,W) #,Lid) #,inner))])))
  #`(app (lambda (#,bs)
           (and (bitstring? #,bs)
                (let ([#,Lid (bitstring-len #,bs)] [#,(car offs) 0])
                  #,reader-body)))
         (list #,@(map (lambda (sg) (compile-pattern (bit-seg-subject sg))) segs))))
