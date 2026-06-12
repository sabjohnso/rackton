#lang racket/base

;; Rackton — source recording for the REPL's ,source command.
;;
;; The kernel records, for every name an input binds, the input form
;; that bound it; ,source NAME plays those forms back.  Names come
;; from the parsed top-form AST — the same structures the pipeline
;; elaborates — so this module never re-derives binding structure
;; from raw syntax.  The prelude is indexed the same way, lazily, by
;; parsing its source forms exactly as prelude.rkt itself does.
;;
;; Public API (all pure; the prelude index is a memoized constant):
;;   sources-record       — sources × input datum × parsed tops → sources
;;   sources-record-names — sources × names × input datum → sources
;;                          (for bindings with no Rackton AST, e.g. macros)
;;   sources-lookup       — sources × name × bound? →
;;                          (listof datum) | 'no-source | #f
;;
;; `sources` maps name → list of input datums, newest first.  A
;; definition replaces the name's entry; a standalone `(: name τ)`
;; signature or an `instance` prepends — so a class's entry grows one
;; datum per distinct instance head, and a re-evaluated instance
;; replaces its earlier self instead of duplicating.

(provide sources-record
         sources-record-names
         sources-lookup)

(require racket/match
         racket/promise
         "ast.rkt"
         (only-in "surface.rkt" parse-top)
         (only-in "prelude.rkt" prelude-source-forms))

;; ----- recording ----------------------------------------------------

(define (sources-record sources datum tops)
  (for/fold ([s sources]) ([t (in-list tops)])
    (match t
      [(top:def name _ _)        (record-replace s name datum)]
      [(top:dec name _ _)        (record-prepend s name datum)]
      [(top:data name _ ctors _ _ _)
       (for/fold ([s (record-replace s name datum)])
                 ([c (in-list ctors)])
         (record-replace s (data-ctor-name c) datum))]
      [(top:class _ head methods _)
       (for/fold ([s (record-replace s (constraint-class head) datum)])
                 ([m (in-list methods)]
                  #:when (method-sig? m))
         (record-replace s (method-sig-name m) datum))]
      [(or (top:instance _ head _ _)
           (top:derive-instance _ head _ _))
       (record-instance s (constraint-class head) datum)]
      [(top:alias name _ _ _)    (record-replace s name datum)]
      [(top:effect name ops _)
       (for/fold ([s (record-replace s name datum)])
                 ([op (in-list ops)])
         (record-replace s (effect-op-name op) datum))]
      [(top:foreign name _ _ _ _)          (record-replace s name datum)]
      [(top:foreign-c name _ _ _ _ _ _ _)  (record-replace s name datum)]
      [_ s])))

;; Record `datum` as the definition of each of `names` — for bindings
;; the Rackton pipeline never sees, like session macros.
(define (sources-record-names sources names datum)
  (for/fold ([s sources]) ([n (in-list names)])
    (record-replace s n datum)))

(define (record-replace s name datum)
  (hash-set s name (list datum)))

(define (record-prepend s name datum)
  (hash-update s name (lambda (ds) (cons datum ds)) '()))

;; An instance prepends under its class, after dropping any earlier
;; instance with the same head — the REPL allows instance
;; re-definition, so the replay must show only the live one.
(define (record-instance s cls datum)
  (define head (instance-head-datum datum))
  (hash-update s cls
               (lambda (ds)
                 (cons datum
                       (for/list ([d (in-list ds)]
                                  #:unless (equal? (instance-head-datum d) head))
                         d)))
               '()))

;; The head constraint of an instance form's datum:
;; `(instance (C T) …)` and `(instance ((Ctx a) … => (C T)) …)` both
;; yield `(C T)`.  #f for anything else, which compares unequal to
;; every real head.
(define (instance-head-datum d)
  (match d
    [(list 'instance h _ ...)
     (match h
       [(list _ ... '=> head) head]
       [_ h])]
    [_ #f]))

;; ----- lookup --------------------------------------------------------

;; The prelude's name → forms index, built on first ,source use by
;; parsing each prelude form the same way prelude.rkt does (parsing
;; does not depend on inference parameters, so a plain parse-top
;; reproduces the binding structure exactly).
(define prelude-index
  (delay
    (for/fold ([s (hasheq)]) ([f (in-list prelude-source-forms)])
      (sources-record s f (list (parse-top (datum->syntax #f f)))))))

;; `bound?` tells lookup the env knows the name even though no source
;; was recorded — an import or a runtime builtin.
(define (sources-lookup sources name bound?)
  (cond
    [(hash-ref sources name #f) => values]
    [(hash-ref (force prelude-index) name #f) => values]
    [bound? 'no-source]
    [else #f]))
