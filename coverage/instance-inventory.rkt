#lang racket/base

;; Coverage Phase 1 — authoritative instance inventory (Coverage.org).
;;
;; Enumerates every type-class instance the type system actually knows,
;; from the SOURCE OF TRUTH rather than a grep over source text: the
;; prelude's `instance-table` (built by inferring the prelude program at
;; load) plus each shipped module's `rackton-schemes` sidecar — the same
;; serialized `rackton-instances` that cross-module inference consumes
;; (see private/infer.rkt's importer and private/scheme-codec.rkt's
;; `decode-instance-info`).
;;
;; The result is the DENOMINATOR for the coverage matrix: the deduplicated
;; set of (class, type-head) cells, each tagged with where it was first
;; seen.  No coverage signal yet — that is Phase 2.
;;
;; Run:   racket coverage/instance-inventory.rkt
;; Reuse: (require "coverage/instance-inventory.rkt") then
;;        (collect-instance-records) / (collect-instance-cells).

(require racket/list
         racket/path
         racket/file
         racket/runtime-path
         (only-in "../private/prelude.rkt" prelude-env)
         "../private/env.rkt"
         "../private/types.rkt"
         "../private/scheme-codec.rkt")

(provide collect-instance-records   ; -> (listof (list class type source))
         collect-instance-cells)    ; -> (listof (cons class type)), deduped

(define-runtime-path pkg-root "..")

;; Shipped library directories that define Rackton instances.  Mirrors
;; the Phase 0 scan scope (prelude + these) so the counts are comparable;
;; tests/ and examples/ are excluded (their instances are test-local).
(define stdlib-dirs '("control" "data" "numeric" "text" "system" "foreign"))

;; The head constructor name of a type:
;;   Integer    -> 'Integer
;;   (Maybe a)  -> 'Maybe
;;   (->)       -> '->
(define (type-head-name t)
  (cond
    [(tcon? t) (tcon-name t)]
    [(tapp? t) (type-head-name (tapp-head t))]
    [(tvar? t) (tvar-name t)]
    [else '?]))

;; Is a type rooted at a concrete constructor (not a bare variable)?
(define (concrete-head? t)
  (cond
    [(tcon? t) #t]
    [(tapp? t) (concrete-head? (tapp-head t))]
    [else #f]))

;; The class's PRIMARY type among a pred's arguments: the first argument
;; whose head is a concrete constructor.  A multi-parameter class often
;; leads with a bare type variable — e.g. `(MonadState s (StateT s m))`
;; has args `s` (variable) then `(StateT s m)` — and the type that names
;; the cell is the concrete one (`StateT`), reached via the `m -> s`
;; functional dependency.  Falls back to the first argument if none is
;; concrete (a fully general / blanket instance over a variable).
(define (primary-type args)
  (cond
    [(null? args) '|<nullary>|]
    [(findf concrete-head? args) => type-head-name]
    [else (type-head-name (car args))]))

;; One instance-info -> a record (list class type source).
(define (ii->record class-name ii source)
  (list class-name
        (primary-type (pred-args (instance-info-head ii)))
        source))

;; ----- prelude instances: read straight off the instance-table --------

(define (prelude-records)
  (for*/list ([(cls iis) (in-hash (env-instance-table prelude-env))]
              [ii (in-list iis)])
    (ii->record cls ii "prelude")))

;; ----- stdlib instances: decode each module's sidecar -----------------

(define (rkt-files-under dir)
  (if (directory-exists? dir)
      (filter (lambda (p)
                (and (path-has-extension? p #".rkt")
                     (not (regexp-match? #rx"compiled" (path->string p)))))
              (find-files file-exists? dir))
      '()))

(define (stdlib-files)
  (append-map (lambda (d) (rkt-files-under (build-path pkg-root d)))
              stdlib-dirs))

(define (module-records path)
  (define abspath (path->string (simplify-path path)))
  (define submod `(submod (file ,abspath) rackton-schemes))
  (define insts
    (with-handlers ([exn:fail? (lambda (_) '())])
      (dynamic-require submod 'rackton-instances)))
  (define rel (path->string (find-relative-path (simplify-path pkg-root)
                                                (simplify-path path))))
  (for/list ([entry (in-list insts)])
    (define decoded (decode-instance-info entry))   ; (class . instance-info)
    (ii->record (car decoded) (cdr decoded) rel)))

;; ----- combine + dedup to one record per (class, type) cell -----------
;; First-seen wins, and the prelude is scanned first, so a cell defined
;; in the prelude is attributed to "prelude" even when a stdlib sidecar
;; re-carries it (inherited through that module's own requires).

(define (collect-instance-records)
  (define raw (append (prelude-records)
                      (append-map module-records (stdlib-files))))
  (define seen (make-hash))
  (for/list ([r (in-list raw)]
             #:unless (hash-has-key? seen (cons (first r) (second r))))
    (hash-set! seen (cons (first r) (second r)) #t)
    r))

(define (collect-instance-cells)
  (for/list ([r (in-list (collect-instance-records))])
    (cons (first r) (second r))))

;; ----- report ---------------------------------------------------------

(module+ main
  (define records (collect-instance-records))
  (define by-class (make-hash))
  (define by-type  (make-hash))
  (for ([r (in-list records)])
    (hash-update! by-class (first r)  add1 0)
    (hash-update! by-type  (second r) add1 0))

  (printf "Authoritative instance inventory (Coverage Phase 1)\n")
  (printf "===================================================\n")
  (printf "total instance cells: ~a\n" (length records))
  (printf "distinct protocols:   ~a\n" (hash-count by-class))
  (printf "distinct types:       ~a\n\n" (hash-count by-type))

  (define (dump title h)
    (printf "## by ~a\n" title)
    (for ([p (in-list (sort (hash->list h) > #:key cdr))])
      (printf "~a  ~a\n" (~r3 (cdr p)) (car p)))
    (printf "\n"))
  (dump "protocol" by-class)
  (dump "type"     by-type)

  (printf "## flat inventory (class  type  source)\n")
  (for ([r (in-list (sort records cell<?))])
    (printf "~a\t~a\t~a\n" (first r) (second r) (third r))))

;; right-justify a small count in 3 columns
(define (~r3 n)
  (define s (number->string n))
  (string-append (make-string (max 0 (- 3 (string-length s))) #\space) s))

;; A sanity guard so the instrument fails loudly if the instance-table /
;; sidecar shape changes underneath it (rather than silently emitting a
;; truncated denominator).  Not an exhaustive check — a floor, a ceiling,
;; and a few cells the standard library must always have.
(module+ test
  (require rackunit)
  (define cells (collect-instance-cells))
  (check-true (> (length cells) 150)
              (format "inventory unexpectedly small: ~a" (length cells)))
  (check-true (< (length cells) 400)
              (format "inventory unexpectedly large: ~a" (length cells)))
  (for ([c (in-list '((Eq . Integer) (Functor . Maybe) (Monad . List)
                      (Comonad . Identity) (MonadState . StateT)
                      (Bifunctor . Pair) (Arrow . ->)))])
    (check-not-false (member c cells) (format "missing expected cell: ~a" c))))

;; sort records by class then type, by name
(define (cell<? a b)
  (define ca (symbol->string (first a))) (define cb (symbol->string (first b)))
  (if (string=? ca cb)
      (string<? (format "~a" (second a)) (format "~a" (second b)))
      (string<? ca cb)))
