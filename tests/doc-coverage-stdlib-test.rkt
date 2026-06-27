#lang racket/base

;; tests/doc-coverage-stdlib-test.rkt — every public export of every
;; standard-library module must be documented in the reference.
;;
;; The sibling doc-coverage-test enforces this for the prelude (the
;; `rackton` main module); this one extends it to each importable stdlib
;; module under rackton/{data,control,system,text,numeric,unit}.
;;
;; "Documented" means: a @defproc / @defform / @defidform / @defthing /
;; @defmodule entry for the name exists somewhere in
;; scribblings/reference/ — checked by text-grep, the same definition of
;; coverage the prelude test uses.

(require rackunit
         racket/list
         racket/file
         racket/path)

;; ----- the stdlib modules to check (leaf modules only) --------------
;; Aggregators (rackton/unit, rackton/system, rackton/batteries,
;; rackton/control/monad/trans) only re-export these leaves, so they are
;; covered transitively and are intentionally omitted.

(define stdlib-modules
  '(;; data
    rackton/data/bits rackton/data/bool rackton/data/char
    rackton/data/complex rackton/data/either rackton/data/result
    rackton/data/foldable
    rackton/data/function rackton/data/functor rackton/data/lazy
    rackton/data/nestream rackton/data/istream
    rackton/data/arrow-lazy
    rackton/data/lens rackton/data/list rackton/data/list/nonempty
    rackton/data/map rackton/data/maybe rackton/data/monoid
    rackton/data/ord rackton/data/ratio rackton/data/semigroup
    rackton/data/set rackton/data/traversable rackton/data/tuple
    ;; control
    rackton/control/applicative rackton/control/concurrent
    rackton/control/monad rackton/control/monad/except
    rackton/control/monad/reader rackton/control/monad/state
    rackton/control/monad/writer rackton/control/stm
    ;; numeric
    rackton/numeric/conversions rackton/numeric/integer
    rackton/numeric/natural rackton/numeric/real rackton/numeric/show
    ;; system
    rackton/system/directory rackton/system/environment
    rackton/system/exception rackton/system/exit rackton/system/file
    rackton/system/io rackton/system/random rackton/system/ref
    rackton/system/time
    ;; text
    rackton/text/bytes rackton/text/printf rackton/text/read
    rackton/text/show rackton/text/string
    ;; network
    rackton/network/tcp rackton/network/udp
    ;; unit
    rackton/unit/check rackton/unit/gen rackton/unit/laws
    rackton/unit/lazy rackton/unit/prng rackton/unit/property
    rackton/unit/tree))

;; ----- introspection (mirrors doc-coverage-test) -------------------

(define (module-exports modpath)
  (define-values (vals stxs) (module->exports modpath))
  (define (collect bucket)
    (apply append (for/list ([phase-row (in-list bucket)])
                    (for/list ([entry (in-list (cdr phase-row))])
                      (car entry)))))
  (append (collect vals) (collect stxs)))

(define (list->seteq xs)
  (for/hasheq ([x (in-list xs)]) (values x #t)))

(define base-export-set    (list->seteq (module-exports 'racket/base)))
(define prelude-export-set
  (begin (dynamic-require 'rackton #f)
         (list->seteq (module-exports 'rackton))))

;; A name needs its own stdlib entry unless it is a racket/base name, a
;; `$`-prefixed runtime impl identifier, or a prelude name re-exported by
;; the module (documented with the prelude).
(define (needs-own-doc? sym)
  (define name (symbol->string sym))
  (and (not (hash-ref base-export-set sym #f))
       (not (hash-ref prelude-export-set sym #f))
       (not (regexp-match? #px"^\\|?\\$" name))))

;; ----- documented-name extraction (mirrors doc-coverage-test) ------

(define reference-dir
  (build-path (path-only (collection-file-path "main.rkt" "rackton"))
              "scribblings" "reference"))

(define def-form-regexp
  (pregexp
   (string-append
    "@def(?:proc|proc\\*|form|form\\*|form/none|idform|thing|param|module)"
    "\\*?\\["
    "(?:\\s*#:[A-Za-z-]+\\s+(?:\"[^\"]*\"|\\([^)]*\\)|[^\\s\\[\\]()]+))*"
    "\\s*[\\[\\(]*"
    "([^\\s()\\[\\]]+)")))

(define documented-name-set
  (let ([set (make-hasheq)])
    (for ([p (in-list (directory-list reference-dir #:build? #t))]
          #:when (regexp-match? #rx"\\.scrbl$" (path->string p)))
      (define contents (file->string p))
      (for ([m (in-list (regexp-match* def-form-regexp contents
                                       #:match-select cdr))])
        (hash-set! set (string->symbol (car m)) #t)))
    set))

(define (documented? name) (hash-ref documented-name-set name #f))

;; ----- the test ----------------------------------------------------

(test-case "every stdlib module export is documented in the reference"
  (define problems
    (for/list ([mod (in-list stdlib-modules)]
               #:do [(dynamic-require mod #f)
                     (define missing
                       (sort (filter (lambda (n)
                                       (and (needs-own-doc? n)
                                            (not (documented? n))))
                                     (module-exports mod))
                             string<? #:key symbol->string))]
               #:unless (null? missing))
      (cons mod missing)))
  (unless (null? problems)
    (for ([p (in-list problems)])
      (printf "~a: ~a undocumented\n  ~a\n"
              (car p) (length (cdr p)) (cdr p))))
  (check-equal? problems '()
                "some stdlib module exports are not documented"))
