#lang racket/base

;; Rackton — public entry point.
;;
;; This module serves three usages:
;;
;;   ;; 1. Embed Rackton code inside a regular Racket module via the
;;   ;;    (rackton ...) macro form:
;;   (require rackton)
;;   (rackton
;;     (data (Maybe a) None (Some a))
;;     (: from-just (-> a (-> (Maybe a) a)))
;;     (define (from-just d m)
;;       (match m [(None) d] [(Some x) x])))
;;
;;   ;; 2. Use rackton as a module language with the `module` form.
;;   ;;    Bodies are auto-wrapped in (rackton/main ...) and every
;;   ;;    definition is auto-provided:
;;   (module example rackton
;;     (: x Integer)
;;     (define x 3))
;;
;;   ;; 3. The same shape as (2), reached via the file-level reader:
;;   #lang rackton
;;   (define x 3)
;;
;; The supported subset covers:
;;   literals (Integer / Boolean / String),
;;   lambda / application, let, if, ascription, match,
;;   define / declare (:) / data,
;;   Hindley–Milner inference with let-polymorphism,
;;   ADTs with pattern matching.

(require "private/elaborate.rkt"
         "private/adt.rkt"
         "private/dict.rkt"
         "private/prelude-runtime.rkt"
         ;; Identifier stubs for surface forms, type constructors,
         ;; classes, and return-typed methods that have no other
         ;; Racket-level binding.  Re-exported below so Scribble's
         ;; (for-label rackton) can resolve cross-references to their
         ;; reference-doc entries.
         "private/lang-bindings.rkt"
         ;; `==` is excluded (it is Rackton's Eq method).  `match-let` is
         ;; excluded so Racket's macro does not leak in as a binding:
         ;; Rackton no longer has a match-let form — destructuring now
         ;; lives in `let` / `where` bindings (see private/surface.rkt).
         (except-in racket/match == match-let)
         (for-syntax racket/base)
         ;; Explicit re-require of the racket/base names we expose for
         ;; (racket ...) escape bodies and module-form modules.  The
         ;; except-in list mirrors lang/runtime.rkt: every name that
         ;; the Rackton prelude shadows is excluded, so re-exporting
         ;; (all-from-out racket/base) below can't collide with the
         ;; (all-from-out "private/prelude-runtime.rkt") line.
         (except-in racket/base
                    + - * < > <= >=
                    not and or
                    length reverse append sort foldr filter
                    substring string-length string-append
                    modulo quotient abs min max
                    number->string string->number
                    read-line print println
                    file-exists? sqrt compose
                    random getenv path->string
                    delete-file make-directory directory-list copy-file
                    peek-byte
                    current-seconds
                    char-upcase char-downcase
                    char-alphabetic? char-numeric? char-whitespace?
                    char->integer integer->char
                    symbol->string string->symbol
                    string-ref string->list
                    bytes-length bytes-ref bytes-append
                    bytes->list list->bytes make-bytes
                    bytes->string/utf-8 string->bytes/utf-8
                    string
                    void when unless
                    exp log sin cos tan
                    numerator denominator
                    real-part imag-part magnitude))

(provide rackton
         rackton/main

         ;; runtime support exposed for the macro's output
         define-data-ctor
         define-class-method
         register-instance-method!
         lookup-return-method
         match

         ;; module-language essentials so this module can serve as the
         ;; LANG in `(module name rackton form ...)`.  The custom
         ;; #%module-begin auto-wraps user bodies in (rackton/main ...)
         ;; and auto-provides every definition — the same shape that
         ;; `#lang rackton` produces via the reader.
         (rename-out [rackton-module-begin #%module-begin])

         ;; non-conflicting parts of racket/base — available inside
         ;; module-form modules and inside (racket ...) escapes.
         ;; #%module-begin is excepted because we provide our own
         ;; (custom-wrapping) version above.
         ;; `sort` is excluded here too: it is a racket/base name that
         ;; reaches this module via the #lang racket/base language import
         ;; (the explicit except-in above only covers the explicit
         ;; require), and it now lives in rackton/data/list — leaving it
         ;; in would collide with that module for any client of both.
         ;; (Other Data.List additions use non-racket/base names — e.g.
         ;; `empty?`, `fold-left` — precisely so racket/base's `null?` /
         ;; `foldl` stay available for `(racket …)` escapes.)
         (except-out (all-from-out racket/base) #%module-begin sort copy-file peek-byte)

         ;; prelude — class methods, ADTs, and combinators.  Names that
         ;; have been slimmed out to stdlib modules (which foreign-import
         ;; them from prelude-runtime directly) are excluded here so they
         ;; are not auto-available — users must require the stdlib module.
         (except-out (all-from-out "private/prelude-runtime.rkt")
                     ;; internal IO-action constructor (used by
                     ;; private/ffi-runtime); never user-facing
                     $io
                     ;; rackton/control/stm
                     new-tvar read-tvar write-tvar retry or-else atomically
                     stm-fmap stm-pure stm-ap stm-bind
                     ;; rackton/control/concurrent
                     fork-io wait-thread new-mvar new-empty-mvar take-mvar
                     put-mvar read-mvar modify-mvar new-chan send-chan recv-chan
                     ;; rackton/system
                     make-ref read-ref write-ref
                     read-file write-file file-exists? delete-file
                     make-directory list-directory
                     try raise-io
                     random-integer random-float current-time-seconds
                     getenv argv
                     append-file does-directory-exist? get-current-directory
                     get-prog-name set-env
                     rename-file copy-file-io create-directory-if-missing
                     get-current-time-millis get-cpu-time-millis
                     ;; rackton/system/exit
                     exit-with-code
                     ;; rackton/system/io
                     stdin stdout stderr open-file-with-mode h-close
                     h-put-str h-put-str-ln h-flush h-get-contents
                     h-get-line
                     ;; rackton/numeric/show (float formatters)
                     show-f-float show-e-float show-g-float)

         ;; Surface-form / type-ctor / class / return-typed-method
         ;; stubs.  Bound so (for-label rackton) in scribble docs can
         ;; resolve @racket[protocol], @racket[Maybe], @racket[Eq],
         ;; @racket[pure], etc. to their reference entries.
         (all-from-out "private/lang-bindings.rkt"))

(define-syntax (rackton-module-begin stx)
  (syntax-case stx ()
    [(_ form ...)
     ;; Wrap user forms in (rackton/main ...) so the elaborator runs
     ;; type-checking and emits the rackton-schemes sidecar.  Exports
     ;; are driven by the user's (provide ...) forms inside the body —
     ;; with no provide form, nothing escapes.  Same shape that
     ;; `#lang rackton` produces via the reader.
     #'(#%plain-module-begin
        (rackton/main form ...))]))

(module+ test
  (require rackunit)
  (rackton
    (define (id x) x)
    (define (compose f g) (lambda (x) (f (g x))))

    (: fact (-> Integer Integer))
    (define (fact n)
      (if (= n 0) 1 (* n (fact (- n 1)))))

    (data (Maybe a) None (Some a))

    (: from-maybe (-> a (-> (Maybe a) a)))
    (define (from-maybe d m)
      (match m
        [(None)   d]
        [(Some x) x]))

    (: map-maybe (-> (-> a b) (-> (Maybe a) (Maybe b))))
    (define (map-maybe f m)
      (match m
        [(None)   None]
        [(Some x) (Some (f x))])))

  (check-equal? (id 42) 42)
  (check-equal? ((compose (lambda (n) (* n 2)) (lambda (n) (+ n 1))) 5) 12)
  (check-equal? (fact 5)  120)
  (check-equal? (fact 6)  720)
  (check-equal? (from-maybe 0 None) 0)
  (check-equal? (from-maybe 0 (Some 7)) 7)
  (check-equal? (map-maybe (lambda (n) (* n n)) (Some 4)) (Some 16))
  (check-equal? (map-maybe (lambda (n) (* n n)) None) None))
