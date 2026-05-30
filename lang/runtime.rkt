#lang racket/base

;; The module language used by `#lang rackton` files.
;;
;; A file written as
;;
;;     #lang rackton
;;     (data (Maybe a) None (Some a))
;;     (define (from-maybe d m) (match m [(None) d] [(Some x) x]))
;;
;; is read by lang/reader.rkt into
;;
;;     (module name rackton/lang/runtime
;;       (rackton (data (Maybe a) None (Some a))
;;                (define (from-maybe d m) ...))
;;       (provide (all-defined-out)))
;;
;; This module re-exports the bindings that the surrounding module
;; expander needs (`#%module-begin` & friends, `provide`, `all-defined-out`)
;; together with the `rackton` macro itself.

(require ;; main.rkt re-exports `#%module-begin` as its custom
         ;; auto-wrapping `rackton-module-begin`.  We want
         ;; `#lang rackton` files to use racket/base's plain
         ;; `#%module-begin` instead — the reader has already
         ;; wrapped the body in (rackton/main …), so a normal
         ;; module-begin is what's wanted.  Excluding main.rkt's
         ;; rename here is what keeps `(require racket/base)` below
         ;; from raising "identifier already required".
         (except-in "../main.rkt" #%module-begin)
         ;; Re-export the racket/base bindings that don't conflict with
         ;; Rackton's own prelude.  This lets a host-language escape
         ;; body (`(racket τ (vars) body)`) use common Racket forms
         ;; like `let`, `cond`, `read`, `displayln`, etc. — anything
         ;; from racket/base that isn't shadowed by Rackton.
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
                    delete-file make-directory directory-list
                    current-seconds
                    char-upcase char-downcase
                    char-alphabetic? char-numeric? char-whitespace?
                    char->integer integer->char
                    string-ref string->list
                    bytes-length bytes-ref bytes-append
                    bytes->list list->bytes make-bytes
                    bytes->string/utf-8 string->bytes/utf-8
                    string
                    void when unless
                    exp log sin cos tan
                    numerator denominator
                    real-part imag-part magnitude))

(provide
 ;; the macro that does the real work
 rackton

 ;; module-language essentials
 #%module-begin
 #%datum
 #%app
 #%top
 #%top-interaction
 provide
 require
 all-defined-out
 only-in

 ;; runtime support so the rackton macro's emitted code resolves
 define-data-ctor
 define-class-method
 register-instance-method!
 lookup-return-method
 match

 ;; prelude classes, instances, ADTs, and combinators come from main.rkt
 (all-from-out "../main.rkt")

 ;; non-conflicting parts of racket/base, available inside `(racket …)`
 ;; escape bodies as well as in any user-emitted code
 ;; `sort` excluded — it reaches here via #lang racket/base and now
 ;; lives in rackton/data/list (Phase 2 slim); leaving it would collide
 ;; with that module in any #lang rackton file that requires it.
 ;; (Other Data.List additions avoid racket/base names — e.g. `empty?`,
 ;; `fold-left` — so racket/base's `null?` / `foldl` stay usable here.)
 (except-out (all-from-out racket/base) sort))
