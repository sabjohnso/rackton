#lang racket/base

;; The module language used by `#lang rackton` files.
;;
;; A file written as
;;
;;     #lang rackton
;;     (define-data (Maybe a) None (Some a))
;;     (define (from-maybe d m) (match m [(None) d] [(Some x) x]))
;;
;; is read by lang/reader.rkt into
;;
;;     (module name rackton/lang/runtime
;;       (rackton (define-data (Maybe a) None (Some a))
;;                (define (from-maybe d m) ...))
;;       (provide (all-defined-out)))
;;
;; This module re-exports the bindings that the surrounding module
;; expander needs (`#%module-begin` & friends, `provide`, `all-defined-out`)
;; together with the `rackton` macro itself.

(require "../main.rkt"
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
                    current-seconds))

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
 match

 ;; prelude classes, instances, ADTs, and combinators come from main.rkt
 (all-from-out "../main.rkt")

 ;; non-conflicting parts of racket/base, available inside `(racket …)`
 ;; escape bodies as well as in any user-emitted code
 (all-from-out racket/base))
