#lang racket/base

;; Rackton — algebraic-data-type runtime.
;;
;; `(define-data-ctor name arity)` emits, for a constructor `name`:
;;   - a transparent struct `$ctor:name` with `arity` fields
;;   - a value binding `$val:name`:
;;       * for arity 0, the singleton instance
;;       * for arity > 0, the struct constructor procedure
;;   - a match expander `name` whose
;;       * pattern transformer accepts exactly `arity` sub-patterns
;;         and rewrites to a struct pattern
;;       * expression transformer rewrites a bare reference to the
;;         value binding, and an application to a direct struct call
;;
;; Constructor identifiers therefore work both as expressions and as
;; match patterns, mirroring Coalton/Haskell ergonomics.

(provide define-data-ctor)

(require (for-syntax racket/base
                     syntax/parse
                     racket/syntax))

(require racket/match)

(define-syntax (define-data-ctor stx)
  (syntax-parse stx
    [(_ name:id arity:nat)
     (define n (syntax->datum #'arity))
     (define field-ids
       (for/list ([i (in-range n)])
         (format-id #'name "f~a" i)))
     (define pat-ids
       (for/list ([i (in-range n)])
         (format-id #'name "p~a" i)))
     (define arg-ids
       (for/list ([i (in-range n)])
         (format-id #'name "a~a" i)))
     (with-syntax ([$ctor  (format-id #'name "$ctor:~a" #'name)]
                   [$ctor? (format-id #'name "$ctor:~a?" #'name)]
                   [$val   (format-id #'name "$val:~a" #'name)]
                   [(field ...) field-ids]
                   [(pat ...)   pat-ids]
                   [(arg ...)   arg-ids])
       (cond
         [(zero? n)
          #'(begin
              (struct $ctor () #:transparent)
              (define $val ($ctor))
              (define-match-expander name
                (lambda (mstx)
                  (syntax-parse mstx
                    [(_) #'(? $ctor?)]))
                (lambda (estx)
                  (syntax-parse estx
                    [_:id #'$val]))))]
         [else
          #'(begin
              (struct $ctor (field ...) #:transparent)
              (define $val $ctor)
              (define-match-expander name
                (lambda (mstx)
                  (syntax-parse mstx
                    [(_ pat ...) #'($ctor pat ...)]))
                (lambda (estx)
                  (syntax-parse estx
                    [_:id #'$val]
                    [(_ arg ...) #'($ctor arg ...)]))))]))]))
