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

(provide define-data-ctor define-tuple-ctor)

(require (for-syntax racket/base
                     syntax/parse
                     racket/syntax))

(require racket/match)

;; `(define-tuple-ctor name arity maker)` defines a constructor that is
;; backed by the tuple representation rather than its own struct: the
;; value binding `$val:name` builds a tuple via `maker` (the n-ary
;; `rackton-tuple-make`), and the match expander destructures the tuple's
;; underlying vector.  This is how `Pair` becomes the binary tuple — its
;; values are `Tuple`-tagged vectors, interchangeable with `(tuple a b)`.
;; `maker` is supplied by the caller so this module needn't depend on
;; prelude-runtime (which would be a cycle).
(define-syntax (define-tuple-ctor stx)
  (syntax-parse stx
    [(_ name:id arity:nat maker:id)
     (define n (syntax->datum #'arity))
     (define pat-ids (for/list ([i (in-range n)]) (format-id #'name "p~a" i)))
     (define arg-ids (for/list ([i (in-range n)]) (format-id #'name "a~a" i)))
     (with-syntax ([$val      (format-id #'name "$val:~a" #'name)]
                   [(pat ...) pat-ids]
                   [(arg ...) arg-ids])
       #'(begin
           (define ($val arg ...) (maker arg ...))
           (define-match-expander name
             (lambda (mstx)
               (syntax-parse mstx
                 [(_ pat ...) #'(vector pat ...)]))
             (lambda (estx)
               (syntax-parse estx
                 [_:id      #'$val]
                 [(_ arg ...) #'(maker arg ...)])))))]))

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
