#lang racket/base

;; Rackton — instance impl-name contract.
;;
;; The names of the per-instance method implementations that codegen
;; emits (e.g. `$show:Maybe_Integer`, `$pure:StateT-Identity`) are a
;; contract shared between two stages of the pipeline:
;;
;;   - inference (`private/infer.rkt`) resolves a method use at a call
;;     site to the impl symbol it expects to reference, and
;;   - codegen (`private/codegen.rkt`) emits the `define` for that
;;     impl under the same symbol.
;;
;; If the two stages disagree on the symbol, dispatch silently binds
;; the wrong impl (or none).  Previously each stage carried its own
;; copy of these functions, kept aligned only by a "must agree
;; byte-for-byte" comment — a single point of failure spread across
;; two modules.  This module is the one place the contract lives;
;; both stages require it.  The `module+ test` below pins the exact
;; emitted strings so a change to the encoding is caught directly
;; rather than surfacing as a dispatch bug.

(provide head-fingerprint
         overlap-impl-symbol
         return-impl-symbol)

(require racket/match
         "types.rkt")

;; Deep fingerprint of a head argument, encoding nested ctors so e.g.
;; (Box Integer) → "Box_Integer" and (Box a) → "Box_*".  Used as the
;; impl-name suffix for instances in an overlap group so two
;; same-outer-ctor instances don't clobber each other.  Tvars render
;; as "*" — overlap means the specific instance always wins at compile
;; time for monomorphic call sites, and tvar positions in a
;; less-specific match show up as wildcards.
(define (head-fingerprint t)
  (match t
    [(tcon n) (symbol->string n)]
    [(tvar _) "*"]
    [(tapp h args)
     (string-append (head-fingerprint h)
                    (apply string-append
                           (for/list ([a (in-list args)])
                             (string-append "_" (head-fingerprint a)))))]
    [_ "*"]))

;; Impl name for an overlap-group instance, using a deep-fingerprint
;; of each head arg.  Encodes nested ctors deeply so two
;; same-outer-ctor overlap-group instances get distinct impl names.
(define (overlap-impl-symbol method-name head-arg-types)
  (string->symbol
   (format "$~a:~a"
           method-name
           (apply string-append
                  (let loop ([ts head-arg-types])
                    (cond
                      [(null? ts) '()]
                      [(null? (cdr ts)) (list (head-fingerprint (car ts)))]
                      [else (cons (head-fingerprint (car ts))
                                  (cons "-" (loop (cdr ts))))]))))))

;; Build the impl name for a return-typed method on an instance whose
;; head args are the type-constructor symbols `tcon-names`.  Unlike
;; `overlap-impl-symbol`, the args are already resolved tcon symbols
;; (fundep-determined and tvar positions having been dropped by the
;; caller), so they join directly without fingerprinting.
(define (return-impl-symbol method-name tcon-names)
  (string->symbol
   (format "$~a:~a"
           method-name
           (apply string-append
                  (let loop ([xs tcon-names])
                    (cond
                      [(null? xs) '()]
                      [(null? (cdr xs)) (list (symbol->string (car xs)))]
                      [else (cons (symbol->string (car xs))
                                  (cons "-" (loop (cdr xs))))]))))))

(module+ test
  (require rackunit)

  (test-case "head-fingerprint"
    (check-equal? (head-fingerprint (tcon 'Integer)) "Integer")
    (check-equal? (head-fingerprint (tvar 'a)) "*")
    (check-equal? (head-fingerprint (tapp (tcon 'Box) (list (tcon 'Integer))))
                  "Box_Integer")
    (check-equal? (head-fingerprint (tapp (tcon 'Box) (list (tvar 'a))))
                  "Box_*")
    (check-equal? (head-fingerprint
                   (tapp (tcon 'Either) (list (tcon 'String) (tcon 'Integer))))
                  "Either_String_Integer"))

  (test-case "overlap-impl-symbol — single head arg, fingerprinted"
    (check-equal? (overlap-impl-symbol 'show
                                       (list (tapp (tcon 'Maybe)
                                                   (list (tcon 'Integer)))))
                  '$show:Maybe_Integer)
    (check-equal? (overlap-impl-symbol 'eq (list (tcon 'Bool)))
                  '$eq:Bool))

  (test-case "overlap-impl-symbol — multiple head args joined by -"
    (check-equal? (overlap-impl-symbol 'convert
                                       (list (tcon 'Box) (tcon 'Int)))
                  '$convert:Box-Int))

  (test-case "return-impl-symbol — tcon symbols joined by -"
    (check-equal? (return-impl-symbol 'pure (list 'StateT 'Identity))
                  '$pure:StateT-Identity)
    (check-equal? (return-impl-symbol 'mempty (list 'List))
                  '$mempty:List))

  ;; The two encoders agree for the single-tcon case — the shared
  ;; prefix that made drift dangerous when these lived in two modules.
  (test-case "overlap and return encoders agree on plain tcons"
    (check-equal? (overlap-impl-symbol 'pure (list (tcon 'List)))
                  (return-impl-symbol 'pure (list 'List)))))
