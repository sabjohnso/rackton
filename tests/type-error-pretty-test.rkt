#lang racket/base

;; Readable type-error pretty-printing:
;;   - curried binary arrows are flattened to n-ary (-> A B C)
;;   - long types wrap and align across lines
;;   - free type variables get nice single-letter names, shared across
;;     the expected/got pair of a mismatch
;;
;; Two layers are exercised: the pure datum/string formatter in
;; private/types.rkt, and the end-to-end error messages produced by the
;; inferencer.

(require rackunit
         (for-syntax racket/base)
         "../private/types.rkt"
         ;; `pred` is the class-predicate struct from private/types.rkt;
         ;; the prelude also exports an `Enum` method named `pred`, so drop
         ;; the latter to keep importing both modules here.
         (except-in "../main.rkt" pred))

;; The end-to-end cases below assert on WRAPPED type output, so the wrap
;; width must be fixed.  They provoke errors via top-level `eval`, where
;; the `rackton` expander auto-detects the terminal width — and under
;; `raco test` in a terminal the test process has a real tty.  Pinning
;; COLUMNS (which detection prefers) makes the budget a deterministic 66
;; regardless of how the suite is launched.
(putenv "COLUMNS" "79")

(define-syntax-rule (rackton-error form ...)
  (with-handlers ([exn:fail? exn-message])
    (eval #'(rackton form ...)
          (variable-reference->namespace (#%variable-reference)))))

;; ---- unit: pure formatter ------------------------------------------

(define a (tvar 'a0))
(define b (tvar 'b7))

;; (-> Integer (-> Integer Boolean))  ==>  (-> Integer Integer Boolean)
(define curried3
  (make-arrow t-int (make-arrow t-int t-bool)))

(test-case "type->pretty-datum flattens curried arrows to n-ary"
  (check-equal? (type->pretty-datum curried3)
                '(-> Integer Integer Boolean)))

(test-case "type->pretty-datum leaves non-arrow applications alone"
  (check-equal? (type->pretty-datum (make-tapp (tcon 'List) (list t-int)))
                '(List Integer)))

(test-case "format-pretty-datum renders a datum to a string"
  (check-equal? (format-pretty-datum '(-> Integer Integer Boolean))
                "(-> Integer Integer Boolean)"))

(test-case "format-types shares one renaming across several types"
  ;; a0 and b7 are distinct internal names; rendered together they must
  ;; map to distinct nice names, consistently across both results.
  (define shared (make-arrow a (make-tapp (tcon 'List) (list a))))
  (define other  (make-arrow a (make-tapp (tcon 'Maybe) (list a))))
  (define rs (format-types (list shared other)))
  ;; The same internal var `a0` reads as the same letter in both.
  (check-equal? (car rs)  "(-> a (List a))")
  (check-equal? (cadr rs) "(-> a (Maybe a))"))

(test-case "format-type renames internal fresh names to single letters"
  (define t (make-arrow b b))
  (check-equal? (format-type t) "(-> a a)"))

;; ---- end to end: inference error messages --------------------------

(test-case "function-type mismatch flattens arrows in the message"
  (define msg
    (rackton-error
     (define g (ann (lambda (x) x)
                    (-> (-> Integer Integer Boolean)
                        (List Integer)
                        (Maybe String))))))
  ;; n-ary arrow appears...
  (check-regexp-match #rx"\\(-> Integer Integer Boolean\\)" msg)
  ;; ...and the nested curried form does NOT.
  (check-false (regexp-match? #rx"\\(-> Integer \\(-> Integer" msg)))

(test-case "no internal fresh tvar names leak into the message"
  (define msg
    (rackton-error
     (define g (ann (lambda (x) x) (-> Integer String)))))
  ;; got is the identity (-> a a); reject any letter+digits fresh name.
  (check-false (regexp-match? #rx"got:.*[a-z][0-9]" msg)))

(test-case "instance method-body mismatch is pretty-printed"
  ;; A wrong Functor instance for Pair — the body returns (Pair a f)
  ;; instead of (Pair a (f b)).  The diagnostic must not leak internal
  ;; fresh names (a0/a3/b4) and must flatten the curried spine.
  (define msg
    (rackton-error
     (instance (Functor (Pair a))
       (define (fmap f (Pair a b))
         (Pair a f)))))
  (check-regexp-match #rx"method fmap body has the wrong type" msg)
  ;; aligned expected/got block
  (check-regexp-match #rx"\n  expected: " msg)
  (check-regexp-match #rx"\n  got: +" msg)
  ;; no internal fresh names like a0, a3, b4
  (check-false (regexp-match? #rx"[a-z][0-9]" msg))
  ;; the expected type's curried spine is flattened: the two Pair
  ;; results sit side by side, not chained through another binary ->.
  (check-false (regexp-match? #rx"\\(Pair a a\\) \\(-> " msg)))

(test-case "definition/declaration mismatch uses the aligned block"
  (define msg
    (rackton-error
     (: foo (-> Integer String))
     (define (foo x) x)))
  (check-regexp-match #rx"definition of foo has the wrong type" msg)
  (check-regexp-match #rx"\n  expected: " msg)
  (check-regexp-match #rx"\n  got: +" msg))

(test-case "wide types wrap with aligned continuation lines"
  (define msg
    (rackton-error
     (define g (ann (lambda (x) x)
                    (-> (Pair String Integer)
                        (List (Maybe Integer))
                        (Either String Integer)
                        Boolean)))))
  ;; A continuation line indented to align under the value column.
  (check-regexp-match #px"\n {12}" msg))
