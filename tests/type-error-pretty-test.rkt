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
         ;; the aligned expected/got diagnostic block, exercised directly
         ;; (under a pinned width) for the wrap-and-align case below.
         (only-in "../private/infer.rkt" expected/got-block)
         ;; `pred` is the class-predicate struct from private/types.rkt;
         ;; the prelude also exports an `Enum` method named `pred`, so drop
         ;; the latter to keep importing both modules here.
         (except-in "../main.rkt" pred))

;; The end-to-end cases below assert only on type-message CONTENT (n-ary
;; arrow flattening, nice variable names, the "wrong type" headline) —
;; never on line wrapping — so they are independent of the ambient
;; terminal width.  The one case that does check wrapping and alignment
;; ("wide types wrap …") pins `current-type-columns` and exercises the
;; diagnostic block directly, rather than provoking the error through the
;; width-detecting elaborator.
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
  ;; A type too wide for the budget wraps, and the expected/got block
  ;; indents continuation lines under the value column (12).  This pins
  ;; `current-type-columns` directly rather than provoking the error
  ;; through the elaborator: the live REPL refreshes the width from the
  ;; terminal, so an end-to-end check would wrap or not depending on the
  ;; frame the suite runs in (e.g. a wide Emacs compilation buffer).
  (define wide
    (make-arrow
     (make-tapp (tcon 'Pair) (list (tcon 'String) t-int))
     (make-arrow
      (make-tapp (tcon 'List) (list (make-tapp (tcon 'Maybe) (list t-int))))
      (make-arrow
       (make-tapp (tcon 'Either) (list (tcon 'String) t-int))
       t-bool))))
  (define block
    (parameterize ([current-type-columns 66])
      (expected/got-block wide (make-arrow a a))))
  ;; The block carries both labels...
  (check-regexp-match #rx"  expected: " block)
  (check-regexp-match #rx"\n  got:      " block)
  ;; ...and a continuation line indented to align under the value column.
  (check-regexp-match #px"\n {12}" block))
