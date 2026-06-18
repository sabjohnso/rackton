#lang racket/base

;; GADT index refinement reaches a `match` in a TAIL position of a signed
;; function — an `if` branch or a `let`/`letrec` body — not only when the
;; match is the function's direct body.  The function's declared result
;; type is pushed (via current-expected-type) into the tail match so each
;; arm is checked against it under its own local index refinement, instead
;; of the arms being unified with one another (which fixes the scrutinee
;; index to the first arm's type and breaks the rest).
;;
;; When no result type reaches the match (no signature, or the match is in
;; a non-tail position such as a constructor argument), the match cannot be
;; principally typed; the error now carries an explicit GADT hint.

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

(define-syntax-rule (compile-error-message form ...)
  (with-handlers ([exn:fail? exn-message])
    (eval #'(rackton form ...)
          (variable-reference->namespace (#%variable-reference)))
    "compiled without error"))

(rackton
  (data Ty TInt TBool)
  (data Stack SNil (SCons Ty Stack))
  (data (Mem g a) (MZ : (Mem (SCons a g) a)) (MS : (-> (Mem g a) (Mem (SCons b g) a))))
  (data (STy a) (STyInt : (STy TInt)) (STyBool : (STy TBool)))
  (data (TypedVar g)
    (TVInt  : (-> (Mem g TInt)  (TypedVar g)))
    (TVBool : (-> (Mem g TBool) (TypedVar g))))

  ;; the refining match in an `if` branch (both branches are tail positions)
  (: hv-if (-> Boolean (STy a) (TypedVar (SCons a g))))
  (define (hv-if flag sty)
    (if flag
        (match sty [(STyInt) (TVInt MZ)] [(STyBool) (TVBool MZ)])
        (match sty [(STyInt) (TVInt MZ)] [(STyBool) (TVBool MZ)])))

  ;; the refining match in a `let` body
  (: hv-let (-> (STy a) (TypedVar (SCons a g))))
  (define (hv-let sty)
    (let ([ignore 0])
      (match sty [(STyInt) (TVInt MZ)] [(STyBool) (TVBool MZ)])))

  ;; observe which constructor each produced
  (: tag (-> (TypedVar g) Integer))
  (define (tag tv) (match tv [(TVInt _) 0] [(TVBool _) 1]))

  (: r-if-int Integer)   (define r-if-int   (tag (hv-if #t STyInt)))
  (: r-if-bool Integer)  (define r-if-bool  (tag (hv-if #f STyBool)))
  (: r-let-int Integer)  (define r-let-int  (tag (hv-let STyInt)))
  (: r-let-bool Integer) (define r-let-bool (tag (hv-let STyBool))))

(test-case "GADT refinement reaches a match in an if-branch tail position"
  (check-equal? r-if-int 0)
  (check-equal? r-if-bool 1))

(test-case "GADT refinement reaches a match in a let-body tail position"
  (check-equal? r-let-int 0)
  (check-equal? r-let-bool 1))

(test-case "a GADT match with no result type to check against errors with a GADT hint"
  (define msg
    (compile-error-message
     (data Ty TInt TBool)
     (data Stack SNil (SCons Ty Stack))
     (data (Mem g a) (MZ : (Mem (SCons a g) a)) (MS : (-> (Mem g a) (Mem (SCons b g) a))))
     (data (STy a) (STyInt : (STy TInt)) (STyBool : (STy TBool)))
     (data (TypedVar g)
       (TVInt  : (-> (Mem g TInt)  (TypedVar g)))
       (TVBool : (-> (Mem g TBool) (TypedVar g))))
     ;; no signature: nothing pins the result, so the arms collide
     (define (head-var sty)
       (match sty [(STyInt) (TVInt MZ)] [(STyBool) (TVBool MZ)]))))
  (check-regexp-match #rx"GADT match" msg))

(test-case "a non-GADT arm-type conflict does not get the GADT hint"
  ;; matching Some-of-Integer against Some-of-String is a genuine type
  ;; error, NOT a GADT refinement — the hint must not misfire here.
  (define msg
    (compile-error-message
     (define (f x)
       (match x
         [(Some 1)   10]
         [(Some "a") 20]))))
  (check-true (not (regexp-match? #rx"GADT match" msg))))
