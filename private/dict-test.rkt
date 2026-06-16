#lang racket/base

;; Tests for private/dict.rkt: the runtime class-method dispatch
;; mechanism — value→tag mapping, the generic-method dispatcher (with
;; currying and the no-instance error), instance registration,
;; return-typed lookup, and the `define/curried` helper.

(module+ test
  (require rackunit
           "dict.rkt"
           "adt.rkt")   ; define-data-ctor, for a real `$ctor:` struct

  ;; ----- dispatch-tag: value -> type tag -----------------------------

  (test-case "dispatch-tag maps primitives to their type tags"
    (check-equal? (dispatch-tag 5)        'Integer)
    (check-equal? (dispatch-tag 1/2)      'Rational)   ; exact non-integer
    (check-equal? (dispatch-tag 3.14)     'Float)
    (check-equal? (dispatch-tag 1.0+2.0i) 'Complex)      ; inexact non-real
    (check-equal? (dispatch-tag 1+2i)     'ComplexExact) ; exact non-real
    (check-equal? (dispatch-tag #t)       'Boolean)
    (check-equal? (dispatch-tag "s")      'String)
    (check-equal? (dispatch-tag #\a)      'Char)
    (check-equal? (dispatch-tag #"by")    'Bytes))

  (define-data-ctor TBox 1)
  (test-case "dispatch-tag of an ADT value is its $ctor struct name"
    (check-equal? (dispatch-tag (TBox 5)) '$ctor:TBox))

  (test-case "dispatch-tag errors on an untaggable value"
    ;; A vector is no longer untaggable — it is the tuple representation
    ;; (tag 'Tuple).  A box has no Rackton type, so it stays untaggable.
    (check-exn exn:fail? (lambda () (dispatch-tag (box 1)))))

  ;; ----- generic method: register + dispatch -------------------------

  (define $eq (make-hasheq))
  (define-class-method m $eq 0 2)   ; method m, dispatch on arg 0, arity 2
  (register-instance-method! $eq 'Integer (lambda (x y) (+ x y)))
  (register-instance-method! $eq 'String  (lambda (x y) (string-append x y)))

  (test-case "generic dispatches on the arg at the dispatch position"
    (check-equal? (m 3 4)     7)
    (check-equal? (m "a" "b") "ab"))

  (test-case "generic supports partial (curried) application"
    (check-equal? ((m 3) 4) 7)
    (check-equal? ((m "a") "b") "ab"))

  (test-case "generic raises when no instance is registered for the tag"
    (check-exn exn:fail? (lambda () (m 1.5 2.5))))   ; no Float instance

  ;; Dispatch on a non-zero position.
  (define $snd (make-hasheq))
  (define-class-method m2 $snd 1 2)   ; dispatch on arg 1
  (register-instance-method! $snd 'String (lambda (a b) b))
  (test-case "generic dispatches on a non-zero position"
    (check-equal? (m2 99 "picked-by-second-arg") "picked-by-second-arg"))

  ;; ----- return-typed lookup -----------------------------------------

  (define $ret (make-hasheq))
  (register-instance-method! $ret 'Maybe 'pure:Maybe)
  (test-case "lookup-return-method finds a registered impl by tag"
    (check-equal? (lookup-return-method $ret 'Maybe 'pure) 'pure:Maybe))
  (test-case "lookup-return-method errors for an unregistered tag"
    (check-exn exn:fail? (lambda () (lookup-return-method $ret 'List 'pure))))

  ;; ----- define/curried ----------------------------------------------

  (define/curried (add3 a b c) (+ a b c))
  (test-case "define/curried accepts full arity and any shorter prefix"
    (check-equal? (add3 1 2 3)     6)   ; full
    (check-equal? ((add3 1) 2 3)   6)   ; 1 + 2
    (check-equal? ((add3 1 2) 3)   6)   ; 2 + 1
    (check-equal? (((add3 1) 2) 3) 6))  ; fully curried

  (define/curried (idc x) x)
  (test-case "define/curried on arity 1 is a plain definition"
    (check-equal? (idc 42) 42)))
