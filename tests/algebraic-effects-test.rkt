#lang racket/base

;; Algebraic effects + handlers.
;;
;; (define-effect E (op argTypes... -> resultType) ...) declares an
;; effect E with named operations.  Calling an operation suspends
;; execution and transfers control to the nearest enclosing
;; (handle EXPR [op (args ...) k body] ... [return v body]).
;; The handler may resume by invoking k with a value of the op's
;; result type.
;;
;; Untyped row: effects are not tracked in types.  An operation
;; called outside its handler is a runtime error.

(require rackunit
         "../main.rkt")

(rackton
  ;; ----- 55.A State effect: counter ----------------------------
  (define-effect Counter
    (peek -> Integer)
    (bump -> Unit))

  ;; The handler takes the program AS A THUNK so the effect ops
  ;; aren't performed until the body runs INSIDE the handle.
  (: run-counter (-> Integer (-> (-> Integer) (Pair Integer Integer))))
  (define (run-counter s0 prog)
    (handle (prog)
      [peek () k        -> (MkPair s0 s0)]
      [bump () k        -> (MkPair s0 (+ s0 1))]
      [return v         -> (MkPair v s0)]))

  (: prog-peek (-> Integer))
  (define (prog-peek) (peek))

  (: r-peek (Pair Integer Integer))
  (define r-peek (run-counter 42 prog-peek))

  ;; ----- 55.B Reader effect ------------------------------------
  (define-effect Env
    (ask -> Integer))

  (: run-env (-> Integer (-> (-> Integer) Integer)))
  (define (run-env val prog)
    (handle (prog)
      [ask () k         -> (k val)]
      [return v         -> v]))

  (: prog-env (-> Integer))
  (define (prog-env) (+ (ask) (ask)))

  (: r-env Integer)
  (define r-env (run-env 7 prog-env))

  ;; ----- 55.C Exception effect ---------------------------------
  (define-effect Exn
    (raise-e -> Integer))

  (: run-catch (-> Integer (-> (-> Integer) Integer)))
  (define (run-catch fallback prog)
    (handle (prog)
      [raise-e () k     -> fallback]
      [return v         -> v]))

  (: prog-no-raise (-> Integer))
  (define (prog-no-raise) 42)

  (: prog-raise (-> Integer))
  (define (prog-raise) (raise-e))

  (: r-no-raise Integer)
  (define r-no-raise (run-catch 99 prog-no-raise))

  (: r-raise Integer)
  (define r-raise (run-catch 99 prog-raise))

  ;; helper to extract first of a pair without using Racket-side match
  (: first-of (-> (Pair Integer Integer) Integer))
  (define (first-of p) (match p [(MkPair v _) v])))

;; ----- assertions ------------------------------------------------

(test-case "Counter effect: peek returns initial state"
  (check-equal? (first-of r-peek) 42))

(test-case "Env effect: ask twice resumes both times"
  (check-equal? r-env 14))

(test-case "Exception effect: no raise returns value"
  (check-equal? r-no-raise 42))

(test-case "Exception effect: raise returns fallback"
  (check-equal? r-raise 99))
