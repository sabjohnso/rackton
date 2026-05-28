#lang racket/base

;; WriterT transformer.
;;
;; (ExceptT was originally bundled but dropped after surfacing that
;; its `flatmap` Err branch needs an inner `pure` that the class-method
;; dispatch wrapper can't yet carry as a dict arg.  See the phase
;; notes for the deferral.)

(require rackunit
         "../main.rkt")

(rackton
  ;; ----- WriterT String IO ----------------------------------
  ;; Accumulates a String log alongside an IO action.

  (: logged-greeting (WriterT String IO Integer))
  (define logged-greeting
    (do [_ <- (tell "hello, ")]
        [_ <- (tell "world")]
      (pure 42)))

  (: greeting-result (IO (Pair String Integer)))
  (define greeting-result (run-writer-t logged-greeting))

  ;; ----- WriterT (List Integer) over IO ---------------------
  ;; Accumulates a Cons-list log.

  (: trace-step (-> Integer (WriterT (List Integer) IO Unit)))
  (define (trace-step n) (tell (Cons n Nil)))

  (: traced-counter (WriterT (List Integer) IO Integer))
  (define traced-counter
    (do [_ <- (trace-step 1)]
        [_ <- (trace-step 2)]
        [_ <- (trace-step 3)]
      (pure 6)))

  (: traced-result (IO (Pair (List Integer) Integer)))
  (define traced-result (run-writer-t traced-counter))

  ;; ----- WriterT over Maybe — exercises the inner-pure dict
  ;; with a non-IO inner monad ---------------------------------
  (: maybe-pair (WriterT String Maybe Integer))
  (define maybe-pair
    (do [_ <- (tell "step-1 ")]
        [_ <- (tell "step-2")]
      (pure 7)))

  (: maybe-pair-result (Maybe (Pair String Integer)))
  (define maybe-pair-result (run-writer-t maybe-pair))

  ;; ----- eval / exec drop one side --------------------------
  (: only-log (IO String))
  (define only-log (exec-writer-t logged-greeting))

  (: only-val (IO Integer))
  (define only-val (eval-writer-t logged-greeting))

  ;; ----- lift-writer-t round-trip ---------------------------
  (: hoisted (WriterT String IO Integer))
  (define hoisted (lift-writer-t (pure-io 100)))

  (: hoisted-result (IO (Pair String Integer)))
  (define hoisted-result (run-writer-t hoisted)))

;; ---------- assertions ----------------------------------

(test-case "WriterT String IO: concatenates the log"
  (check-equal? (run-io greeting-result)
                (MkPair "hello, world" 42)))

(test-case "WriterT (List Integer) IO: appends the list log"
  (check-equal? (run-io traced-result)
                (MkPair (Cons 1 (Cons 2 (Cons 3 Nil))) 6)))

(test-case "WriterT String Maybe: works with a non-IO inner monad"
  (check-equal? maybe-pair-result
                (Some (MkPair "step-1 step-2" 7))))

(test-case "exec-writer-t drops the value"
  (check-equal? (run-io only-log) "hello, world"))

(test-case "eval-writer-t drops the log"
  (check-equal? (run-io only-val) 42))

(test-case "lift-writer-t starts with empty log"
  (check-equal? (run-io hoisted-result)
                (MkPair "" 100)))
