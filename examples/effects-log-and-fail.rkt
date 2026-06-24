#lang rackton

;; effects-log-and-fail.rkt — typed algebraic effects with rackton/effects.
;;
;; A checked-division pipeline that LOGS each step (the Writer effect) and may
;; THROW on divide-by-zero (the Except effect).  The TYPE of each computation
;; records exactly which effects it uses; the handlers discharge them one at a
;; time; and `run-eff` will only run a computation whose row is EMPTY — so you
;; cannot forget to handle an effect.
;;
;; Note the explicit `ebind` chaining: a graded monad's bind changes the row
;; type, so `Eff` is not a `Monad` instance and `do`-notation is unavailable.
;; `weaken` lets the two branches of an `if` agree on a row.
;;
;; Run it with `racket examples/effects-log-and-fail.rkt`.

(require rackton/effects)

;; checked division: log the operation, throw on a zero divisor.
(: checked-div (-> Integer Integer (Eff (EffRow Present Present) Integer)))
(define (checked-div a b)
  (ebind (tell (string-append "div " (string-append (integer->string a)
                  (string-append " " (integer->string b)))))
         (lambda (u)
           (if (== b 0)
               (throw "division by zero")
               (with-except (epure (div a b)))))))

;; two pipelines: 100/5/2 succeeds; 100/5/0 throws on the second step.
(: pipeline-ok (Eff (EffRow Present Present) Integer))
(define pipeline-ok
  (ebind (checked-div 100 5) (lambda (h) (checked-div h 2))))

(: pipeline-bad (Eff (EffRow Present Present) Integer))
(define pipeline-bad
  (ebind (checked-div 100 5) (lambda (h) (checked-div h 0))))

;; discharge BOTH effects (Except, then Writer), then run.
(: run-prog (-> (Eff (EffRow Present Present) Integer)
                (Pair (Either String Integer) (List String))))
(define (run-prog p) (run-eff (handle-writer (handle-except p))))

;; ===== formatting ==================================================
(: outcome->str (-> (Either String Integer) String))
(define (outcome->str r)
  (match r
    [(Right v) (string-append "ok: " (integer->string v))]
    [(Left e)  (string-append "error: " e)]))

(: log->str (-> (List String) String))
(define (log->str xs)
  (foldr (lambda (s acc) (string-append "\n    log| " (string-append s acc))) "" xs))

(: report (-> String (Eff (EffRow Present Present) Integer) (IO Unit)))
(define (report label p)
  (match (run-prog p)
    [(Pair r log)
     (println (string-append label
                (string-append (outcome->str r) (log->str log))))]))

(: main Unit)
(define main
  (run-io
   (do [_ <- (println "Typed algebraic effects (rackton/effects): log + may-fail")]
       [_ <- (println "")]
       [_ <- (report "100 / 5 / 2  =>  " pipeline-ok)]
       [_ <- (println "")]
       [_ <- (report "100 / 5 / 0  =>  " pipeline-bad)]
       [_ <- (println "")]
       [_ <- (println "Both effects are in each pipeline's TYPE; run-eff accepts")]
       [_ <- (println "only the empty row, so a forgotten handler is a type error.")]
     (pure Unit))))
