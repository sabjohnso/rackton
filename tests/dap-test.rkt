#lang racket/base

;; End-to-end test of the DAP adapter: the debugger review's
;; proof-of-concept (Review-Debugger.org) replayed through the
;; protocol.  A client drives `run-dap-loop` over pipes: launch the
;; recursive-fact program, set a breakpoint on the recursive branch,
;; and observe five stops with `n` counting down — then the
;; breakpoint-mapping helpers as plain functions.
;;
;; Requires the `drracket` package (gui-debugger) — the same runtime
;; prerequisite the adapter itself documents.

(require rackunit
         racket/file
         racket/list
         json
         "../private/dap.rkt"
         (only-in "../private/lsp.rkt" read-lsp-message write-lsp-message))

;; ----- a DAP client harness -------------------------------------------

(define-values (adapter-in client-out) (make-pipe))
(define-values (client-in adapter-out) (make-pipe))

(define adapter-thread
  (thread (lambda () (run-dap-loop adapter-in adapter-out))))

(define seq-counter (box 0))
(define (next-seq!) (set-box! seq-counter (add1 (unbox seq-counter)))
  (unbox seq-counter))

(define (send! command [arguments (hasheq)])
  (write-lsp-message client-out
                     (hasheq 'seq (next-seq!) 'type "request"
                             'command command 'arguments arguments))
  (unbox seq-counter))

;; Read one message, failing the test run instead of hanging.
(define (recv!)
  (define ch (make-channel))
  (thread (lambda () (channel-put ch (read-lsp-message client-in))))
  (define msg (sync/timeout 60 ch))
  (unless msg (error 'dap-test "timed out waiting for a message"))
  (when (eof-object? msg) (error 'dap-test "adapter closed the stream"))
  msg)

;; Read until a response to `req-seq` arrives; collect events seen.
(define (await-response req-seq)
  (let loop ([events '()])
    (define m (recv!))
    (if (and (equal? (hash-ref m 'type #f) "response")
             (equal? (hash-ref m 'request_seq #f) req-seq))
        (values m (reverse events))
        (loop (cons m events)))))

(define (await-event name)
  (let loop ()
    (define m (recv!))
    (if (and (equal? (hash-ref m 'type #f) "event")
             (equal? (hash-ref m 'event #f) name))
        m
        (loop))))

;; ----- the program under debug ------------------------------------------

(define dir (make-temporary-file "rackton-dap-~a" 'directory))
(define prog (build-path dir "fact.rkt"))
(call-with-output-file prog #:exists 'truncate
  (lambda (out)
    (for ([l (in-list '("#lang rackton"
                        "(provide main)"
                        "(: fact (-> Integer Integer))"
                        "(define (fact n)"
                        "  (if (== n 0)"
                        "      1"
                        "      (* n (fact (- n 1)))))"
                        "(: main Integer)"
                        "(define main (fact 5))"))])
      (displayln l out))))

;; ----- the session ----------------------------------------------------------

(test-case "initialize and launch"
  (define-values (r _es) (await-response (send! "initialize")))
  (check-true (hash-ref r 'success))
  (check-true (hash-ref (hash-ref r 'body) 'supportsConfigurationDoneRequest))
  (define-values (r2 _es2)
    (await-response (send! "launch"
                           (hasheq 'program (path->string prog)))))
  (check-true (hash-ref r2 'success))
  (void (await-event "initialized")))

(test-case "breakpoints verify against breakable positions"
  (define-values (r _es)
    (await-response
     (send! "setBreakpoints"
            (hasheq 'source (hasheq 'path (path->string prog))
                    'breakpoints (list (hasheq 'line 7)
                                       (hasheq 'line 3))))))
  (check-true (hash-ref r 'success))
  (define bps (hash-ref (hash-ref r 'body) 'breakpoints))
  (check-true (hash-ref (first bps) 'verified)
              "line 7 (the recursive branch) is breakable")
  (check-false (hash-ref (second bps) 'verified)
               "line 3 (an erased signature) is not"))

(test-case "five stops with n counting down, then termination"
  (define-values (r _es) (await-response (send! "configurationDone")))
  (check-true (hash-ref r 'success))
  (define ns
    (for/list ([i (in-range 5)])
      (void (await-event "stopped"))
      ;; one thread
      (define-values (tr _e1) (await-response (send! "threads")))
      (check-equal? (length (hash-ref (hash-ref tr 'body) 'threads)) 1)
      ;; top frame is the breakpoint line
      (define-values (st _e2)
        (await-response (send! "stackTrace" (hasheq 'threadId 1))))
      (define frames (hash-ref (hash-ref st 'body) 'stackFrames))
      (check-equal? (hash-ref (first frames) 'line) 7)
      ;; locals contain n
      (define-values (sc _e3)
        (await-response
         (send! "scopes" (hasheq 'frameId (hash-ref (first frames) 'id)))))
      (define vref (hash-ref (first (hash-ref (hash-ref sc 'body) 'scopes))
                             'variablesReference))
      (define-values (vs _e4)
        (await-response (send! "variables" (hasheq 'variablesReference vref))))
      (define n-var
        (for/first ([v (in-list (hash-ref (hash-ref vs 'body) 'variables))]
                    #:when (equal? (hash-ref v 'name) "n"))
          (hash-ref v 'value)))
      (define-values (cr _e5)
        (await-response (send! "continue" (hasheq 'threadId 1))))
      (check-true (hash-ref cr 'success))
      n-var))
  (check-equal? ns '("5" "4" "3" "2" "1")
                "the recursion's n at each breakpoint stop")
  (void (await-event "terminated")))

;; ----- stepping (a fresh session) -------------------------------------

(define-values (adapter-in2 client-out2) (make-pipe))
(define-values (client-in2 adapter-out2) (make-pipe))
(define adapter-thread2
  (thread (lambda () (run-dap-loop adapter-in2 adapter-out2))))

(define (send2! command [arguments (hasheq)])
  (write-lsp-message client-out2
                     (hasheq 'seq (next-seq!) 'type "request"
                             'command command 'arguments arguments))
  (unbox seq-counter))

(define recv2-tag (box "start"))
(define (recv2!)
  (define ch (make-channel))
  (thread (lambda () (channel-put ch (read-lsp-message client-in2))))
  (define msg (sync/timeout 60 ch))
  (unless msg (error 'dap-test "timed out (session 2) at: ~a" (unbox recv2-tag)))
  (when (eof-object? msg) (error 'dap-test "stream closed (session 2)"))
  msg)

(define (await-response2 req-seq)
  (set-box! recv2-tag (format "response ~a" req-seq))
  (let loop ()
    (define m (recv2!))
    (if (and (equal? (hash-ref m 'type #f) "response")
             (equal? (hash-ref m 'request_seq #f) req-seq))
        m
        (loop))))

(define (await-event2 name)
  (set-box! recv2-tag (format "event ~a" name))
  (let loop ()
    (define m (recv2!))
    (if (and (equal? (hash-ref m 'type #f) "event")
             (equal? (hash-ref m 'event #f) name))
        m
        (loop))))

(test-case "stepIn stops again with reason step"
  (void (await-response2 (send2! "initialize")))
  (void (await-response2 (send2! "launch"
                                 (hasheq 'program (path->string prog)))))
  (void (await-event2 "initialized"))
  (void (await-response2
         (send2! "setBreakpoints"
                 (hasheq 'source (hasheq 'path (path->string prog))
                         'breakpoints (list (hasheq 'line 5))))))
  (void (await-response2 (send2! "configurationDone")))
  (define first-stop (await-event2 "stopped"))
  (check-equal? (hash-ref (hash-ref first-stop 'body) 'reason) "breakpoint")
  (void (await-response2 (send2! "stepIn" (hasheq 'threadId 1))))
  (define step-stop (await-event2 "stopped"))
  (check-equal? (hash-ref (hash-ref step-stop 'body) 'reason) "step")
  ;; Run to completion: continue only in reaction to a stop, so the
  ;; response-await (which discards events) can never swallow the
  ;; terminated event.
  (void (await-response2 (send2! "continue" (hasheq 'threadId 1))))
  (set-box! recv2-tag "run-to-completion")
  (let loop ()
    (define m (recv2!))
    (cond
      [(and (equal? (hash-ref m 'type #f) "event")
            (equal? (hash-ref m 'event #f) "terminated")) (void)]
      [(and (equal? (hash-ref m 'type #f) "event")
            (equal? (hash-ref m 'event #f) "stopped"))
       (void (await-response2 (send2! "continue" (hasheq 'threadId 1))))
       (set-box! recv2-tag "run-to-completion")
       (loop)]
      [else (loop)]))
  (void (await-response2 (send2! "disconnect")))
  (check-true (and (sync/timeout 30 adapter-thread2) #t)))

(test-case "disconnect ends the loop"
  (define-values (r _es) (await-response (send! "disconnect")))
  (check-true (hash-ref r 'success))
  (check-true (and (sync/timeout 30 adapter-thread) #t)
              "the adapter loop returns"))

;; ----- pure helpers -----------------------------------------------------------

(test-case "line→position mapping picks breakable positions per line"
  (define pos->line (hash 106 7 111 7 79 5 159 9))
  (define breakable '(79 106 111 159))
  (check-equal? (breakpoint-position 7 breakable pos->line) 106
                "the first breakable position on the line")
  (check-equal? (breakpoint-position 3 breakable pos->line) #f))

(delete-directory/files dir)
