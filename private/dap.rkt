#lang racket/base

;; Rackton — the DAP (Debug Adapter Protocol) kernel.
;;
;; Debugs a `#lang rackton` program by annotating its fully-expanded
;; form with DrRacket's debugger machinery (gui-debugger) — viable
;; because codegen preserves user srclocs end to end; the live
;; verification is in Review-Debugger.org.  gui-debugger ships in the
;; `drracket` package, loaded here by dynamic-require: a documented
;; runtime prerequisite, not a package dependency.
;;
;; Shape: `run-dap-loop` reads requests from one port and writes
;; responses/events to another (the same Content-Length framing as
;; the LSP — the codec is shared).  The debuggee runs in its own
;; thread; a break that should stop posts a "stopped" event and
;; blocks on a resume channel until continue/step.  Debuggee output
;; is redirected into "output" events so it cannot corrupt the
;; protocol stream.
;;
;; v1 verbs: initialize, launch, setBreakpoints (validated against
;; the file-sourced breakable positions the annotator reports),
;; configurationDone, threads, stackTrace, scopes, variables,
;; continue, next, stepIn, stepOut, disconnect.  Stepping compares
;; stack depths, so a step over a proper tail call behaves like the
;; tail call itself: no extra frame, the step lands where the
;; control flow actually goes.

(provide run-dap-loop
         breakpoint-position)

(require racket/match
         racket/list
         racket/path
         syntax/modread
         (only-in "lsp.rkt" read-lsp-message write-lsp-message))

;; ----- pure helpers ------------------------------------------------------

;; The breakable position chosen for a requested line: the first (in
;; position order) whose line matches; #f when the line has none —
;; the breakpoint is then reported unverified.
(define (breakpoint-position line breakable pos->line)
  (for/first ([p (in-list (sort breakable <))]
              #:when (eqv? (hash-ref pos->line p #f) line))
    p))

;; ----- the gui-debugger boundary --------------------------------------------

;; All gui-debugger access goes through this one dynamically-loaded
;; record, so the adapter fails with instructions — not a crash —
;; when the prerequisite is missing.
(struct debugger-api (annotate debug-key mark-source mark-bindings
                      binding-id binding-value))

(define (load-debugger-api)
  (define (need mod sym)
    (dynamic-require
     mod sym
     (lambda ()
       (error 'rackton/dap
              "the debugger needs the gui-debugger collection; install it with: raco pkg install drracket"))))
  (debugger-api (need 'gui-debugger/annotator 'annotate-for-single-stepping)
                (need 'gui-debugger/marks 'debug-key)
                (need 'gui-debugger/marks 'mark-source)
                (need 'gui-debugger/marks 'mark-bindings)
                (need 'gui-debugger/marks 'mark-binding-binding)
                (need 'gui-debugger/marks 'mark-binding-value)))

;; ----- session ---------------------------------------------------------------

(struct session
  (send!            ; jsexpr → void (enqueued to the single writer)
   api-box          ; debugger-api, after launch
   program-box      ; path
   namespace-box    ; the debuggee's namespace
   annotated-box    ; annotated module syntax
   breakable-box    ; (listof position) — file-sourced only
   pos->line-box    ; hash position → line
   active-box       ; hash position → #t (current breakpoints)
   resume-ch        ; debuggee blocks here when stopped
   frames-box       ; (cons debug-info marks-list) while stopped, else #f
   step-box         ; #f | (cons mode depth-at-resume)
   debuggee-box))   ; thread | #f

(define (make-session send!)
  (session send! (box #f) (box #f) (box #f) (box #f) (box '())
           (box (hash)) (box (hash)) (make-channel) (box #f) (box #f)
           (box #f)))

;; ----- the loop ---------------------------------------------------------------

(define (run-dap-loop in out)
  (define out-ch (make-channel))
  (define writer
    (thread
     (lambda ()
       (let loop ([seq 1])
         (define m (channel-get out-ch))
         (unless (eq? m 'quit)
           (write-lsp-message out (hash-set m 'seq seq))
           (loop (add1 seq)))))))
  (define s (make-session (lambda (m) (channel-put out-ch m))))
  (let loop ()
    (define msg (read-lsp-message in))
    (cond
      [(eof-object? msg) (void)]
      [(handle-request! s msg) (void)]   ; #t = disconnect
      [else (loop)]))
  (let ([dbg (unbox (session-debuggee-box s))])
    (when (and dbg (thread-running? dbg)) (kill-thread dbg)))
  (channel-put out-ch 'quit)
  (thread-wait writer))

;; ----- request handling ---------------------------------------------------------

;; Returns #t when the session should end (disconnect).
(define (handle-request! s msg)
  (define cmd (hash-ref msg 'command ""))
  (define args (hash-ref msg 'arguments (hasheq)))
  (define (respond body #:success [ok #t] #:message [m #f])
    ((session-send! s)
     (let ([r (hasheq 'type "response"
                      'request_seq (hash-ref msg 'seq 0)
                      'command cmd
                      'success ok
                      'body body)])
       (if m (hash-set r 'message m) r))))
  (define (event name [body (hasheq)])
    ((session-send! s) (hasheq 'type "event" 'event name 'body body)))
  (define (guarded thunk)
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (respond (hasheq) #:success #f
                                #:message (exn-message e))
                       #f)])
      (thunk)))
  (match cmd
    ["initialize"
     (respond (hasheq 'supportsConfigurationDoneRequest #t))
     #f]
    ["launch"
     (guarded
      (lambda ()
        (prepare! s (string->path (hash-ref args 'program)))
        (respond (hasheq))
        (event "initialized")
        #f))]
    ["setBreakpoints"
     (define requested
       (for/list ([b (in-list (hash-ref args 'breakpoints '()))])
         (hash-ref b 'line)))
     (define chosen
       (for/list ([line (in-list requested)])
         (breakpoint-position line
                              (unbox (session-breakable-box s))
                              (unbox (session-pos->line-box s)))))
     (set-box! (session-active-box s)
               (for/hash ([p (in-list chosen)] #:when p) (values p #t)))
     (respond (hasheq 'breakpoints
                      (for/list ([line (in-list requested)]
                                 [p (in-list chosen)])
                        (hasheq 'verified (and p #t) 'line line))))
     #f]
    ["configurationDone"
     (respond (hasheq))
     (start-debuggee! s event)
     #f]
    ["threads"
     (respond (hasheq 'threads (list (hasheq 'id 1 'name "main"))))
     #f]
    ["stackTrace"
     (respond (stack-trace s))
     #f]
    ["scopes"
     (respond (hasheq 'scopes
                      (list (hasheq 'name "Locals"
                                    'variablesReference
                                    (add1 (hash-ref args 'frameId 0))
                                    'expensive #f))))
     #f]
    ["variables"
     (respond (hasheq 'variables
                      (frame-variables s (sub1 (hash-ref args
                                                         'variablesReference
                                                         1)))))
     #f]
    [(or "continue" "next" "stepIn" "stepOut")
     (define mode (match cmd
                    ["continue" #f]
                    ["next" 'next] ["stepIn" 'step-in] ["stepOut" 'step-out]))
     (set-box! (session-step-box s)
               (and mode (cons mode (current-stop-depth s))))
     (respond (if (equal? cmd "continue")
                  (hasheq 'allThreadsContinued #t)
                  (hasheq)))
     ;; Fire-and-forget: only meaningful when the debuggee is waiting.
     (when (unbox (session-frames-box s))
       (thread (lambda () (channel-put (session-resume-ch s) 'go))))
     #f]
    ["disconnect"
     (respond (hasheq))
     #t]
    [_
     (respond (hasheq) #:success #f
              #:message (format "unsupported command: ~a" cmd))
     #f]))

;; ----- preparing the debuggee -----------------------------------------------------

(define (prepare! s program)
  (define api (load-debugger-api))
  (define ns (make-base-namespace))
  (define raw
    (call-with-input-file program
      (lambda (in)
        (port-count-lines! in)
        (with-module-reading-parameterization
         (lambda () (read-syntax program in))))))
  (define expanded
    (parameterize ([current-namespace ns]) (expand raw)))
  ;; positions → lines, user file only
  (define pos->line (make-hash))
  (let walk ([stx expanded])
    (when (syntax? stx)
      (when (and (equal? (syntax-source stx) program)
                 (syntax-position stx) (syntax-line stx))
        (hash-set! pos->line (syntax-position stx) (syntax-line stx)))
      (let inner ([e (syntax-e stx)])
        (cond [(pair? e) (inner (car e)) (inner (cdr e))]
              [(syntax? e) (walk e)]
              [else (void)]))))
  (define-values (annotated breakable)
    ((debugger-api-annotate api)
     expanded
     (lambda (src)
       (if (equal? src program)
           (lambda (pos) (break-now? s pos))
           (lambda (pos) #f)))
     (lambda (debug-info marks) (on-break! s debug-info marks) #f)
     (lambda (debug-info marks . vals) (apply values vals))
     (lambda args (void))
     (lambda args (void))))
  (set-box! (session-api-box s) api)
  (set-box! (session-program-box s) program)
  (set-box! (session-namespace-box s) ns)
  (set-box! (session-annotated-box s) annotated)
  (set-box! (session-pos->line-box s)
            (for/hash ([(k v) (in-hash pos->line)]) (values k v)))
  (set-box! (session-breakable-box s)
            (filter (lambda (p) (hash-ref pos->line p #f)) breakable)))

;; Consulted (compiled into the annotation) before every expression.
(define (break-now? s pos)
  (or (hash-ref (unbox (session-active-box s)) pos #f)
      (and (unbox (session-step-box s)) #t)))

;; Stack depth helpers — depth compares drive next/stepOut.
(define (marks-of s marks)
  (define api (unbox (session-api-box s)))
  (continuation-mark-set->list marks (debugger-api-debug-key api)))

(define (current-stop-depth s)
  (define f (unbox (session-frames-box s)))
  (if f (length (cdr f)) 0))

;; The debuggee side of a break: decide whether to stop; when
;; stopping, stash the frames, announce, and block until resumed.
(define (on-break! s debug-info marks)
  (define frames (marks-of s marks))
  (define pos
    (let* ([api (unbox (session-api-box s))]
           [src ((debugger-api-mark-source api) debug-info)])
      (syntax-position src)))
  (define at-breakpoint?
    (hash-ref (unbox (session-active-box s)) pos #f))
  (define stop?
    (or at-breakpoint?
        (match (unbox (session-step-box s))
          [#f #f]
          [(cons 'step-in _) #t]
          [(cons 'next d) (<= (length frames) d)]
          [(cons 'step-out d) (< (length frames) d)])))
  (when stop?
    (set-box! (session-step-box s) #f)
    (set-box! (session-frames-box s) (cons debug-info frames))
    ((session-send! s)
     (hasheq 'type "event" 'event "stopped"
             'body (hasheq 'reason (if at-breakpoint? "breakpoint" "step")
                           'threadId 1
                           'allThreadsStopped #t)))
    (void (channel-get (session-resume-ch s)))
    (set-box! (session-frames-box s) #f)))

;; ----- running -------------------------------------------------------------------

(define (start-debuggee! s event)
  (define ns (unbox (session-namespace-box s)))
  (define program (unbox (session-program-box s)))
  (define modname
    (string->symbol
     (path->string (path-replace-extension (file-name-from-path program)
                                           #""))))
  (set-box!
   (session-debuggee-box s)
   (thread
    (lambda ()
      (parameterize ([current-namespace ns]
                     [current-output-port (output-event-port s "stdout")]
                     [current-error-port (output-event-port s "stderr")])
        (with-handlers ([exn:fail?
                         (lambda (e)
                           (event "output"
                                  (hasheq 'category "stderr"
                                          'output (string-append
                                                   (exn-message e) "\n"))))])
          (eval (unbox (session-annotated-box s)))
          (namespace-require `(quote ,modname))))
      (event "exited" (hasheq 'exitCode 0))
      (event "terminated")))))

;; Debuggee prints become "output" events — the protocol stream stays
;; clean.
(define (output-event-port s category)
  (define-values (pin pout) (make-pipe))
  (thread
   (lambda ()
     (let loop ()
       (define line (read-line pin 'any))
       (unless (eof-object? line)
         ((session-send! s)
          (hasheq 'type "event" 'event "output"
                  'body (hasheq 'category category
                                'output (string-append line "\n"))))
         (loop)))))
  pout)

;; ----- inspecting the stop ----------------------------------------------------------

(define (stack-trace s)
  (define f (unbox (session-frames-box s)))
  (cond
    [(not f) (hasheq 'stackFrames '() 'totalFrames 0)]
    [else
     (define api (unbox (session-api-box s)))
     (define program (unbox (session-program-box s)))
     (define frames
       (for/list ([m (in-list (cons (car f) (cdr f)))] [i (in-naturals)])
         (define src ((debugger-api-mark-source api) m))
         (hasheq 'id i
                 'name (frame-name src)
                 'source (hasheq 'path (path->string program))
                 'line (or (syntax-line src) 0)
                 'column (add1 (or (syntax-column src) 0)))))
     (hasheq 'stackFrames frames 'totalFrames (length frames))]))

(define (frame-name src)
  (define d (syntax->datum src))
  (format "~a" (if (pair? d) (car d) d)))

(define (frame-variables s idx)
  (define f (unbox (session-frames-box s)))
  (cond
    [(not f) '()]
    [else
     (define api (unbox (session-api-box s)))
     (define all (cons (car f) (cdr f)))
     (cond
       [(or (< idx 0) (>= idx (length all))) '()]
       [else
        (define m (list-ref all idx))
        (define seen (make-hash))
        (for*/list ([b (in-list ((debugger-api-mark-bindings api) m))]
                    [name (in-value (symbol->string
                                     (syntax-e
                                      ((debugger-api-binding-id api) b))))]
                    #:unless (hash-ref seen name #f))
          (hash-set! seen name #t)
          (hasheq 'name name
                  'value (with-handlers ([exn:fail? (lambda (_) "#<unavailable>")])
                           (format "~s" ((debugger-api-binding-value api) b)))
                  'variablesReference 0))])]))
