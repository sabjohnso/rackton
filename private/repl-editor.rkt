#lang racket/base

;; Rackton — expeditor-backed input layer for the standalone REPL.
;;
;; Wraps Racket's expression editor (expeditor) behind a four-function
;; interface so the REPL loop in repl.rkt never touches expeditor
;; directly: open / read / close, plus the two pure functions expeditor
;; consults while editing.  Only expeditor's documented public API is
;; used — no private modules (project rule).
;;
;; The editor gives the REPL multi-line entries that are editable as a
;; whole (arrow keys move anywhere; Return accepts only when
;; `rackton-editor-ready?` says the entry is complete; Esc-Return
;; inserts a newline mid-form), history navigation including
;; prefix/contains search (Esc-p / Esc-P on the text before the
;; cursor), s-expression motion commands, and syntax coloring via the
;; standard Racket lexer.
;;
;; Public API:
;;   rackton-editor-open       — history list → editor handle, or #f
;;                               (not a terminal / unrecognized)
;;   rackton-editor-read       — editor → one datum (commands included)
;;                               or eof
;;   rackton-editor-close      — editor → updated history list
;;   rackton-editor-read-datum — port → datum; the entry reader
;;   rackton-editor-ready?     — port → boolean; the accept test

(provide rackton-editor-open
         rackton-editor-read
         rackton-editor-close
         rackton-editor-sync-completions!
         rackton-editor-read-datum
         rackton-editor-ready?
         rackton-lexer
         rackton-history-path
         rackton-history-load
         rackton-history-save!
         make-completion-namespace
         completion-namespace-sync!
         completion-namespace-ns)

(require expeditor
         ;; The documented configuration API: key binding and the
         ;; editing commands custom bindings compose from.
         (only-in (submod expeditor configure)
                  expeditor-bind-key!
                  make-ee-insert-string
                  ee-forward-exp)
         (only-in racket/port port->string)
         (only-in racket/file make-directory*)
         (only-in racket/set seteq set-member?)
         (only-in syntax-color/racket-lexer racket-lexer)
         "repl-input.rkt")

;; ----- the entry reader -------------------------------------------

;; Convert one accepted entry into a datum.  A leading comma marks a
;; REPL command (`,type EXPR`, …): the whole remaining entry goes
;; through `rackton-parse-command-line`, because a command like
;; `,type EXPR` is two datums that a plain `read` would split.
;; Anything else is one `read`; a whitespace-only entry reads as eof.
(define (rackton-editor-read-datum in)
  (skip-whitespace in)
  (if (eqv? (peek-char in) #\,)
      (rackton-parse-command-line (port->string in))
      (read in)))

;; ----- the accept test --------------------------------------------

;; Decide whether Return accepts the entry or inserts a newline.
;; The rule matches expeditor's default — accept once every datum
;; reads, keep editing on an unterminated form — except that comma
;; commands are recognized: a bare `,` accepts (it is the kernel's
;; no-op), and a command's arguments must each read completely.
;; A malformed-but-closed entry (e.g. mismatched delimiters) accepts,
;; so the kernel reports the read error instead of trapping the user
;; in the editor.
(define (rackton-editor-ready? in)
  (skip-whitespace in)
  (cond
    [(eof-object? (peek-char in)) #f]      ; empty entry — keep editing
    [(eqv? (peek-char in) #\,)
     (read-char in)                        ; the command word and args follow
     (datums-read-to-eof? in)]
    [else (datums-read-to-eof? in)]))

;; #t when reading datum-by-datum reaches eof, #f when the input ends
;; mid-form (`exn:fail:read:eof`).  Any other read error counts as
;; ready — accepting lets the kernel produce the error message.
(define (datums-read-to-eof? in)
  (with-handlers ([exn:fail:read:eof? (lambda (_) #f)]
                  [exn:fail:read?     (lambda (_) #t)])
    (let loop ()
      (if (eof-object? (read in)) #t (loop)))))

(define (skip-whitespace in)
  (let loop ()
    (define c (peek-char in))
    (when (and (char? c) (char-whitespace? c))
      (read-char in)
      (loop))))

;; ----- history persistence ----------------------------------------

;; History lives in the user's preferences directory as a written list
;; of strings, most recent first — the same order expeditor passes it
;; around in.  Loading is forgiving: a missing, unreadable, or
;; malformed file is an empty history, never an error, so a damaged
;; file can't keep the REPL from starting.

(define history-cap 200)

(define (rackton-history-path)
  (build-path (find-system-path 'pref-dir) "rackton-history"))

(define (rackton-history-load path)
  (with-handlers ([exn:fail? (lambda (_) '())])
    (define entries (call-with-input-file path read))
    (if (and (list? entries) (andmap string? entries))
        entries
        '())))

(define (rackton-history-save! path entries)
  (with-handlers ([exn:fail? (lambda (_) (void))])   ; best effort
    (define capped
      (if (> (length entries) history-cap)
          (for/list ([e (in-list entries)] [_ (in-range history-cap)]) e)
          entries))
    (make-directory* (path-only* path))
    (call-with-output-file path #:exists 'truncate
      (lambda (out) (write capped out) (newline out)))))

(define (path-only* path)
  (define-values (dir _name _dir?) (split-path path))
  dir)

;; ----- completion namespace ---------------------------------------

;; Expeditor's Tab completion offers the symbols of the current
;; namespace.  Session env names that have no runtime binding — types,
;; classes — would never complete, so the editor owns a dedicated
;; namespace holding a dummy definition per completable name, and
;; installs it (via `current-namespace`) around each read.  This leans
;; only on the *default* completion behavior, not on any private hook:
;; if a future expeditor completes differently, completion weakens but
;; nothing breaks.

(struct completion-namespace (ns seen))

(define (make-completion-namespace)
  (completion-namespace (make-empty-namespace) (make-hash)))

;; Define any names not yet present.  `names` are strings (the
;; kernel's completion candidates); `seen` makes a repeat sync cheap.
(define (completion-namespace-sync! cns names)
  (define seen (completion-namespace-seen cns))
  (for ([n (in-list names)]
        #:unless (hash-ref seen n #f))
    (hash-set! seen n #t)
    (namespace-set-variable-value! (string->symbol n) #t #f
                                   (completion-namespace-ns cns))))

;; ----- syntax coloring ----------------------------------------------

;; Rackton's keyword heads.  `rackton-lexer` re-tags these as
;; 'hash-colon-keyword — the standard color-lexer token type that
;; expeditor renders apart from ordinary identifiers — so a `define`
;; or `match` stands out the way `#:deriving` already does.
(define rackton-keywords
  (seteq 'define 'data 'newtype 'struct 'protocol 'instance
         'define-alias 'define-effect
         'define-syntax 'define-syntax-rule 'define-syntaxes
         'require 'provide 'foreign 'foreign-c
         'lambda 'λ 'case-lambda 'case-λ
         'let 'let& 'let% 'let+
         'if 'cond 'match 'do 'ann 'delay 'handle 'update 'via
         'racket 'quote 'and
         'All ': '=> '->))

;; The standard Racket lexer with Rackton keywords re-tagged.
;; Everything else — strings, numbers, comments, parens — passes
;; through, so `"define"` stays a string literal.
(define (rackton-lexer in)
  (define-values (lexeme type paren start end) (racket-lexer in))
  (values lexeme
          (if (and (eq? type 'symbol)
                   (set-member? rackton-keywords (string->symbol lexeme)))
              'hash-colon-keyword
              type)
          paren start end))

;; ----- structural editing -----------------------------------------

;; Wrap the s-expression after the cursor in parentheses: insert `(`,
;; step over the expression, insert `)`.  Composed purely from
;; documented commands — each takes (ee entry c) and returns the
;; entry.  With nothing after the cursor, `ee-forward-exp` stays put
;; (with a beep) and the command degenerates to inserting `()`.
;; Expeditor's other structural commands (s-expression motion,
;; transpose, kill, match-jump) are built in and listed by ,keys.
(define ee-rackton-wrap-next
  (let ([insert-open (make-ee-insert-string "(")]
        [insert-close (make-ee-insert-string ")")])
    (lambda (ee entry c)
      (insert-close ee (ee-forward-exp ee (insert-open ee entry c) c) c))))

;; Key bindings go into expeditor's process-global dispatch table, so
;; bind exactly once, on first open.
(define editor-keys-bound? (box #f))

(define (bind-editor-keys!)
  (unless (unbox editor-keys-bound?)
    (set-box! editor-keys-bound? #t)
    (expeditor-bind-key! "\\e(" ee-rackton-wrap-next)))   ; Esc-(

;; ----- editor lifecycle -------------------------------------------

;; The handle pairs expeditor's terminal state with the completion
;; namespace, so callers thread one opaque value.
(struct rackton-editor (ee completions))

;; Returns an editor handle, or #f when the expeditor cannot run here
;; (stdin/stdout not terminals, or the terminal is unrecognized) — the
;; caller falls back to plain line reading.
(define (rackton-editor-open history)
  (define ee (expeditor-open history))
  (and ee
       (begin
         (bind-editor-keys!)
         (rackton-editor ee (make-completion-namespace)))))

;; Make `names` (strings) completable on the next read.
(define (rackton-editor-sync-completions! ed names)
  (completion-namespace-sync! (rackton-editor-completions ed) names))

;; Read one entry as a datum (form or `(unquote …)` command), or eof.
;; The expeditor parameters are set here, around each read, so this
;; module owns them for exactly the editor's dynamic extent.
(define (rackton-editor-read ed #:prompt [prompt "λ>"])
  (parameterize ([current-expeditor-reader rackton-editor-read-datum]
                 [current-expeditor-ready-checker rackton-editor-ready?]
                 [current-expeditor-lexer rackton-lexer]
                 [current-namespace
                  (completion-namespace-ns (rackton-editor-completions ed))])
    (expeditor-read (rackton-editor-ee ed) #:prompt prompt)))

;; Close the editor and return the updated history (most recent first),
;; for the caller to persist.
(define (rackton-editor-close ed)
  (expeditor-close (rackton-editor-ee ed)))
