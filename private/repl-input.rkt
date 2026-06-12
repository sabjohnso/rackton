#lang racket/base

;; Rackton — REPL input reading and history persistence.
;;
;; The reading half of the REPL, split out from the kernel so input
;; layers (the plain line loop here, the structural editor in
;; repl-term.rkt) can share it without depending on the state
;; machine.  Everything here is pure with respect to session state:
;; ports in, datums out — except the best-effort history file I/O.
;;
;; Public API:
;;   rackton-parse-command-line — one line → command datum, or #f
;;   rackton-read-form          — port → one form, accumulating lines
;;                                until parens balance
;;   rackton-editor-read-datum  — port over one accepted editor entry
;;                                → datum (comma commands included)
;;   rackton-editor-ready?      — port → may this entry be accepted?
;;   rackton-history-path/load/save! — cross-session history

(provide rackton-parse-command-line
         rackton-read-form
         rackton-editor-read-datum
         rackton-editor-ready?
         rackton-history-path
         rackton-history-load
         rackton-history-save!)

(require (only-in racket/string string-trim)
         (only-in racket/port port->string)
         (only-in racket/file make-directory*))

;; Parse one line of input into a command datum, or #f when it is not a
;; command.  A leading `,` (which never begins a valid Rackton form) marks
;; a command: the rest of the line is read as the command word and its
;; arguments, producing `(unquote word arg ...)`.  A bare `,` yields
;; `(unquote)` — the accepted no-op.  Each `read` is guarded so a
;; malformed tail simply stops accumulation instead of raising.
(define (rackton-parse-command-line str)
  (define s (string-trim str))
  (and (positive? (string-length s))
       (char=? (string-ref s 0) #\,)
       (let ([ip (open-input-string (substring s 1))])
         (let loop ([acc '()])
           (define d
             (with-handlers ([exn:fail:read? (lambda (_) eof)])
               (read ip)))
           (if (eof-object? d)
               (cons 'unquote (reverse acc))
               (loop (cons d acc)))))))

;; Read one rackton form from `port`, accumulating lines
;; until parens balance.  `prompt-cont` is called with the current
;; depth count (always positive on continuation prompts) and
;; should return a continuation-prompt string to display; in tests
;; pass `(lambda (_) "")` to silence.  Returns the parsed form or
;; eof when the port is exhausted with nothing read.
(define (rackton-read-form port [prompt-cont (lambda (_) "..> ")])
  (let loop ([buf ""] [depth 0])
    (define line (read-line port))
    (cond
      [(eof-object? line)
       (cond
         [(zero? (string-length buf)) eof]
         [(rackton-parse-command-line buf) => values]
         [else
          ;; Buffer non-empty but parens never closed — let read
          ;; raise the natural "expected )" error so the caller
          ;; sees a useful message.
          (read (open-input-string buf))])]
      [else
       (define buf* (string-append buf line " "))
       (define new-depth (+ depth (line-paren-delta line)))
       (cond
         [(<= new-depth 0)
          ;; Parens balanced (or never opened).  A leading comma marks a
          ;; REPL command (handled first); otherwise try to read, and if
          ;; the buffer is purely whitespace, keep looping.
          (cond
            [(rackton-parse-command-line buf*) => values]
            [else
             (define trimmed-port (open-input-string buf*))
             (define form
               (with-handlers ([exn:fail:read? (lambda (_) #f)])
                 (read trimmed-port)))
             (cond
               [(eof-object? form) (loop "" 0)]    ;; blank-ish line
               [form form]
               [else (loop buf* new-depth)])])]
         [else
          (display (prompt-cont new-depth))
          (flush-output)
          (loop buf* new-depth)])])))

;; ----- whole-entry reading (the structural editor) -------------------

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

;; Decide whether the editor may accept the entry.  The rule matches
;; "every datum reads completely" — keep editing on an unterminated
;; form — except that comma commands are recognized: a bare `,`
;; accepts (it is the kernel's no-op), and a command's arguments must
;; each read completely.  A malformed-but-closed entry (e.g.
;; mismatched delimiters) accepts, so the kernel reports the read
;; error instead of trapping the user in the editor.
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
;; of strings, most recent first — the order the editor passes it
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

;; Net `(` - `)` for one line, ignoring `;` comments and
;; string contents.  Brackets `[]` and braces `{}` count too —
;; Racket treats them as parens.
(define (line-paren-delta line)
  (define n (string-length line))
  (let loop ([i 0] [delta 0] [in-string? #f] [in-comment? #f])
    (cond
      [(= i n) delta]
      [in-comment? delta]
      [in-string?
       (define c (string-ref line i))
       (cond
         [(char=? c #\") (loop (add1 i) delta #f #f)]
         [(char=? c #\\) (loop (+ i 2) delta #t #f)]
         [else (loop (add1 i) delta #t #f)])]
      [else
       (define c (string-ref line i))
       (cond
         [(char=? c #\;) delta]
         [(char=? c #\") (loop (add1 i) delta #t #f)]
         [(or (char=? c #\() (char=? c #\[) (char=? c #\{))
          (loop (add1 i) (add1 delta) #f #f)]
         [(or (char=? c #\)) (char=? c #\]) (char=? c #\}))
          (loop (add1 i) (sub1 delta) #f #f)]
         [else (loop (add1 i) delta #f #f)])])))
