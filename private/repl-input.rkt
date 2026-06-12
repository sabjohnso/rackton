#lang racket/base

;; Rackton — REPL input reading.
;;
;; The reading half of the REPL, split out from the kernel so input
;; layers (the plain line loop here, the expeditor layer in
;; repl-editor.rkt) can share it without depending on the state
;; machine.  Everything here is pure with respect to session state:
;; ports in, datums out.
;;
;; Public API:
;;   rackton-parse-command-line — one line → command datum, or #f
;;   rackton-read-form          — port → one form, accumulating lines
;;                                until parens balance

(provide rackton-parse-command-line
         rackton-read-form)

(require (only-in racket/string string-trim))

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
