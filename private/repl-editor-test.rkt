#lang racket/base

;; Tests for repl-editor.rkt — the expeditor-backed input layer.
;;
;; The editor itself needs a terminal, so the testable pieces are the
;; two pure functions expeditor consults: `rackton-editor-read-datum`
;; (how an accepted entry becomes a datum, including `,command` lines)
;; and `rackton-editor-ready?` (when Return accepts the entry instead
;; of inserting a newline).

(module+ test
  (require rackunit
           racket/file
           "repl-editor.rkt")

  (define (read-datum-of str)
    (rackton-editor-read-datum (open-input-string str)))

  (define (ready? str)
    (rackton-editor-ready? (open-input-string str)))

  ;; ----- rackton-editor-read-datum ----------------------------------

  (check-equal? (read-datum-of "(define x 1)") '(define x 1)
                "plain form reads as one datum")

  (check-equal? (read-datum-of "  42") 42
                "leading whitespace is skipped")

  (check-equal? (read-datum-of ",type (f x)") '(unquote type (f x))
                "comma line parses as a command with its arguments")

  (check-equal? (read-datum-of "  ,info Foo") '(unquote info Foo)
                "command detection survives leading whitespace")

  (check-equal? (read-datum-of ",") '(unquote)
                "bare comma is the no-op command")

  (check-pred eof-object? (read-datum-of "   ")
              "whitespace-only entry reads as eof")

  ;; ----- rackton-editor-ready? --------------------------------------

  (check-true (ready? "(define x 1)")
              "complete form accepts")

  (check-false (ready? "(define x")
               "unclosed paren keeps editing")

  (check-false (ready? "(define (f x)\n  (g x)")
               "multi-line entry with unclosed paren keeps editing")

  (check-true (ready? "(define (f x)\n  (g x))")
              "balanced multi-line entry accepts")

  (check-false (ready? "\"abc")
               "unterminated string keeps editing")

  (check-true (ready? "(]")
              "malformed-but-closed input accepts, so the kernel reports the error")

  (check-false (ready? "")
               "empty entry keeps editing")

  (check-true (ready? ",help")
              "argument-free command accepts")

  (check-true (ready? ",")
              "bare comma accepts (kernel treats it as a no-op)")

  (check-true (ready? ",type (f x)")
              "command with complete argument accepts")

  (check-false (ready? ",type (f")
               "command with unclosed argument keeps editing")

  ;; ----- history persistence ----------------------------------------

  (define (with-temp-history-file proc)
    (define path (make-temporary-file "rackton-history-test-~a"))
    (dynamic-wind void
                  (lambda () (proc path))
                  (lambda () (when (file-exists? path) (delete-file path)))))

  (with-temp-history-file
   (lambda (path)
     (rackton-history-save! path '("(+ 1 2)" "(define x 3)"))
     (check-equal? (rackton-history-load path) '("(+ 1 2)" "(define x 3)")
                   "history round-trips through its file")))

  (with-temp-history-file
   (lambda (path)
     (delete-file path)
     (check-equal? (rackton-history-load path) '()
                   "missing history file loads as empty")))

  (with-temp-history-file
   (lambda (path)
     (call-with-output-file path #:exists 'truncate
       (lambda (out) (display "(not a \"list of" out)))
     (check-equal? (rackton-history-load path) '()
                   "corrupt history file loads as empty")))

  (with-temp-history-file
   (lambda (path)
     (call-with-output-file path #:exists 'truncate
       (lambda (out) (write '("ok" 42 "also ok") out)))
     (check-equal? (rackton-history-load path) '()
                   "history with non-string entries loads as empty")))

  (with-temp-history-file
   (lambda (path)
     (rackton-history-save! path (for/list ([i (in-range 500)])
                                   (format "entry-~a" i)))
     (define loaded (rackton-history-load path))
     (check-true (<= (length loaded) 200)
                 "saved history is capped")
     (check-equal? (car loaded) "entry-0"
                   "capping keeps the most recent entries (front of list)")))

  ;; ----- keyword highlighting -----------------------------------------

  ;; The wrapper lexer re-tags Rackton keyword symbols so they color
  ;; differently from ordinary identifiers; everything else passes
  ;; through the standard Racket lexer untouched.

  (define (lex-all str)
    ;; (listof (cons lexeme type)), whitespace dropped.
    (define in (open-input-string str))
    (let loop ()
      (define-values (lexeme type paren start end) (rackton-lexer in))
      (cond
        [(eof-object? lexeme) '()]
        [(eq? type 'white-space) (loop)]
        [else (cons (cons lexeme type) (loop))])))

  (check-equal? (lex-all "(define x 1)")
                '(("(" . parenthesis)
                  ("define" . hash-colon-keyword)
                  ("x" . symbol)
                  ("1" . constant)
                  (")" . parenthesis))
                "a Rackton keyword head is re-tagged; the rest is untouched")

  (check-equal? (cdr (assoc "match" (lex-all "(match m ((Some x) x))")))
                'hash-colon-keyword
                "expression keywords re-tag too")

  (check-equal? (cdr (assoc "definex" (lex-all "(definex 1)")))
                'symbol
                "a keyword prefix alone does not re-tag")

  (check-equal? (cdr (assoc "\"define\"" (lex-all "\"define\"")))
                'string
                "a keyword inside a string literal stays a string")

  ;; ----- completion namespace ---------------------------------------

  (define cns (make-completion-namespace))

  (completion-namespace-sync! cns '("frobnicate" "Maybe"))
  (check-true (and (memq 'frobnicate
                         (parameterize ([current-namespace
                                         (completion-namespace-ns cns)])
                           (namespace-mapped-symbols)))
                   #t)
              "synced var name is visible to namespace completion")
  (check-true (and (memq 'Maybe
                         (parameterize ([current-namespace
                                         (completion-namespace-ns cns)])
                           (namespace-mapped-symbols)))
                   #t)
              "synced type name is visible to namespace completion")

  (completion-namespace-sync! cns '("frobnicate" "Maybe" "Wibble"))
  (check-true (and (memq 'Wibble
                         (parameterize ([current-namespace
                                         (completion-namespace-ns cns)])
                           (namespace-mapped-symbols)))
                   #t)
              "a later sync adds newly defined names")

  ;; The namespace mirrors the candidate list: a name absent from a
  ;; later sync stops completing — this is what makes ,clear forget
  ;; the cleared session's names.
  (completion-namespace-sync! cns '("Wibble"))
  (define after-removal
    (parameterize ([current-namespace (completion-namespace-ns cns)])
      (namespace-mapped-symbols)))
  (check-false (memq 'frobnicate after-removal)
               "a name dropped from the sync list no longer completes")
  (check-true (and (memq 'Wibble after-removal) #t)
              "names still in the sync list survive removal of others"))
