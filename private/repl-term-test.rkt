#lang racket/base

;; Tests for repl-term.rkt — the terminal shell's pure parts: the key
;; decoder (input characters → key names), the layout engine (entry →
;; rows + cursor placement), and the editor state machine (state × key
;; → state) that routes keys through the paredit commands.

(module+ test
  (require rackunit
           racket/string
           racket/list
           racket/file
           (only-in racket/port with-output-to-string)
           "repl-entry.rkt"
           (only-in "complete-context.rkt" completion-word-start)
           "repl-term.rkt")

  ;; ----- key decoding -------------------------------------------------

  (define (decode str)
    ;; Decode every key in the string; returns the list of keys.
    (let loop ([cs (string->list str)] [acc '()])
      (if (null? cs)
          (reverse acc)
          (let-values ([(key rest) (decode-key cs)])
            (loop rest (cons key acc))))))

  (check-equal? (decode "ab") '((self . #\a) (self . #\b)))
  (check-equal? (decode "\u01") '("C-a"))
  (check-equal? (decode "\u0B") '("C-k"))
  (check-equal? (decode "\r") '("RET"))
  (check-equal? (decode "\t") '("TAB"))
  (check-equal? (decode "\u7F") '("DEL"))
  (check-equal? (decode "\e[A") '("up"))
  (check-equal? (decode "\e[1;5C") '("C-right"))
  (check-equal? (decode "\e[3~") '("delete"))
  (check-equal? (decode "\e[Z") '("S-TAB"))
  (check-equal? (decode "\ep") '("M-p"))
  (check-equal? (decode "\e(") '("M-("))
  (check-equal? (decode "\e\u13") '("C-M-s"))
  (check-equal? (decode "\e\e[A") '("M-up"))
  (check-equal? (decode "\e") '("ESC")
                "a lone escape with nothing after it is the ESC key")
  (check-equal? (decode "\e[200~ab\u1B[201~") '((paste . "ab"))
                "bracketed paste arrives as one key")

  ;; ----- layout ---------------------------------------------------------

  ;; (layout-rows text width prompt-length) → list of (start end prefix?)
  ;; rows; (layout-cursor text point width prompt-length) → (row . col).

  (check-equal? (layout-rows "abc" 10 3)
                '((0 3))
                "one short logical line is one row")
  (check-equal? (layout-rows "abcdefghij" 8 3)
                '((0 5) (5 10))
                "a long line wraps at width minus the prompt")
  (check-equal? (layout-rows "ab\ncd" 10 3)
                '((0 2) (3 5))
                "logical lines split on newline (the newline is no row text)")
  (check-equal? (layout-rows "" 10 3)
                '((0 0))
                "an empty entry still has one row")

  (check-equal? (layout-cursor "abc" 0 10 3) '(0 . 3))
  (check-equal? (layout-cursor "abc" 3 10 3) '(0 . 6))
  (check-equal? (layout-cursor "ab\ncd" 4 10 3) '(1 . 4)
                "point after the newline lands on the second row")
  (check-equal? (layout-cursor "abcdefghij" 5 8 3) '(1 . 3)
                "point at a wrap boundary starts the next row")

  ;; ----- syntax categories ------------------------------------------------

  ;; A toy classifier standing in for the kernel's env-backed one.
  (define (kind-of sym)
    (case sym
      [(Maybe List) 'type]
      [(Just Cons) 'constructor]
      [else #f]))

  (check-equal? (text-categories "(define x (Just 1))" kind-of)
                '(("(" . paren) ("define" . keyword) ("x" . identifier)
                  ("(" . paren) ("Just" . constructor) ("1" . literal)
                  (")" . paren) (")" . paren)))

  (check-equal? (cdr (assoc "Maybe" (text-categories "(f Maybe)" kind-of)))
                'type)
  (check-equal? (cdr (assoc "Wibble" (text-categories "(f Wibble)" kind-of)))
                'identifier
                "a capitalized name the env does not know is an identifier")
  (check-equal? (cdr (assoc "\"s\"" (text-categories "\"s\"" kind-of)))
                'string)
  (check-equal? (cdr (assoc "; c" (text-categories "x ; c" kind-of)))
                'comment)

  ;; ----- palette -----------------------------------------------------------

  (check-equal? (hash-ref (scheme-palette 'standard) 'type) 'cyan)
  (check-equal? (hash-ref (scheme-palette 'standard) 'constructor) 'magenta)
  (check-equal? (hash-ref (scheme-palette 'standard) 'keyword) 'red)
  (check-true (for/and ([(c col) (in-hash (scheme-palette 'plain))])
                (eq? col 'none)))
  (check-false (scheme-palette 'no-such-scheme))

  (check-equal? (color->sgr 'cyan) "36")
  (check-equal? (color->sgr 'light-magenta) "95")
  (check-false (color->sgr 'none))
  (check-false (color->sgr 'default))

  (check-true (valid-color? 'magenta))
  (check-false (valid-color? 'mauve))
  (check-true (valid-category? 'constructor))
  (check-false (valid-category? 'sparkles))

  ;; resolution: scheme + overrides; NO_COLOR forces plain
  (parameterize ([current-color-scheme 'standard]
                 [current-color-overrides '((type . light-cyan))])
    (check-equal? (hash-ref (resolved-palette) 'type) 'light-cyan
                  "an override beats the scheme")
    (check-equal? (hash-ref (resolved-palette) 'keyword) 'red))

  (parameterize ([current-environment-variables
                  (make-environment-variables #"NO_COLOR" #"1")]
                 [current-color-scheme 'standard]
                 [current-color-overrides '()])
    (check-equal? (hash-ref (resolved-palette) 'keyword) 'none
                  "NO_COLOR forces the plain palette"))

  ;; emission uses the palette
  (define (emitted text palette)
    (with-output-to-string
      (lambda () (emit-colored text 0 (string-length text) palette kind-of))))
  (check-regexp-match #rx"\e\\[31mdefine"
                      (emitted "(define x)" (scheme-palette 'standard)))
  (check-regexp-match #rx"\e\\[35mJust"
                      (emitted "(Just 1)" (scheme-palette 'standard)))
  (check-false (regexp-match #rx"\e\\["
                             (emitted "(define x)" (scheme-palette 'plain)))
               "the plain palette emits no escapes")

  ;; ----- persistence ---------------------------------------------------------

  (let ([pref-file (make-temporary-file "rackton-colors-test-~a")])
    ;; An empty file is not valid preference format; start absent.
    (delete-file pref-file)
    (dynamic-wind
     void
     (lambda ()
       (parameterize ([current-color-pref-file pref-file]
                      [current-color-scheme 'standard]
                      [current-color-overrides '()])
         (check-true (set-color! 'type 'light-cyan))
         (check-true (set-color-scheme! 'plain))
         (check-false (set-color! 'sparkles 'red))
         (check-false (set-color-scheme! 'no-such-scheme))
         ;; a fresh load sees what was saved
         (parameterize ([current-color-scheme 'standard]
                        [current-color-overrides '()])
           (load-color-prefs!)
           (check-equal? (current-color-scheme) 'plain)
           (check-equal? (assq 'type (current-color-overrides))
                         '(type . light-cyan)))))
     (lambda () (when (file-exists? pref-file) (delete-file pref-file)))))

  ;; ----- the editor state machine -----------------------------------------

  (define cfg
    (editor-config
     (lambda (text)            ; ready?: balanced and non-blank
       (and (entry-balanced? (entry text 0))
            (not (string=? (string-trim text) ""))))
     ;; completions: the whole entry and the point, so the source — not
     ;; the editor — decides both the region to replace and what may
     ;; replace it.  This stand-in answers module paths inside a require
     ;; and definition keywords everywhere else.
     (lambda (text pos)
       (define start (completion-word-start text pos))
       (define prefix (substring text start pos))
       (define pool
         (if (regexp-match? #rx"\\(require" (substring text 0 start))
             '("rackton/data/list" "rackton/data/maybe")
             '("define" "define-alias" "define-struct")))
       (values start (filter (lambda (s) (string-prefix? s prefix)) pool)))
     "λ> "
     (lambda (_sym) #f)))

  (define (feed st keys)
    (for/fold ([st st]) ([k (in-list keys)])
      (editor-step st cfg k)))

  (define (typed str [history '()])
    (feed (make-editor-state history) (decode str)))

  (define (entry-of st) (entry->marked (editor-state-entry st)))

  ;; typing routes through paredit
  (check-equal? (entry-of (typed "(+ 1 2")) "(+ 1 2|)"
                "open paren inserted its closer; typing continues inside")
  (check-equal? (entry-of (typed "(+ 1 2)")) "(+ 1 2)|"
                "the closing paren moves past the inserted closer")
  (check-equal? (entry-of (typed "(\"a\u7F")) "(\"|\")"
                "backspace in a string is the paredit backspace")
  (check-equal? (entry-of (typed "ab\u7F\u7F")) "|")

  ;; RET accepts only when ready
  (check-equal? (editor-state-done (typed "(+ 1 2)\r")) 'accept)
  (check-false  (editor-state-done (typed "(+ 1 2\r")))
  (check-regexp-match #rx"\\(\\+ 1 2\n" (entry-text (editor-state-entry
                                                     (typed "(+ 1 2\r")))
                      "RET on an incomplete form inserts a newline")

  ;; the newline is indented under the open paren
  (check-equal? (entry-of (typed "(foo\r")) "(foo\n  |)"
                "the continuation line indents two past the opener")

  ;; kill + yank round-trip
  (check-equal? (entry-of (typed "(a b)\u01\u02\u0B\u19"))
                "(a b)|"
                "C-k kills the rest and C-y yanks it back (C-a/C-b position)")

  ;; C-d on an empty entry is eof
  (check-equal? (editor-state-done (typed "\u04")) 'eof)
  (check-false  (editor-state-done (typed "x\u04")))

  ;; paste inserts raw, no electric routing
  (check-equal? (entry-of (feed (make-editor-state '())
                                '((paste . "(a"))))
                "(a|"
                "pasted text bypasses the electric layer")

  ;; ----- history ------------------------------------------------------------

  (define hist '("(third)" "(second)" "(first)"))

  (check-equal? (entry-of (feed (make-editor-state hist) (decode "\e[A")))
                "(third)|"
                "Up on an empty entry recalls the most recent input")
  (check-equal? (entry-of (feed (make-editor-state hist) (decode "\e[A\e[A")))
                "(second)|")
  (check-equal? (entry-of (feed (make-editor-state hist) (decode "\e[A\e[A\e[B")))
                "(third)|"
                "Down comes back toward the present")
  (check-equal? (entry-of (feed (make-editor-state hist)
                                (decode "(x\ry\e[A")))
                "(x|\n  y)"
                "Up below the first row is cursor motion, not history")

  ;; M-p searches by the text before the cursor
  (check-equal? (entry-of (feed (make-editor-state
                                 '("(second x)" "(first x)" "zzz"))
                                (append (decode "(f") (list "M-p"))))
                "(first x)|"
                "prefix search recalls the matching entry")

  ;; ----- completion ------------------------------------------------------------

  (check-equal? (entry-of (typed "defi\t")) "define|"
                "Tab extends to the common prefix")
  (check-regexp-match #rx"define-alias"
                      (or (editor-state-message (typed "define\t")) "")
                      "an ambiguous Tab lists candidates in the message")

  ;; The candidate source sees the whole entry, so it can answer a
  ;; different universe of names depending on where the point is — and
  ;; the region it reports is what gets replaced, slashes included.
  (check-equal? (entry-of (typed "(require rackton/data/m\t"))
                "(require rackton/data/maybe|)"
                "Tab inside a require completes a whole module path")
  (check-regexp-match #rx"rackton/data/list"
                      (or (editor-state-message (typed "(require rackton/data/\t")) "")
                      "an ambiguous module path lists its candidates"))
