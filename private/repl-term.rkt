#lang racket/base

;; Rackton — the terminal shell of the structural editor.
;;
;; Replaces expeditor for terminal sessions.  Layered like paredit's
;; host (see Review-Paredit.org): repl-entry.rkt is the buffer model,
;; repl-paredit.rkt the commands; this module adds
;;
;;   - a key decoder: input characters → key names ("C-k", "M-(",
;;     "C-right", (self . #\a), (paste . "…")), pure;
;;   - a layout engine: entry text → visual rows + cursor cell, pure;
;;   - the editor state machine: state × key → state, pure — typing
;;     routes delimiters and deletion through the paredit commands,
;;     Return accepts a complete entry typed to its end, history and
;;     completion live here;
;;   - a thin imperative shell: stty raw mode, bracketed paste, ANSI
;;     redraw with the same token coloring the lexer gives, and the
;;     open/read/close interface the REPL loop drives.
;;
;; The keymap is one declarative table, paredit-style: each command
;; carries its alternative key sequences and its one-line doc, so the
;; dispatch table and the ,keys help text are generated from the same
;; source and cannot drift apart.

(provide decode-key
         layout-rows
         layout-cursor
         (struct-out editor-config)
         make-editor-state
         editor-step
         editor-state-entry
         editor-state-done
         editor-state-message
         keymap-text
         rackton-keyword-names
         ;; coloring: classification, palettes, ,colors support
         token-category
         text-categories
         emit-colored
         scheme-palette
         color->sgr
         valid-color?
         valid-category?
         current-color-scheme
         current-color-overrides
         current-color-pref-file
         resolved-palette
         load-color-prefs!
         set-color-scheme!
         set-color!
         colors-summary
         rackton-term-open
         rackton-term-read
         rackton-term-close)

(require racket/match
         racket/list
         racket/string
         (only-in racket/set seteq set-member? set->list)
         (only-in racket/port with-output-to-string)
         (only-in racket/system system)
         (only-in racket/file get-preference put-preferences)
         "repl-entry.rkt"
         "repl-paredit.rkt"
         (only-in "term.rkt" detect-display-columns))

;; ----- key decoding ---------------------------------------------------

;; (decode-key chars) → (values key remaining-chars).  Keys are
;; (self . char) for plain insertions, (paste . string) for a
;; bracketed paste, 'partial when the input is an incomplete escape
;; sequence (the caller reads more), or a key-name string ("C-k",
;; "M-(", "C-right", "RET", …).
(define (decode-key cs)
  (match cs
    ['() (values 'partial '())]
    [(cons #\u1B rest) (decode-escape rest)]
    [(cons #\return rest) (values "RET" rest)]
    [(cons #\newline rest) (values "C-j" rest)]
    [(cons #\tab rest) (values "TAB" rest)]
    [(cons #\u7F rest) (values "DEL" rest)]
    [(cons (? control-char? c) rest) (values (control-name "C-" c) rest)]
    [(cons c rest) (values (cons 'self c) rest)]))

(define (control-char? c) (< (char->integer c) 32))

(define (control-name prefix c)
  (define n (char->integer c))
  (string-append prefix
                 (string (if (<= 1 n 26)
                             (integer->char (+ n 96))      ; ^A → a
                             (integer->char (+ n 64))))))  ; ^] → ]

(define (decode-escape cs)
  (match cs
    ['() (values "ESC" '())]
    [(cons #\[ rest) (decode-csi rest "")]
    [(cons #\u1B (cons #\[ rest))
     (define-values (k r) (decode-csi rest ""))
     (values (if (string? k) (string-append "M-" k) k) r)]
    [(cons #\O (cons c rest)) (values (ss3-name c) rest)]
    [(cons #\O '()) (values 'partial '())]
    [(cons #\return rest) (values "M-RET" rest)]
    [(cons (? control-char? c) rest) (values (control-name "C-M-" c) rest)]
    [(cons c rest) (values (format "M-~a" c) rest)]))

(define (ss3-name c)
  (case c
    [(#\H) "home"] [(#\F) "end"]
    [(#\P) "F1"] [(#\Q) "F2"] [(#\R) "F3"] [(#\S) "F4"]
    [else (format "SS3-~a" c)]))

(define (decode-csi cs params)
  (match cs
    ['() (values 'partial '())]
    [(cons c rest)
     (if (char<? c #\@)
         (decode-csi rest (string-append params (string c)))
         (csi-key params c rest))]))

(define (csi-key params final rest)
  (define base
    (case final
      [(#\A) "up"] [(#\B) "down"] [(#\C) "right"] [(#\D) "left"]
      [(#\H) "home"] [(#\F) "end"] [(#\Z) "S-TAB"]
      [(#\~) (case params
               [("1" "7") "home"] [("4" "8") "end"]
               [("3") "delete"] [("5") "pgup"] [("6") "pgdn"]
               [("200") 'paste-begin]
               [else #f])]
      [else #f]))
  (cond
    [(eq? base 'paste-begin) (decode-paste rest '())]
    [(not base) (values (format "CSI-~a~a" params final) rest)]
    [else
     (define modifier
       (match params
         [(regexp #rx"^1;([0-9]+)$" (list _ m))
          (case m [("3") "M-"] [("5") "C-"] [("7") "C-M-"] [else ""])]
         [_ ""]))
     (values (string-append modifier base) rest)]))

;; Everything up to the ESC[201~ terminator is one pasted unit.
(define (decode-paste cs acc)
  (match cs
    [(list* #\u1B #\[ #\2 #\0 #\1 #\~ rest)
     (values (cons 'paste (list->string (reverse acc))) rest)]
    ['() (values 'partial '())]
    [(cons c rest) (decode-paste rest (cons c acc))]))

;; ----- layout -----------------------------------------------------------

;; The logical lines of `text` as (start . end) spans, newlines
;; excluded.
(define (logical-lines text)
  (let loop ([start 0] [acc '()])
    (define nl
      (for/first ([i (in-range start (string-length text))]
                  #:when (char=? (string-ref text i) #\newline))
        i))
    (if nl
        (loop (add1 nl) (cons (cons start nl) acc))
        (reverse (cons (cons start (string-length text)) acc)))))

;; Visual rows as (start end) spans: logical lines wrapped to the
;; width left of the prompt (continuation lines are padded to the
;; prompt's width, so every row has the same text budget).
(define (layout-rows text width prompt-len)
  (define avail (max 1 (- width prompt-len)))
  (append*
   (for/list ([line (in-list (logical-lines text))])
     (match-define (cons ls le) line)
     (if (= ls le)
         (list (list ls ls))
         (for/list ([a (in-range ls le avail)])
           (list a (min le (+ a avail))))))))

;; The visual cell of the point: (row . column), column including the
;; prompt width.  A point at a wrap boundary belongs to the next row.
(define (layout-cursor text point width prompt-len)
  (define rows (layout-rows text width prompt-len))
  (or (for/first ([r (in-list rows)] [i (in-naturals)]
                  #:when (or (< point (second r))
                             (and (= point (second r))
                                  (or (= (second r) (string-length text))
                                      (char=? (string-ref text (second r))
                                              #\newline)))))
        (cons i (+ prompt-len (- point (first r)))))
      (cons (sub1 (length rows)) prompt-len)))

;; ----- the keymap --------------------------------------------------------

;; One declarative table, paredit-commands style: section title, then
;; (keys command doc).  Generates both the dispatch table and ,keys.
(define keymap-table
  `(("Entry"
     (("RET" "C-j") accept-or-newline
      "accept when complete and typed to the end; otherwise newline")
     (("M-RET" "C-o") newline "newline and indent, without accepting")
     (("TAB") complete "complete the name before the cursor")
     (("M-q") reindent "reindent the whole entry")
     (("C-g" "C-c") clear-entry "discard the entry")
     (("C-d") delete-or-eof "delete forward; on an empty entry, exit"))
    ("History"
     (("up") up "previous line; on the first line, previous history")
     (("down") down "next line; on the last line, later history")
     (("M-p") history-search
      "search history for entries starting like the text before the cursor"))
    ("Editing"
     (("DEL") backward-delete "paredit backward delete")
     (("delete") forward-delete "paredit forward delete")
     (("C-k") kill "kill to end of line, structurally")
     (("C-y") yank "reinsert the last kill")
     (("left" "C-b") backward-char "back one character")
     (("right" "C-f") forward-char "forward one character")
     (("C-a" "home") line-start "start of line")
     (("C-e" "end") line-end "end of line")
     (("M-<") entry-start "start of entry")
     (("M->") entry-end "end of entry")
     (("M-f") forward-word "forward one word")
     (("M-b") backward-word "back one word"))
    ("S-expressions"
     (("C-M-f") forward-sexp "forward one expression")
     (("C-M-b") backward-sexp "back one expression")
     (("C-M-u") backward-up "up out of the enclosing list")
     (("C-M-d") down-list "down into the next list")
     (("C-right") slurp-forward "slurp the next expression in")
     (("C-left") barf-forward "barf the last expression out")
     (("C-M-left") slurp-backward "slurp the previous expression in")
     (("C-M-right") barf-backward "barf the first expression out")
     (("M-s") splice "splice the enclosing list away")
     (("M-r") raise "raise the next expression over its enclosing list")
     (("M-(") wrap "wrap the next expression in parentheses"))))

(define key->command
  (for*/hash ([section (in-list keymap-table)]
              [binding (in-list (cdr section))]
              [key (in-list (first binding))])
    (values key (second binding))))

;; The ,keys text, generated from the table.
(define (keymap-text)
  (with-output-to-string
    (lambda ()
      (for ([section (in-list keymap-table)])
        (printf "~a:\n" (car section))
        (for ([binding (in-list (cdr section))])
          (printf "  ~a~a\n"
                  (let ([keys (string-join (first binding) " / ")])
                    (string-append keys
                                   (make-string (max 1 (- 14 (string-length keys)))
                                                #\space)))
                  (third binding)))))))

;; ----- the editor state machine --------------------------------------------

;; ready?: text → boolean (is this acceptable input);
;; completions: text × point → (values start candidate-strings).  The
;;     source is handed the whole entry, not a prefix the editor guessed
;;     at, because what may be completed depends on where the point is —
;;     a module path inside a `require`, a name elsewhere — and so does
;;     the extent of the text a candidate replaces.  It returns that
;;     extent's start, and the candidates are complete replacements for
;;     [start, point).
;; prompt: string;
;; name-kind: symbol → 'type | 'constructor | #f (the kernel's
;; env-backed classifier, for coloring).
(struct editor-config (ready? completions prompt name-kind) #:transparent)

;; done: #f | 'accept | 'eof.  hist-pos: #f or the index of the
;; history entry currently shown; stash: the in-progress entry saved
;; when history navigation started.
(struct editor-state (entry history hist-pos stash kill message done)
  #:transparent)

(define (make-editor-state history)
  (editor-state (entry "" 0) history #f #f #f #f #f))

(define (set-entry st en)
  (struct-copy editor-state st [entry en] [hist-pos #f] [stash #f]))

(define (with-msg st m) (struct-copy editor-state st [message m]))

(define (editor-step st cfg key)
  (define st0 (struct-copy editor-state st [message #f]))
  (match key
    [(cons 'self c) (set-entry st0 (self-insert (editor-state-entry st0) c))]
    [(cons 'paste s)
     (set-entry st0 (entry-insert (editor-state-entry st0) s))]
    ['input-eof (struct-copy editor-state st0 [done 'eof])]
    [(? string? name)
     (define cmd (hash-ref key->command name #f))
     (if cmd ((command-handler cmd) st0 cfg) st0)]
    [_ st0]))

;; Delimiter and quote keys go through paredit; everything else
;; inserts itself.
(define (self-insert en c)
  (case c
    [(#\() (paredit-open-round en)]
    [(#\[) (paredit-open-square en)]
    [(#\) #\]) (paredit-close-round en)]
    [(#\") (paredit-doublequote en)]
    [else (entry-insert en (string c))]))

;; ----- command handlers -----------------------------------------------------

(define (command-handler name)
  (hash-ref command-table name
            (lambda () (lambda (st _cfg) st))))

;; Pure entry commands lift directly.
(define ((entry-command f) st _cfg)
  (set-entry st (f (editor-state-entry st))))

;; Position commands: text × point → position | #f; #f keeps the point.
(define ((motion-command f) st _cfg)
  (define en (editor-state-entry st))
  (define pos (f (entry-text en) (entry-point en)))
  (if pos (set-entry st (entry-goto en pos)) st))

(define (entry-text-of st) (entry-text (editor-state-entry st)))
(define (point-of st) (entry-point (editor-state-entry st)))

;; The line span containing `pos`.
(define (line-span text pos)
  (for/first ([l (in-list (logical-lines text))]
              #:when (and (<= (car l) pos) (<= pos (cdr l))))
    l))

(define (line-index text pos)
  (for/first ([l (in-list (logical-lines text))] [i (in-naturals)]
              #:when (<= (car l) pos (cdr l)))
    i))

;; Vertical motion between logical lines, clamping the column.
(define (move-line st delta)
  (define text (entry-text-of st))
  (define p (point-of st))
  (define lines (logical-lines text))
  (define i (line-index text p))
  (define j (+ i delta))
  (cond
    [(or (< j 0) (>= j (length lines))) st]
    [else
     (define col (- p (car (list-ref lines i))))
     (match-define (cons ls le) (list-ref lines j))
     (set-entry st (entry-goto (editor-state-entry st)
                               (min (+ ls col) le)))]))

;; Accept: the entry must be ready AND typed to its (whitespace-)end —
;; with electric delimiters the text is almost always balanced, so the
;; cursor position is what distinguishes "done" from "editing inside".
(define (try-accept st cfg)
  (define text (entry-text-of st))
  (cond
    [(and ((editor-config-ready? cfg) text)
          (= (skip-ws-forward text (point-of st)) (string-length text)))
     (struct-copy editor-state st [done 'accept])]
    [else #f]))

(define (skip-ws-forward text pos)
  (let loop ([i pos])
    (if (and (< i (string-length text))
             (char-whitespace? (string-ref text i)))
        (loop (add1 i))
        i)))

;; The indentation column for a new line opened at `pos`: two past the
;; innermost unclosed opener's column.
(define (indent-column text pos)
  (match (enclosing-delimiters text pos)
    [(cons (list _ opos _) _)
     (define ls (car (line-span text opos)))
     (+ (- opos ls) 2)]
    [_ 0]))

(define (newline+indent en)
  (entry-insert en (string-append "\n" (make-string
                                        (indent-column (entry-text en)
                                                       (entry-point en))
                                        #\space))))

;; Reindent every line after the first to the simple rule.
(define (reindent-entry en)
  (define text (entry-text en))
  (define lines (logical-lines text))
  (for/fold ([en en]) ([l (in-list (reverse (cdr lines)))])
    (define ls (car l))
    (define content (skip-ws-forward (entry-text en) ls))
    (define target (indent-column (entry-text en) ls))
    (entry-insert-at (entry-delete en ls content)
                     ls (make-string target #\space))))

;; Word motion: a word is a run of non-delimiter, non-space characters.
(define (word-char? c)
  (not (or (char-whitespace? c)
           (memv c '(#\( #\) #\[ #\] #\{ #\} #\" #\; #\' #\` #\,)))))

(define (forward-word-position text pos)
  (define len (string-length text))
  (let skip ([i pos])
    (cond [(= i len) (and (> i pos) i)]
          [(word-char? (string-ref text i))
           (let run ([j i])
             (if (and (< j len) (word-char? (string-ref text j))) (run (add1 j)) j))]
          [else (skip (add1 i))])))

(define (backward-word-position text pos)
  (let skip ([i pos])
    (cond [(= i 0) #f]
          [(word-char? (string-ref text (sub1 i)))
           (let run ([j i])
             (if (and (> j 0) (word-char? (string-ref text (sub1 j)))) (run (sub1 j)) j))]
          [else (skip (sub1 i))])))

;; History recall replaces the entry; the in-progress entry is stashed
;; the first time and restored when navigating back past the newest.
(define (history-move st delta)
  (define hist (editor-state-history st))
  (define pos (editor-state-hist-pos st))
  (define next (if pos (+ pos delta) (if (> delta 0) 0 #f)))
  (cond
    [(not next) st]
    [(< next 0)
     (struct-copy editor-state st
                  [entry (or (editor-state-stash st) (entry "" 0))]
                  [hist-pos #f] [stash #f])]
    [(>= next (length hist)) st]
    [else
     (define text (list-ref hist next))
     (struct-copy editor-state st
                  [entry (entry text (string-length text))]
                  [hist-pos next]
                  [stash (or (editor-state-stash st)
                             (editor-state-entry st))])]))

(define (history-search st)
  (define prefix (substring (entry-text-of st) 0 (point-of st)))
  (define hist (editor-state-history st))
  (define start (if (editor-state-hist-pos st)
                    (add1 (editor-state-hist-pos st))
                    0))
  (define found
    (for/first ([i (in-range start (length hist))]
                #:when (string-prefix? (list-ref hist i) prefix))
      i))
  (cond
    [(not found) (with-msg st "no matching history entry")]
    [else
     (define text (list-ref hist found))
     (struct-copy editor-state st
                  [entry (entry text (string-length text))]
                  [hist-pos found]
                  [stash (or (editor-state-stash st)
                             (editor-state-entry st))])]))

;; Completion: extend the text before the cursor to the candidates'
;; common prefix; when it cannot be extended, list the candidates.  The
;; region to extend is the source's to choose — see `editor-config`.
(define (complete-at st cfg)
  (define text (entry-text-of st))
  (define p (point-of st))
  (define-values (start candidates)
    ((editor-config-completions cfg) text p))
  (define prefix (substring text start p))
  (cond
    [(null? candidates) (with-msg st "no completions")]
    [else
     (define common (common-prefix candidates))
     (if (> (string-length common) (string-length prefix))
         (set-entry st (entry-insert (editor-state-entry st)
                                     (substring common (string-length prefix))))
         (with-msg st (string-join candidates " ")))]))

(define (common-prefix strs)
  (for/fold ([acc (car strs)]) ([s (in-list (cdr strs))])
    (let loop ([i 0])
      (if (and (< i (string-length acc)) (< i (string-length s))
               (char=? (string-ref acc i) (string-ref s i)))
          (loop (add1 i))
          (substring acc 0 i)))))

(define command-table
  (hash
   'accept-or-newline
   (lambda (st cfg)
     (or (try-accept st cfg)
         (set-entry st (newline+indent (editor-state-entry st)))))
   'newline (entry-command newline+indent)
   'complete complete-at
   'reindent (entry-command reindent-entry)
   'clear-entry (lambda (st _cfg) (set-entry st (entry "" 0)))
   'delete-or-eof
   (lambda (st _cfg)
     (if (zero? (entry-length (editor-state-entry st)))
         (struct-copy editor-state st [done 'eof])
         ((entry-command paredit-forward-delete) st _cfg)))
   'up
   (lambda (st _cfg)
     (if (zero? (line-index (entry-text-of st) (point-of st)))
         (history-move st +1)
         (move-line st -1)))
   'down
   (lambda (st _cfg)
     (define text (entry-text-of st))
     (if (= (line-index text (point-of st))
            (sub1 (length (logical-lines text))))
         (history-move st -1)
         (move-line st +1)))
   'history-search (lambda (st _cfg) (history-search st))
   'backward-delete (entry-command paredit-backward-delete)
   'forward-delete (entry-command paredit-forward-delete)
   'kill
   (lambda (st _cfg)
     (define-values (en killed) (paredit-kill (editor-state-entry st)))
     (if killed
         (struct-copy editor-state (set-entry st en) [kill killed])
         st))
   'yank
   (lambda (st _cfg)
     (if (editor-state-kill st)
         (set-entry st (entry-insert (editor-state-entry st)
                                     (editor-state-kill st)))
         st))
   'backward-char (motion-command (lambda (_t p) (and (> p 0) (sub1 p))))
   'forward-char (motion-command (lambda (t p)
                                   (and (< p (string-length t)) (add1 p))))
   'line-start (motion-command (lambda (t p) (car (line-span t p))))
   'line-end (motion-command (lambda (t p) (cdr (line-span t p))))
   'entry-start (motion-command (lambda (_t _p) 0))
   'entry-end (motion-command (lambda (t _p) (string-length t)))
   'forward-word (motion-command forward-word-position)
   'backward-word (motion-command backward-word-position)
   'forward-sexp (motion-command forward-sexp-position)
   'backward-sexp (motion-command backward-sexp-position)
   'backward-up (motion-command backward-up-position)
   'down-list (motion-command down-list-position)
   'slurp-forward (entry-command paredit-slurp-forward)
   'slurp-backward (entry-command paredit-slurp-backward)
   'barf-forward (entry-command paredit-barf-forward)
   'barf-backward (entry-command paredit-barf-backward)
   'splice (entry-command paredit-splice)
   'raise (entry-command paredit-raise)
   'wrap (entry-command paredit-wrap-round)))

;; ----- coloring: categories --------------------------------------------

(define rackton-keyword-set
  (apply seteq
         '(define data newtype struct protocol instance
            define-alias define-effect
            define-syntax define-syntax-rule define-syntaxes
            require provide foreign foreign-c
            lambda λ case-lambda case-λ
            let let& let% let+
            if cond match do ann delay handle update via
            racket quote and
            All : => ->)))

;; The keywords as sorted completion candidates, for the kernel's
;; completion list — one source for coloring and completion alike.
(define rackton-keyword-names
  (sort (map symbol->string (set->list rackton-keyword-set)) string<?))

(define color-categories
  '(paren keyword identifier type constructor string literal comment error))

(define (valid-category? c) (and (memq c color-categories) #t))

;; The semantic category of a token.  `name-kind` is the kernel's
;; env-backed classifier (symbol → 'type | 'constructor | #f) — the
;; lexer alone cannot tell `Maybe` from `Just`, both capitalized
;; symbols; only the session env knows.  Unknown names (e.g. a type
;; being defined in this very entry) are plain identifiers.
(define (token-category text t name-kind)
  (case (tok-type t)
    [(parenthesis) 'paren]
    [(string) 'string]
    [(constant) 'literal]
    [(comment sexp-comment) 'comment]
    [(error) 'error]
    [(white-space) #f]
    [else
     (define sym (string->symbol (substring text (tok-start t) (tok-end t))))
     (cond
       [(set-member? rackton-keyword-set sym) 'keyword]
       [(name-kind sym) => values]
       [else 'identifier])]))

;; Every non-whitespace token of `text` with its category, as
;; (lexeme . category) pairs — the testable face of classification.
(define (text-categories text name-kind)
  (for*/list ([t (in-list (tokenize text))]
              [cat (in-value (token-category text t name-kind))]
              #:when cat)
    (cons (substring text (tok-start t) (tok-end t)) cat)))

;; ----- coloring: palettes ------------------------------------------------

;; A palette maps each category to a color name.  'none and 'default
;; both mean "no escape emitted" — the terminal's own color.
(define color-sgr-table
  (hasheq 'black "30" 'red "31" 'green "32" 'yellow "33"
          'blue "34" 'magenta "35" 'cyan "36" 'light-gray "37"
          'dark-gray "90" 'light-red "91" 'light-green "92"
          'light-yellow "93" 'light-blue "94" 'light-magenta "95"
          'light-cyan "96" 'white "97"))

(define color-names
  (append (sort (hash-keys color-sgr-table)
                string<? #:key symbol->string)
          '(default none)))

(define (valid-color? c) (and (memq c color-names) #t))

(define (color->sgr c) (hash-ref color-sgr-table c #f))

;; Built-in schemes.  'standard is the editor's historical look plus
;; the type/constructor split; 'plain is color-free.
(define standard-scheme
  (hasheq 'paren 'red 'keyword 'red 'identifier 'light-blue
          'type 'cyan 'constructor 'magenta
          'string 'green 'literal 'green
          'comment 'yellow 'error 'light-red))

(define color-schemes
  (hasheq 'standard standard-scheme
          'plain (for/hasheq ([c (in-list color-categories)])
                   (values c 'none))))

(define scheme-names
  (sort (hash-keys color-schemes) string<? #:key symbol->string))

(define (scheme-palette name) (hash-ref color-schemes name #f))

;; The live selection: a scheme plus per-category overrides, kept in
;; parameters so tests can sandbox them; persisted via the preferences
;; file (current-color-pref-file: #f = the system preferences).
(define current-color-scheme (make-parameter 'standard))
(define current-color-overrides (make-parameter '()))
(define current-color-pref-file (make-parameter #f))

;; The palette in force: scheme + overrides — unless NO_COLOR is set
;; in the environment (the informal standard), which forces 'plain.
(define (resolved-palette)
  (cond
    [(getenv "NO_COLOR") (scheme-palette 'plain)]
    [else
     (for/fold ([palette (scheme-palette (current-color-scheme))])
               ([ov (in-list (current-color-overrides))])
       (hash-set palette (car ov) (cdr ov)))]))

;; ----- coloring: persistence ----------------------------------------------

;; Preference value: (list scheme-name overrides-alist).  Loading is
;; forgiving — anything malformed leaves the defaults.

(define (load-color-prefs!)
  (define v (get-preference 'rackton-colors (lambda () #f) 'timestamp
                            (current-color-pref-file)))
  (when (and (list? v) (= (length v) 2))
    (define scheme (car v))
    (define overrides (cadr v))
    (when (scheme-palette scheme)
      (current-color-scheme scheme))
    (when (list? overrides)
      (current-color-overrides
       (for/list ([ov (in-list overrides)]
                  #:when (and (pair? ov)
                              (valid-category? (car ov))
                              (valid-color? (cdr ov))))
         ov)))))

(define (save-color-prefs!)
  (with-handlers ([exn:fail? (lambda (_) (void))])   ; best effort
    (put-preferences '(rackton-colors)
                     (list (list (current-color-scheme)
                                 (current-color-overrides)))
                     #f
                     (current-color-pref-file))))

;; Switch scheme (overrides persist across switches).  #f on an
;; unknown name.
(define (set-color-scheme! name)
  (and (scheme-palette name)
       (begin (current-color-scheme name)
              (save-color-prefs!)
              #t)))

;; Override one category.  #f on an unknown category or color.
(define (set-color! category color)
  (and (valid-category? category)
       (valid-color? color)
       (begin (current-color-overrides
               (cons (cons category color)
                     (filter (lambda (ov) (not (eq? (car ov) category)))
                             (current-color-overrides))))
              (save-color-prefs!)
              #t)))

;; The ,colors listing.
(define (colors-summary)
  (define palette (resolved-palette))
  (with-output-to-string
    (lambda ()
      (printf "scheme: ~a~a\n"
              (current-color-scheme)
              (if (getenv "NO_COLOR") "  (NO_COLOR is set: colors are off)" ""))
      (for ([cat (in-list color-categories)])
        (printf "  ~a~a\n"
                (let ([s (symbol->string cat)])
                  (string-append s (make-string (- 13 (string-length s)) #\space)))
                (hash-ref palette cat)))
      (printf "set with: ,colors SCHEME or ,colors CATEGORY COLOR\n")
      (printf "schemes: ~a\n" (string-join (map symbol->string scheme-names) " "))
      (printf "colors:  ~a\n" (string-join (map symbol->string color-names) " ")))))

;; ----- coloring: emission ---------------------------------------------------

;; Emit text[a,b) with per-token coloring under `palette`.
(define (emit-colored text a b palette name-kind)
  (define toks (tokenize text))
  (for ([t (in-list toks)]
        #:unless (or (<= (tok-end t) a) (>= (tok-start t) b)))
    (define s (max a (tok-start t)))
    (define e (min b (tok-end t)))
    (define cat (token-category text t name-kind))
    (define sgr (and cat (color->sgr (hash-ref palette cat 'none))))
    (if sgr
        (printf "\e[~am~a\e[0m" sgr (substring text s e))
        (display (substring text s e)))))

;; ----- the imperative shell -------------------------------------------------

(struct term-handle (history-box saved-stty) #:transparent)

(define (stty-saved-settings)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (define out (string-trim
                 (with-output-to-string (lambda () (system "stty -g")))))
    (and (positive? (string-length out)) out)))

(define (stty-raw!) (void (system "stty raw -echo")))
(define (stty-restore! settings) (void (system (string-append "stty " settings))))

;; Returns a handle, or #f when not on a terminal (the caller falls
;; back to plain line reading).
(define (rackton-term-open history)
  (and (terminal-port? (current-input-port))
       (terminal-port? (current-output-port))
       (let ([saved (stty-saved-settings)])
         (and saved (term-handle (box history) saved)))))

;; The updated history (most recent first), restoring the terminal.
(define (rackton-term-close h)
  (unbox (term-handle-history-box h)))

;; Read one accepted entry as raw text, or eof.  Raw mode and
;; bracketed paste are scoped to this call, so kernel output (and
;; breaks during evaluation) happen on a normal terminal.
(define (rackton-term-read h
                           #:prompt [prompt "λ> "]
                           #:ready? [ready? (lambda (_) #t)]
                           #:completions [completions (lambda (_text pos)
                                                        (values pos '()))]
                           #:name-kind [name-kind (lambda (_) #f)])
  (define cfg (editor-config ready? completions prompt name-kind))
  (define in (current-input-port))
  (define hist-box (term-handle-history-box h))
  (define width (or (detect-display-columns) 80))
  (stty-raw!)
  (display "\e[?2004h")
  (dynamic-wind
   void
   (lambda ()
     (let loop ([st (make-editor-state (unbox hist-box))]
                [prev-rows-up 0]
                [pending '()])
       (define rows-up (draw! st cfg width prev-rows-up))
       (cond
         [(eq? (editor-state-done st) 'accept)
          (finish-line! st cfg width rows-up)
          (define text (entry-text (editor-state-entry st)))
          (set-box! hist-box
                    (let ([hist (unbox hist-box)])
                      (if (and (pair? hist) (equal? (car hist) text))
                          hist
                          (cons text hist))))
          text]
         [(eq? (editor-state-done st) 'eof)
          (finish-line! st cfg width rows-up)
          eof]
         [else
          (define-values (key rest) (read-one-key in pending))
          (loop (editor-step st cfg key) rows-up rest)])))
   (lambda ()
     (display "\e[?2004l")
     (stty-restore! (term-handle-saved-stty h))
     (flush-output))))

;; Read enough characters to decode one key.  A lone ESC is
;; disambiguated by a short wait for a continuation byte — the wait
;; must come BEFORE decoding, since `(ESC)` decodes (totally) to the
;; ESC key rather than to 'partial.
(define (read-one-key in pending)
  (let loop ([buf pending])
    (cond
      [(null? buf)
       (define c (read-char in))
       (if (eof-object? c) (values 'input-eof '()) (loop (list c)))]
      [(and (equal? buf '(#\u1B))
            (begin (sleep 0.02) (char-ready? in)))
       (define c (read-char in))
       (if (eof-object? c) (values "ESC" '()) (loop (list #\u1B c)))]
      [else
       (define-values (key rest) (decode-key buf))
       (cond
         [(not (eq? key 'partial)) (values key rest)]
         [else
          (define c (read-char in))
          (if (eof-object? c)
              (values 'input-eof '())
              (loop (append buf (list c))))])])))

;; Repaint: clear from the entry's first row down, emit every row with
;; coloring, the transient message below, and park the cursor on its
;; cell.  Returns the cursor's row index (rows to move up next time).
(define (draw! st cfg width prev-rows-up)
  (define en (editor-state-entry st))
  (define text (entry-text en))
  (define prompt (editor-config-prompt cfg))
  (define plen (string-length prompt))
  (define rows (layout-rows text width plen))
  (match-define (cons crow ccol)
    (layout-cursor text (entry-point en) width plen))
  (define palette (resolved-palette))
  (display "\e[?25l\r")
  (when (> prev-rows-up 0) (printf "\e[~aA" prev-rows-up))
  (display "\e[J")
  (for ([r (in-list rows)] [i (in-naturals)])
    (display (if (zero? i) prompt (make-string plen #\space)))
    (emit-colored text (first r) (second r)
                  palette (editor-config-name-kind cfg))
    (display "\r\n"))
  (define msg (editor-state-message st))
  (define msg-lines
    (cond [msg (printf "\e[2m~a\e[0m\r"
                       (let ([m msg])
                         (if (> (string-length m) (sub1 width))
                             (substring m 0 (sub1 width))
                             m)))
               1]
          [else 0]))
  ;; The cursor sits on the line below the last row (the message, if
  ;; any, was printed there without a newline); the target is row crow.
  (printf "\e[~aA" (- (length rows) crow))
  (display "\r")
  (when (> ccol 0) (printf "\e[~aC" ccol))
  (display "\e[?25h")
  (flush-output)
  crow)

;; Move past the entry and leave the terminal on a fresh line.
(define (finish-line! st cfg width rows-up)
  (define en (editor-state-entry st))
  (define rows (layout-rows (entry-text en) width
                            (string-length (editor-config-prompt cfg))))
  (define down (max 0 (- (length rows) 1 rows-up)))
  (when (> down 0) (printf "\e[~aB" down))
  (display "\r\n")
  (flush-output))
