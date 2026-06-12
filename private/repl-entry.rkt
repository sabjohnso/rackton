#lang racket/base

;; Rackton — the pure entry model under the REPL's structural editor.
;;
;; An entry is an immutable (text, point) pair: the multi-line input
;; being edited and the cursor offset within it (0 ≤ point ≤ length).
;; Everything here is a pure function — the terminal shell
;; (repl-term.rkt) owns all I/O, and the paredit commands
;; (repl-paredit.rkt) are pure functions over entries.
;;
;; This module is the Racket analogue of what Emacs gives paredit.el:
;;
;;   parse-partial-sexp / syntax table  →  tokenize (the standard
;;       Racket lexer — so structure agrees with the real reader and
;;       with the display coloring, and char literals like #\( or
;;       parens inside strings/comments are never mistaken for
;;       delimiters)
;;   in-string-p / in-comment-p         →  entry-in-string? / -comment?
;;   backward-up-list / up-list / …     →  *-position functions
;;
;; Positions are 0-based offsets between characters, like Emacs
;; markers minus one.  Motion functions return a position or #f when
;; the motion is impossible — callers refuse rather than guess
;; (paredit's error discipline).

(provide (struct-out entry)
         entry-insert
         entry-insert-at
         entry-delete
         entry-goto
         entry-length
         marked->entry
         entry->marked
         string-span-at
         entry-in-string?
         entry-in-comment?
         entry-enclosing-openers
         entry-balanced?
         tokenize
         (struct-out tok)
         forward-sexp-position
         backward-sexp-position
         up-list-position
         backward-up-position
         down-list-position
         closer-for-opener
         enclosing-delimiters)

(require racket/match
         (only-in syntax-color/racket-lexer racket-lexer))

;; ----- the entry ----------------------------------------------------

(struct entry (text point) #:transparent)

(define (entry-length en)
  (string-length (entry-text en)))

;; Insert `str` at the point; the point ends up after it.
(define (entry-insert en str)
  (define t (entry-text en))
  (define p (entry-point en))
  (entry (string-append (substring t 0 p) str (substring t p))
         (+ p (string-length str))))

;; Insert `str` at an arbitrary position; the point stays with the
;; text around it (a point exactly at `pos` stays before the
;; insertion — the complement of entry-insert).
(define (entry-insert-at en pos str)
  (define t (entry-text en))
  (define p (entry-point en))
  (entry (string-append (substring t 0 pos) str (substring t pos))
         (if (> p pos) (+ p (string-length str)) p)))

;; Delete [from,to); the point stays with the text around it.
(define (entry-delete en from to)
  (define t (entry-text en))
  (define p (entry-point en))
  (entry (string-append (substring t 0 from) (substring t to))
         (cond [(<= p from) p]
               [(>= p to)   (- p (- to from))]
               [else        from])))

(define (entry-goto en pos)
  (entry (entry-text en) pos))

;; paredit.el's example notation: a `|` marks the point.
;; "(a |b)" ⇄ (entry "(a b)" 3).  Used by tests and documentation;
;; inputs containing block comments need explicit entries instead,
;; since `#|`/`|#` contain the marker character.
(define (marked->entry str)
  (define i (for/first ([c (in-string str)] [k (in-naturals)]
                        #:when (char=? c #\|))
              k))
  (entry (string-append (substring str 0 i) (substring str (add1 i)))
         i))

(define (entry->marked en)
  (string-append (substring (entry-text en) 0 (entry-point en))
                 "|"
                 (substring (entry-text en) (entry-point en))))

;; ----- tokenizing ----------------------------------------------------

;; A token: `type` is the Racket lexer's classification ('parenthesis,
;; 'symbol, 'string, 'comment, 'sexp-comment, 'white-space, 'constant,
;; 'error, …), `paren` its delimiter tag for parenthesis tokens, and
;; [start,end) its 0-based extent.
(struct tok (type paren start end) #:transparent)

(define (tokenize text)
  (define in (open-input-string text))
  ;; Count positions in characters, not bytes — otherwise every token
  ;; after a non-ASCII character (λ!) reports shifted offsets.
  (port-count-lines! in)
  (let loop ([acc '()])
    (define-values (lexeme type paren start end) (racket-lexer in))
    (if (eof-object? lexeme)
        (reverse acc)
        (loop (cons (tok type paren (sub1 start) (sub1 end)) acc)))))

(define (opener? t)
  (and (eq? (tok-type t) 'parenthesis)
       (memq (tok-paren t) '(|(| |[| |{|))
       #t))

(define (closer? t)
  (and (eq? (tok-type t) 'parenthesis)
       (memq (tok-paren t) '(|)| |]| |}|))
       #t))

;; Atom-ish tokens read as one sexp: symbols, literals, strings — and
;; 'error tokens (an unterminated string), which span to eof.
(define (atom? t)
  (not (memq (tok-type t) '(parenthesis white-space comment sexp-comment))))

;; Tokens that separate sexps without being one.  A '#;' prefix is
;; treated as a separator, so the commented datum reads as the next
;; sexp — predictable, and what motion through it feels like in Emacs.
(define (skippable? t)
  (memq (tok-type t) '(white-space comment sexp-comment)))

(define (closer-for-opener c)
  (case c [(#\() #\)] [(#\[) #\]] [(#\{) #\}] [else #f]))

;; ----- context queries ------------------------------------------------

;; A string token: a real 'string, or the 'error token an unterminated
;; string produces (it starts with a double quote).
(define (string-tok? text t)
  (or (eq? (tok-type t) 'string)
      (and (eq? (tok-type t) 'error)
           (char=? (string-ref text (tok-start t)) #\"))))

;; Inside a string: strictly after the opening quote and strictly
;; before the closing one — except an unterminated string has no
;; closing quote, so its end (eof) is still inside.
(define (entry-in-string? en)
  (define text (entry-text en))
  (define p (entry-point en))
  (for/or ([t (in-list (tokenize text))])
    (and (string-tok? text t)
         (> p (tok-start t))
         (or (< p (tok-end t))
             (and (= p (tok-end t))
                  (unterminated-string? text t))))))

(define (unterminated-string? text t)
  (or (eq? (tok-type t) 'error)
      (< (- (tok-end t) (tok-start t)) 2)
      (not (char=? (string-ref text (sub1 (tok-end t))) #\"))))

;; The string token whose interior contains `pos` (by the same
;; interiority rule as entry-in-string?), as (list start end
;; terminated?), or #f.
(define (string-span-at text pos)
  (for/first ([t (in-list (tokenize text))]
              #:when (and (string-tok? text t)
                          (> pos (tok-start t))
                          (or (< pos (tok-end t))
                              (and (= pos (tok-end t))
                                   (unterminated-string? text t)))))
    (list (tok-start t) (tok-end t) (not (unterminated-string? text t)))))

;; Inside a comment.  A line comment's extent runs up to (and
;; including) the position just before its newline — inserting there
;; still lands in the comment — while a terminated block comment ends
;; at its `|#`, after which the point is outside.
(define (entry-in-comment? en)
  (define text (entry-text en))
  (define p (entry-point en))
  (for/or ([t (in-list (tokenize text))])
    (and (eq? (tok-type t) 'comment)
         (> p (tok-start t))
         (if (block-comment? text t)
             (< p (tok-end t))
             (<= p (tok-end t))))))

(define (block-comment? text t)
  (and (>= (- (tok-end t) (tok-start t)) 2)
       (char=? (string-ref text (tok-start t)) #\#)
       (char=? (string-ref text (add1 (tok-start t))) #\|)))

;; The unclosed openers around `point`, innermost first, as
;; (char . position) pairs.  Strings, comments, and char literals are
;; already non-parenthesis tokens, so only structural delimiters count.
(define (entry-enclosing-openers en)
  (openers-before (entry-text en) (entry-point en)))

(define (openers-before text pos)
  (for/fold ([stack '()])
            ([t (in-list (tokenize text))]
             #:when (<= (tok-end t) pos))
    (cond [(opener? t) (cons (cons (string-ref text (tok-start t)) (tok-start t))
                             stack)]
          [(closer? t) (if (null? stack) stack (cdr stack))]
          [else stack])))

;; Balanced the way the reader decides it: every datum reads to eof
;; without running out of input.  (Mismatched delimiters are "ready"
;; in the same sense as in repl-editor.rkt: the reader will produce
;; the error message, which is not this module's concern.)
(define (entry-balanced? en)
  (define in (open-input-string (entry-text en)))
  (with-handlers ([exn:fail:read:eof? (lambda (_) #f)]
                  [exn:fail:read?     (lambda (_) #t)])
    (let loop ()
      (if (eof-object? (read in)) #t (loop)))))

;; ----- motion ----------------------------------------------------------

;; All motion functions: text × position → position | #f.

;; Forward over one sexp: from inside an atom, to its end; otherwise
;; skip separators and step over the next atom or balanced list.
(define (forward-sexp-position text pos)
  (define toks (tokenize text))
  (cond
    [(for/first ([t (in-list toks)]
                 #:when (and (atom? t) (< (tok-start t) pos) (< pos (tok-end t))))
       t)
     => tok-end]
    [else
     (let loop ([ts (skip-to toks pos)])
       (match ts
         ['() #f]
         [(cons t rest)
          (cond [(skippable? t) (loop rest)]
                [(atom? t)      (tok-end t)]
                [(opener? t)    (match-forward ts)]
                [else           #f])]))]))     ; closer

;; The tokens starting at or after `pos`.
(define (skip-to toks pos)
  (for/list ([t (in-list toks)] #:when (>= (tok-start t) pos)) t))

;; ts starts with an opener; the position after its matching closer.
(define (match-forward ts)
  (let loop ([ts (cdr ts)] [depth 1])
    (match ts
      ['() #f]
      [(cons t rest)
       (cond [(opener? t) (loop rest (add1 depth))]
             [(closer? t) (if (= depth 1) (tok-end t) (loop rest (sub1 depth)))]
             [else        (loop rest depth)])])))

;; Backward over one sexp — the mirror image, scanning the tokens that
;; end at or before `pos` from the right.
(define (backward-sexp-position text pos)
  (define toks (tokenize text))
  (cond
    [(for/first ([t (in-list toks)]
                 #:when (and (atom? t) (< (tok-start t) pos) (< pos (tok-end t))))
       t)
     => tok-start]
    [else
     (let loop ([ts (reverse (for/list ([t (in-list toks)]
                                        #:when (<= (tok-end t) pos))
                               t))])
       (match ts
         ['() #f]
         [(cons t rest)
          (cond [(skippable? t) (loop rest)]
                [(atom? t)      (tok-start t)]
                [(closer? t)    (match-backward ts)]
                [else           #f])]))]))    ; opener

(define (match-backward ts)
  (let loop ([ts (cdr ts)] [depth 1])
    (match ts
      ['() #f]
      [(cons t rest)
       (cond [(closer? t) (loop rest (add1 depth))]
             [(opener? t) (if (= depth 1) (tok-start t) (loop rest (sub1 depth)))]
             [else        (loop rest depth)])])))

;; After the closer of the enclosing list, or #f when there is no
;; enclosing list or it is unterminated.
(define (up-list-position text pos)
  (match (openers-before text pos)
    ['() #f]
    [(cons (cons _open opos) _)
     (match-forward (skip-to (tokenize text) opos))]))

;; On the opening delimiter of the enclosing list.
(define (backward-up-position text pos)
  (match (openers-before text pos)
    ['() #f]
    [(cons (cons _open opos) _) opos]))

;; Every enclosing list around `pos`, innermost first, each as
;; (list open-char open-pos close-pos-or-#f) — close-pos is the
;; position OF the closing delimiter (one before where up-list lands),
;; #f when the list is unterminated.  This is what the slurp/barf
;; delimiter-rotation loops walk.
(define (enclosing-delimiters text pos)
  (define toks (tokenize text))
  (for/list ([opener (in-list (openers-before text pos))])
    (match-define (cons open-char open-pos) opener)
    (define after-close (match-forward (skip-to toks open-pos)))
    (list open-char open-pos (and after-close (sub1 after-close)))))

;; Just inside the next list, skipping atoms and separators — #f if a
;; closer (or eof) comes first.
(define (down-list-position text pos)
  (let loop ([ts (skip-to (tokenize text) pos)])
    (match ts
      ['() #f]
      [(cons t rest)
       (cond [(opener? t) (tok-end t)]
             [(closer? t) #f]
             [else        (loop rest)])])))
