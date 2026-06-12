#lang racket/base

;; Rackton — paredit commands, ported from paredit.el.
;;
;; Every command is a pure function over an entry (see repl-entry.rkt)
;; and obeys paredit's contract: starting from a balanced entry, the
;; result is balanced — no keystroke routed through these commands can
;; break structure.  A command that cannot apply (no enclosing list,
;; nothing to slurp, …) returns the entry unchanged: refusal, never
;; corruption.  `paredit-kill` additionally returns the killed text,
;; for the shell's yank buffer; everything else is entry → entry.
;;
;; The transforms follow paredit.el's algorithms — in particular the
;; slurp delimiter-rotation loop, where the actual delimiter character
;; is read from the text at each level, which is what makes mixed
;; (), [], {} work without this module knowing a delimiter "set".
;;
;; Deviations from paredit.el, all toward simplicity:
;;   - no automatic reindentation after transforms (entries are small;
;;     the shell has an explicit reindent command);
;;   - deleting into a character literal (#\x) removes the whole
;;     literal rather than paredit's two-character dance;
;;   - slurping from inside a string (paredit's slurp-into-string) is
;;     not supported — the command refuses there.

(provide paredit-open-round
         paredit-open-square
         paredit-close-round
         paredit-doublequote
         paredit-backward-delete
         paredit-forward-delete
         paredit-kill
         paredit-slurp-forward
         paredit-slurp-backward
         paredit-barf-forward
         paredit-barf-backward
         paredit-splice
         paredit-raise
         paredit-wrap-round)

(require racket/match
         racket/list
         "repl-entry.rkt")

;; ----- small text utilities -----------------------------------------

(define (char-at text i)
  (and (>= i 0) (< i (string-length text)) (string-ref text i)))

(define (entry-char-before en) (char-at (entry-text en) (sub1 (entry-point en))))
(define (entry-char-after en)  (char-at (entry-text en) (entry-point en)))

(define (opener-char? c) (and c (memv c '(#\( #\[ #\{)) #t))
(define (closer-char? c) (and c (memv c '(#\) #\] #\})) #t))

;; Characters that prefix a datum and so want no space between
;; themselves and an inserted delimiter: quote, quasiquote, unquote,
;; reader prefixes.
(define (prefix-char? c) (and c (memv c '(#\' #\` #\, #\# #\@ #\~)) #t))

;; paredit-space-for-delimiter-p, simplified: when inserting a
;; delimiter at `pos`, does a space belong before / after it?
(define (space-before? text pos)
  (define c (char-at text (sub1 pos)))
  (and c
       (not (char-whitespace? c))
       (not (opener-char? c))
       (not (prefix-char? c))))

(define (space-after? text pos)
  (define c (char-at text pos))
  (and c
       (not (char-whitespace? c))
       (not (closer-char? c))))

;; Position of the next newline at or after pos, or the text length.
(define (end-of-line-position text pos)
  (or (for/first ([i (in-range pos (string-length text))]
                  #:when (char=? (string-ref text i) #\newline))
        i)
      (string-length text)))

(define (skip-whitespace-backward text pos)
  (let loop ([i pos])
    (if (and (> i 0) (char-whitespace? (string-ref text (sub1 i))))
        (loop (sub1 i))
        i)))

(define (skip-whitespace-forward text pos)
  (let loop ([i pos])
    (if (and (< i (string-length text)) (char-whitespace? (string-ref text i)))
        (loop (add1 i))
        i)))

;; Inside a string: is `pos` immediately after an odd run of
;; backslashes (i.e. between an escape and its escaped character)?
(define (in-string-escape? text pos)
  (let loop ([i pos] [n 0])
    (if (and (> i 0) (char=? (string-ref text (sub1 i)) #\\))
        (loop (sub1 i) (add1 n))
        (odd? n))))

;; The #\x-style character-literal token whose extent touches `pos`
;; under `which` ('backward: pos in (start,end]; 'forward: pos in
;; [start,end); 'interior: strictly inside), as (cons start end), or #f.
(define (char-literal-span text pos which)
  (for/first ([t (in-list (tokenize text))]
              #:when (and (eq? (tok-type t) 'constant)
                          (>= (- (tok-end t) (tok-start t)) 2)
                          (eqv? (char-at text (tok-start t)) #\#)
                          (eqv? (char-at text (add1 (tok-start t))) #\\)
                          (case which
                            [(backward) (and (> pos (tok-start t))
                                             (<= pos (tok-end t)))]
                            [(forward)  (and (>= pos (tok-start t))
                                             (< pos (tok-end t)))]
                            [(interior) (and (> pos (tok-start t))
                                             (< pos (tok-end t)))])))
    (cons (tok-start t) (tok-end t))))

;; Inserting anything inside a character literal would re-tokenize it
;; unpredictably (e.g. `#(` becomes a vector opener) — the electric
;; commands refuse there.
(define (inside-char-literal? text pos)
  (and (char-literal-span text pos 'interior) #t))

;; ----- electric: delimiters ------------------------------------------

;; `(` — insert a balanced pair with the point inside, spacing it off
;; from neighboring atoms; inside a string or comment, insert the
;; character itself.
(define ((make-open-pair open close) en)
  (cond
    [(or (entry-in-string? en) (entry-in-comment? en))
     (entry-insert en (string open))]
    [(inside-char-literal? (entry-text en) (entry-point en)) en]
    [else
     (define text (entry-text en))
     (define p (entry-point en))
     (let* ([en (if (space-before? text p) (entry-insert en " ") en)]
            [en (entry-insert en (string open))]
            [en (entry-insert-at en (entry-point en) (string close))]
            [en (if (space-after? (entry-text en) (add1 (entry-point en)))
                    (entry-insert-at en (add1 (entry-point en)) " ")
                    en)])
       en)]))

(define paredit-open-round  (make-open-pair #\( #\)))
(define paredit-open-square (make-open-pair #\[ #\]))

;; `)` — never inserts structure: move past the enclosing list's
;; closing delimiter, deleting the whitespace before it.  Inside a
;; string or comment, insert the character; with no (terminated)
;; enclosing list, refuse.
(define (paredit-close-round en)
  (cond
    [(or (entry-in-string? en) (entry-in-comment? en))
     (entry-insert en ")")]
    [(inside-char-literal? (entry-text en) (entry-point en)) en]
    [else
     (define text (entry-text en))
     (match (enclosing-delimiters text (entry-point en))
       [(cons (list _open _opos (? values close-pos)) _)
        (define ws-start (skip-whitespace-backward text close-pos))
        (entry-goto (entry-delete en ws-start close-pos)
                    (add1 ws-start))]
       [_ en])]))

;; `"` — outside: a fresh pair, point inside.  In a string: step over
;; the closing quote when at it, otherwise insert an escaped quote.
;; In a comment: the character itself.
(define (paredit-doublequote en)
  (define text (entry-text en))
  (define p (entry-point en))
  (cond
    [(entry-in-comment? en) (entry-insert en "\"")]
    [(inside-char-literal? text p) en]
    [(string-span-at text p)
     => (match-lambda
          [(list _start end #t)
           #:when (= p (sub1 end))
           (entry-goto en end)]
          [_ (entry-insert en "\\\"")])]
    [else
     (let* ([en (if (space-before? text p) (entry-insert en " ") en)]
            [en (entry-insert en "\"")]
            [en (entry-insert-at en (entry-point en) "\"")]
            [en (if (space-after? (entry-text en) (add1 (entry-point en)))
                    (entry-insert-at en (add1 (entry-point en)) " ")
                    en)])
       en)]))

;; ----- electric: deletion ---------------------------------------------

;; A line comment's tail is safe to uncomment only if it contains no
;; structure characters; used when a delete would remove the `;`.
(define (comment-tail-safe? text pos)
  (for/and ([i (in-range pos (end-of-line-position text pos))])
    (not (memv (string-ref text i) '(#\( #\) #\[ #\] #\{ #\} #\")))))

;; Backspace.  See paredit-backward-delete in paredit.el; cases in
;; the same order.
(define (paredit-backward-delete en)
  (define text (entry-text en))
  (define p (entry-point en))
  (define (plain) (entry-delete en (sub1 p) p))
  (cond
    [(zero? p) en]
    [(string-span-at text p)
     => (match-lambda
          [(list start end terminated?)
           (cond
             ;; right after the opening quote
             [(= (sub1 p) start)
              (if (and terminated? (= end (+ start 2)))   ; empty ""
                  (entry-delete en start end)
                  en)]                                     ; refuse
             ;; about to delete a backslash that escapes the next char
             [(in-string-escape? text p)
              (entry-delete en (sub1 p) (add1 p))]
             ;; about to delete an escaped char: take its backslash too
             [(and (eqv? (char-at text (- p 2)) #\\)
                   (in-string-escape? text (sub1 p)))
              (entry-delete en (- p 2) p)]
             [else (plain)])])]
    [(entry-in-comment? en)
     ;; Deleting the `;` itself uncomments the tail — only when safe.
     (if (and (eqv? (entry-char-before en) #\;)
              (not (comment-tail-safe? text p)))
         en
         (plain))]
    [(char-literal-span text p 'backward)
     => (lambda (span) (entry-delete en (car span) (cdr span)))]
    [(or (closer-char? (entry-char-before en))
         (eqv? (entry-char-before en) #\"))
     ;; A well-formed sexp ends here: move into it.  A spurious
     ;; closer (no sexp) gets deleted.
     (if (backward-sexp-position text p)
         (entry-goto en (sub1 p))
         (plain))]
    [(and (opener-char? (entry-char-before en))
          (eqv? (entry-char-after en)
                (closer-for-opener (entry-char-before en))))
     (entry-delete en (sub1 p) (add1 p))]                  ; empty pair
    [(opener-char? (entry-char-before en))
     ;; Beginning of a non-empty list: refuse, unless the opener is
     ;; spurious (never closed).
     (if (forward-sexp-position text (sub1 p))
         en
         (plain))]
    [else (plain)]))

;; C-d / Delete — the mirror image.
(define (paredit-forward-delete en)
  (define text (entry-text en))
  (define p (entry-point en))
  (define (plain) (entry-delete en p (add1 p)))
  (cond
    [(= p (string-length text)) en]
    [(string-span-at text p)
     => (match-lambda
          [(list start end terminated?)
           (cond
             ;; at the closing quote
             [(and terminated? (= p (sub1 end)))
              (if (= end (+ start 2))                      ; empty ""
                  (entry-delete en start end)
                  en)]                                     ; refuse
             ;; between a backslash and its escaped char: delete both
             [(in-string-escape? text p)
              (entry-delete en (sub1 p) (add1 p))]
             ;; on a backslash: delete it and the escaped char
             [(eqv? (char-at text p) #\\)
              (entry-delete en p (+ p 2))]
             [else (plain)])])]
    [(entry-in-comment? en)
     ;; Deleting the comment's newline would swallow the next line.
     (if (eqv? (entry-char-after en) #\newline) en (plain))]
    [(eqv? (entry-char-after en) #\;)
     (if (comment-tail-safe? text (add1 p)) (plain) en)]
    [(char-literal-span text p 'forward)
     => (lambda (span) (entry-delete en (car span) (cdr span)))]
    [(opener-char? (entry-char-after en))
     (entry-goto en (add1 p))]                             ; move into
    [(eqv? (entry-char-after en) #\")
     ;; An empty string deletes whole; a non-empty one is entered.
     (if (eqv? (char-at text (add1 p)) #\")
         (entry-delete en p (+ p 2))
         (entry-goto en (add1 p)))]
    [(closer-char? (entry-char-after en))
     (if (and (opener-char? (entry-char-before en))
              (eqv? (entry-char-after en)
                    (closer-for-opener (entry-char-before en))))
         (entry-delete en (sub1 p) (add1 p))
         en)]                                              ; refuse
    [else (plain)]))

;; ----- kill -------------------------------------------------------------

;; C-k: kill to the end of the line, structurally — whole sexps that
;; begin on this line (even if they continue past it), stopping at the
;; enclosing list's closer; inside a string or comment, kill the
;; rest of the contents on the line.  Returns the new entry and the
;; killed text (#f when there was nothing to kill).
(define (paredit-kill en)
  (define text (entry-text en))
  (define p (entry-point en))
  (define (kill-to limit)
    (if (> limit p)
        (values (entry-delete en p limit) (substring text p limit))
        (values en #f)))
  (cond
    [(string-span-at text p)
     => (match-lambda
          [(list _start end terminated?)
           (define content-end (if terminated? (sub1 end) end))
           (kill-to (min content-end (end-of-line-position text p)))])]
    [(entry-in-comment? en)
     (kill-to (end-of-line-position text p))]
    [(inside-char-literal? text p)
     (values en #f)]   ; killing half a #\x literal leaves a stray #
    [else
     (define eol (end-of-line-position text p))
     (define bound
       (match (enclosing-delimiters text p)
         [(cons (list _ _ (? values close-pos)) _) close-pos]
         [_ (string-length text)]))
     ;; Consume whole sexps whose start lies on this line.
     (define after-sexps
       (let loop ([cur p])
         (define fs (forward-sexp-position text cur))
         (define ss (and fs (backward-sexp-position text fs)))
         (if (and fs ss (< ss (min eol bound)) (<= fs bound))
             (loop fs)
             cur)))
     ;; Then absorb a trailing comment/whitespace run up to eol, when
     ;; no closer intervenes.
     (define end
       (if (and (< after-sexps (min eol bound))
                (for/and ([i (in-range after-sexps (min eol bound))])
                  (not (closer-char? (string-ref text i)))))
           (min eol bound)
           after-sexps))
     (cond
       [(> end p) (kill-to end)]
       [(and (= end p) (eqv? (char-at text p) #\newline)
             (not (entry-in-comment? en)))
        (kill-to (add1 p))]                ; kill the line break
       [else (values en #f)])]))

;; ----- transforms ----------------------------------------------------------

;; Forward slurp — paredit's delimiter-rotation loop: delete the
;; enclosing closer; if no sexp follows, move outward one level at a
;; time, each level's closer character shifting inward by one, until a
;; sexp can be stepped over; the outermost visited closer lands after
;; it.  Refuses in comments/strings, at top level, and when nothing
;; can ever be slurped.
(define (paredit-slurp-forward en)
  (cond
    [(or (entry-in-string? en) (entry-in-comment? en)) en]
    [else
     (define text (entry-text en))
     (match (enclosing-delimiters text (entry-point en))
       ['() en]
       [(cons (list _ _ #f) _) en]            ; unterminated — refuse
       [(cons (list open0 _ close-pos0) outers)
        (define close0 (char-at text close-pos0))
        (let loop ([en* (entry-delete en close-pos0 (add1 close-pos0))]
                   [carry close0]
                   [probe close-pos0]
                   [outers outers])
          (define fs (forward-sexp-position (entry-text en*) probe))
          (cond
            [fs (entry-insert-at en* fs (string carry))]
            [else
             (match outers
               ['() en]                       ; nothing to slurp — refuse
               [(cons (list _ _ #f) _) en]    ; unterminated above — refuse
               [(cons (list _ _ cpos) rest)
                ;; cpos is in the original text; one deletion before it
                ;; has happened since.
                (define q (sub1 cpos))
                (define c (char-at (entry-text en*) q))
                (loop (entry-insert-at (entry-delete en* q (add1 q))
                                       q (string carry))
                      c (add1 q) rest)])]))])]))

;; Backward slurp — the mirror: the opener walks left over the
;; preceding sexp, rotating outward through levels when none precedes.
(define (paredit-slurp-backward en)
  (cond
    [(or (entry-in-string? en) (entry-in-comment? en)) en]
    [else
     (define text (entry-text en))
     (match (enclosing-delimiters text (entry-point en))
       ['() en]
       [(cons (list open0 open-pos0 _) outers)
        (let loop ([en* (entry-delete en open-pos0 (add1 open-pos0))]
                   [carry open0]
                   [probe open-pos0]
                   [outers outers])
          (define bs (backward-sexp-position (entry-text en*) probe))
          (cond
            [bs (entry-insert-at en* bs (string carry))]
            [else
             (match outers
               ['() en]
               [(cons (list c opos _) rest)
                ;; opos is before every edit so far — still valid.
                (loop (entry-insert-at (entry-delete en* opos (add1 opos))
                                       opos (string carry))
                      c opos rest)])]))])]))

;; Forward barf: the enclosing closer hops back over the last sexp.
(define (paredit-barf-forward en)
  (cond
    [(or (entry-in-string? en) (entry-in-comment? en)) en]
    [else
     (define text (entry-text en))
     (match (enclosing-delimiters text (entry-point en))
       [(cons (list _ open-pos (? values close-pos)) _)
        (define last-start (backward-sexp-position text close-pos))
        (cond
          [(not last-start) en]                ; empty list — refuse
          [else
           (define dest (skip-whitespace-backward text last-start))
           (define c (char-at text close-pos))
           (if (<= dest (add1 open-pos))
               en                              ; would barf everything out
               (entry-insert-at (entry-delete en close-pos (add1 close-pos))
                                dest (string c)))])]
       [_ en])]))

;; Backward barf: the opener hops forward over the first sexp.
(define (paredit-barf-backward en)
  (cond
    [(or (entry-in-string? en) (entry-in-comment? en)) en]
    [else
     (define text (entry-text en))
     (match (enclosing-delimiters text (entry-point en))
       [(cons (list open open-pos close-pos) _)
        (define first-end (forward-sexp-position text (add1 open-pos)))
        (cond
          [(not first-end) en]
          [(and close-pos (>= (skip-whitespace-forward text first-end)
                              close-pos))
           en]                                 ; would barf everything out
          [else
           (define dest (skip-whitespace-forward text first-end))
           (entry-insert-at (entry-delete en open-pos (add1 open-pos))
                            (sub1 dest) (string open))])]
       [_ en])]))

;; Splice: remove the enclosing list's delimiters.
(define (paredit-splice en)
  (cond
    [(or (entry-in-string? en) (entry-in-comment? en)) en]
    [else
     (match (enclosing-delimiters (entry-text en) (entry-point en))
       [(cons (list _ open-pos (? values close-pos)) _)
        (entry-delete (entry-delete en close-pos (add1 close-pos))
                      open-pos (add1 open-pos))]
       [_ en])]))

;; Raise: the sexp at (or after) the point replaces its enclosing list.
(define (paredit-raise en)
  (cond
    [(or (entry-in-string? en) (entry-in-comment? en)) en]
    [else
     (define text (entry-text en))
     (define p (entry-point en))
     (match (enclosing-delimiters text p)
       [(cons (list _ open-pos (? values close-pos)) _)
        (define fs (forward-sexp-position text p))
        (define ss (and fs (backward-sexp-position text fs)))
        (cond
          [(and fs ss (<= fs close-pos))
           (define sexp-text (substring text ss fs))
           (entry (string-append (substring text 0 open-pos)
                                 sexp-text
                                 (substring text (add1 close-pos)))
                  (+ open-pos (max 0 (- p ss))))]
          [else en])]
       [_ en])]))

;; M-( — wrap the following sexp in parentheses, point just inside.
;; With nothing to wrap, degenerate to an empty pair (paredit's
;; zero-sexp wrap); in strings/comments, a literal open paren.
(define (paredit-wrap-round en)
  (cond
    [(or (entry-in-string? en) (entry-in-comment? en))
     (entry-insert en "(")]
    [(inside-char-literal? (entry-text en) (entry-point en)) en]
    [else
     (define text (entry-text en))
     (define p (entry-point en))
     (define fs (forward-sexp-position text p))
     (cond
       [(not fs) (paredit-open-round en)]
       [else
        (let* ([en (entry-insert-at en fs ")")]
               [en (if (space-before? (entry-text en) (entry-point en))
                       (entry-insert en " ")
                       en)]
               [en (entry-insert en "(")])
          en)])]))
