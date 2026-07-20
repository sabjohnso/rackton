#lang racket/base

;; Rackton — what the point is positioned to complete.
;;
;; Completion used to be context-free: whatever name preceded the cursor
;; was matched against the session's bindings, so inside `(require …)`
;; the user was offered variable names.  This module supplies the missing
;; question — *what kind of thing* goes here — from the enclosing forms
;; alone, so every client (the terminal editor, the language server, the
;; Emacs mode's fallback) decides the same way.
;;
;; Three categories:
;;
;;   'identifier    — a name from the environment.  The default: anything
;;                    this module cannot positively classify.
;;   'module-path   — a collection path, as in `(require rackton/data/list)`.
;;   'relative-path — a file path inside a string, as in `(require "u.rkt")`.
;;
;; The decision is structural, never textual, and must hold *mid-edit*:
;; while the user is still typing, the text is unbalanced and often
;; unreadable.  So it works from the token stream and the chain of
;; enclosing lists (repl-entry.rkt) rather than from a parsed datum, and
;; the sub-form grammar it consults is the shared table in
;; require-spec-shape.rkt — the same one inference peels specs with.
;;
;; Public API:
;;   completion-context : string nat -> (values kind nat)
;;       the category at `pos`, and the start of the text a candidate
;;       replaces (so [start, pos) is the prefix typed so far).
;;   completion-word-char? / completion-word-start
;;       the identifier-prefix rule, shared so clients cannot disagree
;;       about where a name begins.

(provide completion-context
         completion-word-char?
         completion-word-start)

(require racket/list
         (only-in "repl-entry.rkt"
                  tokenize tok tok-start tok-end
                  tok-opener? tok-closer? tok-skippable?
                  string-span-at enclosing-delimiters)
         (only-in "require-spec-shape.rkt" require-wrapper-base-index))

;; ----- the category ---------------------------------------------------

(define (completion-context text pos)
  (define frames (enclosing-frames text pos))
  (define span (string-span-at text pos))
  (cond
    ;; A string literal is a module reference only where a bare path
    ;; would be; the prefix is then its contents up to the point.
    [span (if (module-path-position? frames)
              (values 'relative-path (add1 (car span)))
              (values 'identifier (completion-word-start text pos)))]
    [(module-path-position? frames)
     (values 'module-path (completion-word-start text pos))]
    [else (values 'identifier (completion-word-start text pos))]))

;; Does this chain of enclosing forms put the point where a module
;; reference belongs?  `(require SPEC …)` does, and a wrapper sub-form
;; passes the question outward from its own reference position — so
;; `(prefix-in p: (only-in HERE …))` counts, while the same sub-form
;; written outside a require does not.
(define (module-path-position? frames)
  (cond
    [(null? frames) #f]
    [else
     (define head  (frame-head  (car frames)))
     (define index (frame-index (car frames)))
     (cond
       [(eq? head 'require) (>= index 1)]
       [(eqv? index (require-wrapper-base-index head))
        (module-path-position? (cdr frames))]
       [else #f])]))

;; ----- the enclosing forms --------------------------------------------

;; One enclosing list, as its head symbol (#f when the list is empty or
;; does not start with a symbol) and the argument position `pos` occupies
;; — 0 being the head itself.
(struct frame (head index) #:transparent)

;; Every list enclosing `pos`, innermost first.
(define (enclosing-frames text pos)
  (define toks (tokenize text))
  (for/list ([d (in-list (enclosing-delimiters text pos))])
    (frame-of toks text (cadr d) pos)))

(define (frame-of toks text open-pos pos)
  (define children (direct-children toks text open-pos))
  (define before (filter (lambda (c) (< (car c) pos)) children))
  (define head
    (and (pair? children) (datum-symbol text (car children))))
  (cond
    [(null? before) (frame head 0)]
    ;; Inside (or at the end of) the last datum begun before the point:
    ;; that datum's own position.  Past its end: a new datum is being
    ;; started at the next position.
    [(<= pos (cdr (last before))) (frame head (sub1 (length before)))]
    [else (frame head (length before))]))

;; The direct child datums of the list opened at `open-pos`, in order, as
;; [start, end) pairs.  Nested lists count as one child; separators —
;; whitespace, comments, `#;` — are not children.  A child left
;; unterminated by the point of editing runs to the end of the text.
(define (direct-children toks text open-pos)
  (let loop ([ts (for/list ([t (in-list toks)]
                            #:when (> (tok-start t) open-pos))
                   t)]
             [depth 0]
             [start #f]
             [acc '()])
    (cond
      [(null? ts)
       (reverse (if start (cons (cons start (string-length text)) acc) acc))]
      [else
       (define t (car ts))
       (define rest (cdr ts))
       (cond
         ;; Inside a nested list: only its own delimiters matter.
         [(> depth 0)
          (cond
            [(tok-opener? t) (loop rest (add1 depth) start acc)]
            [(tok-closer? t)
             (if (= depth 1)
                 (loop rest 0 #f (cons (cons start (tok-end t)) acc))
                 (loop rest (sub1 depth) start acc))]
            [else (loop rest depth start acc)])]
         [(tok-skippable? t) (loop rest 0 #f acc)]
         [(tok-opener? t)    (loop rest 1 (tok-start t) acc)]
         [(tok-closer? t)    (reverse acc)]      ; this list ends here
         [else (loop rest 0 #f (cons (cons (tok-start t) (tok-end t)) acc))])])))

;; The child datum's text as a symbol, when it reads as one.
(define (datum-symbol text child)
  (define s (substring text (car child) (min (cdr child) (string-length text))))
  (and (positive? (string-length s))
       (not (memv (string-ref s 0) '(#\( #\[ #\{ #\" #\#)))
       (string->symbol s)))

;; ----- the identifier prefix ------------------------------------------

;; A name runs back to the nearest delimiter.  `/` and `.` are ordinary
;; name characters, so a partially typed collection path is captured
;; whole without a separate rule.
(define (completion-word-char? c)
  (not (or (char-whitespace? c)
           (memv c '(#\( #\) #\[ #\] #\{ #\} #\" #\; #\' #\` #\,)))))

(define (completion-word-start text pos)
  (let loop ([i pos])
    (if (and (> i 0) (completion-word-char? (string-ref text (sub1 i))))
        (loop (sub1 i))
        i)))
