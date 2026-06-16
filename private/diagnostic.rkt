#lang racket/base

;; Diagnostic document algebra — a small Wadler/Leijen pretty-printer.
;;
;; Tenets: errors are built as structured `doc` values and laid out to a
;; target width by ONE renderer (`render-doc`).  Width is an argument to
;; rendering, not a parameter threaded through every message — so every
;; diagnostic reflows uniformly, and later concerns (colour, source
;; snippets) attach in one place.  Separation of "what the error says"
;; (the doc) from "how it is presented" (the renderer).
;;
;; The algebra (six constructors) and its laws are property-tested in
;; tests/diagnostic-test.rkt.  This file is the engine only; domain
;; constructors (types, predicate lists, expected/got blocks) and the
;; raise/render boundary build on top of it.
;;
;; Combinators:
;;   doc-nil                      — empty
;;   (doc-text s)                 — literal text (must contain no newline)
;;   doc-line                     — break that flattens to a space
;;   doc-softline                 — break that flattens to nothing
;;   (doc-cat d ...)              — concatenation
;;   (doc-nest n d)               — add n to the indent of breaks inside d
;;   (doc-group d)                — lay d out flat if it fits, else broken
;;   (render-doc d width)         — string

(provide doc?
         doc-nil doc-nil?
         doc-text doc-text? doc-text-string
         doc-line doc-softline doc-line? doc-line-flat
         doc-cat doc-cat? doc-cat-parts
         doc-nest doc-nest? doc-nest-amount doc-nest-body
         doc-group doc-group? doc-group-body
         render-doc
         labeled-block)

;; ----- representation ------------------------------------------------

(struct doc:nil   ()            #:transparent)
(struct doc:text  (string)      #:transparent)
(struct doc:line  (flat)        #:transparent) ; flat = string when flattened
(struct doc:cat   (parts)       #:transparent) ; parts : (listof doc)
(struct doc:nest  (amount body) #:transparent)
(struct doc:group (body)        #:transparent)

(define (doc? x)
  (or (doc:nil? x) (doc:text? x) (doc:line? x)
      (doc:cat? x) (doc:nest? x) (doc:group? x)))

;; ----- constructors / accessors (public names) ----------------------

(define doc-nil (doc:nil))
(define doc-nil? doc:nil?)

(define (doc-text s) (doc:text s))
(define doc-text? doc:text?)
(define doc-text-string doc:text-string)

(define doc-line (doc:line " "))
(define doc-softline (doc:line ""))
(define doc-line? doc:line?)
(define doc-line-flat doc:line-flat)

(define (doc-cat . ds) (doc:cat ds))
(define doc-cat? doc:cat?)
(define doc-cat-parts doc:cat-parts)

(define (doc-nest n d) (doc:nest n d))
(define doc-nest? doc:nest?)
(define doc-nest-amount doc:nest-amount)
(define doc-nest-body doc:nest-body)

(define (doc-group d) (doc:group d))
(define doc-group? doc:group?)
(define doc-group-body doc:group-body)

;; ----- rendering -----------------------------------------------------
;;
;; The standard Wadler/Leijen algorithm.  The worklist holds frames
;; (vector indent mode doc), mode ∈ {flat break}.  A group is rendered
;; flat iff its flattened first line fits the remaining width (`fits?`),
;; otherwise broken.  This decision is local to each group, so nested
;; groups break independently.

(define (frame i mode d) (vector i mode d))
(define (frame-indent f) (vector-ref f 0))
(define (frame-mode f)   (vector-ref f 1))
(define (frame-doc f)    (vector-ref f 2))

;; Does the upcoming content fit in `remaining` columns before the next
;; forced line break?  Only the first line matters.
(define (fits? remaining items)
  (cond
    [(< remaining 0) #f]
    [(null? items) #t]
    [else
     (define f (car items))
     (define rest (cdr items))
     (define i (frame-indent f))
     (define mode (frame-mode f))
     (define d (frame-doc f))
     (cond
       [(doc:nil? d) (fits? remaining rest)]
       [(doc:text? d) (fits? (- remaining (string-length (doc:text-string d))) rest)]
       [(doc:cat? d)
        (fits? remaining (append (map (lambda (p) (frame i mode p)) (doc:cat-parts d)) rest))]
       [(doc:nest? d)
        (fits? remaining (cons (frame (+ i (doc:nest-amount d)) mode (doc:nest-body d)) rest))]
       [(doc:line? d)
        (if (eq? mode 'flat)
            (fits? (- remaining (string-length (doc:line-flat d))) rest)
            #t)] ; a break ends the line → everything so far fit
       [(doc:group? d)
        ;; inside fits, a group keeps the ambient mode
        (fits? remaining (cons (frame i mode (doc:group-body d)) rest))])]))

(define (render-doc doc width)
  (define out (open-output-string))
  (let loop ([col 0] [items (list (frame 0 'break doc))])
    (cond
      [(null? items) (void)]
      [else
       (define f (car items))
       (define rest (cdr items))
       (define i (frame-indent f))
       (define mode (frame-mode f))
       (define d (frame-doc f))
       (cond
         [(doc:nil? d) (loop col rest)]
         [(doc:text? d)
          (define s (doc:text-string d))
          (write-string s out)
          (loop (+ col (string-length s)) rest)]
         [(doc:cat? d)
          (loop col (append (map (lambda (p) (frame i mode p)) (doc:cat-parts d)) rest))]
         [(doc:nest? d)
          (loop col (cons (frame (+ i (doc:nest-amount d)) mode (doc:nest-body d)) rest))]
         [(doc:line? d)
          (cond
            [(eq? mode 'flat)
             (define s (doc:line-flat d))
             (write-string s out)
             (loop (+ col (string-length s)) rest)]
            [else
             (write-string "\n" out)
             (write-string (make-string i #\space) out)
             (loop i rest)])]
         [(doc:group? d)
          (define flat-frame (frame i 'flat (doc:group-body d)))
          (if (fits? (- width col) (cons flat-frame rest))
              (loop col (cons flat-frame rest))
              (loop col (cons (frame i 'break (doc:group-body d)) rest)))])]))
  (get-output-string out))

;; ----- common layout -------------------------------------------------

;; A header followed by a list of pre-rendered item strings, grouped so
;; the whole thing sits on one line when it fits the width
;;   "header: a b c"
;; and otherwise breaks to one item per line, indented `indent` columns
;; past the header's own leading whitespace so the items sit *under* the
;; label rather than level with it:
;;   "  header:
;;      a
;;      b"
;; (`header` already includes any leading indent and trailing
;; punctuation.)
(define (labeled-block header items #:indent [indent 2])
  (doc-cat (doc-text header)
           (doc-group
            (doc-nest (+ (leading-space-count header) indent)
                      (apply doc-cat
                             (map (lambda (s) (doc-cat doc-line (doc-text s))) items))))))

;; The number of leading space characters in `s` — the header's own
;; indentation, which item lines are measured against.
(define (leading-space-count s)
  (let loop ([i 0])
    (if (and (< i (string-length s)) (char=? (string-ref s i) #\space))
        (loop (add1 i))
        i)))
