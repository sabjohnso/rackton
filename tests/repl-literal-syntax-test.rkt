#lang racket/base

;; The bracket/brace literal syntax ([..] lists, {..} maps, #{..} sets)
;; must work at the REPL too.  The literals are detected via the reader's
;; `paren-shape` property, so the REPL has to read input with
;; `read-syntax` (plain `read` drops paren-shape) and thread the syntax
;; through the kernel.  These tests drive the real reader + kernel.

(require rackunit
         "../private/repl.rkt")

;; Feed each source line through the line reader (which preserves
;; paren-shape) into the kernel, accumulating session state and the
;; per-step output strings.
(define (drive lines)
  (for/fold ([state (rackton-repl-init)] [out '()]
             #:result (values state (reverse out)))
            ([line (in-list lines)])
    (define form
      (rackton-read-form (open-input-string (string-append line "\n"))
                         (lambda (_) "")))
    (define-values (state* o) (rackton-repl-step state form))
    (values state* (cons o out))))

(define (last-out lines)
  (define-values (_ outs) (drive lines))
  (car (reverse outs)))

;; ----- literals evaluate (not parsed as applications) ---------------

(test-case "a list literal evaluates to a List"
  (check-regexp-match #rx"List" (last-out '("[1 2 3]"))))

(test-case "a map literal evaluates to a Map"
  (check-regexp-match #rx"Map" (last-out '("{1 10 2 20}"))))

(test-case "a set literal evaluates to a Set"
  (check-regexp-match #rx"Set" (last-out '("#{1 2 3}"))))

;; ----- nested inside parens (paren-shape survives nesting) ----------

(test-case "a list literal nested in an application evaluates"
  ;; (length [1 2 3]) — under plain `read` this is (length (1 2 3)),
  ;; which fails trying to apply 1.
  (check-regexp-match #rx"3" (last-out '("(length [1 2 3])"))))

;; ----- ,type sees the literal too ----------------------------------

(test-case ",type on a list literal reports (List Integer)"
  (check-regexp-match #rx"List Integer" (last-out '("(unquote type [1 2 3])"))))

;; ----- definitions binding a literal, used later --------------------

(test-case "a definition bound to a map literal is usable"
  ;; the binding escapes the defining step and keeps its Map type
  (check-regexp-match
   #rx"Map"
   (last-out '("(define m {1 10 1 20})"
               "(unquote type m)"))))

(test-case "a bracket list literal works as a match scrutinee result"
  (check-regexp-match
   #rx"6"
   (last-out '("(define xs [1 2 3])"
               "(match xs [[a b c] (+ a (+ b c))] [_ 0])"))))
