#lang racket/base

;; private/lexer-prim — the one host primitive behind the Rackton source
;; formatter (rackton/tools/fmt-lib).
;;
;; It drives the standard Racket lexer over a string and returns a
;; Rackton list of (type . exact-text) pairs covering the ENTIRE input:
;; each token's text is sliced from the source by the lexer's character
;; offsets, so concatenating the texts reproduces the input exactly.
;; That total coverage is what lets the formatter move only whitespace.
;;
;; This is the formatter's sole Racket dependency; everything past the
;; token list is pure Rackton.

(require syntax-color/racket-lexer
         (only-in rackton/private/prelude-runtime Cons Nil Pair))

(provide racket-tokenize)

;; a Racket list -> a Rackton list (Cons/Nil), preserving order.
(define (->rackton-list xs)
  (if (null? xs) Nil (Cons (car xs) (->rackton-list (cdr xs)))))

(define (racket-tokenize s)
  (define in (open-input-string s))
  ;; count positions in characters, not bytes — otherwise tokens after a
  ;; non-ASCII character (λ!) would slice at shifted offsets.
  (port-count-lines! in)
  (let loop ([acc '()])
    (define-values (lexeme type paren start end) (racket-lexer in))
    (if (eof-object? lexeme)
        (->rackton-list (reverse acc))
        (loop (cons (Pair (symbol->string type)
                          (substring s (sub1 start) (sub1 end)))
                    acc)))))
