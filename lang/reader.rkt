#lang s-exp syntax/module-reader

rackton/lang/runtime

#:read         rackton-read
#:read-syntax  rackton-read-syntax
#:whole-body-readers? #t

(require racket/base)

(define (rackton-read in)
  (map syntax->datum (rackton-read-syntax #f in)))

(define (rackton-read-syntax src in)
  (define forms
    (let loop ([acc '()])
      (define f (read-syntax src in))
      (cond
        [(eof-object? f) (reverse acc)]
        [else (loop (cons f acc))])))
  (cond
    [(null? forms) '()]
    [else
     ;; Wrap every user form in a single (rackton/main ...) invocation.
     ;; Exports are driven by the user's (provide ...) forms inside the
     ;; block — with no provide form, nothing escapes.
     (list (datum->syntax #f
                          (cons 'rackton/main forms)
                          (car forms)))]))
