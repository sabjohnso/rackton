#lang racket/base

;; REPL: the ,info command.
;;
;; ,info NAME describes what NAME is bound to.  For a class it lists the
;; parameters, superclasses, methods, and known instances; for a type
;; constructor it lists the constructors (with their schemes) and the
;; classes the type has instances of.  Vars and data constructors keep
;; their one-line scheme output.  These tests drive the kernel directly.

(require rackunit
         racket/list
         "../private/repl.rkt")

(define (drive-session inputs)
  ;; Returns (values final-state outputs), where outputs is the list
  ;; of per-step output strings.
  (for/fold ([state (rackton-repl-init)] [out '()] #:result (values state (reverse out)))
            ([form (in-list inputs)])
    (define-values (state* o) (rackton-repl-step state form))
    (values state* (cons o out))))

(define (info-output name)
  (define-values (_ outs) (drive-session (list (list 'unquote 'info name))))
  (car outs))

(test-case ",info on a protocol lists parameters, superprotocols, and methods"
  (define out (info-output 'Monad))
  (check-regexp-match #rx"protocol" out)
  (check-regexp-match #rx"m" out)              ; the protocol parameter
  (check-regexp-match #rx"Applicative" out)    ; the superprotocol
  (check-regexp-match #rx"flatmap" out)
  (check-regexp-match #rx"join" out))

(test-case ",info on a protocol lists its known instances"
  (define out (info-output 'Monad))
  (check-regexp-match #rx"\\(Monad Maybe\\)" out)
  (check-regexp-match #rx"\\(Monad List\\)" out))

(test-case ",info on a type ctor lists arity and constructors with schemes"
  (define out (info-output 'List))
  (check-regexp-match #rx"type ctor" out)
  (check-regexp-match #rx"arity 1" out)
  (check-regexp-match #rx"Nil" out)
  (check-regexp-match #rx"Cons" out)
  ;; Cons's scheme should be rendered, arrow included.
  (check-regexp-match #rx"->" out))

(test-case ",info on a type ctor lists the protocols it has instances of"
  (define out (info-output 'List))
  (check-regexp-match #rx"\\(Functor List\\)" out)
  (check-regexp-match #rx"\\(Monad List\\)" out))

(test-case ",info marks a sealed type but keeps locally visible ctors"
  ;; #:abstract hides constructors across module boundaries only; inside
  ;; the defining session they are visible, so ,info lists them and adds
  ;; a `sealed` marker.
  (define-values (_ outs)
    (drive-session
     '((data Hidden (MkHidden Integer) #:abstract)
       (unquote info Hidden))))
  (define out (last outs))
  (check-regexp-match #rx"sealed" out)
  (check-regexp-match #rx"MkHidden" out))

(test-case ",info on a session-defined protocol shows its methods"
  (define-values (_ outs)
    (drive-session
     '((protocol (Greet a)
         (: greet (-> a String)))
       (unquote info Greet))))
  (define out (last outs))
  (check-regexp-match #rx"protocol" out)
  (check-regexp-match #rx"greet" out)
  (check-regexp-match #rx"String" out))

;; ----- regression: existing one-line outputs stay -------------------

(test-case ",info on a var prints its scheme"
  (define-values (_ outs)
    (drive-session
     '((define inc (lambda (n) (+ n 1)))
       (unquote info inc))))
  (check-regexp-match #rx"inc :: " (last outs))
  (check-regexp-match #rx"Integer" (last outs)))

(test-case ",info on a data ctor prints its scheme"
  (define out (info-output 'Some))
  (check-regexp-match #rx"data ctor" out)
  (check-regexp-match #rx"Maybe" out))

(test-case ",info on an unbound name says so"
  (check-regexp-match #rx"unbound" (info-output 'no-such-name)))
