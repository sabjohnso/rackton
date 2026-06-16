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

(test-case ",info on a protocol lists its declared laws"
  (define out (info-output 'Eq))
  (check-regexp-match #rx"laws:" out)
  (check-regexp-match #rx"reflexivity" out)
  (check-regexp-match #rx"symmetry" out)
  (check-regexp-match #rx"transitivity" out)
  ;; the law body reads as written
  (check-regexp-match #rx"\\(== x x\\)" out))

(test-case ",info shows a higher-kinded law with its => context"
  (define out (info-output 'Functor))
  (check-regexp-match #rx"laws:" out)
  (check-regexp-match #rx"identity" out)
  ;; the law's `=>` context is present; the law is generic over its
  ;; element type, so the container's `Eq` is at the element variable
  ;; `a`.  Pretty-printing may wrap the constraint and the arrow onto
  ;; separate lines, so allow whitespace.
  (check-regexp-match #px"\\(Eq \\(f a\\)\\)\\s*=>" out))

(test-case ",info on a session-defined protocol shows its laws"
  (define-values (_ outs)
    (drive-session
     '((protocol (Idempotent a)
         (#:requires (Eq a))
         (: op (-> a a))
         #:laws
         ([idempotence (All ([x : a]) (== (op (op x)) (op x)))]))
       (unquote info Idempotent))))
  (define out (last outs))
  (check-regexp-match #rx"laws:" out)
  (check-regexp-match #rx"idempotence" out))

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

(test-case ",info on a primitive scalar type lists the protocols it implements"
  ;; Integer / Boolean / String / Float are never registered as data, so
  ;; ,info must still recognize them rather than report them unbound.
  (define out (info-output 'Integer))
  (check-false (regexp-match #rx"unbound" out))
  (check-regexp-match #rx"primitive type" out)
  (check-regexp-match #rx"\\(Num Integer\\)" out)
  (check-regexp-match #rx"\\(Eq Integer\\)" out))

(test-case ",info recognizes the other primitive types"
  (for ([name '(Boolean String Float)])
    (check-false (regexp-match #rx"unbound" (info-output name)))))

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

;; ----- labeled lists indent their items under the label -------------

(test-case ",info indents a primitive type's implements list under the label"
  ;; The instance heads break to one per line, each indented two columns
  ;; past the `implements:` label (which itself sits at column 2).
  (define out (info-output 'Integer))
  (check-regexp-match #px"\n  implements:\n" out)
  (check-regexp-match #px"\n    \\(Num Integer\\)" out)
  (check-false (regexp-match #px"\n  \\(Num Integer\\)" out)))

(test-case ",info indents a type ctor's implements list under the label"
  (define out (info-output 'List))
  (check-regexp-match #px"\n    \\(Functor List\\)" out))

(test-case ",info indents a protocol's instances list under the label"
  (define out (info-output 'Monad))
  (check-regexp-match #px"\n    \\(Monad Maybe\\)" out))

(test-case ",info breaks after a law label before breaking the law body"
  ;; A law too wide to sit on one line moves its whole body to the next
  ;; line, indented under the label, rather than starting beside the
  ;; label and wrapping the body internally.
  (define out (info-output 'Monad))
  (check-regexp-match #px"\n    right-identity:\n      \\(\\(Eq" out)
  (check-false (regexp-match #px"right-identity: \\(\\(Eq" out)))

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
