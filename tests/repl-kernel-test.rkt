#lang racket/base

;; REPL for #lang rackton.
;;
;; The REPL kernel exposes a step function `rackton-repl-step` that
;; takes a session state and one input s-expr, and returns a new
;; state plus an output string.  These tests drive the kernel
;; directly so we don't need to wrangle real stdin/stdout.

(require rackunit
         "../private/repl.rkt")

(define (drive-session inputs)
  ;; Returns (values final-state outputs), where outputs is the list
  ;; of per-step output strings.
  (for/fold ([state (rackton-repl-init)] [out '()] #:result (values state (reverse out)))
            ([form (in-list inputs)])
    (define-values (state* o) (rackton-repl-step state form))
    (values state* (cons o out))))

(test-case "REPL: evaluate a literal expression"
  (define-values (_ outs) (drive-session '(42)))
  (check-regexp-match #rx"42" (car outs))
  (check-regexp-match #rx"Integer" (car outs)))

(test-case "REPL: define and refer to a binding"
  (define-values (_ outs)
    (drive-session
     '((define x 7)
       x)))
  (check-regexp-match #rx"7" (cadr outs))
  (check-regexp-match #rx"Integer" (cadr outs)))

(test-case "REPL: ,type prints the inferred type without evaluating"
  (define-values (_ outs) (drive-session '((unquote type (lambda (x) x)))))
  (check-regexp-match #rx"->" (car outs)))

(test-case "REPL: bare , is an accepted no-op"
  ;; A lone comma reads as `(unquote)`; it leaves the session untouched,
  ;; emits no output, and does not signal exit.
  (define state (rackton-repl-init))
  (define-values (state* out) (rackton-repl-step state '(unquote)))
  (check-equal? out "")
  (check-false (rackton-repl-quit? state*)))

(test-case "REPL: ,geiser-no-values is a no-op"
  ;; Geiser/racket-mode probes the REPL with `,geiser-no-values`; swallow
  ;; it silently rather than reporting an unknown command.
  (define state (rackton-repl-init))
  (define-values (state* out)
    (rackton-repl-step state '(unquote geiser-no-values)))
  (check-equal? out "")
  (check-false (rackton-repl-quit? state*)))

(test-case "REPL: ,clear wipes prior definitions"
  ;; `,clear` resets the session to a fresh prelude env, so a name bound
  ;; before the clear is unbound afterward.
  (define-values (_ outs)
    (drive-session
     '((define keep 99)
       (unquote clear)
       keep)))
  (check-regexp-match #rx"clear" (list-ref outs 1))
  (check-regexp-match #rx"(?i:unbound|error)" (list-ref outs 2)))

(test-case "REPL: env persists a data and uses ctor later"
  (define-values (_ outs)
    (drive-session
     '((data (Box a) (MkBox a))
       (MkBox 5))))
  (check-regexp-match #rx"MkBox" (cadr outs))
  (check-regexp-match #rx"Box" (cadr outs)))

(test-case "REPL: ill-typed form is reported and session continues"
  (define-values (_ outs)
    (drive-session
     '((define y (+ 1 "no"))
       (define z 1)
       z)))
  ;; First entry errors; later entries still work.
  (check-regexp-match #rx"(?i:type|error|mismatch)" (list-ref outs 0))
  (check-regexp-match #rx"1" (list-ref outs 2)))

(test-case "REPL: re-declaring an instance replaces it instead of erroring"
  ;; At a REPL you iterate by re-evaluating forms; a second instance with
  ;; the same head must replace the first (not raise the module-level
  ;; coherence error), and the new method must win on the next call.
  (define-values (_ outs)
    (drive-session
     '((protocol (Greet a) (: greet (-> a String)))
       (instance (Greet Integer) (define (greet _) "hello"))
       (instance (Greet Integer) (define (greet _) "hi"))
       (greet 5))))
  (check-false (regexp-match? #rx"(?i:duplicate|error)" (list-ref outs 2))
               (list-ref outs 2))
  (check-regexp-match #rx"hi" (list-ref outs 3)))

(test-case "REPL: return-typed method of a required module's instance"
  ;; `pure` resolves to a per-instance impl name like $pure:Stream, which a
  ;; required module does NOT export — the call site must go through the
  ;; runtime dispatch table (lookup-return-method), where the module's
  ;; instantiation registered it.  Regression: the kernel handed codegen an
  ;; empty return-typed-methods set, so the call site emitted a direct
  ;; $pure:Stream reference, unbound at the top level.
  (define-values (_ outs)
    (drive-session
     '((require rackton/data/lazy)
       (stream-take 5 (stream-append (pure 3) (pure 4))))))
  (check-false (regexp-match? #rx"(?i:undefined|error)" (cadr outs))
               (cadr outs))
  (check-regexp-match #rx"3" (cadr outs))
  (check-regexp-match #rx"4" (cadr outs)))

(test-case "REPL: return-typed method of a prelude instance"
  ;; The prelude registers its impls in the same dispatch table under plain
  ;; type tags ('Maybe, 'List, …), so routing through the table must keep
  ;; prelude `pure` working too.
  (define-values (_ outs)
    (drive-session '((ann (pure 9) (Maybe Integer)))))
  (check-false (regexp-match? #rx"(?i:undefined|error)" (car outs))
               (car outs))
  (check-regexp-match #rx"9" (car outs))
  (check-regexp-match #rx"Maybe" (car outs)))

(test-case "REPL: a (: name τ) input constrains a define in a LATER input"
  ;; Regression: the kernel discarded the declared-signature map that
  ;; infer-program/phases returned, so a later define was inferred as
  ;; undeclared and the signature was silently ignored (f generalized
  ;; to (All (a) (-> a a)) instead of being checked at (-> Integer Integer)).
  (define-values (_ outs)
    (drive-session
     '((: f (-> Integer Integer))
       (define f (lambda (x) x)))))
  (check-regexp-match #rx"Integer.*Integer" (cadr outs))
  (check-false (regexp-match? #rx"All" (cadr outs)) (cadr outs)))

(test-case "REPL: a prior (: name τ) resolves a return-typed method in the body"
  ;; The declared type is what fixes `pure`'s target; without the carried
  ;; signature this errored with "ambiguous use of pure".
  (define-values (_ outs)
    (drive-session
     '((: m (Maybe Integer))
       (define m (pure 3))
       m)))
  (check-false (regexp-match? #rx"(?i:ambiguous|error)" (cadr outs))
               (cadr outs))
  (check-regexp-match #rx"3" (caddr outs))
  (check-regexp-match #rx"Maybe" (caddr outs)))

(test-case "REPL: a define consumes its signature; redefinition is free"
  ;; REPL iteration: once (define g …) lands, the signature is spent —
  ;; re-evaluating g at a different type must not be checked against it.
  (define-values (_ outs)
    (drive-session
     '((: g Integer)
       (define g 1)
       (define g "now a string")
       g)))
  (check-false (regexp-match? #rx"(?i:error|mismatch)" (caddr outs))
               (caddr outs))
  (check-regexp-match #rx"now a string" (list-ref outs 3)))

(test-case "REPL: ,quit signals exit"
  (define-values (st _outs) (drive-session '((unquote quit))))
  (check-true (rackton-repl-quit? st)))
