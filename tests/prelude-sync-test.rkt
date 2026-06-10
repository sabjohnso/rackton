#lang racket/base

;; Enforce the compile-time/runtime prelude name-sync invariant.
;;
;; CLAUDE.md: private/prelude.rkt (compile-time: a Rackton program parsed
;; into prelude-env) and private/prelude-runtime.rkt (runtime: dispatch
;; tables + builtin registrations) "must keep their names in sync."  Until
;; now that was a hand-maintained invariant with no automated check — a
;; prelude method declared with no matching runtime dispatch table would
;; surface only as a call-time error, and only if some test happened to
;; exercise that exact name (Review.org / PLAN.org item #1).
;;
;; The concrete contract: every class method M declared by a `protocol`
;; form in the compile-time prelude is lowered by codegen to dispatch on a
;; runtime table `$dispatch:M` (positional methods read the tag off an
;; argument; return-typed methods do `lookup-return-method` on the same
;; table).  So the runtime MUST define `$dispatch:M` for every prelude
;; protocol method, and — since prelude-runtime.rkt holds exactly the
;; auto-prelude classes' tables — no stray `$dispatch:` table without a
;; declaring method.
;;
;; This test reads BOTH halves as s-expressions (not grep — comments and
;; backtick-quoted prose would poison a textual scan) and diffs the two
;; name sets.

(require rackunit
         racket/list
         racket/runtime-path)

(define-runtime-path prelude.rkt         "../private/prelude.rkt")
(define-runtime-path prelude-runtime.rkt "../private/prelude-runtime.rkt")

;; Read every top-level datum from a #lang module file.  The reader cannot
;; consume the `#lang` line, so skip it, then read plain s-expressions.
(define (read-module-forms path)
  (with-input-from-file path
    (lambda ()
      (read-line)                 ; discard the `#lang racket/base` line
      (let loop ([acc '()])
        (define f (read))
        (if (eof-object? f) (reverse acc) (loop (cons f acc)))))))

;; The set of class-method names declared by `protocol` forms inside the
;; quoted `prelude-source-forms` program.  Only `(: name …)` clauses whose
;; head is a symbol count — top-level `(: builtin …)` declarations (which
;; sit outside any protocol) are excluded by construction, since we descend
;; only into protocol bodies.
(define (prelude-protocol-methods)
  (define forms (read-module-forms prelude.rkt))
  (define src-def
    (for/or ([f (in-list forms)])
      (and (pair? f) (eq? (car f) 'define) (pair? (cdr f))
           (eq? (cadr f) 'prelude-source-forms) (caddr f))))
  (unless src-def
    (error 'prelude-sync-test "could not find prelude-source-forms in ~a" prelude.rkt))
  ;; src-def is `(quote (form …))`
  (define top (if (and (pair? src-def) (eq? (car src-def) 'quote)) (cadr src-def) src-def))
  (for*/list ([form   (in-list top)]
              #:when  (and (pair? form) (eq? (car form) 'protocol))
              [clause (in-list (cddr form))]
              #:when  (and (pair? clause) (eq? (car clause) ':)
                           (pair? (cdr clause)) (symbol? (cadr clause))))
    (cadr clause)))

;; The set of method names with a runtime dispatch table, read off the
;; top-level `(define $dispatch:NAME …)` forms in prelude-runtime.rkt.
(define dispatch-prefix "$dispatch:")
(define (runtime-dispatch-methods)
  (define forms (read-module-forms prelude-runtime.rkt))
  (for*/list ([f (in-list forms)]
              #:when (and (pair? f) (eq? (car f) 'define)
                          (pair? (cdr f)) (symbol? (cadr f)))
              [s (in-value (symbol->string (cadr f)))]
              #:when (and (> (string-length s) (string-length dispatch-prefix))
                          (string=? (substring s 0 (string-length dispatch-prefix))
                                    dispatch-prefix)))
    (string->symbol (substring s (string-length dispatch-prefix)))))

(define prelude-methods (remove-duplicates (prelude-protocol-methods)))
(define runtime-methods (remove-duplicates (runtime-dispatch-methods)))

(define (sym< a b) (string<? (symbol->string a) (symbol->string b)))

;; The critical direction (the one that motivated the finding): a prelude
;; method with no runtime dispatch table would fail only at call time.
(test-case "every prelude protocol method has a runtime $dispatch table"
  (define missing (sort (filter (lambda (m) (not (memq m runtime-methods)))
                                prelude-methods)
                        sym<))
  (check-equal? missing '()
                (format (string-append
                         "prelude.rkt declares these protocol methods with no "
                         "$dispatch:<name> in prelude-runtime.rkt — they would be "
                         "unbound/no-instance at call time: ~a")
                        missing)))

;; The reverse direction: a stray dispatch table with no declaring method
;; (e.g. a method renamed in prelude.rkt but not in the runtime) is dead
;; weight and a sign the halves drifted.
(test-case "every runtime $dispatch table has a declaring prelude method"
  (define orphan (sort (filter (lambda (m) (not (memq m prelude-methods)))
                               runtime-methods)
                       sym<))
  (check-equal? orphan '()
                (format (string-append
                         "prelude-runtime.rkt defines these $dispatch:<name> tables "
                         "with no matching protocol method in prelude.rkt: ~a")
                        orphan)))

;; Sanity: the extraction found a non-trivial method set (guards against a
;; future refactor silently making both sets empty, which would pass the
;; subset checks vacuously).
(test-case "extraction found the prelude method set"
  (check-true (> (length prelude-methods) 50)
              (format "expected >50 prelude protocol methods, found ~a"
                      (length prelude-methods))))
