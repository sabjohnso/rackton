#lang racket/base

;; Tests for analyze.rkt — the analysis layer under the tooling
;; (search, LSP, DAP).  `analyze-module` runs parse+infer over a
;; `#lang rackton` file with every error caught and converted to a
;; structured diagnostic; the result carries per-name definition
;; sites with schemes, and answers position queries.
;;
;; Fixtures are written to a temporary directory at test time —
;; fixtures with deliberate errors cannot be package files, or
;; `raco setup` would refuse them.

(module+ test
  (require rackunit
           racket/file
           racket/list
           "analyze.rkt"
           (only-in "types.rkt" scheme->datum))

  (define fixture-dir (make-temporary-file "rackton-analyze-~a" 'directory))

  (define (fixture name . lines)
    (define path (build-path fixture-dir name))
    (call-with-output-file path #:exists 'truncate
      (lambda (out)
        (for ([l (in-list lines)]) (displayln l out))))
    path)

  (define (analyze . lines)
    (analyze-module (apply fixture "mod.rkt" lines)))

  ;; ----- in-memory analysis ---------------------------------------------

  ;; analyze-text works on buffer contents that need not match (or
  ;; even have) a file on disk — the LSP's unsaved-edit path.
  (let ([a (analyze-text "#lang rackton\n(: n Integer)\n(define n 7)\n"
                         (build-path fixture-dir "unsaved.rkt"))])
    (check-equal? (analysis-diagnostics a) '())
    (check-equal? (srcloc-line (defsite-srcloc (analysis-def-of a 'n))) 3))

  ;; ----- a clean module ------------------------------------------------

  (define clean
    (analyze "#lang rackton"
             "(provide (all-defined-out))"
             "(: inc (-> Integer Integer))"
             "(define (inc x) (+ x 1))"
             "(data Box (MkBox Integer))"))

  (check-equal? (analysis-diagnostics clean) '()
                "a clean module has no diagnostics")

  (check-equal? (scheme->datum (analysis-scheme-of clean 'inc))
                '(-> Integer Integer))

  (let ([d (analysis-def-of clean 'inc)])
    (check-equal? (defsite-kind d) 'value)
    (check-equal? (srcloc-line (defsite-srcloc d)) 4
                  "the define's location, not the signature's"))

  (check-equal? (defsite-kind (analysis-def-of clean 'Box)) 'type)
  (check-equal? (srcloc-line (defsite-srcloc (analysis-def-of clean 'Box))) 5)
  (check-equal? (defsite-kind (analysis-def-of clean 'MkBox)) 'constructor)
  (check-false (analysis-def-of clean 'zzz))

  ;; ----- diagnostics ----------------------------------------------------

  (let ([bad (analyze "#lang rackton"
                      "(: bad Integer)"
                      "(define bad (+ 1 \"x\"))")])
    (check-equal? (length (analysis-diagnostics bad)) 1)
    (define d (car (analysis-diagnostics bad)))
    (check-equal? (diag-severity d) 'error)
    (check-regexp-match #rx"mismatch|expected" (diag-message d))
    (check-true (and (srcloc-line (diag-srcloc d))
                     (>= (srcloc-line (diag-srcloc d)) 2))
                "the diagnostic points into the file, not at line 1"))

  (let ([unparsable (analyze "#lang rackton"
                             "(define)")])
    (check-equal? (length (analysis-diagnostics unparsable)) 1)
    (check-equal? (diag-severity (car (analysis-diagnostics unparsable)))
                  'error))

  ;; A parse error in one form must NOT suppress a type error in
  ;; another, well-formed form: inference still runs over the forms
  ;; that parsed, and both diagnostics are reported.
  (let ([a (analyze "#lang rackton"
                    "(define)"                 ; parse error
                    "(: bad Integer)"
                    "(define bad \"x\")")])     ; type error: String vs Integer
    (check-true (>= (length (analysis-diagnostics a)) 2)
                "both the parse error and the type error are reported")
    (check-true (for/or ([d (in-list (analysis-diagnostics a))])
                  (regexp-match? #rx"mismatch|expected|String|Integer"
                                 (diag-message d)))
                "the type error in the well-formed form still surfaces"))

  ;; ----- macros ---------------------------------------------------------

  ;; A `define-syntax` / `define-syntax-rule` form is not a parse error;
  ;; its uses are expanded before inference, exactly as in compilation.
  (let ([a (analyze "#lang rackton"
                    "(define-syntax-rule (twice x) (+ x x))"
                    "(: four Integer)"
                    "(define four (twice 2))")])
    (check-equal? (analysis-diagnostics a) '()
                  "a macro definition and use analyze cleanly")
    (check-equal? (srcloc-line (defsite-srcloc (analysis-def-of a 'four))) 4
                  "a macro-using definition keeps its source line"))

  ;; A type error *inside* a macro expansion is still caught — proof the
  ;; use was actually expanded, not merely dropped.
  (let ([a (analyze "#lang rackton"
                    "(define-syntax-rule (twice x) (+ x x))"
                    "(: oops Integer)"
                    "(define oops (twice \"s\"))")])   ; expands to (+ "s" "s")
    (check-true (pair? (analysis-diagnostics a))
                "a type error inside a macro expansion is reported"))

  ;; Even when inference fails, definition sites are still collected.
  (let ([bad (analyze "#lang rackton"
                      "(define (f x) x)"
                      "(define oops (+ 1 \"x\"))")])
    (check-true (pair? (analysis-diagnostics bad)))
    (check-equal? (srcloc-line (defsite-srcloc (analysis-def-of bad 'f))) 2
                  "defs survive a failed inference"))

  ;; ----- position queries -------------------------------------------------

  ;; name-at: line and column are 1-based and 0-based respectively,
  ;; like srclocs.
  (define posy
    (analyze "#lang rackton"
             "(: inc (-> Integer Integer))"
             "(define (inc x) (+ x 1))"
             "(: y Integer)"
             "(define y (inc 41))"))

  (let-values ([(sym site) (name-at posy 5 12)])   ; over `inc` in (inc 41)
    (check-equal? (sym . eq? . 'inc) #t)
    (check-equal? (srcloc-line (defsite-srcloc site)) 3
                  "the use site resolves to the definition"))

  (let-values ([(sym site) (name-at posy 5 8)])    ; over `y`'s definition
    (check-equal? sym 'y))

  (let-values ([(sym site) (name-at posy 2 1)])    ; over `:`, a keyword
    (check-equal? sym ':)
    (check-false site "keywords have no definition site"))

  ;; A prelude name resolves to a scheme but no local definition site.
  (define posy2
    (analyze "#lang rackton"
             "(define z (length Nil))"))
  (let-values ([(sym site) (name-at posy2 2 11)])  ; over `length`
    (check-equal? sym 'length)
    (check-false site)
    (check-true (and (analysis-scheme-of posy2 'length) #t)
                "prelude names still answer scheme queries"))

  ;; ----- cross-module ---------------------------------------------------

  (define lib
    (fixture "lib.rkt"
             "#lang rackton"
             "(provide thing (data-out Pair2))"
             "(: thing Integer)"
             "(define thing 42)"
             "(data (Pair2 a b) (MkPair2 a b))"))
  (let ([user (analyze "#lang rackton"
                       "(require \"lib.rkt\")"
                       "(: more Integer)"
                       "(define more (+ thing 1))")])
    (check-equal? (analysis-diagnostics user) '()
                  "a require resolves through the sidecar")
    (check-equal? (scheme->datum (analysis-scheme-of user 'thing)) 'Integer
                  "imported names answer scheme queries")
    (check-equal? (analysis-requires user) (list lib)
                  "required module paths are retained, resolved"))

  ;; ----- bad requires are flagged ---------------------------------------

  ;; A require of a missing file yields exactly one diagnostic, located
  ;; at the require spec — not swallowed as if it were a sidecar-less
  ;; plain Racket module.
  (let ([bad (analyze "#lang rackton"
                      "(require \"nonexistent.rkt\")"
                      "(define x 1)")])
    (check-equal? (length (analysis-diagnostics bad)) 1
                  "a require of a missing file yields one diagnostic")
    (check-regexp-match #rx"nonexistent" (diag-message (car (analysis-diagnostics bad))))
    (check-equal? (srcloc-line (diag-srcloc (car (analysis-diagnostics bad)))) 2
                  "the diagnostic points at the require, not line 1"))

  ;; An unresolvable collection module path is flagged too.
  (let ([bad (analyze "#lang rackton"
                      "(require totally/bogus/collection)"
                      "(define x 1)")])
    (check-equal? (length (analysis-diagnostics bad)) 1
                  "a require of a bad collection path yields one diagnostic"))

  ;; A require of a real plain Racket module (no rackton sidecar) is
  ;; still tolerated — no diagnostic.
  (let ()
    (fixture "plain.rkt" "#lang racket/base" "(provide foo)" "(define foo 5)")
    (define ok (analyze "#lang rackton"
                        "(require \"plain.rkt\")"
                        "(define x 1)"))
    (check-equal? (analysis-diagnostics ok) '()
                  "a sidecar-less plain Racket module require is tolerated"))

  ;; ----- the workspace index (sidecar defs table) -------------------------

  (define entries (module-index-entries lib))

  (let ([thing (for/first ([e (in-list entries)]
                           #:when (eq? (index-entry-name e) 'thing))
                 e)])
    (check-true (and thing #t) "exported values index")
    (check-equal? (index-entry-kind thing) 'value)
    (check-equal? (scheme->datum (index-entry-scheme thing)) 'Integer)
    (check-equal? (srcloc-line (index-entry-srcloc thing)) 4
                  "the index carries the definition's line from the sidecar"))

  (let ([ctor (for/first ([e (in-list entries)]
                          #:when (eq? (index-entry-name e) 'MkPair2))
                e)])
    (check-true (and ctor #t) "exported constructors index")
    (check-equal? (index-entry-kind ctor) 'constructor)
    (check-equal? (srcloc-line (index-entry-srcloc ctor)) 5))

  (check-true (for/first ([e (in-list entries)]
                          #:when (eq? (index-entry-name e) 'Pair2))
                (eq? (index-entry-kind e) 'type))
              "exported types index")

  (check-false (for/or ([e (in-list entries)])
                 (eq? (index-entry-name e) 'unexported))
               "unexported names stay private")

  (check-equal? (module-index-entries (fixture "plain.rkt"
                                               "#lang racket/base"
                                               "(provide x)"
                                               "(define x 1)"))
                '()
                "a non-rackton module contributes nothing")

  (check-equal? (length (index-modules (list lib)))
                (length entries))

  (delete-directory/files fixture-dir))
