#lang racket/base

;; rackton/tools/gen-test-deps-lib — the core of the change-driven test
;; selector.
;;
;; `raco make` writes a `compiled/<name>_rkt.dep` file next to every
;; compiled module recording that module's direct dependencies; an
;; in-repo dependency appears as `(collects #"rackton" <parts> …)`
;; (the whole repository is the `rackton` collection). This library
;; reads those records — it does not recompute the graph — and turns
;; them into GNU Make prerequisites: for each discovered test it emits
;;
;;   $(STAMP_DIR)/<relpath>.stamp: <relpath> <dep> <dep> …
;;
;; plus a `TEST_STAMPS := …` variable. The thin shell in
;; gen-test-deps.rkt supplies the repository root and prints the result.
;;
;; The module is organized functional-core / imperative-shell:
;;
;;   * pure string/path transforms — test-name?, skip-dir?,
;;     dep-file-for, strip-indirect, parse-dep-form,
;;     dep-entry->in-repo-path, render-fragment;
;;   * a pure graph closure — transitive-closure — parameterized on its
;;     neighbor function so its laws can be property-tested off-disk;
;;   * the filesystem boundary — read-dep-form, direct-neighbors,
;;     list-source-files, list-test-files, compute-mapping.

(require racket/path
         racket/list
         racket/string
         racket/set)

(provide test-name?
         skip-dir?
         dep-file-for
         strip-indirect
         parse-dep-form
         dep-entry->in-repo-path
         transitive-closure
         render-fragment
         path->repo-relative
         mode-for-args
         read-dep-form
         direct-neighbors
         list-source-files
         list-test-files
         compute-mapping)

;; ===========================================================================
;; Pure transforms
;; ===========================================================================

;; A file is a test iff its name is `<something>-test.rkt` or
;; `test-<something>.rkt`. Helper libraries and fixtures follow other
;; conventions (`*-lib.rkt`, `*-fixture.rkt`, `sample.rkt`, …) and are
;; therefore excluded automatically.
(define test-name-rx #rx"(-test\\.rkt$|^test-.*\\.rkt$)")
(define (test-name? name) (regexp-match? test-name-rx name))

;; Directories never descended into: build output, VCS metadata, and
;; generated documentation. This is the sole definition of the
;; exclusion policy; the Makefile obtains its source list from this
;; library rather than repeating it.
(define (skip-dir? name) (and (member name '("compiled" ".git" "doc")) #t))

;; The `.dep` file for `dir/name.rkt` is `dir/compiled/name_rkt.dep`.
(define (dep-file-for src)
  (define base (path->string (file-name-from-path src)))
  (define mangled (regexp-replace #rx"\\.rkt$" base "_rkt"))
  (define dir (path-only src))
  (define rel (build-path "compiled" (string-append mangled ".dep")))
  (if dir (build-path dir rel) rel))

;; Unwrap `(indirect <spec> …)` to `(<spec> …)`.
(define (strip-indirect entry)
  (if (and (pair? entry) (eq? (car entry) 'indirect))
      (cdr entry)
      entry))

;; A `.dep` file's content is `(version arch (src-sha . recorded-sha)
;; entry …)`. Return just the entries, or '() for anything that does
;; not match that shape.
(define (parse-dep-form form)
  (if (and (list? form) (>= (length form) 3))
      (cdddr form)
      '()))

;; Resolve a single dependency entry to an in-repo absolute path, or #f
;; if it names a module outside this repository. The leading #"rackton"
;; maps to `repo-root`, so the remaining parts join beneath it.
(define (dep-entry->in-repo-path entry repo-root)
  (define e (strip-indirect entry))
  (cond
    [(and (pair? e)
          (eq? (car e) 'collects)
          (pair? (cdr e))
          (equal? (cadr e) #"rackton"))
     (simplify-path
      (apply build-path repo-root
             (map (lambda (b) (bytes->string/utf-8 b)) (cddr e)))
      #f)]
    [else #f]))

;; The transitive closure of nodes reachable from `root`, including
;; `root`. Pure: `neighbors` supplies each node's successors, and the
;; `seen` set guards against cycles, so the walk always terminates.
;; Nodes are compared with `equal?`.
(define (transitive-closure root neighbors)
  (let loop ([frontier (list root)] [seen (set)])
    (cond
      [(null? frontier) seen]
      [(set-member? seen (car frontier)) (loop (cdr frontier) seen)]
      [else
       (define cur (car frontier))
       (loop (append (neighbors cur) (cdr frontier)) (set-add seen cur))])))

;; A repository-relative, canonical string for `p`. The single
;; definition of the relative-path convention, shared by compute-mapping
;; and the shell's source listing.
(define (path->repo-relative root p)
  (path->string (find-relative-path root (simplify-path p #f))))

;; The shell's command-line contract: `--list-sources` selects the
;; source-inventory mode, anything else the default fragment mode.
(define (mode-for-args args)
  (if (and (pair? args) (equal? (car args) "--list-sources"))
      'list-sources
      'fragment))

;; Render the Make fragment from a mapping of
;; (test-rel . (listof prereq-rel)). Pure; the same mapping drives both
;; the TEST_STAMPS variable and the per-test rules, so the two cannot
;; drift.
(define (render-fragment mapping)
  (define out (open-output-string))
  (fprintf out "# Generated by tools/gen-test-deps.rkt — do not edit.\n")
  (fprintf out "# Maps each test's stamp file to its transitive in-repo sources.\n\n")
  (fprintf out "TEST_STAMPS :=")
  (for ([m (in-list mapping)])
    (fprintf out " $(STAMP_DIR)/~a.stamp" (car m)))
  (fprintf out "\n\n")
  (for ([m (in-list mapping)])
    (fprintf out "$(STAMP_DIR)/~a.stamp:" (car m))
    (for ([p (in-list (cdr m))])
      (fprintf out " ~a" p))
    (fprintf out "\n"))
  (get-output-string out))

;; ===========================================================================
;; Filesystem boundary
;; ===========================================================================

;; Read and parse a `.dep` file's entries, or '() when it is absent.
(define (read-dep-form src)
  (define dep (dep-file-for src))
  (if (file-exists? dep)
      (parse-dep-form (call-with-input-file dep read))
      '()))

;; The direct in-repo dependencies of `src` (absolute, canonical
;; paths). This is the neighbor function transitive-closure walks.
(define (direct-neighbors src repo-root)
  (for*/list ([entry (in-list (read-dep-form src))]
              [p (in-value (dep-entry->in-repo-path entry repo-root))]
              #:when p)
    p))

;; Every `.rkt` under `root`, skipping excluded directories. Absolute,
;; canonical paths.
(define (walk-rkt root keep?)
  (let loop ([dir root] [acc '()])
    (for/fold ([acc acc]) ([entry (in-list (sort (directory-list dir) path<?))])
      (define full (build-path dir entry))
      (cond
        [(directory-exists? full)
         (if (skip-dir? (path->string entry)) acc (loop full acc))]
        [(and (regexp-match? #rx"\\.rkt$" (path->string entry))
              (keep? (path->string entry)))
         (cons (simplify-path full #f) acc)]
        [else acc]))))

(define (list-source-files root) (walk-rkt root (lambda (_) #t)))
(define (list-test-files root)   (walk-rkt root test-name?))

;; Discover every test under `root` and compute, for each, its
;; transitive in-repo prerequisites — returning a sorted mapping of
;; repo-relative (test . (listof prereq)) with the test itself included
;; among the prerequisites.
(define (compute-mapping root)
  (define (rel p) (path->repo-relative root p))
  (define tests (sort (list-test-files root) string<? #:key path->string))
  (for/list ([t (in-list tests)])
    (define closure (transitive-closure t (lambda (n) (direct-neighbors n root))))
    (cons (rel t) (sort (map rel (set->list closure)) string<?))))
