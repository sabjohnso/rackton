#lang racket/base

;; Tests for rackton/tools/gen-test-deps-lib — the pure core of the
;; change-driven test selector.
;;
;; The library is split functional-core / imperative-shell: the string
;; and path transforms (test-name?, dep-file-for, strip-indirect,
;; parse-dep-form, dep-entry->in-repo-path, render-fragment) and the
;; graph closure (transitive-closure) are pure and pinned here, and the
;; filesystem boundary (direct-neighbors, list-source-files,
;; compute-mapping) is exercised against a temporary fixture tree.
;;
;; transitive-closure is a reachability algebra; its laws — reflexivity,
;; fixpoint closure under the neighbor function, and termination on a
;; cyclic graph — are stated as a rackcheck property over arbitrary
;; generated graphs, which is why the closure takes its neighbor
;; function as a parameter rather than reading the disk itself.
;;
;; RED: rackton/tools/gen-test-deps-lib does not exist yet.

(module+ test
  (require rackunit
           rackcheck
           racket/set
           racket/file
           racket/list
           racket/path
           "gen-test-deps-lib.rkt")

  ;; ----- test-name? --------------------------------------------------
  (check-true  (test-name? "unify-test.rkt")   "…-test.rkt is a test")
  (check-true  (test-name? "test-demo.rkt")    "test-….rkt is a test")
  (check-false (test-name? "sample.rkt")       "plain source is not a test")
  (check-false (test-name? "multi-file-lib.rkt") "…-lib.rkt is a helper")
  (check-false (test-name? "reserved-main-fixture.rkt") "…-fixture.rkt is a helper")
  (check-false (test-name? "types.rkt")        "ordinary module is not a test")

  ;; ----- skip-dir? ---------------------------------------------------
  (check-true  (skip-dir? "compiled"))
  (check-true  (skip-dir? ".git"))
  (check-true  (skip-dir? "doc"))
  (check-false (skip-dir? "private"))

  ;; ----- dep-file-for ------------------------------------------------
  (check-equal? (path->string (dep-file-for (string->path "private/unify-test.rkt")))
                "private/compiled/unify-test_rkt.dep")
  (check-equal? (path->string (dep-file-for (string->path "batteries.rkt")))
                "compiled/batteries_rkt.dep")

  ;; ----- strip-indirect ----------------------------------------------
  (check-equal? (strip-indirect '(indirect collects #"a" #"b.rkt"))
                '(collects #"a" #"b.rkt"))
  (check-equal? (strip-indirect '(collects #"a" #"b.rkt"))
                '(collects #"a" #"b.rkt"))

  ;; ----- parse-dep-form ----------------------------------------------
  (check-equal? (parse-dep-form '("9.1" ta6le ("src" . "rec") e1 e2)) '(e1 e2))
  (check-equal? (parse-dep-form '("9.1" ta6le ("src" . "rec"))) '()
                "no entries after the header")
  (check-equal? (parse-dep-form '("9.1")) '() "too short to be a dep form")
  (check-equal? (parse-dep-form 42) '() "non-list is empty")

  ;; ----- dep-entry->in-repo-path -------------------------------------
  (let ([root (string->path "/repo")])
    (check-equal? (dep-entry->in-repo-path '(collects #"rackton" #"private" #"unify.rkt") root)
                  (simplify-path (build-path root "private" "unify.rkt") #f))
    (check-equal? (dep-entry->in-repo-path '(indirect collects #"rackton" #"types.rkt") root)
                  (simplify-path (build-path root "types.rkt") #f)
                  "indirect wrapping resolves too")
    (check-false (dep-entry->in-repo-path '(collects #"racket" #"base.rkt") root)
                 "a non-rackton collection is not in-repo")
    (check-false (dep-entry->in-repo-path '(collects #"racket" #"match.rkt") root))
    ;; defensive: a truncated entry has no collection name to match
    (check-false (dep-entry->in-repo-path '(collects) root)
                 "a bare (collects) is not in-repo")
    (check-false (dep-entry->in-repo-path '() root) "the empty entry is not in-repo"))

  ;; ----- mode-for-args: the shell's command-line contract ------------
  (check-equal? (mode-for-args '("--list-sources")) 'list-sources)
  (check-equal? (mode-for-args '("--list-sources" "extra")) 'list-sources)
  (check-equal? (mode-for-args '()) 'fragment "no argument selects the fragment")
  (check-equal? (mode-for-args '("--other")) 'fragment "an unknown flag is the fragment")

  ;; ----- render-fragment ---------------------------------------------
  (let ([frag (render-fragment
               (list (cons "t/a-test.rkt" '("t/a-test.rkt" "src/x.rkt"))
                     (cons "t/b-test.rkt" '("t/b-test.rkt"))))])
    (check-true (regexp-match? #rx"TEST_STAMPS :=" frag))
    (check-true (regexp-match? #px"TEST_STAMPS :=(.*)\\$\\(STAMP_DIR\\)/t/a-test\\.rkt\\.stamp" frag))
    (check-true (regexp-match? #px"\\$\\(STAMP_DIR\\)/t/a-test\\.rkt\\.stamp: t/a-test\\.rkt src/x\\.rkt" frag))
    (check-true (regexp-match? #px"\\$\\(STAMP_DIR\\)/t/b-test\\.rkt\\.stamp: t/b-test\\.rkt(\\s|$)" frag)))

  ;; ----- transitive-closure: reachability algebra --------------------
  ;; Generate an arbitrary directed graph over a small node set as an
  ;; edge list; cycles arise naturally, so a passing run also witnesses
  ;; termination.
  (define gen-edge (gen:tuple (gen:integer-in 0 6) (gen:integer-in 0 6)))
  (define gen-graph (gen:list gen-edge #:max-length 24))

  (define (edges->neighbors edges)
    (define adj (make-hash))
    (for ([e (in-list edges)])
      (hash-update! adj (car e) (lambda (xs) (cons (cadr e) xs)) '()))
    (lambda (n) (hash-ref adj n '())))

  ;; An independent reference reachable set, computed breadth-first, to
  ;; pin soundness (no unreachable node) alongside completeness — an
  ;; over-approximating closure that returned every node would satisfy
  ;; reflexivity and fixpoint but not equality with this.
  (define (reachable root neighbors)
    (let loop ([queue (list root)] [seen (set)])
      (cond
        [(null? queue) seen]
        [(set-member? seen (car queue)) (loop (cdr queue) seen)]
        [else (loop (append (neighbors (car queue)) (cdr queue))
                    (set-add seen (car queue)))])))

  (check-property
   (make-config #:tests 500)
   (property closure-laws ([edges gen-graph] [root (gen:integer-in 0 6)])
     (define neighbors (edges->neighbors edges))
     (define result (transitive-closure root neighbors))
     (and
      ;; reflexivity: the root is reachable from itself
      (set-member? result root)
      ;; fixpoint: the set is closed under the neighbor relation
      (for/and ([m (in-set result)])
        (for/and ([nb (in-list (neighbors m))])
          (set-member? result nb)))
      ;; soundness + completeness: exactly the reachable set, no more
      (equal? result (reachable root neighbors)))))

  ;; ----- filesystem boundary against a fixture tree ------------------
  (define (with-fixture proc)
    (define root (make-temporary-file "gtd~a" 'directory))
    (dynamic-wind
     void
     (lambda () (proc root))
     (lambda () (delete-directory/files root))))

  ;; direct-neighbors reads a .dep file and returns its in-repo deps.
  (test-case "direct-neighbors resolves a .dep's rackton entries"
    (with-fixture
     (lambda (root)
       (make-directory* (build-path root "compiled"))
       (display-to-file "" (build-path root "src.rkt"))
       (display-to-file "" (build-path root "foo-test.rkt"))
       (write-to-file '("9.1" ta6le ("s" . "r") (collects #"rackton" #"src.rkt"))
                      (build-path root "compiled" "foo-test_rkt.dep"))
       (check-equal? (direct-neighbors (build-path root "foo-test.rkt") root)
                     (list (simplify-path (build-path root "src.rkt") #f))))))

  ;; compute-mapping discovers tests, walks the closure, and relativizes.
  (test-case "compute-mapping yields sorted test → prereq rules"
    (with-fixture
     (lambda (root)
       (make-directory* (build-path root "compiled"))
       (display-to-file "" (build-path root "src.rkt"))
       (display-to-file "" (build-path root "foo-test.rkt"))
       (write-to-file '("9.1" ta6le ("s" . "r") (collects #"rackton" #"src.rkt"))
                      (build-path root "compiled" "foo-test_rkt.dep"))
       (define mapping (compute-mapping root))
       (check-equal? mapping
                     (list (cons "foo-test.rkt" '("foo-test.rkt" "src.rkt")))))))

  ;; list-source-files enumerates every .rkt, skipping build output.
  (test-case "list-source-files skips compiled/ and finds all sources"
    (with-fixture
     (lambda (root)
       (make-directory* (build-path root "compiled"))
       (display-to-file "" (build-path root "src.rkt"))
       (display-to-file "" (build-path root "foo-test.rkt"))
       (display-to-file "" (build-path root "compiled" "junk.rkt"))
       (define rels (map (lambda (p) (path->string (find-relative-path root p)))
                         (list-source-files root)))
       (check-equal? (sort rels string<?) '("foo-test.rkt" "src.rkt"))))))
