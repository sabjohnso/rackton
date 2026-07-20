#lang racket/base

;; Tests for private/module-complete.rkt — prefix-directed enumeration of
;; the module paths a `require` spec may name.
;;
;; The enumeration is directed by the prefix rather than indexed ahead of
;; time, so each call reads at most the directories the prefix already
;; names.  The example cases run against Rackton's own installed
;; collection, which is present whenever these tests run; the properties
;; pin the two invariants every client relies on — a candidate always
;; extends the prefix it was asked for, and a candidate that names a
;; directory ends in a slash so completion can continue into it.

(module+ test
  (require rackunit
           rackcheck
           racket/list
           racket/string
           racket/file
           "module-complete.rkt")

  ;; ----- collection paths ----------------------------------------------

  (test-case "a leading segment completes to collection names"
    (define cs (collection-path-completions "rackt"))
    (check-not-false (member "rackton/" cs) "rackton is an installed collection")
    (check-true (andmap (lambda (c) (string-prefix? c "rackt")) cs)))

  (test-case "an interior segment completes within the collection"
    (define cs (collection-path-completions "rackton/da"))
    (check-not-false (member "rackton/data/" cs)
                "rackton/data is a directory, so it keeps its slash"))

  (test-case "a trailing slash lists the directory's whole contents"
    (define cs (collection-path-completions "rackton/data/"))
    (check-not-false (member "rackton/data/maybe" cs)
                "a .rkt module appears without its extension")
    (check-not-false (member "rackton/data/list" cs)
                "list.rkt is a module")
    (check-not-false (member "rackton/data/list/" cs)
                "list is also a directory; both module paths are offered"))

  (test-case "a final segment filters the directory's contents"
    (define cs (collection-path-completions "rackton/data/may"))
    (check-equal? cs '("rackton/data/maybe")))

  (test-case "build artefacts and info files are not module paths"
    (define cs (collection-path-completions "rackton/data/"))
    (check-false (member "rackton/data/compiled/" cs))
    (check-false (member "rackton/data/info" cs)))

  (test-case "a prefix naming nothing yields no candidates, not an error"
    (check-equal? (collection-path-completions "no-such-collection-xyz/deep/") '())
    (check-equal? (collection-path-completions "rackton/no-such-dir-xyz/") '()))

  (test-case "a prefix with an unusable segment completes to nothing"
    ;; Completing it would mean silently dropping the characters that
    ;; make it unusable, and the editor would then insert a candidate
    ;; that is not an extension of what was typed.
    (check-equal? (collection-path-completions "/rackton/da") '())
    (check-equal? (collection-path-completions "rackton//data/l") '())
    (check-equal? (collection-path-completions "rackton/./data/l") '()))

  (test-case "candidates are sorted and free of duplicates"
    (define cs (collection-path-completions "rackton/"))
    (check-equal? cs (sort (remove-duplicates cs) string<?)))

  ;; ----- relative paths ------------------------------------------------

  (test-case "a relative prefix completes against the anchoring directory"
    (define dir (make-temporary-directory))
    (make-directory (build-path dir "sub"))
    (display-to-file "" (build-path dir "helpers.rkt"))
    (display-to-file "" (build-path dir "notes.txt"))
    (check-equal? (relative-path-completions "he" dir) '("helpers.rkt"))
    (check-equal? (relative-path-completions "" dir) '("helpers.rkt" "sub/"))
    (check-equal? (relative-path-completions "sub/" dir) '())
    (delete-directory/files dir))

  (test-case "a relative path keeps its .rkt extension"
    ;; Unlike a collection path, a string spec names a file, so the
    ;; extension is part of what the user must type.
    (define dir (make-temporary-directory))
    (display-to-file "" (build-path dir "helpers.rkt"))
    (check-equal? (relative-path-completions "helpers" dir) '("helpers.rkt"))
    (delete-directory/files dir))

  (test-case "an unreadable anchor yields no candidates, not an error"
    (check-equal? (relative-path-completions "a" "/no/such/directory/xyz") '()))

  ;; ----- properties ----------------------------------------------------

  ;; Prefixes drawn from the shape a user actually types: real segments of
  ;; an installed collection, truncated at an arbitrary point — and the
  ;; malformed shapes a half-typed path passes through on the way there, a
  ;; leading or doubled slash among them.  The editor extends whatever is
  ;; typed, so the laws below must hold for those too.
  (define gen:module-prefix
    (gen:let ([full (gen:one-of '("rackton/data/maybe" "rackton/text/pretty"
                                  "rackton/control/monad" "racket/list"
                                  "rackton" "zzz/nope"
                                  "/rackton/data" "rackton//data/li"
                                  "rackton/./data" "//" "rackton/data//"))]
              [n (gen:integer-in 0 18)])
      (substring full 0 (min n (string-length full)))))

  ;; A small tree on disk for the relative-path laws, with the same
  ;; shapes: a nested directory, a module, a non-module.
  (define tree (make-temporary-directory))
  (make-directory* (build-path tree "sub" "deep"))
  (display-to-file "" (build-path tree "helpers.rkt"))
  (display-to-file "" (build-path tree "notes.txt"))
  (display-to-file "" (build-path tree "sub" "inner.rkt"))

  (define gen:relative-prefix
    (gen:let ([full (gen:one-of '("helpers.rkt" "sub/inner.rkt" "sub/deep/"
                                  "notes.txt" "/sub" "sub//inner" "nope/x"))]
              [n (gen:integer-in 0 14)])
      (substring full 0 (min n (string-length full)))))

  (check-property
   (property every-candidate-extends-the-prefix ([pfx gen:module-prefix])
     (for/and ([c (in-list (collection-path-completions pfx))])
       (string-prefix? c pfx))))

  (check-property
   (property a-candidate-is-a-module-path-or-a-directory ([pfx gen:module-prefix])
     ;; Exactly two candidate shapes: a directory to descend into (slash),
     ;; or a module path to require (no slash at the end, no extension).
     (for/and ([c (in-list (collection-path-completions pfx))])
       (or (string-suffix? c "/")
           (not (string-suffix? c ".rkt"))))))

  (check-property
   (property a-relative-candidate-extends-the-prefix ([pfx gen:relative-prefix])
     ;; The same law over the other universe, which shares the splitting
     ;; and re-joining that the collection paths exercise above.
     (for/and ([c (in-list (relative-path-completions pfx tree))])
       (string-prefix? c pfx))))

  (check-property
   (property a-relative-candidate-is-a-file-or-a-directory ([pfx gen:relative-prefix])
     (for/and ([c (in-list (relative-path-completions pfx tree))])
       (or (string-suffix? c "/") (string-suffix? c ".rkt")))))

  (check-property
   (property a-module-candidate-is-a-fixed-point ([pfx gen:module-prefix])
     ;; Accepting a module candidate and completing again re-offers it, so
     ;; a second TAB never silently drops what the first one inserted.  A
     ;; directory candidate is excluded: completing past its slash lists
     ;; its contents, which is the point of the slash.
     (for/and ([c (in-list (collection-path-completions pfx))]
               #:unless (string-suffix? c "/"))
       (and (member c (collection-path-completions c)) #t))))

  (delete-directory/files tree))
