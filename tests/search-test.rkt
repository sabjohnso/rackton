#lang racket/base

;; Hoogle-style search: full-signature queries (unification, argument
;; permutation, ranking), result-type queries, name queries — at the
;; REPL (,search / ,returns) and over the installed collections (the
;; rackton/search CLI's machinery).

(require rackunit
         racket/list
         racket/string
         "../private/repl.rkt"
         "../private/repl-search.rkt"
         "../private/analyze.rkt"
         (only-in "../private/scheme-codec.rkt" sexp->type)
         (only-in "../private/types.rkt" scheme scheme->datum))

(define (drive-session inputs)
  (for/fold ([state (rackton-repl-init)] [out '()] #:result (values state (reverse out)))
            ([form (in-list inputs)])
    (define-values (state* o) (rackton-repl-step state form))
    (values state* (cons o out))))

(define (last-output inputs)
  (define-values (_ outs) (drive-session inputs))
  (car (reverse outs)))

;; ----- ,search: full signatures ---------------------------------------

(test-case ",search finds an exact-shape signature"
  (define out (last-output '((unquote search (-> Integer (-> Integer Integer))))))
  (check-regexp-match #px"(?m:^\\+ :: )" out)
  (check-false (regexp-match #rx"show :: " out)))

(test-case ",search matches modulo unification of type variables"
  ;; map's exact shape, with the query's own variable names
  (define out (last-output '((unquote search (-> (-> x y) (-> (List x) (List y)))))))
  (check-regexp-match #rx"fmap :: " out))

(test-case ",search matches with arguments permuted, ranked after exact"
  (define out (last-output
               '((: apply-to (-> Integer (-> (-> Integer Integer) Integer)))
                 (define (apply-to x f) (f x))
                 (unquote search (-> (-> Integer Integer) (-> Integer Integer))))))
  ;; apply-to takes the same arguments in the other order
  (check-regexp-match #rx"apply-to :: " out)
  ;; an exact-shape session match must list before the permuted one
  (define out2 (last-output
                '((: exactly (-> (-> Integer Integer) (-> Integer Integer)))
                  (define (exactly f x) (f x))
                  (: apply-to (-> Integer (-> (-> Integer Integer) Integer)))
                  (define (apply-to x f) (f x))
                  (unquote search (-> (-> Integer Integer) (-> Integer Integer))))))
  (check-true (< (caar (regexp-match-positions #rx"exactly" out2))
                 (caar (regexp-match-positions #rx"apply-to" out2)))
              "exact-shape matches rank before permuted ones"))

(test-case ",search with a non-arrow type finds values of that type"
  (define out (last-output '((: answer Integer)
                             (define answer 42)
                             (unquote search Integer))))
  (check-regexp-match #rx"answer :: " out))

(test-case ",search with a string searches names"
  (define out (last-output '((unquote search "fold"))))
  (check-regexp-match #rx"foldr :: " out))

(test-case ",search reports no matches"
  (define out (last-output '((unquote search (-> Bytes (-> Bytes Char))))))
  (check-regexp-match #rx"no matches" out))

;; ----- ,returns: result-type search --------------------------------------

(test-case ",returns finds functions by result type"
  (define out (last-output '((unquote returns (List Integer)))))
  (check-regexp-match #rx"reverse :: " out)
  (check-regexp-match #rx"filter :: " out))

(test-case ",returns excludes bare-variable results"
  (define out (last-output '((define (idf x) x)
                             (unquote returns Integer))))
  (check-false (regexp-match #rx"idf :: " out)))

(test-case ",returns respects constraints"
  ;; sum :: (Foldable t) => (t Integer) -> Integer returns Integer,
  ;; and (Foldable List) exists, so it must appear.
  (define out (last-output '((unquote returns Integer))))
  (check-regexp-match #rx"sum :: " out))

;; ----- searching the index (the CLI's machinery) ----------------------------

(test-case "entry search runs over plain (name . scheme) pairs"
  (define (mono d) (scheme '() (sexp->type d)))
  (define entries
    (list (cons 'mk (mono '(-> Integer Box)))
          (cons 'unmk (mono '(-> Box Integer)))))
  (define hits (search-entries entries '(-> Integer Box)
                               #:kind 'signature #:env #f))
  (check-equal? (map car hits) '(mk)))

(test-case "the installed rackton collections index and search"
  (define entries (rackton-collection-entries))
  (check-true (> (length entries) 50)
              "the stdlib contributes a substantial index")
  (check-true (for/or ([e (in-list entries)])
                (eq? (index-entry-name e) 'Stream))
              "a known stdlib export appears"))
