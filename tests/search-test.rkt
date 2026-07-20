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

;; ----- constrained-variable queries -------------------------------------

;; `((Num a) => a)` is not a bare variable: the constraints narrow it to
;; the types having those instances.

(test-case ",search on a constrained variable is not a bare query"
  (define out (last-output
               '((unquote search ((Additive-Magma a) (Multiplicative-Magma a) => a)))))
  (check-false (regexp-match #rx"bare type variable" out)))

(test-case ",search on a constrained variable finds values of an instance type"
  (define out (last-output '((: answer Integer)
                             (define answer 42)
                             (unquote search ((Num a) => a)))))
  (check-regexp-match #rx"answer :: " out))

(test-case ",search on a constrained variable excludes non-instance types"
  ;; Peano has no Num instance, so its value must not be listed.
  (define setup '((define-data Peano Zero (Succ Peano))
                  (: p Peano)
                  (define p Zero)))
  (define out (last-output (append setup '((unquote search ((Num a) => a))))))
  (check-false (regexp-match #rx"(?m:^p :: )" out))
  ;; the binding is findable without the constraint, so the exclusion
  ;; above is the constraint's doing and not a broken setup
  (check-regexp-match #rx"(?m:^p :: )"
                      (last-output (append setup '((unquote search Peano))))))

(test-case ",returns on a constrained variable respects the constraint"
  (define setup '((define-data Peano Zero (Succ Peano))
                  (: mk-peano (-> Unit Peano))
                  (define (mk-peano u) Zero)))
  (check-false (regexp-match
                #rx"mk-peano :: "
                (last-output (append setup '((unquote returns ((Num a) => a)))))))
  (check-regexp-match #rx"mk-peano :: "
                      (last-output (append setup '((unquote returns Peano))))))

(test-case ",accepts on a constrained variable is not a bare query"
  (define out (last-output '((unquote accepts ((Num a) => a)))))
  (check-false (regexp-match #rx"bare type variable" out))
  (check-regexp-match #rx"\\+ :: " out))

;; ----- jointly unsatisfiable constraints --------------------------------

;; A candidate whose own class and the query's classes share no
;; instance type cannot be an answer, even though every predicate is
;; over a type variable and so individually irrefutable.

(test-case ",search drops a candidate whose classes share no instance type"
  (define out (last-output
               '((unquote search ((Additive-Magma a) (Multiplicative-Magma a) => a)))))
  ;; no type has Monoid together with both magma classes
  (check-false (regexp-match #px"(?m:^mempty\\b)" out))
  ;; Category, MonadEnv and MonadState share no instance with them either
  (check-false (regexp-match #px"(?m:^ident\\b)" out))
  (check-false (regexp-match #px"(?m:^ask-en\\b)" out))
  ;; the numeric-tower values remain
  (check-regexp-match #px"(?m:^one\\b)" out)
  (check-regexp-match #px"(?m:^zero\\b)" out))

(test-case ",returns keeps a candidate whose classes do share an instance type"
  ;; Floating and the magma classes meet at Float and Complex
  (define out (last-output
               '((unquote returns ((Additive-Magma a) (Multiplicative-Magma a) => a)))))
  (check-regexp-match #px"(?m:^pi\\b)" out))

(test-case ",accepts drops a class with no instances at all"
  ;; Coerce is a class the session env carries no instance for
  (define out (last-output '((data Blank MkBlank)
                             (unquote accepts Blank))))
  (check-regexp-match #rx"no functions" out))

;; ----- nested qualifiers ------------------------------------------------

;; `traverse :: (Traversable t) => ((Applicative f) => (-> (-> a (f b))
;; (t a) (f (t b))))` nests its contexts.  Peeling only the outer one
;; leaves a qualified type where the arrow belongs, hiding the argument
;; positions.

(test-case ",search sees through a nested constraint context"
  (define out (last-output
               '((unquote search (-> (-> a (f b)) (t a) (f (t b)))))))
  (check-regexp-match #px"(?m:^traverse$|^traverse ::)" out))

(test-case ",accepts sees through a nested constraint context"
  (define out (last-output '((unquote accepts (List Integer)))))
  (check-regexp-match #px"(?m:^traverse$|^traverse ::)" out))

(test-case "a nested context does not match as a plain value"
  ;; traverse's body is an arrow under two contexts, so an arity-0
  ;; query must not list it.
  (define out (last-output '((unquote search ((Num a) => a)))))
  (check-false (regexp-match #px"(?m:^traverse$|^traverse ::)" out)))

(test-case "a class with no instance in scope refutes its candidate"
  ;; the prelude carries no MonadTrans instance, so `lift` cannot be
  ;; applied to anything a bare session can build
  (define out (last-output '((unquote accepts (List Integer)))))
  (check-false (regexp-match #px"(?m:^lift$|^lift ::)" out)))

(test-case "entry search with no env judges the query against the prelude"
  (define (mono d) (scheme '() (sexp->type d)))
  (define entries (list (cons 'mk-int (mono '(-> Unit Integer)))
                        (cons 'mk-unit (mono '(-> Integer Unit)))))
  ;; Integer has an Additive-Magma instance in the prelude; Unit does not
  (check-equal? (map car (search-entries entries '((Additive-Magma a) => (-> b a))
                                         #:kind 'signature #:env #f))
                '(mk-int)))

(test-case "entry search keeps an unknown query class from refuting everything"
  (define (mono d) (scheme '() (sexp->type d)))
  (define entries (list (cons 'mk (mono '(-> Integer Box)))))
  (define hits (search-entries entries '((Nonesuch a) => (-> Integer a))
                               #:kind 'signature #:env #f))
  (check-equal? (map car hits) '(mk)))

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
