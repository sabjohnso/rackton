#lang racket/base

;; List sugar must be shadow-proof: bracket literals `[…]`, variadic
;; rest-arg gathering, and list patterns always build/match the prelude
;; `List`, even in a module that locally shadows the `Cons` / `Nil`
;; constructor names.  Constructors are prefab-keyed by name, so the
;; runtime value is a valid List regardless; the defect this pins is
;; that the sugar's `Cons`/`Nil` used to resolve to the SHADOWING
;; constructor at type-check time.

(require rackunit
         (for-syntax racket/base)
         "../main.rkt"
         "../private/repl.rkt")

;; Bracket literals and list PATTERNS need real bracket `paren-shape`,
;; which only read-syntax sets — a REPL datum `[1 2 3]` is just the
;; application `(1 2 3)`.  So exercise them through an embedded rackton
;; block, whose source carries genuine brackets, in a module that
;; shadows `Cons` / `Nil` with its own Nonempty-List constructor.
(rackton
  (data (NList a) (Sole a) (Cons a (NList a)))

  ;; bracket literal → prelude List, not NList
  (: xs (List Integer))
  (define xs [1 2 3])
  (: xs-len Integer)
  (define xs-len (length xs))

  ;; list pattern matches the prelude List
  (: first2 (-> (List Integer) Integer))
  (define (first2 ys) (match ys [[a b] a] [_ 0]))
  (: hd Integer)
  (define hd (first2 [10 20])))

(test-case "bracket literal builds a prelude List under Cons shadowing"
  (check-equal? xs-len 3))

(test-case "list pattern matches a prelude List under Cons shadowing"
  (check-equal? hd 10))

;; Variadic gathering is exercised through the REPL kernel (no brackets
;; involved — the rest-list is synthesized by the compiler).  `main.rkt`
;; above shadows some racket/base names (e.g. `reverse`) with prelude
;; functions, so this helper returns just the LAST output string and
;; avoids any shadowed name in the racket-level code.
(define (last-output inputs)
  (for/fold ([state (rackton-repl-init)] [out ""] #:result out)
            ([form (in-list inputs)])
    (define-values (state* o) (rackton-repl-step state form))
    (values state* o)))

(define shadow-cons '(data (NList a) (Sole a) (Cons a (NList a))))

(test-case "variadic rest-gathering builds a prelude List under Cons shadowing"
  ;; `xs` is the gathered rest-list; returning it exposes its type.
  (check-regexp-match
   #rx"List Integer"
   (last-output
    (list shadow-cons
          '(define (collect x . xs) xs)
          (list 'unquote 'type '(collect 1 2 3))))))

(test-case "variadic consuming the rest list runs (the reported nonempty-list)"
  ;; The gathered rest is a prelude List, consumed with the qualified
  ;; prelude `p:Cons` / `p:Nil`, while bare `Cons` builds the NList.
  (check-regexp-match
   #rx"NList Integer"
   (last-output
    (list (list 'require (list 'qualified-in 'p 'rackton/prelude))
          shadow-cons
          '(define (mk x . xs)
             (let recur ([ys xs] [acc (Sole x)])
               (match ys
                 [(p:Cons y rest) (recur rest (Cons y acc))]
                 [p:Nil acc])))
          (list 'unquote 'type '(mk 1 2 3))))))
