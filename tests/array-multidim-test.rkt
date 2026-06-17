#lang racket/base

;; Phase 6 — multidimensional arrays and flattening.
;;
;; Multidimensional arrays are nested: `(Array n (Array m a))`.  This
;; falls out of Phase 5 for free (an array's element type can itself be
;; an array), so construction is `(array (array …) …)` and access is a
;; nested `aref`.  `flatten-major` / `flatten-minor` collapse one level
;; of nesting into a flat `(Array (* n m) a)` — same type, differing only
;; in the order the elements come out (row-major vs column-major).

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

(define-syntax-rule (check-rackton-compile-error form ...)
  (check-exn
   exn:fail?
   (lambda ()
     (eval #'(rackton form ...)
           (variable-reference->namespace (#%variable-reference))))))

;; ----- nested construction + access -------------------------------

(rackton
  (: grid (Array 2 (Array 3 Integer)))
  (define grid (array (array 1 2 3)
                      (array 4 5 6)))

  (: g-1-2 Integer) (define g-1-2 (aref (aref grid 1) 2))   ; row 1, col 2
  (: g-0-0 Integer) (define g-0-0 (aref (aref grid 0) 0)))

(test-case "a nested array is multidimensional; nested aref indexes it"
  (check-equal? g-1-2 6)
  (check-equal? g-0-0 1))

;; ----- flatten-major (row-major: outer index varies slowest) ------

(rackton
  (: fm (Array 6 Integer))
  (define fm (flatten-major (array (array 1 2 3)
                                   (array 4 5 6))))
  (: fm0 Integer) (define fm0 (aref fm 0))
  (: fm1 Integer) (define fm1 (aref fm 1))
  (: fm3 Integer) (define fm3 (aref fm 3))
  (: fm5 Integer) (define fm5 (aref fm 5)))

(test-case "flatten-major lays elements out row by row"
  (check-equal? fm0 1)   ; [0][0]
  (check-equal? fm1 2)   ; [0][1]
  (check-equal? fm3 4)   ; [1][0]
  (check-equal? fm5 6))  ; [1][2]

;; ----- flatten-minor (column-major: outer index varies fastest) ---

(rackton
  (: fn (Array 6 Integer))
  (define fn (flatten-minor (array (array 1 2 3)
                                   (array 4 5 6))))
  (: fn0 Integer) (define fn0 (aref fn 0))
  (: fn1 Integer) (define fn1 (aref fn 1))
  (: fn2 Integer) (define fn2 (aref fn 2)))

(test-case "flatten-minor lays elements out column by column"
  (check-equal? fn0 1)   ; [0][0]
  (check-equal? fn1 4)   ; [1][0]
  (check-equal? fn2 2))  ; [0][1]

;; ----- the flattened size is n*m, reduced to a literal ------------

(test-case "flattening a 2x3 array into a 5-array is a type error"
  ;; (* 2 3) = 6 ≠ 5, caught by the Nat solver.
  (check-rackton-compile-error
   (: bad (Array 5 Integer))
   (define bad (flatten-major (array (array 1 2 3) (array 4 5 6))))))

(test-case "aref past the computed size (* 2 3) is a compile error"
  (check-rackton-compile-error
   (: x Integer)
   (define x (aref (flatten-major (array (array 1 2 3) (array 4 5 6))) 6))))
