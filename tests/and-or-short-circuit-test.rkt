#lang rackton

;; Regression: surface `and` / `or` must short-circuit like `if`, not run
;; both operands eagerly.
;;
;; `and` / `or` were strict binary functions, so `(and e1 e2)` evaluated
;; both operands as application arguments before dispatching.  A guard
;; `(and safe? (unsafe-op))` therefore ran `unsafe-op` even when `safe?`
;; was false.  They now lower to `if` at the surface, so the second (for
;; `and`) or first-skipped (for `or`) operand is not evaluated.
;;
;; The poison operand `(== "z" (substring "x" 3 4))` throws at run time
;; (substring out of range): if the branch is evaluated, the whole test
;; process crashes; if short-circuiting works, it is never touched.  It is
;; inlined (not a top-level binding, which would be forced at module load).

(require "../unit.rkt")

(: suite (List Test))
(define suite
  (list
    (it "and short-circuits on #f (second operand not evaluated)"
        (check-false (and #f (== "z" (substring "x" 3 4)))))
    (it "or short-circuits on #t (second operand not evaluated)"
        (check-true (or #t (== "z" (substring "x" 3 4)))))
    (it "and truth table"
        (all-checks
          (list (check-true  (and #t #t))
                (check-false (and #t #f))
                (check-false (and #f #t))
                (check-false (and #f #f)))))
    (it "or truth table"
        (all-checks
          (list (check-true  (or #t #t))
                (check-true  (or #t #f))
                (check-true  (or #f #t))
                (check-false (or #f #f)))))
    (it "first-class / partial application still resolves the prelude fn"
        (all-checks
          (list (check-true  ((and #t) #t))
                (check-false ((and #t) #f))
                (check-false ((or #f) #f))
                (check-true  ((or #f) #t)))))
    ;; A locally shadowed `and` / `or` must NOT lower to `if`: it stays an
    ;; ordinary application of the user's binding.  Proven two ways at once —
    ;; the result is Integer (the `if`-lowering would demand a Boolean
    ;; condition and reject `3`), and both operands are consumed (`+` / `*`).
    (it "a locally shadowed `and` is an ordinary application, not lowered"
        (check-equal? (let ([and (lambda (a b) (+ a b))]) (and 3 4)) 7))
    (it "a locally shadowed `or` is an ordinary application, not lowered"
        (check-equal? (let ([or (lambda (a b) (* a b))]) (or 3 4)) 12))))

(: test-main (IO Unit))
(define test-main (run-suite "and/or short-circuit" suite))
