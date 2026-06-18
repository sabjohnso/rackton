#lang racket/base

;; Concrete-size array operations.
;;
;; `array-take` / `array-drop` / `array-split-at` are literal-position
;; forms over an array of CONCRETE size: the size must be a known Nat so
;; the result size (k, n-k) is computed and the split point is bounds-
;; checked at compile time.  `array-map` / `array-fold` are ordinary
;; size-polymorphic functions (map preserves the size; fold consumes it).
;; (A size-polymorphic take/drop/split would need the deferred type-level
;; subtraction / symbolic solving — see PLAN.org.)

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

(define-syntax-rule (check-rackton-compile-error form ...)
  (check-exn
   exn:fail?
   (lambda ()
     (eval #'(rackton form ...)
           (variable-reference->namespace (#%variable-reference))))))

;; ----- take / drop -------------------------------------------------

(rackton
  (: tk (Array 2 Integer))
  (define tk (array-take 2 (array 10 20 30 40)))
  (: tk0 Integer) (define tk0 (aref tk 0))
  (: tk1 Integer) (define tk1 (aref tk 1))

  (: dp (Array 2 Integer))
  (define dp (array-drop 2 (array 10 20 30 40)))
  (: dp0 Integer) (define dp0 (aref dp 0))
  (: dp1 Integer) (define dp1 (aref dp 1)))

(test-case "array-take / array-drop slice with computed sizes"
  (check-equal? tk0 10) (check-equal? tk1 20)
  (check-equal? dp0 30) (check-equal? dp1 40))

;; ----- split-at ----------------------------------------------------

(rackton
  (: parts (Pair (Array 1 Integer) (Array 2 Integer)))
  (define parts (array-split-at 1 (array 10 20 30)))
  (: lo0 Integer) (define lo0 (aref (fst parts) 0))
  (: hi0 Integer) (define hi0 (aref (snd parts) 0))
  (: hi1 Integer) (define hi1 (aref (snd parts) 1)))

(test-case "array-split-at splits into a Pair of arrays summing to n"
  (check-equal? lo0 10)
  (check-equal? hi0 20)
  (check-equal? hi1 30))

;; ----- map (size-preserving, polymorphic) --------------------------

(rackton
  (: sq (Array 3 Integer))
  (define sq (array-map (lambda (x) (* x x)) (array 1 2 3)))
  (: sq2 Integer) (define sq2 (aref sq 2))

  ;; element type may change
  (: strs (Array 2 String))
  (define strs (array-map show (array 7 8)))
  (: s0 String) (define s0 (aref strs 0))

  ;; works at a polymorphic size — no concrete size needed
  (: double-all (All (n) (-> (Array n Integer) (Array n Integer))))
  (define (double-all xs) (array-map (lambda (x) (* x 2)) xs))
  (: d1 Integer) (define d1 (aref (double-all (array 5 6 7)) 1)))

(test-case "array-map preserves size and is size-polymorphic"
  (check-equal? sq2 9)
  (check-equal? s0 "7")
  (check-equal? d1 12))

;; ----- aref on a not-yet-resolved array type -----------------------

(rackton
  ;; aref on a size-polymorphic parameter: `xs`'s array type is fixed only
  ;; when the body is later unified with the signature, so at the `aref`
  ;; itself `xs` is still a bare type variable.  This used to be rejected
  ;; ("aref target must have a concrete array type"); aref now unifies the
  ;; target with an array shape instead of demanding one up front.
  (: head-of (All (n a) (-> (Array n a) a)))
  (define (head-of xs) (aref xs 0))
  (: h Integer) (define h (head-of (array 7 8 9))))

(test-case "aref works on a size-polymorphic parameter (typed via signature)"
  (check-equal? h 7))

;; ----- imap (indexed map) ------------------------------------------

(rackton
  ;; element i becomes  i*10 + x  — exposes the index that plain map hides
  (: tagged (Array 3 Integer))
  (define tagged (array-imap (lambda (i x) (+ (* i 10) x)) (array 1 2 3)))
  (: t0 Integer) (define t0 (aref tagged 0))   ; 0*10 + 1 = 1
  (: t1 Integer) (define t1 (aref tagged 1))   ; 1*10 + 2 = 12
  (: t2 Integer) (define t2 (aref tagged 2)))  ; 2*10 + 3 = 23

(test-case "array-imap exposes the element index, preserving size"
  (check-equal? t0 1)
  (check-equal? t1 12)
  (check-equal? t2 23))

;; ----- fold --------------------------------------------------------

(rackton
  (: total Integer)
  (define total (array-fold (lambda (acc x) (+ acc x)) 0 (array 1 2 3 4))))

(test-case "array-fold reduces left-to-right"
  (check-equal? total 10))

;; ----- foldr (right fold) ------------------------------------------

(rackton
  ;; right fold: 1 - (2 - (3 - 0)) = 2  (distinguishes it from the left fold)
  (: rf Integer)
  (define rf (array-foldr (lambda (x acc) (- x acc)) 0 (array 1 2 3))))

(test-case "array-foldr folds from the right"
  (check-equal? rf 2))

;; ----- traverse (mapM-style, applicative) --------------------------

(rackton
  (: checkpos (-> Integer (Maybe Integer)))
  (define (checkpos x) (if (> x 0) (Some x) None))

  ;; all elements pass → Some of the whole array
  (: ok (Maybe (Array 3 Integer)))
  (define ok (array-traverse checkpos (array 1 2 3)))
  (: ok2 Integer)
  (define ok2 (match ok [(Some a) (aref a 2)] [(None) -1]))

  ;; one element fails → None (effects short-circuit)
  (: bad (Maybe (Array 3 Integer)))
  (define bad (array-traverse checkpos (array 1 -2 3)))
  (: bad? Boolean)
  (define bad? (match bad [(Some _) #f] [(None) #t])))

(test-case "array-traverse threads an applicative effect, preserving size"
  (check-equal? ok2 3)      ; (Some (array 1 2 3)) → element 2 is 3
  (check-equal? bad? #t))   ; a failing element collapses to None

;; ----- rotate (cyclic, size-preserving) ----------------------------

(rackton
  ;; positive k rotates left: result[i] = input[(i+k) mod n]
  (: rl (Array 3 Integer)) (define rl (array-rotate 1 (array 1 2 3)))
  (: rl0 Integer) (define rl0 (aref rl 0))   ; 2
  (: rl2 Integer) (define rl2 (aref rl 2))   ; 1
  ;; negative k rotates right
  (: rr (Array 3 Integer)) (define rr (array-rotate -1 (array 1 2 3)))
  (: rr0 Integer) (define rr0 (aref rr 0))   ; 3
  ;; k wraps modulo the size
  (: rw (Array 3 Integer)) (define rw (array-rotate 4 (array 1 2 3)))
  (: rw0 Integer) (define rw0 (aref rw 0)))  ; same as rotate 1 → 2

(test-case "array-rotate cyclically shifts, preserving size"
  (check-equal? rl0 2) (check-equal? rl2 1)
  (check-equal? rr0 3)
  (check-equal? rw0 2))

;; ----- compile-time checks ----------------------------------------

(test-case "taking more than the array holds is a compile error"
  (check-rackton-compile-error
   (: x (Array 5 Integer))
   (define x (array-take 5 (array 1 2 3)))))

(test-case "a non-literal split point is a compile error"
  (check-rackton-compile-error
   (: x (Pair (Array 1 Integer) (Array 1 Integer)))
   (define x (let ([k 1]) (array-split-at k (array 1 2))))))
