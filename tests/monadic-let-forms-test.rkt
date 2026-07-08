#lang rackton

;; Monadic / applicative binding forms — PLAN.org Backlog item 1.
;;
;;   let& — sequential monad bind  (deps allowed; nested flatmap)
;;   let% — parallel/independent monad bind (product then flatmap; body monadic)
;;   let+ — applicative bind (product then fmap; body PURE; result wrapped)
;;   named  let   — pure Scheme-style loop  (letrec + initial call)
;;   named  let%  — monadic loop (loop params are monadic values)
;;
;; All are parser-level desugarings; this file pins their behaviour.

(require "../unit.rkt")

;; --- let& : sequential monad bind, later bindings see earlier ---
(: seq-add (-> Integer (Maybe Integer)))
(define (seq-add n)
  (let& ([a (Some n)]
         [b (Some (+ a 1))]      ;; b references a
         [c (Some (+ b 1))])     ;; c references b
    (Some (+ (+ a b) c))))

(: seq-short (Maybe Integer))
(define seq-short
  (let& ([a (Some 1)]
         [_ None]                ;; short-circuits the chain
         [b (Some 2)])
    (Some (+ a b))))

;; --- let% : independent monad bind over Maybe ---
(: par-add (Maybe Integer))
(define par-add
  (let% ([a (Some 10)]
         [b (Some 20)]
         [c (Some 30)])
    (Some (+ (+ a b) c))))

(: par-short (Maybe Integer))
(define par-short
  (let% ([a (Some 1)]
         [b None])
    (Some (+ a b))))

;; --- let% over List : product = cartesian, body monadic (concat) ---
(: par-list (List Integer))
(define par-list
  (let% ([x (list 1 2)]
         [y (list 10 20)])
    (list (+ x y))))

;; --- let+ : applicative bind, PURE body, result wrapped ---
(: app-add (Maybe Integer))
(define app-add
  (let+ ([a (Some 4)]
         [b (Some 5)])
    (+ a b)))                    ;; pure body — no Some wrapper

(: app-list (List Integer))
(define app-list
  (let+ ([x (list 1 2)]
         [y (list 10 20)])
    (+ x y)))                    ;; pure body

;; --- single-binding degenerate cases ---
(: one-and (Maybe Integer))
(define one-and (let& ([a (Some 7)]) (Some (+ a 1))))
(: one-par (Maybe Integer))
(define one-par (let% ([a (Some 7)]) (Some (+ a 1))))
(: one-app (Maybe Integer))
(define one-app (let+ ([a (Some 7)]) (+ a 1)))

;; --- named plain let : pure loop ---
(: sum-to (-> Integer Integer))
(define (sum-to n)
  (let loop ([i 0] [acc 0])
    (if (> i n) acc (loop (+ i 1) (+ acc i)))))

;; --- named let% : monadic loop over Maybe ---
(: count-down (-> Integer (Maybe Integer)))
(define (count-down n)
  (let% loop ([a (Some n)])
    (if (<= a 0) (Some 0) (loop (Some (- a 1))))))

;; ----- checks -------------------------------------------------------

(: suite (List Test))
(define suite
  (list
    (it "let& sequential, dependent bindings + short-circuit"
        (all-checks
          (list (check-equal? (seq-add 1) (Some 6))    ;; 1 + 2 + 3
                (check-equal? (seq-add 5) (Some 18))   ;; 5 + 6 + 7
                (check-equal? seq-short None))))
    (it "let% independent bindings over Maybe"
        (all-checks
          (list (check-equal? par-add (Some 60))
                (check-equal? par-short None))))
    (it "let% over List is cartesian then concat"
        ;; Cons/Nil (from the prelude) — Rackton lists are not Racket lists.
        (check-equal? par-list (Cons 11 (Cons 21 (Cons 12 (Cons 22 Nil))))))
    (it "let+ applicative, pure body, result wrapped"
        (all-checks
          (list (check-equal? app-add (Some 9))
                (check-equal? app-list (Cons 11 (Cons 21 (Cons 12 (Cons 22 Nil))))))))
    (it "single-binding degenerate forms"
        (all-checks
          (list (check-equal? one-and (Some 8))
                (check-equal? one-par (Some 8))
                (check-equal? one-app (Some 8)))))
    (it "named plain let loop (pure)"
        (all-checks
          (list (check-equal? (sum-to 5) 15)           ;; 0+1+2+3+4+5
                (check-equal? (sum-to 0) 0))))
    (it "named let% monadic loop over Maybe"
        (all-checks
          (list (check-equal? (count-down 5) (Some 0))
                (check-equal? (count-down 0) (Some 0)))))))

(: test-main (IO Unit))
(define test-main (run-suite "monadic-let-forms" suite))
