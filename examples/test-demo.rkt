#lang rackton

;; test-demo.rkt — a tour of the native rackton/unit framework.
;;
;; Run it with `racket examples/test-demo.rkt`; it builds one suite that
;; mixes rackunit-style checks, property-based tests (with integrated
;; shrinking), and algebraic-law bundles, then runs it and prints a
;; hierarchical report.  One property is deliberately false so you can
;; see the shrunk counterexample and the replay seed.

(require rackton/unit)

;; ----- A reverse function to put under test --------------------------

(: rev (-> (List a) (List a)))
(define (rev xs)
  (match xs
    [(Nil)       Nil]
    [(Cons h t)  (append (rev t) (Cons h Nil))]))

;; ----- The suite -----------------------------------------------------

(: suite Test)
(define suite
  (describe "rackton/unit demo"

            ;; plain unit checks
            (describe "checks"
                      (it "arithmetic" (check-equal? (+ 2 2) 4))
                      (it "reverse"    (check-true (int-list-eq (rev (list 1 2)) (list 2 1)))))

            ;; properties with integrated shrinking
            (describe "properties"
                      (it-prop "rev is involutive on Integer lists"
                               (for-all-gen show-int-list (gen-list (int-range 0 9))
                                            (lambda (xs) (int-list-eq (rev (rev xs)) xs))))
                      (it-prop "DELIBERATELY FALSE: every int < 5"
                               (for-all (int-range 0 100) (lambda (x) (< x 5)))))

            ;; algebraic laws
            (describe "laws"
                      (eq-laws  (int-range -50 50))
                      (ord-laws (int-range -50 50))
                      (monoid-laws gen-string ""))))

;; ----- Helpers for the list property ---------------------------------

(: int-list-eq (-> (List Integer) (-> (List Integer) Boolean)))
(define (int-list-eq xs ys)
  (match xs
    [(Nil)        (match ys [(Nil) #t] [(Cons _ _) #f])]
    [(Cons a as)  (match ys
                    [(Nil)        #f]
                    [(Cons b bs)  (if (== a b) (int-list-eq as bs) #f)])]))

(: show-int-list (-> (List Integer) String))
(define (show-int-list xs)
  (match xs
    [(Nil)       "[]"]
    [(Cons h t)  (string-append (integer->string h)
                                (string-append " " (show-int-list t)))]))

;; ----- Run it --------------------------------------------------------

(: main (IO Unit))
(define main (do [s <- (run-tests suite)]
               (println (string-append "passed="
                                       (string-append (integer->string (summary-passed s))
                                                      (string-append "  failed="
                                                                     (integer->string (summary-failed s))))))))
