#lang rackton

;; Coverage-gap fills (Coverage.org Phase 4).
;;
;; The dispatch coverage matrix (coverage/matrix.rkt) flagged a handful of
;; prelude instances that NO test exercised: `show` was never dispatched
;; on Maybe / Either / Unit / Bytes, and `Ord` was never dispatched on
;; Maybe / List (only Eq was).  These instances are prelude (so never
;; monomorphized) and work fine — they simply had no test pinning them.
;; This file exercises each so a regression in any of them now fails.

(require "../unit.rkt"
         rackton/text/bytes)   ;; string->bytes, to build a Bytes value

;; ----- Show on Maybe / Either / Unit / Bytes ------------------------

(: show-some String)  (define show-some  (show (Some 5)))
(: show-none String)  (define show-none  (show (ann None (Maybe Integer))))
(: show-left String)  (define show-left  (show (ann (Left 3)  (Either Integer Boolean))))
(: show-right String) (define show-right (show (ann (Right #t) (Either Integer Boolean))))
(: show-unit String)  (define show-unit  (show Unit))
(: show-bytes String) (define show-bytes (show (string->bytes "hi")))

;; ----- Ord on Maybe (None is least; Some compares by contents) ------

(: ord-none-lt-some Boolean)  (define ord-none-lt-some  (< (ann None (Maybe Integer)) (Some 1)))
(: ord-some-lt-some Boolean)  (define ord-some-lt-some  (< (Some 1) (Some 2)))
(: ord-some-not-lt  Boolean)  (define ord-some-not-lt   (< (Some 2) (Some 1)))
(: ord-maybe-min (Maybe Integer)) (define ord-maybe-min (min (Some 1) (Some 2)))
(: ord-maybe-max (Maybe Integer)) (define ord-maybe-max (max (ann None (Maybe Integer)) (Some 5)))

;; ----- Ord on List (lexicographic; Nil is least) --------------------

(: ord-nil-lt   Boolean) (define ord-nil-lt   (< Nil (Cons 1 Nil)))
(: ord-list-lt  Boolean) (define ord-list-lt  (< (Cons 1 Nil) (Cons 2 Nil)))
(: ord-list-lex Boolean) (define ord-list-lex (< (Cons 1 (Cons 2 Nil)) (Cons 1 (Cons 3 Nil))))
(: ord-list-le  Boolean) (define ord-list-le  (<= (Cons 1 Nil) (Cons 1 Nil)))

;; ----- suite --------------------------------------------------------

(: suite Test)
(define suite
  (describe "previously-untested prelude Show/Ord instances"
            (it "Show Maybe"
                (all-checks (list (check-equal? show-some "(Some 5)")
                                  (check-equal? show-none "None"))))
            (it "Show Either"
                (all-checks (list (check-equal? show-left  "(Left 3)")
                                  (check-equal? show-right "(Right True)"))))
            (it "Show Unit"  (check-equal? show-unit "Unit"))
            (it "Show Bytes" (check-equal? show-bytes "#\"hi\""))
            (it "Ord Maybe"
                (all-checks (list (check-true  ord-none-lt-some)
                                  (check-true  ord-some-lt-some)
                                  (check-false ord-some-not-lt)
                                  (check-equal? ord-maybe-min (Some 1))
                                  (check-equal? ord-maybe-max (Some 5)))))
            (it "Ord List"
                (all-checks (list (check-true ord-nil-lt)
                                  (check-true ord-list-lt)
                                  (check-true ord-list-lex)
                                  (check-true ord-list-le))))))

(: test-main (IO Unit))
(define test-main (run-suite-tree suite))
