#lang rackton

;; rackton/data/char — Data.Char predicates and conversions.

(require rackton/data/char
         "../unit.rkt")

(: d5 Boolean)  (define d5 (digit? #\5))
(: da Boolean)  (define da (digit? #\a))
(: hf Boolean)  (define hf (hex-digit? #\f))
(: hg Boolean)  (define hg (hex-digit? #\g))
(: uA Boolean)  (define uA (upper? #\A))
(: ua Boolean)  (define ua (upper? #\a))
(: la Boolean)  (define la (lower? #\a))
(: alA Boolean) (define alA (alpha? #\A))
(: al5 Boolean) (define al5 (alpha? #\5))
(: an5 Boolean) (define an5 (alpha-num? #\5))
(: ansp Boolean)(define ansp (alpha-num? #\space))
(: sp Boolean)  (define sp (space? #\space))
(: ctl Boolean) (define ctl (control? (chr 0)))
(: pun Boolean) (define pun (punctuation? #\!))
(: d7 Integer)  (define d7 (digit->int #\7))
(: i3 Char)     (define i3 (int->digit 3))
(: ordA Integer)(define ordA (ord #\A))
(: chrB Char)   (define chrB (chr 66))
(: upa Char)    (define upa (to-upper #\a))
(: loZ Char)    (define loZ (to-lower #\Z))

;; ---------- assertions ---------------------------------------

(: suite (List Test))
(define suite
  (list
    (it "digit / hex"
        (all-checks
          (list (check-equal? d5 #t) (check-equal? da #f)
                (check-equal? hf #t) (check-equal? hg #f))))
    (it "case predicates"
        (all-checks
          (list (check-equal? uA #t) (check-equal? ua #f) (check-equal? la #t))))
    (it "alpha / alnum / space"
        (all-checks
          (list (check-equal? alA #t) (check-equal? al5 #f)
                (check-equal? an5 #t) (check-equal? ansp #f)
                (check-equal? sp #t))))
    (it "control / punctuation"
        (all-checks
          (list (check-equal? ctl #t) (check-equal? pun #t))))
    (it "conversions"
        (all-checks
          (list (check-equal? d7 7)
                (check-equal? i3 #\3)
                (check-equal? ordA 65)
                (check-equal? chrB #\B)
                (check-equal? upa #\A)
                (check-equal? loZ #\z))))))

(: test-main (IO Unit))
(define test-main (run-suite "data-char" suite))
