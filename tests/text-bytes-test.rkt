#lang rackton

;; rackton/text/bytes — derived Bytes operations (Data.ByteString).

(require rackton/text/bytes
         "../unit.rkt")

;; helper: render Bytes back to a String for assertions
(: b->s (-> Bytes String))
(define (b->s b) (match (bytes->string b) [(Some s) s] [(None) "?"]))

(: e-null Boolean) (define e-null (bytes-null? bytes-empty))
(: h-null Boolean) (define h-null (bytes-null? (string->bytes "hi")))

(: tk String) (define tk (b->s (bytes-take 3 (string->bytes "hello"))))
(: dp String) (define dp (b->s (bytes-drop 3 (string->bytes "hello"))))

(: sp-l String) (define sp-l (match (bytes-split 2 (string->bytes "hello")) [(Pair a _) (b->s a)]))
(: sp-r String) (define sp-r (match (bytes-split 2 (string->bytes "hello")) [(Pair _ b) (b->s b)]))

(: cc String)
(define cc (b->s (bytes-concat (list (string->bytes "ab")
                                     (string->bytes "cd")
                                     (string->bytes "ef")))))

(: suite (List Test))
(define suite
  (list
   (it "null / empty"
       (all-checks
        (list (check-true  e-null)
              (check-false h-null))))
   (it "take / drop"
       (all-checks
        (list (check-equal? tk "hel")
              (check-equal? dp "lo"))))
   (it "split"
       (all-checks
        (list (check-equal? sp-l "he")
              (check-equal? sp-r "llo"))))
   (it "concat"
       (check-equal? cc "abcdef"))))

(: _ran Unit)
(define _ran (run-io (run-suite "rackton/text/bytes" suite)))
