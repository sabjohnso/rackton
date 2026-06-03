#lang rackton

;; Tests for the lazy `Stream` and its combinators from rackton/data/lazy.
;; Streams may be infinite (the tail is a `Lazy`), so every test builds an
;; unbounded stream and observes only a finite prefix via `stream-take` —
;; which would diverge if the combinators were not lazy.

(require rackton/data/lazy
         "../unit.rkt")

(: nats (Stream Integer))
(define nats (stream-iterate (lambda (n) (+ n 1)) 0))   ; 0 1 2 3 …

(: doubled (Stream Integer))
(define doubled (stream-map (lambda (n) (* n 2)) nats))  ; 0 2 4 6 …

(: evens (Stream Integer))
(define evens (stream-filter (lambda (n) (== (mod n 2) 0)) nats))  ; 0 2 4 …

(: l123 (List Integer))
(define l123 (Cons 1 (Cons 2 (Cons 3 Nil))))

(: suite (List Test))
(define suite
  (list
   (it "stream-iterate / stream-take"
       (check-equal? (stream-take 4 nats) (Cons 0 (Cons 1 (Cons 2 (Cons 3 Nil))))))
   (it "stream-from counts up"
       (check-equal? (stream-take 3 (stream-from 5))
                     (Cons 5 (Cons 6 (Cons 7 Nil)))))
   (it "stream-repeat is constant and infinite"
       (check-equal? (stream-take 3 (stream-repeat 9))
                     (Cons 9 (Cons 9 (Cons 9 Nil)))))
   (it "stream-map stays lazy over an infinite stream"
       (check-equal? (stream-take 3 doubled) (Cons 0 (Cons 2 (Cons 4 Nil)))))
   (it "stream-filter stays lazy over an infinite stream"
       (check-equal? (stream-take 3 evens) (Cons 0 (Cons 2 (Cons 4 Nil)))))
   (it "stream-append crosses finite into infinite"
       (check-equal? (stream-take 5 (stream-append (list->stream l123) (stream-from 10)))
                     (Cons 1 (Cons 2 (Cons 3 (Cons 10 (Cons 11 Nil)))))))
   (it "stream-head / stream-tail"
       (all-checks
        (list (check-equal? (stream-head nats) (Some 0))
              (check-equal? (stream-head (stream-tail nats)) (Some 1)))))
   (it "stream-take past the end of a finite stream stops"
       (check-equal? (stream-take 10 (list->stream l123)) l123))))

(: _ran Unit)
(define _ran (run-io (run-suite "lazy-stream" suite)))
