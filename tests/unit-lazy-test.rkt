#lang racket/base

;; Phase 0 of the native `rackton/unit` test framework: the laziness
;; foundation that integrated shrinking is built on.
;;
;; Rackton is strict, so a shrink tree whose children are an ordinary
;; `(List (Tree a))` would be fully forced at construction — fatal for
;; the (often unbounded) shrink candidates.  `Lazy`/`Stream` defer that
;; work behind a thunk.  The second test is the canary: it takes a
;; finite prefix of an *infinite* stream, which only terminates if
;; construction does not force the deferred tail.

(require rackunit
         "../main.rkt")

(rackton
  (require "../unit/lazy.rkt")

  ;; force-lazy . delay-lazy is the identity on values.
  (: five Integer)
  (define five (force-lazy (delay-lazy 5)))

  ;; An infinite stream of naturals.  Productive only because the tail
  ;; sits inside a `Lazy` thunk and is not evaluated at SCons time.
  (: nats-from (-> Integer (Stream Integer)))
  (define (nats-from n)
    (SCons n (Lazy (lambda (_) (nats-from (+ n 1))))))

  (: first3 (List Integer))
  (define first3 (stream-take 3 (nats-from 0)))

  (: len3 Integer)
  (define len3 (length first3))

  (: sum3 Integer)
  (define sum3 (sum first3)))

(test-case "force-lazy / delay-lazy round-trip"
  (check-equal? five 5))

(test-case "stream-take is lazy: a finite prefix of an infinite stream"
  ;; Terminates ⇒ construction did not force the tail.
  (check-equal? len3 3)
  ;; 0 + 1 + 2 ⇒ the right three elements, in order.
  (check-equal? sum3 3))
