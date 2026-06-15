#lang rackton

;; Tests for the guaranteed-infinite stream `IStream` from
;; rackton/data/istream.  With no empty constructor, `istream-head` and
;; `istream-tail` are total and `istream-take n` always yields exactly
;; `n` elements.  IStream is the canonical infinite Comonad (extract =
;; head, duplicate = the stream of all tails); it is deliberately NOT a
;; cartesian monad (that would diverge), so the suite covers Functor,
;; the zippy FunctorApply, and the comonad — but no monad.

(require rackton/data/istream
         rackton/data/lazy
         rackton/control/apply
         rackton/control/comonad
         "../unit.rkt")

(: nats (IStream Integer))
(define nats (istream-from 0))            ; 0 1 2 3 …

(: suite (List Test))
(define suite
  (list
   (it "istream-head is total"
       (check-equal? (istream-head nats) 0))
   (it "istream-tail is total and stays infinite"
       (check-equal? (istream-take 3 (istream-tail nats))
                     (Cons 1 (Cons 2 (Cons 3 Nil)))))
   (it "istream-take yields exactly n"
       (check-equal? (istream-take 4 nats)
                     (Cons 0 (Cons 1 (Cons 2 (Cons 3 Nil))))))
   (it "istream-iterate"
       (check-equal? (istream-take 4 (istream-iterate (lambda (n) (* n 2)) 1))
                     (Cons 1 (Cons 2 (Cons 4 (Cons 8 Nil))))))
   (it "istream-repeat is constant"
       (check-equal? (istream-take 3 (istream-repeat 9))
                     (Cons 9 (Cons 9 (Cons 9 Nil)))))
   (it "istream-map stays lazy"
       (check-equal? (istream-take 4 (istream-map (lambda (n) (* n 2)) nats))
                     (Cons 0 (Cons 2 (Cons 4 (Cons 6 Nil))))))
   (it "istream-zip-with pairs positionwise"
       (check-equal? (istream-take 3 (istream-zip-with (lambda (a) (lambda (b) (+ a b)))
                                                       (istream-from 0)
                                                       (istream-from 10)))
                     (Cons 10 (Cons 12 (Cons 14 Nil)))))
   (it "istream->stream embeds into Stream"
       (check-equal? (stream-take 4 (istream->stream nats))
                     (Cons 0 (Cons 1 (Cons 2 (Cons 3 Nil))))))
   (it "generic fmap dispatches to istream-map"
       (check-equal? (istream-take 4 (fmap (lambda (n) (* n 2)) nats))
                     (Cons 0 (Cons 2 (Cons 4 (Cons 6 Nil))))))
   (it "generic apply zips positionwise (never truncates)"
       (check-equal? (istream-take 4 (apply (istream-repeat (lambda (n) (+ n 1))) nats))
                     (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil))))))
   ;; --- comonad ---
   (it "extract is the head"
       (check-equal? (extract nats) 0))
   (it "extract . duplicate = id (observed through a prefix)"
       (check-equal? (istream-take 4 (extract (duplicate nats)))
                     (Cons 0 (Cons 1 (Cons 2 (Cons 3 Nil))))))
   (it "extract . tail . duplicate = tail (the duplicate-comonad law)"
       (check-equal? (istream-take 3 (extract (istream-tail (duplicate nats))))
                     (istream-take 3 (istream-tail nats))))
   (it "extend extract = id (observed through a prefix)"
       (check-equal? (istream-take 4 (extend (lambda (w) (extract w)) nats))
                     (Cons 0 (Cons 1 (Cons 2 (Cons 3 Nil))))))
   (it "extend runs a co-Kleisli arrow at every position"
       ;; sum of this element and the next, at each position: 1 3 5 …
       (check-equal? (istream-take 3 (extend (lambda (w) (+ (istream-head w)
                                                            (istream-head (istream-tail w))))
                                             nats))
                     (Cons 1 (Cons 3 (Cons 5 Nil)))))))

(: _ran Unit)
(define _ran (run-io (run-suite "istream" suite)))
