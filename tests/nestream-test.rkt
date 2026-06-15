#lang rackton

;; Tests for the nonempty lazy stream `NEStream` from rackton/data/nestream.
;; The whole point of the type is that the head is always present, so
;; `nestream-head` is total (returns `a`, not `(Maybe a)`).  The tail is a
;; lazy `Stream`, so a nonempty stream may still be infinite; as in the
;; lazy-stream tests we observe infinite values only through finite
;; prefixes (`nestream-take` / `stream-take`).
;;
;; NEStream is BOTH a monad (the nonempty analog of Stream's cartesian
;; list-monad) and a comonad (extract = head), so the suite exercises
;; both and checks they coexist on one type.

(require rackton/data/nestream
         rackton/data/lazy
         rackton/control/comonad
         "../unit.rkt")

(: l123 (List Integer))
(define l123 (Cons 1 (Cons 2 (Cons 3 Nil))))

;; 1 2 3 — a finite nonempty stream.
(: ne123 (NEStream Integer))
(define ne123 (nestream 1 (list->stream (Cons 2 (Cons 3 Nil)))))

;; 0 1 2 3 … — an infinite nonempty stream.
(: nenats (NEStream Integer))
(define nenats (nestream 0 (stream-from 1)))

;; an empty `Stream Integer`, built by filtering everything out, for the
;; partial `stream->nestream` direction.
(: empty-ints (Stream Integer))
(define empty-ints (stream-filter (lambda (n) (== n 999)) (list->stream l123)))

;; n, n*10 as a nonempty stream — for flatmap.
(: ne-twice (-> Integer (NEStream Integer)))
(define (ne-twice n) (nestream n (list->stream (Cons (* n 10) Nil))))

;; collapse a `(Maybe (NEStream a))` to its head, so check-equal? sees a
;; plain `(Maybe a)` rather than thunks behind the lazy tail.
(: head-of (-> (Maybe (NEStream Integer)) (Maybe Integer)))
(define (head-of m)
  (match m
    [(Some ne) (Some (nestream-head ne))]
    [(None)    None]))

;; pure is return-typed, so the signature pins it to NEStream (also
;; checks the (Applicative NEStream) instance resolves across modules).
(: ne-seven (NEStream Integer))
(define ne-seven (pure 7))

(: suite (List Test))
(define suite
  (list
   (it "nestream-head is total"
       (check-equal? (nestream-head nenats) 0))
   (it "nestream-tail drops the head, leaving a Stream"
       (check-equal? (stream-take 3 (nestream-tail nenats))
                     (Cons 1 (Cons 2 (Cons 3 Nil)))))
   (it "nestream-take over an infinite nonempty stream"
       (check-equal? (nestream-take 3 nenats) (Cons 0 (Cons 1 (Cons 2 Nil)))))
   (it "nestream-take past the end of a finite one stops"
       (check-equal? (nestream-take 10 ne123) l123))
   (it "nestream-cons prepends, staying nonempty"
       (check-equal? (nestream-take 3 (nestream-cons 9 nenats))
                     (Cons 9 (Cons 0 (Cons 1 Nil)))))
   (it "nestream-map preserves nonemptiness and stays lazy"
       (check-equal? (nestream-take 3 (nestream-map (lambda (n) (* n 2)) nenats))
                     (Cons 0 (Cons 2 (Cons 4 Nil)))))
   (it "nestream-append crosses finite into infinite"
       (check-equal? (nestream-take 5 (nestream-append ne123 (stream-from 10)))
                     (Cons 1 (Cons 2 (Cons 3 (Cons 10 (Cons 11 Nil)))))))
   (it "nestream->stream forgets the guarantee"
       (check-equal? (stream-take 5 (nestream->stream ne123)) l123))
   (it "stream->nestream recovers a nonempty stream"
       (check-equal? (head-of (stream->nestream (list->stream l123))) (Some 1)))
   (it "stream->nestream of an empty stream is None"
       (check-equal? (head-of (stream->nestream empty-ints)) None))
   ;; --- monad ---
   (it "pure makes a one-element nonempty stream"
       (check-equal? (nestream-take 3 ne-seven) (Cons 7 Nil)))
   (it "flatmap concatenates and stays nonempty"
       (check-equal? (nestream-take 10 (flatmap ne-twice ne123))
                     (Cons 1 (Cons 10 (Cons 2 (Cons 20 (Cons 3 (Cons 30 Nil))))))))
   (it "flatmap stays lazy over an infinite nonempty stream"
       (check-equal? (nestream-take 5 (flatmap ne-twice nenats))
                     (Cons 0 (Cons 0 (Cons 1 (Cons 10 (Cons 2 Nil)))))))
   (it "fapply takes the cross product"
       (check-equal? (nestream-take 10
                                    (fapply (nestream (lambda (n) (+ n 1))
                                                      (list->stream
                                                       (Cons (lambda (n) (* n 10)) Nil)))
                                            ne123))
                     (Cons 2 (Cons 3 (Cons 4 (Cons 10 (Cons 20 (Cons 30 Nil))))))))
   (it "generic fmap dispatches to nestream-map"
       (check-equal? (nestream-take 3 (fmap (lambda (n) (* n 2)) nenats))
                     (Cons 0 (Cons 2 (Cons 4 Nil)))))
   ;; --- comonad ---
   (it "extract is the head"
       (check-equal? (extract ne123) 1))
   (it "extract . duplicate = id (observed through a prefix)"
       (check-equal? (nestream-take 3 (extract (duplicate ne123))) l123))
   (it "duplicate exposes the suffixes"
       (all-checks
        (list
         (check-equal? (nestream-take 3 (extract (duplicate ne123))) l123)
         (check-equal? (match (stream-head (nestream-tail (duplicate ne123)))
                         [(Some s) (Some (nestream-take 2 s))]
                         [(None)   None])
                       (Some (Cons 2 (Cons 3 Nil)))))))
   (it "extend extract = id (observed through a prefix)"
       (check-equal? (nestream-take 3 (extend (lambda (w) (extract w)) ne123))
                     l123))))

(: _ran Unit)
(define _ran (run-io (run-suite "nestream" suite)))
