#lang rackton

;; arrow-loop.rkt — value recursion with ArrowLoop and `proc rec`.
;;
;; The strict function arrow `(->)` has no ArrowLoop: feeding an output
;; back as an input forces the feedback channel before it is produced, so
;; the knot can never be tied.  rackton/data/arrow-lazy provides `LFun`, a
;; function on *lazy* values whose product `LPair` keeps both halves
;; deferred — that is exactly what lets `arrow-loop` (and the `proc rec`
;; sugar that desugars to it) tie a recursive knot.
;;
;; Each stream below is defined *in terms of itself*.  `proc rec` binds a
;; stream `s` and feeds it through an arrow whose own output is `s`:
;;
;;     (rec [s <- (feed ARROW s)])
;;
;; The one rule for keeping a loop productive: the feedback must reach a
;; NON-`arr` lazy primitive before it is forced.  `arr` lifts a *strict*
;; function, so it forces its argument; `lcons` (from arrow-lazy) instead
;; drops its input into a fresh `SCons`'s deferred tail.  So every ARROW
;; here ends in an `lcons` — that deferral is what makes the self-
;; reference terminate.  Routing the feedback through `arr` alone would
;; diverge.
;;
;; The code reads top-down: the program, then the streams it prints, then
;; the helpers they lean on, then the line that runs it all.
;;
;; Run it with `racket examples/arrow-loop.rkt`.

(require rackton/data/arrow-lazy
         rackton/data/lazy)

;; ----- the program --------------------------------------------------
;; Print a finite prefix of each infinite, self-referential stream.

(: main (IO Unit))
(define main
  (let& ([_ (println (labelled "ones (×8)"  (stream-take 8 ones)))]
         [_ (println (labelled "nats (×8)"  (stream-take 8 nats)))])
    (println (labelled "fibs (×12)" (stream-take 12 fibs)))))

;; ----- the self-referential streams ---------------------------------

;; ones = 1 : ones
;; `lcons 1` prepends 1 and defers the feedback, so the knot ties to the
;; infinite stream of 1s.
(: ones (Stream Integer))
(define ones
  (run-lfun
    (proc (_)
          (rec [s <- (feed (lcons 1) s)])
          (feed (arr (lambda (z) z)) s))
    0))

;; nats = 0 : map (+1) nats
;; Fold the recurrence into ONE arrow so it stays a single self-reference.
;; comp is right-to-left, so the right arrow runs first:
;;   comp (lcons 0) (arr (stream-map inc))
;;   applied to `ns` = lcons 0 (map (+1) ns) = 0 : map (+1) ns.
;; `lcons 0` is the outermost step, so the feedback is deferred.
(: nats (Stream Integer))
(define nats
  (run-lfun
    (proc (_)
          (rec [ns <- (feed (comp (lcons 0) (arr (stream-map inc))) ns)])
          (feed (arr (lambda (z) z)) ns))
    0))

;; fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
;;   comp (lcons 0) (comp (lcons 1) (arr tails-plus))
;;   applied to `fs` = 0 : 1 : (fs + tail fs).
;; The two `lcons`es supply the 0 and 1 base cases and defer the
;; feedback; `tails-plus` (under `arr`) is forced only once the prefix it
;; needs is already built.
(: fibs (Stream Integer))
(define fibs
  (run-lfun
    (proc (_)
          (rec [fs <- (feed (comp (lcons 0) (comp (lcons 1) (arr tails-plus))) fs)])
          (feed (arr (lambda (z) z)) fs))
    0))

;; ----- helpers ------------------------------------------------------

;; Label a finite prefix for printing.
(: labelled (-> String (-> (List Integer) String)))
(define (labelled name xs)
  (string-append name (string-append " = " (show xs))))

(: inc (-> Integer Integer))
(define (inc n) (+ n 1))

;; A stream plus its own tail, used by the Fibonacci recurrence.
(: tails-plus (-> (Stream Integer) (Stream Integer)))
(define (tails-plus fs) (zip-plus fs (stream-tail fs)))

;; Add two streams element-wise (a lazy `zipWith (+)`): the recursive
;; call is deferred, so it consumes only the prefix that is demanded.
(: zip-plus (-> (Stream Integer) (-> (Stream Integer) (Stream Integer))))
(define (zip-plus xs ys)
  (match xs
    [(SNil) SNil]
    [(SCons x xt)
     (match ys
       [(SNil) SNil]
       [(SCons y yt)
        (SCons (+ x y) (delay (zip-plus (force xt) (force yt))))])]))
