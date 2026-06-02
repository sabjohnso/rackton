#lang rackton

;; Regression: a local binding whose name collides with a prelude class
;; method must shadow the method, not crash inference.
;;
;; The prelude's `Storable` class declares methods `peek` and `poke`.
;; An algebraic-effect operation (or any define) named `peek` shares
;; that name; a call site `(peek)` then made infer-expr treat it as the
;; Storable method, instantiate the LOCAL scheme (which doesn't bind
;; Storable's class param `a`), and crash with
;;   hash-ref: no value found for key 'a
;; on a clean compile.  The local binding must win.

(require "../unit.rkt")

;; `peek` and `poke` both collide with Storable's methods.
(define-effect Counter
  (peek -> Integer)
  (poke -> Unit))

(: run-counter (-> Integer (-> (-> Integer) Integer)))
(define (run-counter s0 prog)
  (handle (prog)
    [peek () k -> (k s0)]
    [poke () k -> (k Unit)]
    [return v -> v]))

(: prog (-> Integer))
(define (prog) (+ (peek) (peek)))

(: result Integer)
(define result (run-counter 21 prog))

;; A plain define shadowing a method name resolves to the local binding.
(: poke-shadow Integer)
(define poke-shadow 7)

(: suite (List Test))
(define suite
  (list
   (it "effect op named like Storable.peek shadows the method"
       (check-equal? result 42))
   (it "a plain binding named like a method shadows it"
       (check-equal? poke-shadow 7))))

(: _ran Unit)
(define _ran (run-io (run-suite "method-name shadowing" suite)))
