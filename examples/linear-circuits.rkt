#lang rackton

;; linear-circuits.rkt — wiring diagrams with rackton/linear.
;;
;; A morphism in this tower is a "circuit" over wires; `par` runs two
;; circuits side by side (f *** g), `braid` crosses two wires, and `comp`
;; chains circuits.  A LINEAR arrow (`Lin`) has only these: it can route,
;; permute, and transform wires, but it can never COPY a wire (no `dup`) or
;; DROP one (no `discard`).  A CARTESIAN arrow (`Fn`) adds exactly those two
;; capabilities — so fan-out and projection become expressible.
;;
;; The point: `dup` / `discard` on a `Lin` is a TYPE ERROR (see
;; tests/linear-no-copy-test.rkt); linearity is enforced by which protocols
;; the arrow implements, not by convention.
;;
;; Run it with `racket examples/linear-circuits.rkt`.

(require rackton/linear)

;; ===== gates, as both linear and cartesian arrows ==================
(: inc (Lin Integer Integer)) (define inc (lin (lambda (n) (+ n 1))))
(: dbl (Lin Integer Integer)) (define dbl (lin (lambda (n) (* n 2))))
(: inc-fn (Fn Integer Integer)) (define inc-fn (fn (lambda (n) (+ n 1))))
(: dbl-fn (Fn Integer Integer)) (define dbl-fn (fn (lambda (n) (* n 2))))

;; ===== a LINEAR circuit: cross the wires, then transform ===========
;; braid >>> (inc *** dbl)  — uses only routing + transform, no copy/drop.
(: cross-transform (Lin (Ten Lin Integer Integer) (Ten Lin Integer Integer)))
(define cross-transform (comp (par inc dbl) braid))

;; ===== CARTESIAN circuits: fan-out (needs dup) and drop (needs discard)
;; fanout f g = dup >>> (f *** g):  feed one input to two gates.
(: fanout (Fn Integer (Ten Fn Integer Integer)))
(define fanout (comp (par inc-fn dbl-fn) dup))

;; keep the first wire, discard the second.
(: keep-first (Fn (Ten Fn Integer Integer) (Ten Fn Integer Unit)))
(define keep-first (par ident discard))

;; ===== formatting ==================================================
(: pair-str (-> Integer Integer String))
(define (pair-str a b)
  (string-append "(" (string-append (integer->string a)
                                    (string-append ", " (string-append (integer->string b) ")")))))

(: lin-out (-> (Ten Lin Integer Integer) String))
(define (lin-out t) (match t [(LinTen a b) (pair-str a b)]))
(: fn-out (-> (Ten Fn Integer Integer) String))
(define (fn-out t) (match t [(FnTen a b) (pair-str a b)]))
(: drop-out (-> (Ten Fn Integer Unit) String))
(define (drop-out t)
  (match t [(FnTen a u) (string-append "(" (string-append (integer->string a) ", ())"))]))

;; ===== run =========================================================
(: main (IO Unit))
(define main (do [_ <- (println "Wiring diagrams with rackton/linear")]
               [_ <- (println "")]
               [_ <- (println "LINEAR (route / permute / transform — never copy or drop):")]
               [_ <- (println (string-append "  braid >>> (inc *** dbl)  on (3, 5)  =  "
                                             (lin-out (at cross-transform (LinTen 3 5)))))]
               [_ <- (println "")]
               [_ <- (println "CARTESIAN (additionally copy via dup, drop via discard):")]
               [_ <- (println (string-append "  fanout (inc &&& dbl)     on 5       =  "
                                             (fn-out (at-fn fanout 5))))]
               [_ <- (println (string-append "  keep-first (drop 2nd)    on (7, 9)  =  "
                                             (drop-out (at-fn keep-first (FnTen 7 9)))))]
               [_ <- (println "")]
               [_ <- (println "fanout and keep-first need dup / discard, which Lin")]
               [_ <- (println "does not provide — they are type errors on a linear wire.")]
               (pure Unit)))
