#lang rackton

;; rackton/data/nestream — a nonempty lazy stream.  Where `Stream` from
;; rackton/data/lazy may be empty (so `stream-head` returns `(Maybe a)`),
;; an `NEStream` is guaranteed to carry at least one element, so its head
;; is total.  The tail is an ordinary lazy `Stream`, so a nonempty stream
;; may still be infinite — consumers force only the prefix they take.
;;
;; This is the nonempty analog of the list — head + (possibly empty)
;; tail — exactly as rackton/data/list/nonempty's `NonEmpty` is for
;; `List`.  Unlike that module, `NEStream` is given the CARTESIAN
;; Applicative/Monad (matching `Stream`'s list-monad) rather than the
;; zippy `FunctorApply`, so it is BOTH a monad and a comonad: the
;; finite-nonempty counterpart to rackton/data/istream's comonad-only
;; infinite stream.

(require rackton/data/lazy
         rackton/control/comonad)

(provide (all-defined-out))

(data (NEStream a) (NECons a (Stream a)))

;; Construct from a head and a (possibly empty) lazy tail.
(: nestream (-> a (-> (Stream a) (NEStream a))))
(define (nestream h t) (NECons h t))

;; The head — always present, so this is total (no `Maybe`).
(: nestream-head (-> (NEStream a) a))
(define (nestream-head ne) (match ne [(NECons h _) h]))

;; Everything after the head, as a `Stream` (which may be empty).
(: nestream-tail (-> (NEStream a) (Stream a)))
(define (nestream-tail ne) (match ne [(NECons _ t) t]))

;; The first `n` elements as a strict `List`; forces only what it takes.
(: nestream-take (-> Integer (-> (NEStream a) (List a))))
(define (nestream-take n ne)
  (match ne [(NECons h t)
             (if (<= n 0)
                 Nil
                 (Cons h (stream-take (- n 1) t)))]))

;; Prepend an element; the result is still nonempty.
(: nestream-cons (-> a (-> (NEStream a) (NEStream a))))
(define (nestream-cons x ne)
  (match ne [(NECons h t) (NECons x (SCons h (delay t)))]))

;; Map over every element, keeping the tail deferred and nonemptiness
;; intact.
(: nestream-map (-> (-> a b) (-> (NEStream a) (NEStream b))))
(define (nestream-map f ne)
  (match ne [(NECons h t) (NECons (f h) (stream-map f t))]))

;; Append a `Stream` after a nonempty stream; the head keeps it nonempty.
(: nestream-append (-> (NEStream a) (-> (Stream a) (NEStream a))))
(define (nestream-append ne s)
  (match ne [(NECons h t) (NECons h (stream-append t s))]))

;; Append a nonempty front onto a deferred `Stream` rest — the lazy-right
;; variant the Monad's flatmap needs (cf. `stream-append-lazy`).
(: nestream-append-stream (-> (NEStream a) (-> (Lazy (Stream a)) (NEStream a))))
(define (nestream-append-stream ne rest)
  (match ne [(NECons h t) (NECons h (stream-append-lazy t rest))]))

;; Map `f` over the stream and concatenate the resulting nonempty
;; streams; the head of the first keeps the whole result nonempty.  The
;; tail's results are concatenated lazily through `stream-flatmap`.  Both
;; the Monad and the Applicative instances delegate here (like `Stream`'s
;; `stream-flatmap`), so neither has to call the other's class method.
(: nestream-flatmap (-> (-> a (NEStream b)) (-> (NEStream a) (NEStream b))))
(define (nestream-flatmap f ne)
  (match ne
    [(NECons h t)
     (nestream-append-stream
      (f h)
      (delay (stream-flatmap (lambda (x) (nestream->stream (f x))) t)))]))

;; Forget the guarantee: a plain `Stream` with the same elements.
(: nestream->stream (-> (NEStream a) (Stream a)))
(define (nestream->stream ne)
  (match ne [(NECons h t) (SCons h (delay t))]))

;; Recover the guarantee, if it holds — the partial direction.
(: stream->nestream (-> (Stream a) (Maybe (NEStream a))))
(define (stream->nestream s)
  (match s
    [(SNil)       None]
    [(SCons h lz) (Some (NECons h (force lz)))]))

;; --- class instances -------------------------------------------------
;;
;; NEStream is a Functor, the CARTESIAN Applicative/Monad (every function
;; meets every argument, like `(Applicative Stream)`), and the canonical
;; nonempty Comonad (`extract` = head, `duplicate` = the nonempty stream
;; of nonempty suffixes).  The zippy `FunctorApply`/`ComonadApply` that
;; `NonEmpty` uses are deliberately omitted: a type gets one `apply` and
;; the cartesian one is what makes NEStream a `Monad` coherent with
;; `Stream`; `Comonad` needs no `apply`.
;;
;; Methods are written out in full because an imported class's default
;; bodies do not cross the module boundary (the scheme-codec sidecar
;; drops them), so an instance of an imported class must be complete.

(instance (Functor NEStream)
  (define (fmap f ne) (match ne [(NECons h t) (NECons (f h) (stream-map f t))])))

(instance (Applicative NEStream)
  (define (pure x) (NECons x SNil))
  (define (fapply sf sx)
    (nestream-flatmap (lambda (f) (nestream-map f sx)) sf)))

(instance (Monad NEStream)
  (define (flatmap f ne) (nestream-flatmap f ne)))

(instance (Comonad NEStream)
  (define (extract ne) (match ne [(NECons h _) h]))
  ;; duplicate = the nonempty stream of nonempty suffixes: the whole
  ;; thing, then the suffix starting at each tail element.
  (define (duplicate ne)
    (match ne
      [(NECons h t)
       (letrec ([suffixes (lambda (s)
                            (match s
                              [(SNil)       SNil]
                              [(SCons x lz) (SCons (NECons x (force lz))
                                                   (delay (suffixes (force lz))))]))])
         (NECons (NECons h t) (suffixes t)))]))
  (define (extend f w) (nestream-map f (duplicate w))))
