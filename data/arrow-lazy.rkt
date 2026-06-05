#lang rackton

;; rackton/data/arrow-lazy — a lazy-function arrow whose ArrowLoop can tie
;; a value-recursion knot.
;;
;; The prelude's plain function arrow `(->)` is strict, so it has no
;; `ArrowLoop`: feeding an output back as an input forces the feedback
;; before it is produced and diverges.  `LFun` is a function on *lazy*
;; values paired with a *lazy-component* product `LPair`, so the feedback
;; can be threaded unforced.  `arrow-loop` ties the knot with `letrec` +
;; `delay`; the product keeps both halves lazy so projecting one never
;; forces the other.
;;
;; Caveat for `proc rec`: `arr` lifts a *strict* function, so it forces its
;; argument.  A productive loop must therefore consume the recursive
;; feedback through a NON-`arr` primitive that defers it — e.g. `lcons`,
;; which drops the feedback into a `Stream`'s lazy tail.  Routing the
;; feedback through `arr` alone re-forces it and diverges.  See the
;; `lazy-arrow-test` for a runnable example.

(require rackton/data/lazy)

(provide (all-defined-out)
         ;; re-export the lazy vocabulary the runnable examples lean on
         (data-out Stream)
         force stream-head stream-take stream-tail)

;; ----- the lazy arrow and its tensors ------------------------------

;; A lazy-function arrow: a function on lazy values.  Operating on
;; `Lazy a` (rather than `a`) is what lets composition thread a value
;; through without forcing it.
(data (LFun a b)
  (LFun (-> (Lazy a) (Lazy b))))

;; The lazy product: both components are deferred, so `prod-fst` can be
;; taken without forcing the `prod-snd` half (the loop's feedback channel).
(data (LPair a b)
  (LPair (Lazy a) (Lazy b)))

;; The lazy coproduct, dual to `LPair`.
(data (LEither a b)
  (LLeft  (Lazy a))
  (LRight (Lazy b)))

;; ----- Prod / Coprod (product / coproduct tensors) -----------------

(instance (Prod LPair)
  ;; `mk-prod` receives already-evaluated arguments (the language is
  ;; strict); the loop's laziness comes from `arrow-loop`'s own `delay`s,
  ;; not from here.
  (define (mk-prod a b) (LPair (delay a) (delay b)))
  (define (prod-fst p) (match p [(LPair la _) (force la)]))
  (define (prod-snd p) (match p [(LPair _ lb) (force lb)])))

(instance (Coprod LEither)
  (define (inj-left  a) (LLeft  (delay a)))
  (define (inj-right b) (LRight (delay b)))
  (define (co-elim f g s)
    (match s
      [(LLeft  la) (f (force la))]
      [(LRight lb) (g (force lb))])))

;; ----- Category / Arrow --------------------------------------------

(instance (Category LFun)
  (define ident (LFun (lambda (la) la)))
  (define (comp x y)
    (match x
      [(LFun f) (match y [(LFun g) (LFun (lambda (la) (f (g la))))])])))

(instance (Arrow LFun LPair)
  ;; lift a strict function, deferring its application until the output is
  ;; forced.
  (define (arr h) (LFun (lambda (la) (delay (h (force la))))))
  ;; on-first/on-second/split/fanout force only the product *spine*
  ;; (inside the output `delay`); the untouched component passes through
  ;; still-lazy.  on-second/split/fanout are defined explicitly here to
  ;; OVERRIDE the Arrow class defaults: those route through `arr swap`,
  ;; and `swap` (built from `prod-fst`/`prod-snd`) forces BOTH components,
  ;; which would break the laziness ArrowLoop depends on.  Do not delete
  ;; these in favor of the defaults.
  (define (on-first x)
    (match x
      [(LFun f)
       (LFun (lambda (lp)
               (delay (match (force lp)
                        [(LPair la lc) (LPair (f la) lc)]))))]))
  (define (on-second x)
    (match x
      [(LFun g)
       (LFun (lambda (lp)
               (delay (match (force lp)
                        [(LPair lc la) (LPair lc (g la))]))))]))
  (define (split x y)
    (match x
      [(LFun f)
       (match y
         [(LFun g)
          (LFun (lambda (lp)
                  (delay (match (force lp)
                           [(LPair la lc) (LPair (f la) (g lc))]))))])]))
  (define (fanout x y)
    (match x
      [(LFun f)
       (match y
         [(LFun g)
          (LFun (lambda (la) (delay (LPair (f la) (g la)))))])])))

;; ----- ArrowChoice -------------------------------------------------
;; Choosing a branch must inspect which injection arrived, so these force
;; the coproduct spine (never the component).  Like Arrow above, on-right/
;; fork/fanin are defined explicitly to OVERRIDE the class defaults, whose
;; `mirror`/`untag` (via `co-elim` + `inj-*`) would force the components.

(instance (ArrowChoice LFun LPair LEither)
  (define (on-left x)
    (match x
      [(LFun f)
       (LFun (lambda (le)
               (delay (match (force le)
                        [(LLeft  la) (LLeft (f la))]
                        [(LRight lx) (LRight lx)]))))]))
  (define (on-right x)
    (match x
      [(LFun g)
       (LFun (lambda (le)
               (delay (match (force le)
                        [(LLeft  lx) (LLeft lx)]
                        [(LRight la) (LRight (g la))]))))]))
  (define (fork x y)
    (match x
      [(LFun f)
       (match y
         [(LFun g)
          (LFun (lambda (le)
                  (delay (match (force le)
                           [(LLeft  la) (LLeft  (f la))]
                           [(LRight lb) (LRight (g lb))]))))])]))
  (define (fanin x y)
    (match x
      [(LFun f)
       (match y
         [(LFun g)
          (LFun (lambda (le)
                  (match (force le)
                    [(LLeft  la) (f la)]
                    [(LRight lb) (g lb)])))])])))

;; ----- ArrowApply --------------------------------------------------

(instance (ArrowApply LFun LPair)
  (define arrow-app
    (LFun (lambda (lp)
            (match (force lp)
              [(LPair lf la)
               (match (force lf) [(LFun f) (f la)])])))))

;; ----- ArrowLoop (the payoff) --------------------------------------
;; Tie the c-channel back through a lazy `LPair`: `dc` is the feedback,
;; produced lazily from `res`; `input` carries it back in unforced.  The
;; `letrec` knot holds because `(f input)` returns a `Lazy` without forcing
;; `input`, so none of the three bindings forces another at definition
;; time.
(instance (ArrowLoop LFun LPair)
  (define (arrow-loop x)
    (match x
      [(LFun f)
       (LFun (lambda (la)
               (letrec ([input (delay (LPair la dc))]
                        [res   (f input)]
                        [dc    (delay (match (force res) [(LPair _ lc) (force lc)]))])
                 (delay (match (force res) [(LPair lb _) (force lb)])))))])))

;; ----- driver + a non-`arr` lazy primitive -------------------------

;; Run a lazy arrow on a strict input and force the result.
(: run-lfun (-> (LFun a b) (-> a b)))
(define (run-lfun lf x)
  (match lf [(LFun f) (force (f (delay x)))]))

;; `lcons x` is the arrow that prepends `x` to a stream, dropping its
;; (lazy) input into the new `SCons`'s deferred tail.  Because it never
;; forces its argument it can sit in a `proc rec` feedback path and stay
;; productive — unlike an `arr`-lifted function.
(: lcons (-> a (LFun (Stream a) (Stream a))))
(define (lcons x)
  (LFun (lambda (ls) (delay (SCons x ls)))))
