#lang rackton

;; Tests for the Category laws (prelude `#:laws`), both ways:
;;
;;  - INTENSIONAL, via a test-local FREE category whose arrows ARE data
;;    (a sequence of generator labels) and so have decidable `Eq`.  The
;;    laws are checked by comparing whole arrows with `==`.  (Its `comp`
;;    is list append, so these confirm the law statements run and that the
;;    instance is lawful, rather than catching a clever `comp` bug.)
;;
;;  - EXTENSIONAL, on the prelude's `(->)` category, whose arrows have no
;;    decidable equality: compose concrete functions and compare their
;;    OUTPUTS on sampled inputs.  This is the one that would catch a real
;;    composition bug on the instance people actually use.

(require "../unit.rkt")

;; ----- a free category: an arrow is a path of generator labels --------
;; Phantom object parameters `a b`; the runtime payload is just the path.
;; `ident` is the empty path; `comp f g` runs g then f, so its path is
;; g's labels followed by f's.

(data (FreeCat a b) (MkFreeCat (List String)))

(instance (Category FreeCat)
  (define ident (MkFreeCat Nil))
  (define (comp f g)
    (match f
      [(MkFreeCat fs)
       (match g [(MkFreeCat gs) (MkFreeCat (append gs fs))])])))

(instance (Eq (FreeCat a b))
  (define (== x y)
    (match x [(MkFreeCat xs) (match y [(MkFreeCat ys) (== xs ys)])])))

(instance (Show (FreeCat a b))
  (define (show x) (match x [(MkFreeCat xs) (mappend "FreeCat" (show xs))])))

(: gen-cat (Gen (FreeCat Integer Integer)))
(define gen-cat (fmap (lambda (xs) (MkFreeCat xs)) (gen-list gen-string)))

;; ----- a deliberately UNLAWFUL category (teeth check) -----------------
;; Same free representation, but `ident` is NOT the empty path, so it is
;; not a unit of `comp`: `(comp ident f)` appends "BAD" and never equals
;; `f`.  The identity laws must catch it.  (`comp` is honest append, so it
;; stays associative — the violation is specifically the unit law.)

(data (BadCat a b) (MkBadCat (List String)))

(instance (Category BadCat)
  (define ident (MkBadCat (Cons "BAD" Nil)))
  (define (comp f g)
    (match f
      [(MkBadCat fs)
       (match g [(MkBadCat gs) (MkBadCat (append gs fs))])])))

(instance (Eq (BadCat a b))
  (define (== x y)
    (match x [(MkBadCat xs) (match y [(MkBadCat ys) (== xs ys)])])))

(instance (Show (BadCat a b))
  (define (show x) (match x [(MkBadCat xs) (mappend "BadCat" (show xs))])))

(: gen-bad (Gen (BadCat Integer Integer)))
(define gen-bad (fmap (lambda (xs) (MkBadCat xs)) (gen-list gen-string)))

;; The left-identity property RUN over BadCat must FAIL — `run-property`
;; returns `PropFailed`.  The test passes by detecting that failure.
(: bad-caught? Boolean)
(define bad-caught?
  (match (run-property 50 12345
                       (for-all gen-bad (lambda (f) (== (comp ident f) f))))
    [(PropFailed _ _) #t]
    [(PropPassed _)   #f]))

;; ----- concrete (->) arrows for the extensional checks ----------------

(: f1 (-> Integer Integer)) (define f1 (lambda (n) (+ n 1)))
(: g1 (-> Integer Integer)) (define g1 (lambda (n) (* n 2)))
(: h1 (-> Integer Integer)) (define h1 (lambda (n) (- n 3)))

(: gi (Gen Integer))
(define gi (int-range -50 50))

;; ----- suite ----------------------------------------------------------

(: suite Test)
(define suite
  (describe "Category laws"
    ;; intensional — compare whole arrows
    (it-prop "left identity (free, intensional)"
      (for-all gen-cat (lambda (f) (== (comp ident f) f))))
    (it-prop "right identity (free, intensional)"
      (for-all gen-cat (lambda (f) (== (comp f ident) f))))
    (it-prop "associativity (free, intensional)"
      (for-all (gen-pair gen-cat (gen-pair gen-cat gen-cat))
        (lambda (t)
          (match t
            [(Pair f (Pair g h))
             (== (comp (comp f g) h) (comp f (comp g h)))]))))
    ;; extensional — compare outputs of composed (->) arrows
    (it-prop "left identity (-> extensional)"
      (for-all gi (lambda (x) (== ((comp ident f1) x) (f1 x)))))
    (it-prop "right identity (-> extensional)"
      (for-all gi (lambda (x) (== ((comp f1 ident) x) (f1 x)))))
    (it-prop "associativity (-> extensional)"
      (for-all gi
        (lambda (x)
          (== ((comp (comp f1 g1) h1) x)
              ((comp f1 (comp g1 h1)) x)))))
    ;; teeth: an unlawful Category (ident not a unit) must be caught
    (it "an unlawful Category is caught by the identity law"
      (check-true bad-caught?))))

(: main Unit)
(define main (run-io (run-suite-tree suite)))
