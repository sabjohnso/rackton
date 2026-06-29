#lang rackton

;; rackton/linear — a substructural (Freyd-style) arrow tower.
;;
;; A SEPARATE sibling of the shipped Arrow hierarchy (which stays exactly as
;; it is): the prelude's `Category` is reused, and a monoidal product is
;; added so morphisms can run in parallel.  The crucial choice — validated by
;; the spikes behind FreydCategory.org — is that the tensor is a DATA FAMILY,
;; not a type family or a fundep parameter: a data family is GENERATIVE, so
;; the unifier recovers the category index from a tensor type by ordinary
;; structural unification.  That is what makes the nullary/structural
;; operations (`braid`, and later `dup`/`discard`) writable at all.
;;
;; Increment 1 ships the LINEAR core: `Tensored` (the ⊗ on arrows) and
;; `Symmetric` (braiding), with a concrete linear arrow `Lin`.  Copy/discard
;; (the cartesian capabilities) are deliberately NOT here — their absence is
;; what makes `Lin` linear.  Laws are verified extensionally (an arrow has no
;; decidable equality); see tests/linear-test.rkt.

(provide (data-out Ten)
         (protocol-out Tensored)
         (protocol-out Symmetric)
         (protocol-out Copyable)
         (protocol-out Discardable)
         par-first par-second
         (data-out Lin)
         lin run-lin at
         LinTen
         (data-out Fn)
         fn run-fn at-fn
         FnTen)

;; ===== the monoidal product: a DATA family keyed by the category ======
;; `(Ten cat a b)` is generative — each arrow supplies its own product
;; representation via a `data-instance`, and `(Ten cat a b) ~ (Ten Lin x y)`
;; recovers `cat = Lin` structurally.

(data-family (Ten cat a b))

;; ===== the tower ======================================================

;; A category whose morphisms can be tensored: `par f g` runs f and g on the
;; two components of the product independently (Haskell's `f *** g`).
(protocol (Tensored (cat :: (-> * (-> * *))))
  (:requires (Category cat))
  (: par (-> (cat a b) (-> (cat c d) (cat (Ten cat a c) (Ten cat b d))))))

;; A tensored category with a symmetry: `braid` swaps the two components.
(protocol (Symmetric (cat :: (-> * (-> * *))))
  (:requires (Tensored cat))
  (: braid (cat (Ten cat a b) (Ten cat b a))))

;; Derived combinators, as plain functions (not default methods, so they
;; cross module boundaries): act on one component, leaving the other alone.
(: par-first ((Tensored cat) => (-> (cat a b) (cat (Ten cat a c) (Ten cat b c)))))
(define (par-first f) (par f ident))
(: par-second ((Tensored cat) => (-> (cat a b) (cat (Ten cat c a) (Ten cat c b)))))
(define (par-second f) (par ident f))

;; ===== a concrete LINEAR arrow ========================================
;; `Lin a b` wraps a function.  Its tensor `LinTen` is a nominal pair.

(data (Lin a b) (MkLin (-> a b)))
(: lin (-> (-> a b) (Lin a b)))             ; build a linear arrow
(define (lin f) (MkLin f))
(: run-lin (-> (Lin a b) (-> a b)))
(define (run-lin l) (match l [(MkLin f) f]))
(: at (-> (Lin a b) a b))                   ; apply a linear arrow
(define (at l x) ((run-lin l) x))

(data-instance (Ten Lin a b) (LinTen a b))

(instance (Category Lin)
  (define ident (MkLin (lambda (x) x)))
  (define (comp g f) (MkLin (lambda (x) (at g (at f x))))))

(instance (Tensored Lin)
  (define (par f g)
    (MkLin (lambda (q) (match q [(LinTen a c) (LinTen (at f a) (at g c))])))))

(instance (Symmetric Lin)
  (define braid (MkLin (lambda (q) (match q [(LinTen a b) (LinTen b a)])))))

;; ===== the cartesian capabilities: copy and discard ===================
;; A LINEAR arrow withholds these; a CARTESIAN one provides them.  Their
;; ABSENCE on `Lin` is exactly what makes it linear — there is no `dup` to
;; copy a wire and no `discard` to drop one, so the type system rejects any
;; attempt (see tests/linear-no-copy-test.rkt).

;; Copyable: a comonoid comultiplication — duplicate a wire (the diagonal).
(protocol (Copyable (cat :: (-> * (-> * *))))
  (:requires (Symmetric cat))
  (: dup (cat a (Ten cat a a))))

;; Discardable: a counit — drop a wire to the monoidal unit `Unit`.
(protocol (Discardable (cat :: (-> * (-> * *))))
  (:requires (Symmetric cat))
  (: discard (cat a Unit)))

;; A cartesian arrow has symmetry AND copy AND discard.
(define-constraint (Cartesian cat)
  (Symmetric cat) (Copyable cat) (Discardable cat))

;; ===== a concrete CARTESIAN arrow =====================================
;; `Fn` is the plain function arrow — every capability, including copy and
;; discard.  Same runtime shape as `Lin`; the only difference is which
;; protocols it implements.

(data (Fn a b) (MkFn (-> a b)))
(: fn (-> (-> a b) (Fn a b)))
(define (fn f) (MkFn f))
(: run-fn (-> (Fn a b) (-> a b)))
(define (run-fn h) (match h [(MkFn f) f]))
(: at-fn (-> (Fn a b) a b))
(define (at-fn h x) ((run-fn h) x))

(data-instance (Ten Fn a b) (FnTen a b))

(instance (Category Fn)
  (define ident (MkFn (lambda (x) x)))
  (define (comp g f) (MkFn (lambda (x) (at-fn g (at-fn f x))))))
(instance (Tensored Fn)
  (define (par f g)
    (MkFn (lambda (q) (match q [(FnTen a c) (FnTen (at-fn f a) (at-fn g c))])))))
(instance (Symmetric Fn)
  (define braid (MkFn (lambda (q) (match q [(FnTen a b) (FnTen b a)])))))
(instance (Copyable Fn)
  (define dup (MkFn (lambda (x) (FnTen x x)))))
(instance (Discardable Fn)
  (define discard (MkFn (lambda (x) Unit))))
