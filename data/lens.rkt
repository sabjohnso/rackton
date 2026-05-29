#lang rackton

;; rackton/data/lens — composable optics (Lens, Prism, Traversal).
;;
;; Moved out of the auto-prelude (Phase 2 slim): `(require
;; rackton/data/lens)` to use them, and likewise in any module that
;; uses `#:deriving Lens` / `#:deriving Prism` (the generated code
;; refers to `MkLens` / `MkPrism`).
;;
;; Simple (getter, setter) pair encoding.  Each `(Lens s a)` packs a
;; function to extract an `a` from `s` and a function to inject a new
;; `a` back into an existing `s`, producing a new `s`.

(provide (all-defined-out))

(data (Lens s a)
  (MkLens (-> s a) (-> s (-> a s))))

(: view (-> (Lens s a) (-> s a)))
(define (view l s)
  (match l [(MkLens g _) (g s)]))

(: set (-> (Lens s a) (-> a (-> s s))))
(define (set l v s)
  (match l [(MkLens _ ps) ((ps s) v)]))

(: over (-> (Lens s a) (-> (-> a a) (-> s s))))
(define (over l f s)
  (match l [(MkLens g ps) ((ps s) (f (g s)))]))

(: lens-compose
   (-> (Lens s a) (-> (Lens a b) (Lens s b))))
(define (lens-compose outer inner)
  (MkLens
   (lambda (s) (view inner (view outer s)))
   (lambda (s b)
     (set outer (set inner b (view outer s)) s))))

;; --- Prisms -----------------------------------
;;
;; A prism focuses on a single sum-type constructor.  preview returns
;; Some when the target ctor matches, None otherwise.  review always
;; succeeds — it builds the target ctor.

(data (Prism s a)
  (MkPrism (-> s (Maybe a)) (-> a s)))

(: preview (-> (Prism s a) (-> s (Maybe a))))
(define (preview p s)
  (match p [(MkPrism extract _) (extract s)]))

(: review  (-> (Prism s a) (-> a s)))
(define (review p a)
  (match p [(MkPrism _ build) (build a)]))

;; --- Traversals -------------------------------
;;
;; A traversal focuses on zero-or-more sub-parts.  to-list-of gathers
;; them; over-of transforms all of them.

(data (Traversal s a)
  (MkTraversal (-> s (List a))
               (-> (-> a a) (-> s s))))

(: to-list-of (-> (Traversal s a) (-> s (List a))))
(define (to-list-of t s)
  (match t [(MkTraversal get-all _) (get-all s)]))

(: over-of    (-> (Traversal s a) (-> (-> a a) (-> s s))))
(define (over-of t f s)
  (match t [(MkTraversal _ modify-all) ((modify-all f) s)]))

;; A built-in traversal that focuses on every element of a List.
(: list-traversal (Traversal (List a) a))
(define list-traversal
  (MkTraversal id (lambda (f) (lambda (xs) (fmap f xs)))))

;; Promote a Lens to a Traversal with a single focus.
(: lens-as-traversal (-> (Lens s a) (Traversal s a)))
(define (lens-as-traversal l)
  (MkTraversal
   (lambda (s) (Cons (view l s) Nil))
   (lambda (f) (lambda (s) (over l f s)))))
