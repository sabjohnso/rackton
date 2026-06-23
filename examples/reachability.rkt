#lang rackton

;; reachability.rkt — graph reachability as a monotone least fixpoint,
;; using rackton/mono.
;;
;; Reachability is the transitive closure of an edge relation: the least
;; relation R that contains every edge and is closed under composition with
;; the edges.  As a fixpoint equation,
;;
;;     R  =  edges  ⊔  (edges ∘ R)
;;
;; the right-hand side is MONOTONE in R, so `mono-fix` runs it from ⊥ = ∅ up
;; to its least fixed point — exactly the one-rule Datalog program
;;
;;     reach(X,Z) :- edge(X,Z).
;;     reach(X,Z) :- edge(X,Y), reach(Y,Z).
;;
;; The step is assembled from rackton/mono's CLOSED combinator set
;; (`mono-const`, `mono-join`) plus a single trusted leaf for relational
;; composition, so it is monotone by construction — there is no way to write
;; a non-monotone step and hand it to `mono-fix`.
;;
;; Run it with `racket examples/reachability.rkt`.

(require rackton/mono
         rackton/data/set
         rackton/data/list)

;; ===== a relation over named nodes, ordered by inclusion ============
;; A foreign Set carries no dispatch tag, so the lattice carrier is a
;; nominal newtype the program owns: ⊑ is ⊆, ⊔ is ∪, ⊥ is ∅.

(data Rel (MkRel (Set (Pair String String))))
(: edges-of (-> Rel (Set (Pair String String))))
(define (edges-of r) (match r [(MkRel s) s]))
(: rel (-> (List (Pair String String)) Rel))
(define (rel xs) (MkRel (set-from-list xs)))

(instance (Eq Rel)
  (define (== a b)
    (if (set-subset? (edges-of a) (edges-of b))
        (set-subset? (edges-of b) (edges-of a)) #f)))
(instance (Poset Rel)
  (define (leq a b) (set-subset? (edges-of a) (edges-of b))))
(instance (JoinSemilattice Rel)
  (define (lub a b) (MkRel (set-union (edges-of a) (edges-of b)))))
(instance (BoundedJoinSemilattice Rel)
  (define bot (MkRel empty-set)))

;; ===== the trusted leaf: relational composition ====================
;; compose(base, R) = {(x,z) | (x,y) ∈ base, (y,z) ∈ R} — monotone in R.
;; Plain Rackton; it becomes a `Mono` only through `unsafe-mono`.

(: compose-pairs (-> (List (Pair String String))
                     (List (Pair String String))
                     (List (Pair String String))))
(define (compose-pairs bs rs)
  (foldr (lambda (b acc)
           (match b [(Pair x y)
             (foldr (lambda (e acc2)
                      (match e [(Pair y2 z)
                        (if (== y y2) (Cons (Pair x z) acc2) acc2)]))
                    acc rs)]))
         Nil bs))
(: compose-with (-> Rel Rel Rel))
(define (compose-with base r)
  (MkRel (set-from-list (compose-pairs (set-to-list (edges-of base))
                                       (set-to-list (edges-of r))))))

;; ===== the graph and its reachability ==============================
;;   a → b → c → d ,  b → e

(: graph Rel)
(define graph
  (rel (list (Pair "a" "b") (Pair "b" "c") (Pair "c" "d") (Pair "b" "e"))))

(: reach Rel)
(define reach
  (mono-fix (mono-join (mono-const graph)
                       (unsafe-mono (compose-with graph)))))

;; does node x reach node z?
(: reaches? (-> String String Boolean))
(define (reaches? x z) (set-member? (Pair x z) (edges-of reach)))

;; ===== output ======================================================

(: pair->line (-> (Pair String String) String))
(define (pair->line p)
  (match p [(Pair x z) (string-append "  " (string-append x (string-append " -> " z)))]))

(: relation->block (-> Rel String))
(define (relation->block r)
  (foldr (lambda (p acc) (string-append (pair->line p) (string-append "\n" acc)))
         "" (sort (set-to-list (edges-of r)))))

(: query-line (-> String String String))
(define (query-line x z)
  (string-append "  " (string-append x (string-append " reaches " (string-append z
    (string-append "?  " (if (reaches? x z) "yes" "no")))))))

(: main Unit)
(define main
  (run-io
   (do [_ <- (println "Graph reachability via a monotone least fixpoint (rackton/mono)")]
       [_ <- (println "")]
       [_ <- (println "edges:")]
       [_ <- (print (relation->block graph))]
       [_ <- (println "")]
       [_ <- (println (string-append "reachable (transitive closure), "
               (string-append (integer->string (set-size (edges-of reach))) " pairs:")))]
       [_ <- (print (relation->block reach))]
       [_ <- (println "")]
       [_ <- (println "queries:")]
       [_ <- (println (query-line "a" "d"))]
       [_ <- (println (query-line "a" "a"))]
       [_ <- (println (query-line "e" "d"))]
     (pure Unit))))
