#lang rackton

;; sat.rkt — a DPLL SAT solver in Rackton, favoring simplicity.
;;
;; SAT is the satisfiability problem: given a Boolean formula in
;; conjunctive normal form (CNF) — an AND of clauses, each clause an OR
;; of literals — decide whether some assignment of true/false to the
;; variables makes the whole formula true, and if so produce one.
;;
;; The solver is DPLL (Davis–Putnam–Logemann–Loveland) reduced to its
;; two essential moves:
;;
;;   1. Unit propagation: a clause with a single literal forces that
;;      literal's value, which simplifies the rest of the formula.
;;   2. Backtracking search: with no unit clause left, pick a literal,
;;      try it true, and on failure try it false.
;;
;; Pure-literal elimination and clause learning are intentionally left
;; out — unit propagation over a recursively simplified formula is the
;; smallest design that is still a recognizable DPLL solver.
;;
;; Run it with `racket examples/sat.rkt`.

(require rackton/data/map
         rackton/data/list
         rackton/data/maybe)

;; ===== Representation ==============================================

;; A literal is a variable, possibly negated: `(Lit 3 #t)` is x3 and
;; `(Lit 3 #f)` is ¬x3.  Deriving Eq lets `simplify` test a literal for
;; membership and deletion with the prelude's `==`.
(data Lit (Lit Integer Boolean) #:deriving Eq Show)

(define-alias Clause     (List Lit))        ; a disjunction of literals
(define-alias Formula    (List Clause))     ; a conjunction of clauses (CNF)
(define-alias Assignment (Map Integer Boolean))

;; Flip a literal's polarity.
(: neg (-> Lit Lit))
(define (neg l) (match l [(Lit v p) (Lit v (not p))]))

;; Record a literal's forced value in the assignment.
(: assign (-> Lit (-> Assignment Assignment)))
(define (assign l asn) (match l [(Lit v p) (map-insert v p asn)]))

;; ===== Simplification ==============================================

;; Assert that `l` is true and simplify the formula accordingly:
;;   - a clause containing `l` is satisfied, so it is dropped entirely;
;;   - a clause containing `(neg l)` keeps only its other literals.
;; A clause that loses its last literal this way becomes the empty
;; clause — an unsatisfiable conjunct that `dpll` reads as a conflict.
(: simplify (-> Lit (-> Formula Formula)))
(define (simplify l clauses)
  (map-maybe
    (lambda (c)
      (if (elem l c)
          None
          (Some (filter (lambda (k) (not (== k (neg l)))) c))))
    clauses))

;; The literal of the first single-literal (unit) clause, if any.
(: unit-literal (-> Formula (Maybe Lit)))
(define (unit-literal clauses)
  (match (find (lambda (c) (== (length c) 1)) clauses)
    [(Some (Cons l (Nil))) (Some l)]
    [_                      None]))

;; ===== DPLL ========================================================

;; Decide `clauses` under the partial assignment `asn`, returning a
;; completing assignment when one exists.
(: dpll (-> Formula (-> Assignment (Maybe Assignment))))
(define (dpll clauses asn)
  (if (any? empty? clauses)
      None                                  ; an empty clause: conflict
      (if (empty? clauses)
          (Some asn)                        ; nothing left to satisfy: done
          (match (unit-literal clauses)
            ;; A unit clause forces its literal — propagate it.
            [(Some l) (dpll (simplify l clauses) (assign l asn))]
            ;; No unit clause: branch on the first clause's first literal.
            [(None)
             (match clauses
               [(Cons (Cons l _) _)
                (match (dpll (simplify l clauses) (assign l asn))
                  [(Some a) (Some a)]
                  [(None)
                   (let ([nl (neg l)])
                     (dpll (simplify nl clauses) (assign nl asn)))])]
               ;; Unreachable: clauses is non-empty and has no empty
               ;; clause, so its first clause has a first literal.
               [_ None])]))))

;; Solve a formula from the empty assignment.
(: solve (-> Formula (Maybe Assignment)))
(define (solve clauses) (dpll clauses empty-map))

;; ===== Pretty-printing =============================================

(: lit->string (-> Lit String))
(define (lit->string l)
  (match l
    [(Lit v p)
     (string-append (if p "" "¬")
       (string-append "x" (integer->string v)))]))

(: clause->string (-> Clause String))
(define (clause->string c)
  (string-append "("
    (string-append (string-join " ∨ " (fmap lit->string c)) ")")))

(: formula->string (-> Formula String))
(define (formula->string f)
  (string-join " ∧ " (fmap clause->string f)))

(: assignment->string (-> Assignment String))
(define (assignment->string asn)
  (string-join ", "
    (fmap (lambda (kv)
            (match kv
              [(Pair v b)
               (string-append "x"
                 (string-append (integer->string v)
                   (string-append "=" (if b "T" "F"))))]))
          (map-to-list asn))))

;; ===== Demo ========================================================

;; Build a literal more concisely: `(pos 1)` is x1, `(nnot 1)` is ¬x1.
(: pos  (-> Integer Lit))
(define (pos v) (Lit v #t))
(: nnot (-> Integer Lit))
(define (nnot v) (Lit v #f))

;; Satisfiable: (x1 ∨ x2) ∧ (¬x1 ∨ x3) ∧ (¬x2 ∨ ¬x3)
(: sat-formula Formula)
(define sat-formula
  (Cons (Cons (pos 1)  (Cons (pos 2)  Nil))
  (Cons (Cons (nnot 1) (Cons (pos 3)  Nil))
  (Cons (Cons (nnot 2) (Cons (nnot 3) Nil))
  Nil))))

;; Unsatisfiable: (x1) ∧ (¬x1)
(: unsat-unit Formula)
(define unsat-unit
  (Cons (Cons (pos 1)  Nil)
  (Cons (Cons (nnot 1) Nil)
  Nil)))

;; Unsatisfiable: every clause over two variables at once —
;; (x1 ∨ x2) ∧ (x1 ∨ ¬x2) ∧ (¬x1 ∨ x2) ∧ (¬x1 ∨ ¬x2)
(: unsat-all Formula)
(define unsat-all
  (Cons (Cons (pos 1)  (Cons (pos 2)  Nil))
  (Cons (Cons (pos 1)  (Cons (nnot 2) Nil))
  (Cons (Cons (nnot 1) (Cons (pos 2)  Nil))
  (Cons (Cons (nnot 1) (Cons (nnot 2) Nil))
  Nil)))))

(: report (-> String (-> Formula (IO Unit))))
(define (report label f)
  (do [_ <- (println (string-append label (string-append ":  " (formula->string f))))]
    (match (solve f)
      [(Some asn) (println (string-append "    SAT   " (assignment->string asn)))]
      [(None)     (println "    UNSAT")])))

(: main Unit)
(define main
  (run-io
    (do [_ <- (println "DPLL SAT solver:")]
        [_ <- (println "")]
        [_ <- (report "formula 1" sat-formula)]
        [_ <- (report "formula 2" unsat-unit)]
        [_ <- (report "formula 3" unsat-all)]
        [_ <- (println "")]
      (println "done."))))
