#lang racket/base

;; Literal sugar must be shadow-proof for EVERY prelude name it references,
;; not just Cons/Nil: the dotted-pair literal `[a . b]` (→ Pair), map
;; literals `{k v}` (→ empty-map/map-insert), set literals `#{a}`
;; (→ empty-set/set-insert), and the quasiquote splice `,@` (→ append).
;; Each block below SHADOWS the relevant name with a type-INCOMPATIBLE
;; binding and annotates the literal with its prelude type, so the block
;; type-checks only if the sugar resolves to the prelude binding rather
;; than the shadow.  (The whole file fails to compile if any does not.)

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

;; --- Pair: dotted-pair literal (expression + pattern) ---
(rackton
  (data (Foo a) (Pair a))                      ; shadow Pair (unary)
  (: pr (Pair Integer Integer))
  (define pr [10 . 20])                         ; prelude Pair (binary)
  (: fst (-> (Pair Integer Integer) Integer))
  (define (fst p) (match p [[a . b] a]))        ; dotted-pair PATTERN
  (: pair-got Integer)
  (define pair-got (fst pr)))

;; --- Map literal (real runtime check: the literal builds a prelude Map,
;;     NOT a call to the shadowed map-insert) ---
(rackton
  (require (prefix-in m: rackton/data/map))
  (: map-insert (-> a b c Boolean))            ; incompatible shadow
  (define (map-insert k v m) #t)
  (: empty-map Boolean)
  (define empty-map #f)
  (: mp (Map Integer Integer))
  (define mp {1 100 2 200})
  (: map-sz Integer)
  (define map-sz (m:map-size mp)))

;; --- Set literal ---
(rackton
  (require (prefix-in s: rackton/data/set))
  (: set-insert (-> a b Boolean))              ; incompatible shadow
  (define (set-insert x s) #t)
  (: empty-set Boolean)
  (define empty-set #f)
  (: st (Set Integer))
  (define st #{1 2 3})
  (: set-sz Integer)
  (define set-sz (s:set-size st)))

;; --- Quasiquote splice (,@) → append ---
(rackton
  (: append (-> a b Boolean))                  ; incompatible shadow
  (define (append x y) #t)
  (: xs (List Integer))
  (define xs `(1 ,@[2 3] 4))
  (: xs-len Integer)
  (define xs-len (length xs)))

(test-case "dotted-pair literal + pattern under Pair shadowing"
  (check-equal? pair-got 10))

(test-case "map literal builds a real prelude Map under map-insert shadowing"
  (check-equal? map-sz 2))

(test-case "set literal builds a real prelude Set under set-insert shadowing"
  (check-equal? set-sz 3))

(test-case "quasiquote splice under append shadowing"
  (check-equal? xs-len 4))
