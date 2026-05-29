#lang racket/base

;; End-to-end: immutable Map and Set, plus list helpers
;; (concat-map, group-by).

(require rackunit
         "../main.rkt")

(rackton
  (require rackton/data/list)
  ;; Build a Map by chained inserts; immutability means earlier values
  ;; are preserved.
  (: built-map (Map String Integer))
  (define built-map
    (map-insert "alpha" 1
      (map-insert "beta" 2
        (map-insert "gamma" 3 empty-map))))

  (: alpha (Maybe Integer))
  (define alpha (map-lookup "alpha" built-map))

  (: missing (Maybe Integer))
  (define missing (map-lookup "zeta" built-map))

  (: built-size Integer)
  (define built-size (map-size built-map))

  ;; Delete returns a new map.
  (: m-no-beta (Map String Integer))
  (define m-no-beta (map-delete "beta" built-map))

  (: m-no-beta-size Integer)
  (define m-no-beta-size (map-size m-no-beta))

  (: built-map-still-3 Integer)
  (define built-map-still-3 (map-size built-map))   ; immutability check

  ;; Set
  (: digit-set (Set Integer))
  (define digit-set
    (set-insert 1
      (set-insert 2
        (set-insert 3 empty-set))))

  (: has-2 Boolean)
  (define has-2 (set-member? 2 digit-set))

  (: has-99 Boolean)
  (define has-99 (set-member? 99 digit-set))

  ;; Set difference via delete
  (: smaller (Set Integer))
  (define smaller (set-delete 1 digit-set))

  (: smaller-size Integer)
  (define smaller-size (set-size smaller))

  ;; List helpers
  (: cm (List Integer))
  (define cm (concat-map (lambda (n) (Cons n (Cons (* 2 n) Nil)))
                         (Cons 1 (Cons 2 (Cons 3 Nil)))))

  ;; group-by parity (0 / 1)
  (: parity-groups (Map Integer (List Integer)))
  (define parity-groups
    (group-by (lambda (n) (mod n 2))
              (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil)))))))

  (: odd-bucket (Maybe (List Integer)))
  (define odd-bucket (map-lookup 1 parity-groups))

  (: even-bucket (Maybe (List Integer)))
  (define even-bucket (map-lookup 0 parity-groups)))

;; ----- Map -----

(test-case "Map basics"
  (check-equal? alpha           (Some 1))
  (check-equal? missing         None)
  (check-equal? built-size      3))

(test-case "Map delete preserves persistence"
  (check-equal? m-no-beta-size      2)
  (check-equal? built-map-still-3   3))

;; ----- Set -----

(test-case "Set membership"
  (check-true  has-2)
  (check-false has-99))

(test-case "Set delete shrinks"
  (check-equal? smaller-size 2))

;; ----- list helpers -----

(test-case "concat-map"
  ;; Each element n yields [n, 2n]; flatten.
  (check-equal? cm
                (Cons 1 (Cons 2 (Cons 2 (Cons 4 (Cons 3 (Cons 6 Nil))))))))

(test-case "group-by parity"
  ;; (group-by ...) doesn't promise order within a bucket; check membership.
  (check-equal? odd-bucket  (Some (Cons 1 (Cons 3 (Cons 5 Nil)))))
  (check-equal? even-bucket (Some (Cons 2 (Cons 4 Nil)))))
