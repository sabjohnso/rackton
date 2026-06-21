#lang rackton

;; Property-based algebraic laws for the prelude's List operations,
;; using the native test framework's generators + shrinking.  This
;; broadens property testing beyond the type-machinery (unify / types /
;; infer / scheme-codec) into the standard library — exercising
;; reverse / append / length / fmap / filter over 100 generated lists
;; each, with `==` and `show` (now that the prelude has Eq/Show for
;; List and Pair).

(require "../unit.rkt")

(: gli (Gen (List Integer)))
(define gli (gen-list (int-range -20 20)))

(: suite (List Test))
(define suite
  (list
   (it-prop "reverse is an involution"
            (for-all gli (lambda (xs) (== (reverse (reverse xs)) xs))))

   (it-prop "reverse preserves length"
            (for-all gli (lambda (xs) (== (length (reverse xs)) (length xs)))))

   (it-prop "Nil is a left and right unit of append"
            (for-all gli (lambda (xs)
                           (and (== (append Nil xs) xs)
                                (== (append xs Nil) xs)))))

   (it-prop "append length is additive"
            (for-all (gen-pair gli gli)
                     (lambda (p)
                       (match p
                         [(Pair xs ys)
                          (== (length (append xs ys))
                              (+ (length xs) (length ys)))]))))

   (it-prop "append is associative"
            (for-all (gen-pair gli (gen-pair gli gli))
                     (lambda (t)
                       (match t
                         [(Pair xs (Pair ys zs))
                          (== (append (append xs ys) zs)
                              (append xs (append ys zs)))]))))

   (it-prop "reverse is an anti-homomorphism over append"
            (for-all (gen-pair gli gli)
                     (lambda (p)
                       (match p
                         [(Pair xs ys)
                          (== (reverse (append xs ys))
                              (append (reverse ys) (reverse xs)))]))))

   (it-prop "fmap preserves length"
            (for-all gli (lambda (xs)
                           (== (length (fmap (lambda (n) (* n 2)) xs))
                               (length xs)))))

   (it-prop "filter never grows a list"
            (for-all gli (lambda (xs)
                           (<= (length (filter (lambda (n) (> n 0)) xs))
                               (length xs)))))))

(: main Unit)
(define main (run-io (run-suite "List laws" suite)))
