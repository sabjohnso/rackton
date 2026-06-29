#lang racket/base

;; tests/cross-module-test.rkt — round-trip property for the
;; scheme-codec.  The developer chapter scribblings/developer/cross-module.scrbl
;; promises this test: "Any new type or scheme construct must be added
;; to the codec or the property test breaks."  Each construct that the
;; codec covers is exercised here against `equal?`.
;;
;; The codec encodes structured Rackton type information to a plain
;; s-expression form so a `(require "other.rkt")` can recover the
;; importee's schemes from its `rackton-schemes` sidecar submodule.
;; Round-trip equality is what makes that import safe: a scheme
;; written into the sidecar by the exporter must compare `equal?` to
;; the scheme reconstructed by the importer.

(require rackunit
         racket/match
         "../private/types.rkt"
         "../private/env.rkt"
         "../private/scheme-codec.rkt")

;; ----- types and schemes ------------------------------------------

(define (round-trip-scheme s)
  (sexp->scheme (scheme->sexp s)))

(define (round-trip-pred p)
  (sexp->pred (pred->sexp p)))

(define (round-trip-kind k)
  (decode-kind (encode-kind k)))

(test-case "type variable round-trips"
  (define s (scheme '(a) (tvar 'a)))
  (check-equal? (round-trip-scheme s) s))

(test-case "type constructor round-trips"
  (define s (scheme '() (tcon 'Integer)))
  (check-equal? (round-trip-scheme s) s))

(test-case "type application round-trips"
  (define s (scheme '(a) (make-tapp (tcon 'Maybe) (list (tvar 'a)))))
  (check-equal? (round-trip-scheme s) s))

(test-case "arrow type round-trips"
  (define s (scheme '(a b) (make-arrow (tvar 'a) (tvar 'b))))
  (check-equal? (round-trip-scheme s) s))

(test-case "nested arrow (curried) round-trips"
  (define s (scheme '(a b c)
                    (make-arrow (tvar 'a)
                                (make-arrow (tvar 'b) (tvar 'c)))))
  (check-equal? (round-trip-scheme s) s))

(test-case "qualified scheme with one constraint round-trips"
  (define s (scheme '(a)
                    (qual (list (pred 'Eq (list (tvar 'a))))
                          (make-arrow (tvar 'a)
                                      (make-arrow (tvar 'a) (tcon 'Boolean))))))
  (check-equal? (round-trip-scheme s) s))

(test-case "qualified scheme with multiple constraints round-trips"
  (define s (scheme '(a b)
                    (qual (list (pred 'Eq  (list (tvar 'a)))
                                (pred 'Ord (list (tvar 'b))))
                          (make-arrow (tvar 'a) (tvar 'b)))))
  (check-equal? (round-trip-scheme s) s))

(test-case "pred round-trips on its own"
  (define p (pred 'Functor (list (tcon 'Maybe))))
  (check-equal? (round-trip-pred p) p))

;; ----- kinds ------------------------------------------------------

(test-case "kind-star round-trips"
  (check-equal? (round-trip-kind (kind-star)) (kind-star)))

(test-case "kind-arr round-trips"
  (define k (kind-arr (kind-star) (kind-star)))
  (check-equal? (round-trip-kind k) k))

(test-case "nested kind-arr round-trips"
  (define k (kind-arr (kind-star)
                      (kind-arr (kind-star) (kind-star))))
  (check-equal? (round-trip-kind k) k))

;; ----- data-info / tcon-info --------------------------------------

(test-case "data-info round-trips"
  (define di (data-info 'Maybe
                        'Some
                        1
                        (scheme '(a) (make-arrow (tvar 'a)
                                                 (make-tapp (tcon 'Maybe)
                                                            (list (tvar 'a)))))
                        '()
                        '(value)))
  (check-equal? (decode-data-info (encode-data-info di)) di))

(test-case "data-info with existential tvars round-trips"
  (define di (data-info 'ExistsShow
                        'PackShow
                        1
                        (scheme '() (tcon 'ExistsShow))
                        '(a)
                        #f))
  (check-equal? (decode-data-info (encode-data-info di)) di))

(test-case "tcon-info round-trips"
  (define ti (tcon-info 'Maybe 1 (kscheme-mono (arity->star-kind 1)) '(None Some) #f #f))
  (check-equal? (decode-tcon-info (encode-tcon-info ti)) ti))

(test-case "abstract tcon-info round-trips"
  (define ti (tcon-info 'Counter 0 (kscheme-mono (arity->star-kind 0)) '(MkCounter) #t #f))
  (check-equal? (decode-tcon-info (encode-tcon-info ti)) ti))

;; ----- class-info / instance-info --------------------------------

(test-case "class-info with superclass round-trips"
  (define ci (class-info
              'Ord
              '(a)
              (hasheq 'a (kind-star))
              (list (pred 'Eq (list (tvar 'a))))
              (hasheq '< (scheme '(a)
                                 (qual (list (pred 'Ord (list (tvar 'a))))
                                       (make-arrow (tvar 'a)
                                                   (make-arrow (tvar 'a)
                                                               (tcon 'Boolean))))))
              ;; defaults table is not encoded; the codec
              ;; reconstructs an empty hash on decode.
              (hasheq)
              (hasheq '< 0)
              '()
              (hasheq)
              '()
              ;; super-derives table is not encoded either; the codec
              ;; reconstructs an empty hash on decode.
              (hasheq)
              ;; laws are likewise not encoded; decode reconstructs '().
              '()))
  (define round-tripped (decode-class-info (encode-class-info ci)))
  ;; Compare fields individually since the defaults field is dropped
  ;; and reconstituted as `(hasheq)`.
  (check-equal? (class-info-name        round-tripped) (class-info-name        ci))
  (check-equal? (class-info-params      round-tripped) (class-info-params      ci))
  (check-equal? (class-info-kinds       round-tripped) (class-info-kinds       ci))
  (check-equal? (class-info-supers      round-tripped) (class-info-supers      ci))
  (check-equal? (class-info-methods     round-tripped) (class-info-methods     ci))
  (check-equal? (class-info-dispatchpos round-tripped) (class-info-dispatchpos ci))
  (check-equal? (class-info-fundeps     round-tripped) (class-info-fundeps     ci))
  (check-equal? (class-info-dictreqs    round-tripped) (class-info-dictreqs    ci))
  (check-equal? (class-info-type-families round-tripped) (class-info-type-families ci)))

(test-case "instance-info round-trips (class-name prepended)"
  (define ii (instance-info (pred 'Eq (list (tcon 'Integer)))
                            '()
                            (hasheq)
                            (hasheq)
                            "test-origin"
                            #f))
  (define encoded (encode-instance-info 'Eq ii))
  (define decoded (decode-instance-info encoded))
  (check-equal? decoded (cons 'Eq ii)))

(test-case "instance-info with context round-trips"
  (define ii (instance-info (pred 'Eq (list (make-tapp (tcon 'Maybe)
                                                       (list (tvar 'a)))))
                            (list (pred 'Eq (list (tvar 'a))))
                            (hasheq)
                            (hasheq)
                            "test-origin"
                            #f))
  (check-equal? (decode-instance-info (encode-instance-info 'Eq ii))
                (cons 'Eq ii)))

(test-case "instance-info with associated-type bindings round-trips"
  (define ii (instance-info (pred 'Container (list (make-tapp (tcon 'List)
                                                              (list (tvar 'a)))))
                            (list)
                            (hasheq)
                            (hasheq 'Elem (tvar 'a))
                            "test-origin"
                            #f))
  (check-equal? (decode-instance-info (encode-instance-info 'Container ii))
                (cons 'Container ii)))
