#lang racket/base

;; Property tests for private/scheme-codec.rkt — the codec that ferries
;; type information across .rkt module boundaries (the rackton-schemes
;; sidecar).  The header asserts the representation "must round-trip
;; exactly"; these properties pin that down for arbitrary generated
;; types, predicates, kinds, schemes, and the tcon-/data-info records,
;; reusing the shared generators in type-gen.rkt.
;;
;; The codec recovers the tvar/tcon distinction from name casing and
;; relies on types being canonical (flattened via make-tapp/make-arrow);
;; the generators honor both, so a failure here is a genuine codec
;; regression, not a generator artifact.

(module+ test
  (require rackunit
           rackcheck
           "types.rkt"
           "env.rkt"
           "scheme-codec.rkt"
           (submod "type-gen.rkt" test))

  ;; ----- codec-specific generators (build on type-gen) ---------------

  (define gen:tcon-info
    (gen:let ([name      gen:tcon-name]
              [arity     (gen:integer-in 0 4)]
              [ctors     (gen:list gen:tcon-name #:max-length 3)]
              [abstract? gen:boolean]
              [rtag      (gen:choice (gen:const #f) gen:tcon-name)])
      (tcon-info name arity ctors abstract? rtag)))

  (define gen:data-info
    (gen:let ([tname    gen:tcon-name]
              [cname    gen:tcon-name]
              [arity    (gen:integer-in 0 4)]
              [sch      (gen:scheme 3)]
              [ex-tvars (gen:list gen:tvar-name #:max-length 2)])
      (data-info tname cname arity sch ex-tvars)))

  ;; ----- round-trip properties ---------------------------------------

  (check-property
   (property type-roundtrips ([t (gen:type 4)])
     (equal? (sexp->type (type->datum t)) t)))

  (check-property
   (property pred-roundtrips ([p (gen:pred 3)])
     (equal? (sexp->pred (pred->sexp p)) p)))

  (check-property
   (property kind-roundtrips ([k (gen:kind 4)])
     (equal? (decode-kind (encode-kind k)) k)))

  ;; The headline: a scheme — quantified vars over a possibly-qualified
  ;; body — survives scheme->sexp then sexp->scheme unchanged.
  (check-property
   (property scheme-roundtrips ([s (gen:scheme 3)])
     (equal? (sexp->scheme (scheme->sexp s)) s)))

  (check-property
   (property tcon-info-roundtrips ([ti gen:tcon-info])
     (equal? (decode-tcon-info (encode-tcon-info ti)) ti)))

  (check-property
   (property data-info-roundtrips ([di gen:data-info])
     (equal? (decode-data-info (encode-data-info di)) di)))

  ;; ----- worked examples (readability / regression anchors) ----------

  (check-equal? (sexp->type (type->datum (tapp (tcon 'Maybe) (list (tvar 'a)))))
                (tapp (tcon 'Maybe) (list (tvar 'a))))

  (check-equal? (sexp->type (type->datum (make-arrow (tvar 'a) (tvar 'b))))
                (make-arrow (tvar 'a) (tvar 'b)))

  ;; A qualified, quantified scheme — (All (a) ((Eq a) => (-> a Boolean))).
  (let ([s (scheme '(a)
                   (mqual (list (pred 'Eq (list (tvar 'a))))
                          (make-arrow (tvar 'a) (tcon 'Boolean))))])
    (check-equal? (sexp->scheme (scheme->sexp s)) s))

  ;; An empty-quantifier scheme collapses to the bare body datum and back.
  (let ([s (scheme '() (tcon 'Integer))])
    (check-equal? (scheme->sexp s) 'Integer)
    (check-equal? (sexp->scheme (scheme->sexp s)) s)))
