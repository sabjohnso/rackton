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
           "ast.rkt"
           "scheme-codec.rkt"
           (submod "type-gen.rkt" test))

  ;; ----- codec-specific generators (build on type-gen) ---------------

  (define gen:tcon-info
    (gen:let ([name      gen:tcon-name]
              [arity     (gen:integer-in 0 4)]
              [kind      (gen:kind-scheme 3)]
              [ctors     (gen:list gen:tcon-name #:max-length 3)]
              [abstract? gen:boolean]
              [rtag      (gen:choice (gen:const #f) gen:tcon-name)])
      (tcon-info name arity kind ctors abstract? rtag)))

  (define gen:data-info
    (gen:let ([tname    gen:tcon-name]
              [cname    gen:tcon-name]
              [arity    (gen:integer-in 0 4)]
              [sch      (gen:scheme 3)]
              [ex-tvars (gen:list gen:tvar-name #:max-length 2)]
              [fnames   (gen:choice (gen:const #f)
                                    (gen:list gen:tvar-name #:max-length 4))])
      (data-info tname cname arity sch ex-tvars fnames)))

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

  ;; A kind SCHEME — quantified kvars over a kind body — round-trips
  ;; through a tcon-info (the only carrier of kind schemes in a sidecar).
  (check-property
   (property kind-scheme-roundtrips ([k (gen:kind-scheme 4)])
     (let ([ti (tcon-info 'T 0 k '() #f #f)])
       (equal? (tcon-info-kind (decode-tcon-info (encode-tcon-info ti))) k))))

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
    (check-equal? (sexp->scheme (scheme->sexp s)) s))

  ;; A class's DEFAULT method bodies (surface AST) cross modules: encode
  ;; the class, decode it, and the default round-trips.  The decoded AST
  ;; carries a placeholder syntax handle (the importer re-anchors it with
  ;; freshen-ast), so we compare modulo stx by relocating both to one
  ;; handle.  Body modelled on Comonad's `duplicate = extend id`.
  (let* ([stx (datum->syntax #f 'orig)]
         [dup-default
          (e:lam '(w)
                 (e:app (e:var 'extend stx)
                        (list (e:lam '(x) (e:var 'x stx) stx)
                              (e:var 'w stx))
                        stx)
                 stx)]
         [ci (class-info 'Comonad '(w) (hasheq) '() (hasheq)
                         (hasheq 'duplicate dup-default)
                         (hasheq) '() (hasheq) '() '() '())]
         [ci2 (decode-class-info (encode-class-info ci))]
         [dup2 (hash-ref (class-info-defaults ci2) 'duplicate)]
         [norm (lambda (e) (relocate-ast e stx))])
    (check-equal? (norm dup2) (norm dup-default))))
