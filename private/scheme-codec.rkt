#lang racket/base

;; Rackton — codec for schemes / types / preds.
;;
;; Used to ferry type information across .rkt module boundaries.
;; `scheme->sexp` is just `scheme->datum`; `sexp->scheme` is the inverse,
;; reconstructing the structured representation from the datum form.
;; The textual representation must round-trip exactly.

(require racket/match
         racket/list
         "types.rkt"
         "env.rkt"
         "surface.rkt")

(provide scheme->sexp
         sexp->scheme
         sexp->type
         sexp->pred
         encode-data-info
         decode-data-info
         encode-tcon-info
         decode-tcon-info)

(define (scheme->sexp s) (scheme->datum s))

(define (sexp->scheme datum)
  (match datum
    [(list 'All vars body)
     (scheme vars (sexp->type body))]
    [body
     (scheme '() (sexp->type body))]))

(define (sexp->type datum)
  (cond
    [(symbol? datum)
     (if (lowercase-id? datum) (tvar datum) (tcon datum))]
    [(and (list? datum) (memq '=> datum))
     (define i (index-of datum '=>))
     (define preds-syntax (take datum i))
     (define after-arrow (drop datum (add1 i)))
     ;; `(p1 p2 ... => body)` — after `=>` is exactly one body.
     (mqual (map sexp->pred preds-syntax)
            (sexp->type (car after-arrow)))]
    [(and (list? datum)
          (eq? '-> (car datum))
          (= (length datum) 3))
     (make-arrow (sexp->type (cadr datum)) (sexp->type (caddr datum)))]
    [(list? datum)
     (make-tapp (sexp->type (car datum))
                (map sexp->type (cdr datum)))]
    [else
     (error 'sexp->type "cannot decode: ~v" datum)]))

(define (sexp->pred datum)
  (match datum
    [(cons name args) (pred name (map sexp->type args))]))

;; ----- data-info and tcon-info codecs -------------------------

(define (encode-data-info di)
  (list (data-info-type-name di)
        (data-info-ctor-name di)
        (data-info-arity di)
        (scheme->sexp (data-info-scheme di))))

(define (decode-data-info datum)
  (match datum
    [(list type-name ctor-name arity scheme-sexp)
     (data-info type-name ctor-name arity (sexp->scheme scheme-sexp))]))

(define (encode-tcon-info ti)
  (list (tcon-info-name ti)
        (tcon-info-arity ti)
        (tcon-info-ctors ti)))

(define (decode-tcon-info datum)
  (match datum
    [(list name arity ctors) (tcon-info name arity ctors)]))
