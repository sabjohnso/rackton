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
         pred->sexp
         encode-data-info
         decode-data-info
         encode-tcon-info
         decode-tcon-info
         encode-kind
         decode-kind
         encode-class-info
         decode-class-info
         encode-instance-info
         decode-instance-info)

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

;; ----- kinds, classes, instances ------------------------------

(define (encode-kind k)
  (match k
    [(kind-star)     '*]
    [(kind-arr a b)  `(-> ,(encode-kind a) ,(encode-kind b))]))

(define (decode-kind datum)
  (cond
    [(eq? datum '*) (kind-star)]
    [(and (list? datum) (eq? (car datum) '->))
     (kind-arr (decode-kind (cadr datum)) (decode-kind (caddr datum)))]
    [else (error 'decode-kind "bad kind: ~v" datum)]))

(define (pred->sexp p) (pred->datum p))

(define (encode-class-info ci)
  (list (class-info-name ci)
        (class-info-params ci)
        (for/list ([(k v) (in-hash (class-info-kinds ci))])
          (list k (encode-kind v)))
        (map pred->sexp (class-info-supers ci))
        (for/list ([(m s) (in-hash (class-info-methods ci))])
          (list m (scheme->sexp s)))
        (for/list ([(m p) (in-hash (class-info-dispatchpos ci))])
          (list m p))
        (for/list ([fd (in-list (class-info-fundeps ci))])
          (list (car fd) (cdr fd)))
        (for/list ([(m cs) (in-hash (class-info-dictreqs ci))])
          (list m cs))))

(define (decode-class-info datum)
  (match datum
    [(list name params kinds-list supers-list methods-list dispatchpos-list
           fundeps-list dictreqs-list)
     (class-info name
                 params
                 (for/hasheq ([entry (in-list kinds-list)])
                   (values (car entry) (decode-kind (cadr entry))))
                 (map sexp->pred supers-list)
                 (for/hasheq ([entry (in-list methods-list)])
                   (values (car entry) (sexp->scheme (cadr entry))))
                 (hasheq)   ; defaults not transmitted
                 (for/hasheq ([entry (in-list dispatchpos-list)])
                   (values (car entry) (cadr entry)))
                 (for/list ([entry (in-list fundeps-list)])
                   (cons (car entry) (cadr entry)))
                 (for/hasheq ([entry (in-list dictreqs-list)])
                   (values (car entry) (cadr entry))))]
    [(list name params kinds-list supers-list methods-list dispatchpos-list
           fundeps-list)
     (class-info name
                 params
                 (for/hasheq ([entry (in-list kinds-list)])
                   (values (car entry) (decode-kind (cadr entry))))
                 (map sexp->pred supers-list)
                 (for/hasheq ([entry (in-list methods-list)])
                   (values (car entry) (sexp->scheme (cadr entry))))
                 (hasheq)   ; defaults not transmitted
                 (for/hasheq ([entry (in-list dispatchpos-list)])
                   (values (car entry) (cadr entry)))
                 (for/list ([entry (in-list fundeps-list)])
                   (cons (car entry) (cadr entry)))
                 (hasheq))]
    ;; Backward compat: older sidecar submodules omit the fundeps list.
    [(list name params kinds-list supers-list methods-list dispatchpos-list)
     (class-info name
                 params
                 (for/hasheq ([entry (in-list kinds-list)])
                   (values (car entry) (decode-kind (cadr entry))))
                 (map sexp->pred supers-list)
                 (for/hasheq ([entry (in-list methods-list)])
                   (values (car entry) (sexp->scheme (cadr entry))))
                 (hasheq)
                 (for/hasheq ([entry (in-list dispatchpos-list)])
                   (values (car entry) (cadr entry)))
                 '()
                 (hasheq))]))

;; Instance info is encoded with its owning class name as the first
;; element so we know where to install it on decode.  Method bodies
;; are not transmitted — the runtime side reaches the importer via
;; standard Racket module loading.
(define (encode-instance-info class-name ii)
  (list class-name
        (pred->sexp (instance-info-head ii))
        (map pred->sexp (instance-info-context ii))))

(define (decode-instance-info datum)
  (match datum
    [(list class-name head-sexp ctx-list)
     (cons class-name
           (instance-info (sexp->pred head-sexp)
                          (map sexp->pred ctx-list)
                          (hasheq)))]))
