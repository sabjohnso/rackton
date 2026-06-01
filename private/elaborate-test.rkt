#lang racket/base

;; Tests for private/elaborate.rkt: the `rackton` macro's elaboration —
;; the things this stage is responsible for beyond per-form codegen,
;; asserted at the expansion level rather than by running a program:
;;   - it splices into a single (begin …);
;;   - phase ordering reorders forms so a class is set up before any
;;     instance that registers into it, regardless of source order;
;;   - a (provide …) is emitted exactly when the block exports something.

(module+ test
  (require rackunit
           racket/list
           "elaborate.rkt")   ; the `rackton` macro

  ;; Expand the `rackton` macro one step and inspect the emitted forms.
  (define (expand-rackton stx) (syntax->datum (expand-once stx)))

  ;; Flatten nested begins into the flat list of leaf forms.
  (define (flatten-begin form)
    (cond
      [(and (pair? form) (eq? (car form) 'begin)) (append-map flatten-begin (cdr form))]
      [else (list form)]))

  (define (leaf-heads form)
    (map (lambda (f) (if (pair? f) (car f) f)) (flatten-begin form)))

  (test-case "rackton splices into a begin headed by the log-snapshot setters"
    (define ex (expand-rackton #'(rackton (: x Integer) (define x 5))))
    (check-eq? (car ex) 'begin)
    (check-equal? (map car (take (cdr ex) 2))
                  '(set-rackton-monomorphized-log-snapshot!
                    set-rackton-inlined-log-snapshot!)))

  (test-case "phase ordering sets up a class before the instance, despite source order"
    ;; The instance is written BEFORE the protocol here; elaboration must
    ;; still emit the class's dispatch machinery before the instance's
    ;; register-instance-method! call.
    (define heads
      (leaf-heads
       (expand-rackton
        #'(rackton
           (data Foo MkFoo)
           (instance (Eq2 Foo) (define (eq2 x y) #t))
           (protocol (Eq2 a) (: eq2 (-> a (-> a Boolean))))))))
    (define class-idx    (index-of heads 'define-class-method))
    (define register-idx (index-of heads 'register-instance-method!))
    (check-not-false class-idx)
    (check-not-false register-idx)
    (check-true (< class-idx register-idx)))

  (test-case "a provide form is emitted when the block exports"
    (define heads
      (leaf-heads (expand-rackton #'(rackton (: x Integer) (define x 5) (provide x)))))
    (check-true (and (memq 'provide heads) #t)))

  (test-case "no provide form is emitted when the block exports nothing"
    (define heads
      (leaf-heads (expand-rackton #'(rackton (: y Integer) (define y 5)))))
    (check-false (and (memq 'provide heads) #t))))
