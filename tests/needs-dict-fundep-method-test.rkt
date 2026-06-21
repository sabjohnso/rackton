#lang rackton

;; Regression: needs-dict instance methods of a FUNDEP class, in
;; FUNCTION form, must thread the inner monad's return-typed dict.
;;
;; A return-typed method determined by a fundep (e.g. MonadState's
;; put-st : m -> s, where `m -> s`) lives at a head-arg position whose
;; tcon is the DISPATCHING monad, not the determined payload tvar.  The
;; needs-dict-body machinery keys its dict-arg record by that
;; dispatching tcon so codegen's prepend finds it; otherwise a body
;; that references the inner `pure` (put/modify-style) is left with an
;; unbound `$dict-pure-…` reference.  Value-form methods (get-style)
;; always worked; this pins the function-form path.

(require "../unit.rkt")

(newtype (Wrap m a) (MkWrap (m a)))
(: un (-> (Wrap m a) (m a)))
(define (un w) (match w [(MkWrap x) x]))

;; A fundep class with both a VALUE-form (held) and a FUNCTION-form
;; (mk) return-typed method, each needing the inner monad's `pure`.
(protocol (Holder s [m => Monad])
  (#:fundep m -> s)
  (: held (m s))
  (: mk   (-> s (m s))))

(instance ((Monad m) => (Functor (Wrap m)))
  (define (fmap f w) (MkWrap (fmap f (un w)))))
(instance ((Monad m) => (Applicative (Wrap m)))
  (define (pure a) (MkWrap (pure a)))
  (define (fapply wf wa) (MkWrap (fapply (un wf) (un wa)))))
(instance ((Monad m) => (Monad (Wrap m)))
  (define (flatmap f w) (MkWrap (flatmap (lambda (a) (un (f a))) (un w)))))

(instance ((Monad m) => (Holder Integer (Wrap m)))
  (define held   (MkWrap (pure 0)))
  (define (mk x) (MkWrap (pure x))))

;; mk at inner = Maybe and inner = List: each must use its own pure.
(: m-built (Wrap Maybe Integer))
(define m-built (mk 7))
(: l-built (Wrap List Integer))
(define l-built (mk 9))

(: maybe-val (Maybe Integer))
(define maybe-val (un m-built))
(: list-val (List Integer))
(define list-val (un l-built))

(: held-val (Maybe Integer))
(define held-val (un (held-as-maybe)))
(: held-as-maybe (-> (Wrap Maybe Integer)))
(define (held-as-maybe) held)

(: suite (List Test))
(define suite
  (list
   (it "fundep needs-dict function-form method threads inner pure (Maybe)"
       (check-equal? maybe-val (Some 7)))
   (it "fundep needs-dict function-form method threads inner pure (List)"
       (check-equal? list-val (Cons 9 Nil)))
   (it "fundep needs-dict value-form method threads inner pure"
       (check-equal? held-val (Some 0)))))

(: main Unit)
(define main (run-io (run-suite "needs-dict-fundep-method" suite)))
