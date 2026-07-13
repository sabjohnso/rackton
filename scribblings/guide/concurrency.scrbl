#lang scribble/manual
@require[scribble/manual
         (for-label rackton
                    rackton/control/concurrent
                    rackton/control/stm)
         "../rackton-eval.rkt"]
@(define ev (make-rackton-eval))

@title[#:tag "concurrency"]{Concurrency and STM}

Rackton offers three layers of concurrency primitives — threads with
mutable variables, asynchronous channels, and software transactional
memory — plus a polymorphic @racket[Concurrent] protocol that abstracts
over them.

The thread, @racket[MVar], and channel primitives live in
@racketmodname[rackton/control/concurrent]; @racket[TVar] and the
@racket[STM] monad live in @racketmodname[rackton/control/stm].  Each
example below shows the @racket[require] it needs.

@section{Threads and MVars}

A @racket[(MVar a)] is a synchronised mutable variable: take blocks
on empty, put blocks on full.

@rackton-example[#:eval ev #:mode 'io #:run "ping-pong"]{
(require rackton/control/concurrent)

(: ping-pong (IO Unit))
(define ping-pong
  (let& ([m new-empty-mvar]
         [_ (fork-io
              (let& ([_ (put-mvar m 42)])
                (println "sent")))]
         [v (take-mvar m)])
    (println (string-append "got: " (show v)))))
}

@racket[fork-io] spawns a thread; @racket[wait-thread] waits for one
to complete.  Use MVars for synchronous handoffs.

@section{Async channels}

An @racket[(Chan a)] is an unbounded queue.  Sends never block;
receives block on empty.

@rackton-example[#:eval ev #:mode 'io #:run "producer-consumer"]{
(require rackton/control/concurrent)

(: producer-consumer (IO Unit))
(define producer-consumer
  (let& ([ch new-chan]
         [_ (fork-io
              (let& ([_ (send-chan ch 1)]
                     [_ (send-chan ch 2)])
                (send-chan ch 3)))]
         [a (recv-chan ch)]
         [b (recv-chan ch)]
         [c (recv-chan ch)])
    (println (show (+ a (+ b c))))))
}

Channels are best for fire-and-forget pipelines.

@section{Software transactional memory}

For shared state with rollback-on-conflict semantics, use @racket[TVar]
and the @racket[STM] monad:

@rackton-example[#:eval ev #:mode 'defs]{
(require rackton/control/stm)

(: transfer (-> (TVar Integer) (-> (TVar Integer) (-> Integer (STM Unit)))))
(define (transfer from to amount)
  (let& ([src (read-tvar from)]
         [dst (read-tvar to)]
         [_ (write-tvar from (- src amount))]
         [_ (write-tvar to   (+ dst amount))])
    (pure Unit)))

(: do-transfer (-> (TVar Integer) (-> (TVar Integer) (IO Unit))))
(define (do-transfer from to)
  (atomically (transfer from to 100)))
}

@racket[atomically] runs an @racket[STM] transaction.  Inside, reads
and writes go to a per-transaction log.  At commit:

@itemlist[
@item{If every TVar's observed version still matches its current
      version, writes are applied atomically and the result is
      returned.}
@item{Otherwise the transaction is rolled back and retried from
      scratch.}]

@racket[retry] inside a transaction abandons the current attempt and
blocks until any TVar the transaction read has been written to.
@racket[or-else] composes two transactions: if the first retries, the
second is tried.

@rackton-example[#:eval ev #:mode 'defs]{
(require rackton/control/stm)

(: wait-for-positive (-> (TVar Integer) (STM Integer)))
(define (wait-for-positive t)
  (let& ([n (read-tvar t)])
    (if (> n 0) (pure n) retry)))
}

We can watch a transaction roll back and retry.  Two threads each add
to the same account; the @racket[STM] log guarantees no update is lost,
so the final balance is the sum of both deposits:

@rackton-example[#:eval ev #:mode 'io #:run "demo"]{
(require rackton/control/stm
         rackton/control/concurrent)

(: deposit (-> (TVar Integer) (-> Integer (STM Unit))))
(define (deposit acct amount)
  (let& ([balance (read-tvar acct)])
    (write-tvar acct (+ balance amount))))

(: demo (IO Unit))
(define demo
  (let& ([acct (atomically (new-tvar 0))]
         [t1   (fork-io (atomically (deposit acct 100)))]
         [t2   (fork-io (atomically (deposit acct 25)))]
         [_ (wait-thread t1)]
         [_ (wait-thread t2)]
         [final (atomically (read-tvar acct))])
    (println (string-append "balance: " (show final)))))
}

@section{The @racket[Concurrent] protocol}

For polymorphic code that works against either real threads or a
deterministic mock, the @racket[Concurrent] protocol abstracts
@racket[fork-c], @racket[await-c], and @racket[yield-c]:

@rackton-example[#:eval ev #:mode 'defs #:context? #t]{
(: par-add ((Concurrent m) => (-> Integer (-> Integer (m Integer)))))
(define (par-add x y)
  (let& ([fx (fork-c (pure (+ x x)))]
         [fy (fork-c (pure (+ y y)))]
         [a  (await-c fx)]
         [b  (await-c fy)])
    (pure (+ a b))))
}

Run it concurrently with @racket[IO], or deterministically with
@racket[Identity].  The same body works against both — the instance
is resolved at the call site:

@rackton-example[#:eval ev #:mode 'value #:context? #t]{
(run-identity (ann ((par-add 3) 4) (Identity Integer)))
}

@section{Which primitive to use?}

@tabular[#:sep @hspace[2]
         (list
          (list @bold{Need}                          @bold{Use})
          (list "synchronous handoff"                @racket[MVar])
          (list "fire-and-forget pipeline"           @racket[Chan])
          (list "shared state with rollback"         @racket[TVar])
          (list "polymorphic async over Identity/IO" @racket[Concurrent]))]
