#lang scribble/manual
@require[scribble/manual
         (for-label rackton)]

@title[#:tag "concurrency"]{Concurrency and STM}

Rackton offers three layers of concurrency primitives — threads with
mutable variables, asynchronous channels, and software transactional
memory — plus a polymorphic @racket[Concurrent] class that abstracts
over them.

@section{Threads and MVars}

A @racket[(MVar a)] is a synchronised mutable variable: take blocks
on empty, put blocks on full.

@codeblock|{
(: ping-pong (IO Unit))
(define ping-pong
  (do [m <- new-empty-mvar]
      (fork-io
        (do (put-mvar m 42)
            (println "sent")))
      [v <- (take-mvar m)]
    (println (string-append "got: " (show v)))))
}|

@racket[fork-io] spawns a thread; @racket[wait-thread] waits for one
to complete.  Use MVars for synchronous handoffs.

@section{Async channels}

An @racket[(Chan a)] is an unbounded queue.  Sends never block;
receives block on empty.

@codeblock|{
(: producer-consumer (IO Unit))
(define producer-consumer
  (do [ch <- new-chan]
      (fork-io
        (do (send-chan ch 1)
            (send-chan ch 2)
            (send-chan ch 3)))
      [a <- (recv-chan ch)]
      [b <- (recv-chan ch)]
      [c <- (recv-chan ch)]
    (println (show (+ a (+ b c))))))
}|

Channels are best for fire-and-forget pipelines.

@section{Software transactional memory}

For shared state with rollback-on-conflict semantics, use @racket[TVar]
and the @racket[STM] monad:

@codeblock|{
(: transfer (-> (TVar Integer) (-> (TVar Integer) (-> Integer (STM Unit)))))
(define (transfer from to amount)
  (do [src <- (read-tvar from)]
      [dst <- (read-tvar to)]
      (write-tvar from (- src amount))
      (write-tvar to   (+ dst amount))
    (pure Unit)))

(: do-transfer (-> (TVar Integer) (-> (TVar Integer) (IO Unit))))
(define (do-transfer from to)
  (atomically (transfer from to 100)))
}|

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

@codeblock|{
(: wait-for-positive (-> (TVar Integer) (STM Integer)))
(define (wait-for-positive t)
  (do [n <- (read-tvar t)]
    (if (> n 0) (pure n) retry)))
}|

@section{The @racket[Concurrent] class}

For polymorphic code that works against either real threads or a
deterministic mock, the @racket[Concurrent] class abstracts
@racket[fork-c], @racket[await-c], and @racket[yield-c]:

@codeblock|{
(: par-add ((Concurrent m) => (-> Integer (-> Integer (m Integer)))))
(define (par-add x y)
  (do [fx <- (fork-c (pure (+ x x)))]
      [fy <- (fork-c (pure (+ y y)))]
      [a  <- (await-c fx)]
      [b  <- (await-c fy)]
    (pure (+ a b))))
}|

Run it concurrently with @racket[IO], or deterministically with
@racket[Identity].  The same body works against both — the instance
is resolved at the call site.

@section{Which primitive to use?}

@tabular[#:sep @hspace[2]
         (list
          (list @bold{Need}                          @bold{Use})
          (list "synchronous handoff"                @racket[MVar])
          (list "fire-and-forget pipeline"           @racket[Chan])
          (list "shared state with rollback"         @racket[TVar])
          (list "polymorphic async over Identity/IO" @racket[Concurrent]))]
