#lang scribble/manual
@require[scribble/manual
         (for-label rackton rackton/system/directory rackton/system/environment rackton/system/exception rackton/system/exit rackton/system/file rackton/system/io rackton/system/random rackton/system/ref rackton/system/time)]

@title[#:tag "stdlib-system"]{@tt{rackton/system} — IO and the outside world}

@defmodule[rackton/system #:no-declare]
The @racketmodname[rackton/system] umbrella re-exports every module in this
family in one import; require it for the whole toolkit, or a specific
module below for a narrower dependency.

@section{rackton/system/directory}
@defmodule[rackton/system/directory #:no-declare]
@declare-exporting[rackton/system/directory]

Filesystem entry operations: existence checks, deletion, directory
creation, and directory listing. The runtime primitives live in
@tt{private/prelude-runtime} and are reached via @racket[foreign].

@defproc[(file-exists? [path String]) (IO Boolean)]{Reports whether a file exists at @racket[path] (Haskell's @tt{doesFileExist}).}

@defproc[(does-directory-exist? [path String]) (IO Boolean)]{Reports whether a directory exists at @racket[path] (Haskell's @tt{doesDirectoryExist}).}

@defproc[(delete-file [path String]) (IO Unit)]{Removes the file at @racket[path] (Haskell's @tt{removeFile}).}

@defproc[(make-directory [path String]) (IO Unit)]{Creates the directory @racket[path] (Haskell's @tt{createDirectory}).}

@defproc[(create-directory-if-missing [path String]) (IO Unit)]{Creates the directory and any missing parents, with no error if it already exists (Haskell's @tt{createDirectoryIfMissing}).}

@defproc[(list-directory [path String]) (IO (List String))]{Lists the entries of the directory @racket[path] (Haskell's @tt{listDirectory}).}

@defproc[(get-current-directory) (IO String)]{Returns the current working directory (Haskell's @tt{getCurrentDirectory}).}

@defproc[(rename-file [src String] [dest String]) (IO Unit)]{Moves or renames @racket[src] to @racket[dest], replacing an existing destination (Haskell's @tt{renameFile}).}

@defproc[(copy-file [src String] [dest String]) (IO Unit)]{Copies the contents of @racket[src] to @racket[dest], replacing an existing destination (Haskell's @tt{copyFile}).}


@section{rackton/system/environment}
@defmodule[rackton/system/environment #:no-declare]
@declare-exporting[rackton/system/environment]

Process environment and command-line access. The runtime primitives live in
@tt{rackton/private/prelude-runtime} and are reached via @racket[foreign].

@defproc[(getenv [name String]) (IO (Maybe String))]{
  The value of an environment variable, or @racket[None] if unset.}

@defthing[argv (IO (List String))]{
  The command-line arguments.}

@defthing[get-prog-name (IO String)]{
  The running program's name (without directory).}

@defproc[(set-env [name String] [value String]) (IO Unit)]{
  Set an environment variable.}


@section{rackton/system/exception}
@defmodule[rackton/system/exception #:no-declare]
@declare-exporting[rackton/system/exception]

Exceptions in @racket[IO], following Haskell's @tt{Control.Exception} /
@tt{System.IO.Error}: @racket[try] reifies a raised error as a
@racket[Result], and @racket[raise-io] throws one.

@defproc[(try [action (IO a)]) (IO (Result String a))]{
  Runs @racket[action], catching any raised error as @racket[(Err message)].}

@defproc[(raise-io [message String]) (IO a)]{
  Throws an error carrying the given @racket[message].}


@section{rackton/system/exit}
@defmodule[rackton/system/exit #:no-declare]
@declare-exporting[rackton/system/exit]

System.Exit: terminate the process with a status code. @racket[ExitCode] is
a plain Rackton data type, and the lone runtime primitive
@racket[exit-with-code] is reached via @racket[foreign].

@defidform[#:kind "type" ExitCode]{A process exit status.
  @deftogether[(@defidform[#:kind "constructor" ExitSuccess]
                @defidform[#:kind "constructor" ExitFailure])]{
    @racket[ExitSuccess : ExitCode] is status 0;
    @racket[ExitFailure : (-> Integer ExitCode)] is status @racket[n],
    conventionally non-zero.}}

@defproc[(exit-with-code [n Integer]) (IO a)]{The host primitive: terminate
the process with raw status code @racket[n]. Never returns, so the result
type is free.}

@defproc[(exit-with [code ExitCode]) (IO a)]{Terminate with the status named
by an @racket[ExitCode].}

@defthing[exit-success (IO a)]{Terminate with status 0.}

@defthing[exit-failure (IO a)]{Terminate with status 1.}


@section{rackton/system/file}
@defmodule[rackton/system/file #:no-declare]
@declare-exporting[rackton/system/file]

Whole-file I/O, mirroring Haskell's @tt{readFile} / @tt{writeFile} / @tt{appendFile}.
The underlying runtime primitives live in @tt{rackton/private/prelude-runtime} and are reached through @racket[foreign].

@defproc[(read-file [path String]) (IO String)]{Reads the entire contents of the file at @racket[path] as a @racket[String].}

@defproc[(write-file [path String] [contents String]) (IO Unit)]{Replaces the contents of the file at @racket[path] with @racket[contents].}

@defproc[(append-file [path String] [contents String]) (IO Unit)]{Appends @racket[contents] to the file at @racket[path], creating it if it does not exist.}


@section{rackton/system/io}
@defmodule[rackton/system/io #:no-declare]
@declare-exporting[rackton/system/io]

Handle-based file and stream I/O, modeled on Haskell's @tt{System.IO}. A
@racket[Handle] is opaque (a host port); an operation that does not match a
handle's direction errors at runtime. The prelude already provides the
standard-stream conveniences @racket[print], @racket[println], and
@racket[read-line]; this module adds explicit handles. It re-exports
@racket[rackton/system/exception].

@defidform[#:kind "type" Handle]{An opaque file or stream handle backed by a
host port.}

@defidform[#:kind "type" IOMode]{The mode a file is opened in (Haskell's
@tt{IOMode}, minus @tt{ReadWriteMode}).
  @deftogether[(@defidform[#:kind "constructor" ReadMode]
                @defidform[#:kind "constructor" WriteMode]
                @defidform[#:kind "constructor" AppendMode])]{
    @racket[ReadMode : (IOMode)] / @racket[WriteMode : (IOMode)] /
    @racket[AppendMode : (IOMode)] — open for reading, (truncating) writing, or
    appending respectively.}}

@deftogether[(@defthing[stdin Handle]
              @defthing[stdout Handle]
              @defthing[stderr Handle])]{
  The three standard handles: standard input, standard output, and standard
  error.}

@defproc[(open-file-with-mode [path String] [mode Integer]) (IO Handle)]{
  Host primitive that opens @racket[path] using an integer mode code; prefer
  @racket[open-file], which maps the typed @racket[IOMode] constructors to it.}

@defproc[(open-file [path String] [mode IOMode]) (IO Handle)]{
  Opens the file at @racket[path] in the given @racket[IOMode], returning a
  @racket[Handle].}

@defproc[(h-close [h Handle]) (IO Unit)]{
  Closes the handle @racket[h].}

@defproc[(h-put-str [h Handle] [s String]) (IO Unit)]{
  Writes the string @racket[s] to the write handle @racket[h].}

@defproc[(h-put-str-ln [h Handle] [s String]) (IO Unit)]{
  Writes the string @racket[s] followed by a newline to the write handle
  @racket[h].}

@defproc[(h-flush [h Handle]) (IO Unit)]{
  Flushes any buffered output on the handle @racket[h].}

@defproc[(h-get-contents [h Handle]) (IO String)]{
  Reads the rest of the handle's input as one @racket[String]
  (Haskell's @tt{hGetContents}).}

@defproc[(h-get-line [h Handle]) (IO (Maybe String))]{
  Reads the next line as @racket[(Some line)], or @racket[None] at end-of-file
  (safer than Haskell's @tt{hGetLine}, which throws at EOF).}

@defthing[get-contents (IO String)]{
  Reads the rest of standard input as one @racket[String]
  (Haskell's @tt{getContents}).}

@defproc[(with-file [path String] [mode IOMode] [action (-> Handle (IO r))]) (IO r)]{
  Opens a handle for @racket[path] in @racket[mode], runs @racket[action], and
  closes the handle even if the action raises (Haskell's @tt{withFile}
  bracket); a captured error is re-raised after closing.}


@section{rackton/system/random}
@defmodule[rackton/system/random #:no-declare]
@declare-exporting[rackton/system/random]

Random number generation in two layers: IO conveniences backed by the
host RNG, and a pure, splittable @racket[StdGen] implementing SplitMix64
(the algorithm Haskell's @tt{random} uses for @tt{StdGen}) in masked
64-bit integer arithmetic, so a seed reproduces a sequence with no IO.
This module requires @racket[rackton/data/bits].

@defproc[(random-integer [lo Integer] [hi Integer]) (IO Integer)]{
  A uniform random @racket[Integer] in the half-open range
  @tt{[lo hi)} (@racket[hi] exclusive; @racket[hi] must be greater
  than @racket[lo]).}

@defthing[random-float (IO Float)]{
  A uniform random @racket[Float] in @tt{[0 1)}.}

@defproc[(random-r-integer [lo Integer] [hi Integer]) (IO Integer)]{
  A uniform @racket[Integer] in the inclusive range @tt{[lo hi]}
  (Haskell's @tt{randomRIO}), built on @racket[random-integer].}

@defproc[(random-r-float [lo Float] [hi Float]) (IO Float)]{
  A uniform @racket[Float] in @tt{[lo hi]}.}

@defidform[#:kind "type & constructor" StdGen]{
  A pure, splittable SplitMix64 generator carrying a seed and an odd
  gamma.
  
    @racket[StdGen : (-> Integer (-> Integer StdGen))] — build a
    generator from a seed and a gamma.}

@defthing[sm-mod Integer]{
  @racket[2^64], the modulus that masks all SplitMix arithmetic.}

@defthing[sm-gamma Integer]{
  The golden-ratio gamma constant @tt{0x9e3779b97f4a7c15}.}

@defthing[sm-c1 Integer]{
  The first SplitMix64 finalizer multiply constant
  @tt{0xbf58476d1ce4e5b9}.}

@defthing[sm-c2 Integer]{
  The second SplitMix64 finalizer multiply constant
  @tt{0x94d049bb133111eb}.}

@defproc[(mask64 [x Integer]) Integer]{
  Reduce @racket[x] modulo @racket[sm-mod] to a 64-bit value.}

@defproc[(sm-mix64 [z0 Integer]) Integer]{
  The SplitMix64 finalizer: avalanche a 64-bit word.}

@defproc[(mk-std-gen [s Integer]) StdGen]{
  Seed a generator from any @racket[Integer] (Haskell's @tt{mkStdGen}).}

@defproc[(next-word [g StdGen]) (Pair Integer StdGen)]{
  The next 64-bit value and the advanced generator.}

@defproc[(random-r [lo Integer] [hi Integer] [g StdGen]) (Pair Integer StdGen)]{
  A uniform @racket[Integer] in the inclusive range @tt{[lo hi]} and
  the advanced generator (slight modulo bias — best-effort).}

@defproc[(split-gen [g StdGen]) (Pair StdGen StdGen)]{
  Two decorrelated generators derived from @racket[g].}


@section{rackton/system/ref}
@defmodule[rackton/system/ref #:no-declare]
@declare-exporting[rackton/system/ref]

Mutable references in @racket[IO], analogous to Haskell's @tt{Data.IORef}.
The @racket[Ref] type is abstract; the runtime primitives are reached via
@racket[foreign] from @racket[rackton/private/prelude-runtime].

@defidform[#:kind "type" Ref]{An abstract, mutable reference cell holding a value of type @racket[a], manipulated only in @racket[IO].}

@defproc[(make-ref [x a]) (IO (Ref a))]{Allocates a new reference holding @racket[x] (Haskell's @tt{newIORef}).}

@defproc[(read-ref [r (Ref a)]) (IO a)]{Reads the current value stored in @racket[r] (Haskell's @tt{readIORef}).}

@defproc[(write-ref [r (Ref a)] [x a]) (IO Unit)]{Replaces the value stored in @racket[r] with @racket[x] (Haskell's @tt{writeIORef}).}


@section{rackton/system/time}
@defmodule[rackton/system/time #:no-declare]
@declare-exporting[rackton/system/time]

Wall-clock and CPU-time access. Each binding is a foreign primitive that
reaches the underlying runtime clock in @tt{rackton/private/prelude-runtime}.

@defthing[current-time-seconds (IO Integer)]{
  Seconds since the Unix epoch.}

@defthing[get-current-time-millis (IO Integer)]{
  Wall-clock milliseconds since the Unix epoch (finer-grained than
  @racket[current-time-seconds]).}

@defthing[get-cpu-time-millis (IO Integer)]{
  CPU milliseconds consumed by this process (Haskell's @tt{getCPUTime}, in
  milliseconds rather than picoseconds).}


