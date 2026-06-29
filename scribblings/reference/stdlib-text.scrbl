#lang scribble/manual
@require[scribble/manual
         (for-label rackton rackton/text/bytes rackton/text/printf rackton/text/read rackton/text/show rackton/text/string)]

@title[#:tag "stdlib-text" #:style 'toc]{@tt{rackton/text} — strings, bytes, and formatting}

The @tt{text} family handles textual and binary data and its
rendering.  It provides @racketmodname[rackton/text/string] operations
beyond the prelude, byte vectors (@racketmodname[rackton/text/bytes]),
@racketmodname[rackton/text/printf]-style formatting, the
@racketmodname[rackton/text/show] rendering helpers, and
@racketmodname[rackton/text/read] parsing of values from strings.

@local-table-of-contents[]

@section{rackton/text/bytes}
@defmodule[rackton/text/bytes]

Derived @racket[Bytes] operations in the style of @tt{Data.ByteString}. The
prelude ships the @racket[Bytes] type with its primitive operations
(@tt{bytes-length}, @tt{bytes-ref}, @tt{bytes-append},
@tt{bytes->list}, @tt{list->bytes}, etc.); these combinators are
expressed over the list round-trip and @racket[take]/@racket[drop] from
@racket[rackton/data/list].

@defthing[bytes-empty Bytes]{The empty byte string.}

@defproc[(bytes-null? [b Bytes]) Boolean]{Is the byte string empty?}

@defproc[(bytes-take [n Integer] [b Bytes]) Bytes]{The first @racket[n] bytes.}

@defproc[(bytes-drop [n Integer] [b Bytes]) Bytes]{All but the first @racket[n] bytes.}

@defproc[(bytes-split [n Integer] [b Bytes]) (Pair Bytes Bytes)]{Split into the first @racket[n] bytes and the rest — Haskell's @tt{splitAt}.}

@defproc[(bytes-concat [bss (List Bytes)]) Bytes]{Concatenate a list of byte strings.}

@defproc[(bytes->string-lossy [b Bytes]) String]{UTF-8 decode that never
fails — bytes that are not valid UTF-8 become the Unicode replacement char.
Complements the prelude's strict @racket[bytes->string] (which returns
@racket[(Maybe String)]); useful when a String rendering is wanted even for
not-quite-text input.}


@section{rackton/text/printf}
@defmodule[rackton/text/printf]

Type-safe string formatting in the @tt{Text.Printf} tradition, done the
Hindley–Milner way: instead of a runtime-parsed @racket["%d %s"] format
string, a format is built from typed directives and composed, so the
argument-consuming function's type is inferred and checked at compile time
(the "functional unparsing" technique). A wrong argument type or arity is a
compile error, never a runtime format mismatch.

@defidform[#:kind "type" Format]{The type alias @racket[(Format r a)] expands
to @racket[(-> (-> String r) a)]: given a continuation that consumes the
rendered text and produces @racket[r], it returns @racket[a], where @racket[a]
accumulates one curried parameter per value directive.}

@defproc[(fmt-lit [s String]) (Format r r)]{A directive for literal text that
consumes no argument.}

@defthing[fmt-int (Format r (-> Integer r))]{A directive that consumes an
@racket[Integer], rendered with @racket[show].}

@defthing[fmt-flt (Format r (-> Float r))]{A directive that consumes a
@racket[Float], rendered with @racket[show].}

@defthing[fmt-str (Format r (-> String r))]{A directive that consumes a
@racket[String], inserted verbatim with no quoting (contrast @racket[fmt-show]).}

@defthing[fmt-show ((Show a) => (Format r (-> a r)))]{A directive that consumes
any @racket[Show]-able value, rendered with @racket[show] (so @racket[String]s
come out quoted).}

@defproc[(fmt-cat [f (Format b c)] [g (Format a b)]) (Format a c)]{Concatenates
two formats; the combined format takes the arguments of @racket[f] then those
of @racket[g].}

@defproc[(sprintf [fmt (Format String a)]) a]{Runs a format with the identity
continuation, collecting the rendered pieces into the final @racket[String].}


@section{rackton/text/read}
@defmodule[rackton/text/read]

Parse @racket[String]s back into typed values, the Rackton analogue of Haskell's @tt{Text.Read}. Rackton has no @tt{Read} protocol, so these are type-specific readers that each return @racket[(Maybe a)], yielding @racket[None] when the string does not parse.

@defproc[(read-int [s String]) (Maybe Integer)]{
  Parses a decimal @racket[Integer], reusing the prelude's @racket[string->integer] primitive.}

@defproc[(read-float [s String]) (Maybe Float)]{
  Parses any real number as a @racket[Float]; an integer string reads as a @racket[Float] too, matching Haskell's @tt{readMaybe @"@"Double "5" == Just 5.0}.}

@defproc[(read-bool [s String]) (Maybe Boolean)]{
  Parses a @racket[Boolean] written as @racket["True"] or @racket["False"], round-tripping the prelude's @racket[show] for @racket[Boolean].}


@section{rackton/text/show}
@defmodule[rackton/text/show]

Text.Show's @racket[ShowS] machinery: freestanding combinators for building
string output by composing prepend-functions rather than repeatedly
@racket[mappend]-ing strings, which keeps concatenation linear. The
@racket[Show] protocol and @racket[show] itself live in the prelude; only the
combinators are defined here.

@defidform[#:kind "type" ShowS]{
  Type alias for @racket[(-> String String)]: a function that prepends some
  text to a continuation string.}

@defproc[(show-string [str String]) ShowS]{
  Returns a @racket[ShowS] that prepends the literal string @racket[str].}

@defproc[(show-char [c Char]) ShowS]{
  Returns a @racket[ShowS] that prepends the single character @racket[c].}

@defproc[(shows [x a]) ShowS]{
  Returns a @racket[ShowS] that prepends the shown form of any @racket[Show]able
  value (Haskell's @tt{shows}); requires @racket[(Show a)].}

@defproc[(show-paren [b Boolean] [p ShowS]) ShowS]{
  Wraps the @racket[ShowS] @racket[p] in parentheses when @racket[b] is true,
  for precedence-aware showing (Haskell's @tt{showParen}).}

@defproc[(run-shows [f ShowS]) String]{
  Runs a @racket[ShowS] against the empty continuation, yielding the resulting
  string (Haskell's @tt{($ "")}).}


@section{rackton/text/string}
@defmodule[rackton/text/string]

@tt{Data.String} / @tt{Data.Text}-style operations over the prelude's
@racket[String] type, built on the prelude's string and char operations and on
@racket[drop-while] from @racketmodname[rackton/data/list]. Length, substring,
appending, @racket[string-prefix?], @racket[string-split], @racket[string-join],
and the char conversions live in the prelude.

@defproc[(null-string? [s String]) Boolean]{Is the string empty?}

@defproc[(string-append* [s String] ...) String]{
  Variadic concatenation: join any number of strings.  Collapses what
  would otherwise be a chain of nested binary @racket[string-append]
  calls.}

@defproc[(reverse-string [s String]) String]{Reverse the characters of a string.}

@deftogether[(@defproc[(to-upper-string [s String]) String]
              @defproc[(to-lower-string [s String]) String])]{
  Map @tt{char-upcase} / @tt{char-downcase} over every character.}

@deftogether[(@defproc[(strip-start [s String]) String]
              @defproc[(strip-end [s String]) String]
              @defproc[(strip [s String]) String])]{
  Drop leading whitespace, trailing whitespace, or both.}

@defproc[(split-keep [c Char] [s String]) (List String)]{
  Split on every occurrence of @racket[c], keeping empty segments
  (@racket[n] separators yield @racket[n+1] segments).}

@defproc[(lines [s String]) (List String)]{
  Split on newlines; a single trailing newline yields no extra empty line,
  matching Haskell @tt{lines}.}

@defproc[(words [s String]) (List String)]{
  Split on runs of whitespace, dropping empty pieces.}

@defproc[(unwords [ws (List String)]) String]{Join strings with single spaces.}

@defproc[(unlines [ls (List String)]) String]{
  Append a newline after each line, matching Haskell @tt{unlines}.}

@defproc[(chars-prefix? [p (List Char)] [s (List Char)]) Boolean]{
  Is the first char list a prefix of the second?}

@deftogether[(@defproc[(is-prefix? [p String] [s String]) Boolean]
              @defproc[(is-suffix? [p String] [s String]) Boolean])]{
  Does @racket[p] occur at the start / end of @racket[s]?}

@defproc[(chars-infix? [needle (List Char)] [s (List Char)]) Boolean]{
  Does @racket[needle] occur anywhere in the char list @racket[s]?}

@defproc[(is-infix? [needle String] [s String]) Boolean]{
  Does @racket[needle] occur anywhere in @racket[s]?}

@deftogether[(@defproc[(take-string [n Integer] [s String]) String]
              @defproc[(drop-string [n Integer] [s String]) String])]{
  The first @racket[n] characters, or all but the first @racket[n] characters.}

@deftogether[(@defproc[(pad-left [w Integer] [c Char] [s String]) String]
              @defproc[(pad-right [w Integer] [c Char] [s String]) String])]{
  Pad to width @racket[w] by prepending / appending copies of @racket[c]
  (no-op if @racket[s] is already at least that wide).}

@defproc[(repeat-string [n Integer] [s String]) String]{
  Concatenate @racket[n] copies of @racket[s].}

@defproc[(replace-chars [from (List Char)] [to (List Char)] [s (List Char)]) (List Char)]{
  Replace every occurrence of the char list @racket[from] with @racket[to].}

@defproc[(replace [from String] [to String] [s String]) String]{
  Replace every non-empty occurrence of substring @racket[from] with @racket[to].}

@defproc[(break-on-chars [sep (List Char)] [s (List Char)]) (Pair (List Char) (List Char))]{
  Break at the first occurrence of @racket[sep] into (chars-before, chars-from-sep);
  the second component is @racket[Nil] when @racket[sep] is absent.}

@defproc[(break-on [needle String] [s String]) (Pair String String)]{
  Split at the first occurrence of @racket[needle]; the second component starts with
  @racket[needle], or is empty when @racket[needle] is absent.}

@defproc[(split-on-chars [sep (List Char)] [s (List Char)]) (List (List Char))]{
  Split a char list on every occurrence of @racket[sep], keeping empty segments.}

@defproc[(split-on [sep String] [s String]) (List String)]{
  Split on every occurrence of @racket[sep], keeping empty segments; an empty
  @racket[sep] yields the whole string as one segment.}

@defproc[(index-of-chars [i Integer] [needle (List Char)] [s (List Char)]) (Maybe Integer)]{
  Index (offset by @racket[i]) of the first occurrence of @racket[needle], or
  @racket[None]; an empty needle matches at index 0.}

@defproc[(index-of [needle String] [s String]) (Maybe Integer)]{
  Index of the first occurrence of @racket[needle] in @racket[s], or @racket[None].}


