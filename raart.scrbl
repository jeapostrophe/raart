#lang scribble/manual
@(require (for-label raart
                     lux/chaos
                     ansi
                     racket/contract
                     racket/base))

@title{raart: Racket ASCII Art and Interfaces}
@author{Jay McCarthy}

@defmodule[raart]

The @racketmodname[raart] module provides an algebraic model of ASCII
that can be used for art, user interfaces, and diagrams. It is
comparable to @racketmodname[2htdp/image].

Check out some examples in the
@link["https://github.com/jeapostrophe/raart/tree/master/t"]{test}
directory in the source.

@local-table-of-contents[]

@section{Buffers}
@defmodule[raart/buffer]

When drawing, @racketmodname[raart] renders to a @deftech{buffer}. In
almost all circumstances, you should use @racket[make-cached-buffer].

@defproc[(buffer? [x any/c]) boolean?]{Identifiers @tech{buffer}s.}

@defproc[(make-output-buffer [#:output output output-port? (current-output-port)])
         buffer?]{

A @tech{buffer} that displays to @racket[output].

}

@defproc[(make-terminal-buffer [rows exact-nonnegative-integer?]
                               [cols exact-nonnegative-integer?]
                               [#:clear? clear? boolean? #t]
                               [#:output output output-port? (current-output-port)])
         buffer?]{

A @tech{buffer} that displays to a terminal of @racket[rows] rows and
@racket[cols] columns via the port @racket[output]. If @racket[clear?]
is non-false, then the terminal will be cleared before display.

}

@defproc[(make-cached-buffer [rows exact-nonnegative-integer?]
                             [cols exact-nonnegative-integer?]
                             [#:output output output-port? (current-output-port)])
         buffer?]{

A @tech{buffer} that displays to a terminal of @racket[rows] rows and
@racket[cols] columns via the port @racket[output], with minimal
output to the terminal implemented via client-side caching of the
screen content so only updates are output. }

@defthing[color/c contract?]{

A contract that recognizes the ASCII colors from the list
@racket['(black red green yellow blue magenta cyan white brblack brred
brgreen bryellow brblue brmagenta brcyan brwhite)]. The actual color
display depends on the terminal configuration. }

@defthing[style/c contract?]{

A contract that recognizes the ASCII styles from the list
@racket['(normal bold inverse underline)]. The actual font displayed
may depend on the terminal configuration. }

@section{Drawing}
@defmodule[raart/draw]

@racketmodname[raart] represents ASCII art algebraically as an
abstract @racket[raart?] object.

@defproc[(raart? [x any/c]) boolean?]{Identifies ASCII art.}

@defproc[(raart-w [x raart?]) exact-nonnegative-integer?]{Returns the
width of the art.}

@defproc[(raart-h [x raart?]) exact-nonnegative-integer?]{Returns the
height of the art.}

@defproc[(draw [b buffer?] [r raart?]) void?]{Displays @racket[r] to
the @racket[b] buffer.}

@defproc[(draw-here [r raart?]) void?]{Displays @racket[r] with a
freshly created buffer made with @racket[make-output-buffer].}

@defproc[(style [s style/c] [r raart?]) raart?]{@racket[r], except
with the style given by @racket[s].}

@defproc[(fg [c color/c] [r raart?]) raart?]{@racket[r], except with
the foreground color given by @racket[c].}

@defproc[(bg [c color/c] [r raart?]) raart?]{@racket[r], except with
the background color given by @racket[c].}

@defproc[(with-drawing [s (or/c style/c #f)] [fc (or/c color/c #f)]
[bc (or/c color/c #f)] [r raart?]) raart?]{Wraps @racket[r] in calls
to @racket[style], @racket[fg], and @racket[bg] if @racket[s],
@racket[fc], or @racket[bc] (respectively) are provided as non-false.}

@defproc[(blank [w exact-nonnegative-integer? 0] [h
exact-nonnegative-integer? 0]) raart?]{A blank art of width @racket[w]
and height @racket[h].}

@defproc[(char [c (and/c char? (not/c char-iso-control?))]) raart?]{An
art displaying @racket[c].}

@defproc[(text [s string?]) raart?]{An art displaying @racket[s],
which must not contain any @racket[char-iso-control?] characters.}

@defproc[(hline [w exact-nonnegative-integer?]) raart?]{A horizontal
line of @litchar{-} characters of width @racket[w].}

@defproc[(vline [h exact-nonnegative-integer?]) raart?]{A vertical
line of @litchar{|} characters of height @racket[h].}

XXX

@section{lux integration}
@defmodule[raart/lux-chaos]

@racketmodname[raart] provides integration with @racketmodname[lux]
via the @racketmodname[ansi] module.

@defproc[(make-raart [#:mouse? mouse? boolean? #f]) chaos?]{

Returns a @tech[#:doc '(lib "lux/scribblings/lux.scrbl")]{chaos} that
manages the terminal. 

The values that @racket[word-event] is called with are characters or
@racket[screen-size-report] structures. If @racket[mouse?] is
non-false, then @racket[any-mouse-event], @racket[mouse-focus-event],
or @racket[mouse-event] structures may also be provided.

The values that @racket[word-output] should return are @racket[raart?]
objects. The drawing will use @racket[make-cached-buffer] to optimize
the display process.

}
