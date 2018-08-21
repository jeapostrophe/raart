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
