#lang scribble/manual
@(require (for-label raart
                     lux/chaos
                     ansi
                     racket/format
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

@defthing[halign/c contract?]{A contract for the horizontal alignment modes @racket['(left center right)]. @racket['left] means that the art will be extended with blanks to the right@";" @racket['center] places the blanks equally on both sides@";" and @racket['right] places the blanks to the left.}

@defthing[valign/c contract?]{A contract for the vertical alignment modes @racket['(top center bottom)]. @racket['top] means that the art will be extended with blanks below";" @racket['center] places the blanks equally on both sides@";" and @racket['bottom] places the blanks above.}

@defproc[(vappend2 [y raart?] [x raart?]
[#:halign halign (or/c halign/c #f) #f]
[#:reverse? reverse? boolean? #f]) raart?]{
Renders @racket[y] vertically above @racket[x] (although, if
@racket[reverse?] is true, then the reverse) using @racket[halign] to
determine the horizontal alignment. If @racket[halign] is @racket[#f],
then the arts must have the same width. }

@defproc[(vappend [y raart?] [x raart?] ... [#:halign halign (or/c
halign/c #f) #f] [#:reverse? reverse? boolean? #f]) raart?]{Like
@racket[vappend2], but for many arguments.}

@defproc[(vappend* [y-and-xs (non-empty-listof raart?)] [#:halign
halign (or/c halign/c #f) #f] [#:reverse? reverse? boolean? #f]) raart?]{Like
@racket[vappend], but for accepts arguments as a list.}

@defproc[(happend2 [y raart?] [x raart?]  [#:valign valign (or/c
valign/c #f) #f] [#:reverse? reverse? boolean? #f]) raart?]{ Renders
@racket[y] horizontally to the left of @racket[x] (although, if
@racket[reverse?] is true, then the reverse) using @racket[valign] to
determine the vertical alignment. If @racket[valign] is @racket[#f],
then the arts must have the same height.}

@defproc[(happend [y raart?] [x raart?] ... [#:valign valign (or/c
valign/c #f) #f] [#:reverse? reverse? boolean? #f]) raart?]{Like
@racket[happend2], but for many arguments.}

@defproc[(happend* [y-and-xs (non-empty-listof raart?)] [#:valign valign (or/c
valign/c #f) #f] [#:reverse? reverse? boolean? #f]) raart?]{Like
@racket[happend], but for accepts arguments as a list.}

@defproc[(place-at [back raart?]  [dr exact-nonnegative-integer?]  [dh
exact-nonnegative-integer?]  [front raart?]) raart?]{Renders
@racket[front] on top of @racket[back] offset by @racket[dr] rows and
@racket[dh] columns.}

@defform[(place-at* back [dr dc fore] ...)  #:contracts ([back raart?]
[dr exact-nonnegative-integer?] [dc exact-nonnegative-integer?] [fore
raart?])]{Calls @racket[place-at] on a sequence of art objects from
back on the left to front on the right.}

@defproc[(frame [#:style s (or/c style/c #f) #f] [#:fg fc (or/c
color/c #f)] [#:bg bc (or/c color/c #f)] [x raart?]) raart?]{Renders
@racket[x] with a frame where the frame character's style is
controlled by @racket[s], @racket[fc], and @racket[bc].}

@defproc[(matte-at [mw exact-nonnegative-integer?] [mh
exact-nonnegative-integer?] [c exact-nonnegative-integer?] [r
exact-nonnegative-integer?] [x raart?]) raart?]{Mattes @racket[x]
inside a blank of size @racket[mw] columns and @racket[mh] rows at row
@racket[r] and column @racket[c].}

@defproc[(translate [dr exact-nonnegative-integer?] [dc
exact-nonnegative-integer?] [x raart?]) raart?]{Translates @racket[x]
by @racket[dr] rows and @racket[dc] columns.}

@defproc[(matte [w exact-nonnegative-integer?] [h
exact-nonnegative-integer?] [#:halign halign halign/c 'center]
[#:valign valign valign/c 'center] [x raart?]) raart?]{Mattes
@racket[x] inside a blank of size @racket[w]x@racket[h] with the given
alignment.}

@defproc[(inset [dw exact-nonnegative-integer?] [dh
exact-nonnegative-integer?] [x raart?]) raart?]{Insets @racket[x] with
@racket[dw] columns and @racket[dh] rows of blanks.}

@defproc[(mask [mc exact-nonnegative-integer?] [mw
exact-nonnegative-integer?] [mr exact-nonnegative-integer?] [mh
exact-nonnegative-integer?] [x raart?]) raart?]{Renders the portion of
@racket[x] inside the rectangle (@racket[mc],@racket[mr])
to (@racket[(+ mc mw)],@racket[(+ mr mh)]).}

@defproc[(crop [cc exact-nonnegative-integer?] [cw
exact-nonnegative-integer?] [cr exact-nonnegative-integer?] [ch
exact-nonnegative-integer?] [x raart?]) raart?]{Renders the portion of
@racket[x] inside the rectangle (@racket[cc],@racket[cr])
to (@racket[(+ cc cw)],@racket[(+ cr ch)]) and removes the surrounding
blanks.}

@defproc[(table [cells (listof (listof raart?))] [#:frames? frames?
boolean? #t] [#:style s (or/c style/c #f) #f] [#:fg f (or/c color/c
#f) #f] [#:bg b (or/c color/c #f) #f] [#:inset-dw dw
exact-nonnegative-integer? 0] [#:inset-dh dh
exact-nonnegative-integer? 0] [#:valign row-valign valign/c 'top]
[#:halign halign (or/c halign/c (list*of halign/c (or/c halign/c
'()))) 'left]) raart?]{Renders a table of cells where frames are added
if @racket[frames?] is non-false with style and color given by the
arguments. Cells are inset by @racket[inset-dh] rows and
@racket[inset-dw] columns. Cells are horizontally aligned with
@racket[halign]. Rows are vertically aligned with
@racket[row-valign].}

@defproc[(text-rows [cells (listof (listof any/c))]) (listof (listof
raart?))]{Transforms a matrix of content into a matrix of art objects,
using @racket[~a] composed with @racket[text] if they are not already
art objects.}

@defproc[(if-drawn [f (-> exact-nonnegative-integer?
exact-nonnegative-integer? exact-nonnegative-integer?
exact-nonnegative-integer? any)] [x raart?]) raart?]{Renders
@racket[x] and if it ends up being displayed, then calls @racket[f]
with the actual bounding box, given as a row, column, width, and
height.}

@defproc[(place-cursor-after [x raart?] [cr
exact-nonnegative-integer?] [ch exact-nonnegative-integer?])
raart?]{Renders @racket[x] but places the cursor at row @racket[cr]
and column @racket[ch] afterwards.}

@defproc[(without-cursor [x raart?]) raart?]{Renders @racket[x], but
signals to @racket[draw] to not display the cursor, if this is the art
object given to it. (That is, this has no effect if composed with
other drawing operations.)}

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
