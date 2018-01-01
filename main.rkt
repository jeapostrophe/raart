#lang racket/base
(require racket/match
         racket/list
         racket/contract/base
         (for-syntax racket/base
                     syntax/parse)
         (prefix-in A: ansi))

(define current-style (make-parameter 'normal))
(define symbol->style
  `#hasheq([normal . ,A:style-normal]
           [bold . ,A:style-bold]
           [inverse . ,A:style-inverse]
           [underline . ,A:style-underline]))
(define current-fg (make-parameter 'default))
(define current-bg (make-parameter 'default))
(define symbol->color
  `#hasheq(
           [black   .  0] [red       .  1] [green   .  2] [yellow   . 3]
           [blue    .  4] [magenta   .  5] [cyan    .  6] [white    . 7]
           [brblack .  8] [brred     .  9] [brgreen . 10] [bryellow . 11]
           [brblue  . 12] [brmagenta . 13] [brcyan  . 14] [brwhite  . 15]))
(define (select-text-color* c)
  (if (eq? c 'default)
    (A:select-graphic-rendition A:style-default-text-color)
    (A:select-xterm-256-text-color (hash-ref symbol->color c))))
(define (select-background-color* c)
  (if (eq? c 'default)
    (A:select-graphic-rendition A:style-default-background-color)
    (A:select-xterm-256-background-color (hash-ref symbol->color c))))
(define (set-drawing-parameters!)
  (display (A:select-graphic-rendition (hash-ref symbol->style (current-style))))
  (display (select-text-color* (current-fg)))
  (display (select-background-color* (current-bg))))

;; w : exact-nonnegative-integer?
;; h : exact-nonnegative-integer?
;; ! : (row col char -> void) row col -> void
(struct rart (w h !))
(define (draw row col x #:clear? [clear? #t])
  (match-define (rart w h !) x)
  (display (A:dec-soft-terminal-reset))  
  (when clear?
    (display (A:clear-screen/home)))
  (set-drawing-parameters!)
  (! (λ (r c ch)
       (display (A:goto r c))
       (display ch))
     row col)
  (display (A:goto (+ row h) (+ col w))))

(define-syntax (with-maybe-parameterize stx)
  (syntax-parse stx
    [(_ () . body) #'(let () . body)]
    [(_ ([p:id v:id] . m) . body)
     #'(let ([t (λ () (with-maybe-parameterize m . body))])
         (if v (parameterize ([p v]) (t)) (t)))]))

(define (style s x) (with-drawing  s #f #f x))
(define (fg    f x) (with-drawing #f  f #f x))
(define (bg    b x) (with-drawing #f #f  b x))
(define (with-drawing s f b x)
  (match-define (rart w h !) x)
  (rart w h (λ (d r c)
              (with-maybe-parameterize ([current-style s]
                                        [current-fg f]
                                        [current-bg b])
                (set-drawing-parameters!)
                (! d r c))
              (set-drawing-parameters!))))

(define (blank [w 0] [h 1])
  (rart w h void))

(define (char ch)
  (when (char-iso-control? ch)
    (error 'char "Illegal character: ~v" ch))
  (rart 1 1 (λ (d r c) (d r c ch))))

(define (text s)
  (happend* (map char (string->list s))))
(define (hline w)
  (happend* (make-list w (char #\─))))
(define (vline h)
  (vappend* (make-list h (char #\│))))

(define (vappend2 y x)
  (match-define (rart xw xh x!) x)
  (match-define (rart yw yh y!) y)
  (unless (= xw yw)
    (error 'vappend2 "Widths must be equal: ~e vs ~e" xw yw))
  (rart xw (+ xh yh)
        (λ (d r c)
          (x! d (+ r  0) c)
          (y! d (+ r xh) c))))
(define (vappend r1 . rs)
  (foldl vappend2 r1 rs))
(define (vappend* rs) (apply vappend rs))

(define (happend2 y x)
  (match-define (rart xw xh x!) x)
  (match-define (rart yw yh y!) y)
  (unless (= xh yh)
    (error 'happend2 "Heights must be equal: ~e vs ~e" xh yh))
  (rart (+ xw yw) xh
        (λ (d r c)
          (x! d r (+ c  0))
          (y! d r (+ c xw)))))
(define (happend r1 . rs)
  (foldl happend2 r1 rs))
(define (happend* rs) (apply happend rs))

(define (place-at back dr dc front)
  (match-define (rart bw bh b!) back)
  (match-define (rart fw fh f!) front)
  (unless (and (<= fw bw) (<= fh bh))
    (error 'place-at "Foreground must fit inside background"))
  (rart bw bh
        (λ (d r c)
          (b! d r c)
          (f! d (+ r dr) (+ c dc)))))
(define-syntax (place-at* stx)
  (syntax-parse stx
    [(_ b:expr) #'b]
    [(_ b:expr [dr:expr dc:expr f:expr] . more:expr)
     #'(place-at* (place-at b dr dc f) . more)]))

(define (frame #:style [s #f] #:fg [f #f] #:bg [b #f] x)
  (match-define (rart w h _) x)
  (place-at
   (with-drawing s f b
     (vappend
      (happend (char #\┌) (hline w  ) (char #\┐))
      (happend (vline  h) (blank w h) (vline  h))
      (happend (char #\└) (hline w  ) (char #\┘))))
   1 1 x))

(define (matte-at mw mh @c @r x)
  (match-define (rart xw xh x!) x)
  (unless (and (<= (+ xw @c) mw)
               (<= (+ xh @r) mh))
    (error 'matte-at "Original (~ax~a) must fit inside matte (~ax~a)"
           xw xh mw mh))
  (place-at (blank mw mh) @r @c x))

(define (translate dr dc x)
  (match-define (rart xw xh x!) x)
  (matte-at (+ xw dc) (+ xh dr) dc dr x))

(define (matte w h
               #:halign [ws 'center]
               #:valign [hs 'center]
               x)
  (match-define (rart xw xh x!) x)
  (unless (and (<= xw w) (<= xh h))
    (error 'matte "Original (~ax~a) must fit inside matte (~ax~a)"
           xw xh w h))
  (matte-at w h
            (match ws
              ['left   0]
              ['center (floor (/ (- w xw) 2))]
              ['right  (- w xw)])
            (match hs
                ['top    0]
                ['center (floor (/ (- h xh) 2))]
                ['bottom (- h xh)])
            x))

(define (inset dw dh x)
  (match-define (rart w h !) x)
  (matte (+ dw w dw) (+ dh h dh)
         #:halign 'center #:valign 'center
         x))

(define (mask mc mw mr mh x)
  (match-define (rart xw xh x!) x)
  (rart xw xh
        (λ (d r c)
          (x!
           (λ (r c ch)
             (when (and (<= mr r (+ mr mh))
                        (<= mc c (+ mc mw)))
               (d r c ch)))
           r c))))

(define (crop cc cw cr ch x)
  (match-define (rart mw mh m!) (mask cc cw cr ch x))
  (rart cw ch
        (λ (d r c)
          (m! (λ (r c ch)
                (d (- r cr) (- c cc) ch))
              r c))))

(module+ test
  (draw 1 1
        (crop 70 80 10 20
              (matte 80 20
                     #:halign 'right
                     (fg 'blue
                         (frame #:fg 'red
                                (inset
                                 4 5
                                 (happend (style 'underline (text "Left"))
                                          (blank 4)
                                          (style 'bold (text "Right")))))))))
  (newline))

(define (table rows
               #:frames? [frames? #t]
               #:style [s #f] #:fg [f #f] #:bg [b #f]
               #:inset-dw [dw 0]
               #:inset-dh [dh 0]
               #:valign [row-valign 'top]
               #:halign [halign 'left])
  (define (list-ref* i l)
    (cond
      [(not (pair? l)) l]
      [(zero? i) (first l)]
      [else (list-ref* (sub1 i) (rest l))]))
  (define (col-halign-sel i halign)
    (match halign
      [(? symbol?) halign]
      [(? list?) (list-ref* i halign)]))
  (define (col-halign col-i)
    (col-halign-sel col-i halign))
  (define col-ws
    (for/list ([i (in-range (length (first rows)))])
      (define col (map (λ (r) (list-ref r i)) rows))
      (apply max (map rart-w col))))
  (define last-col (sub1 (length col-ws)))

  (define (make-bar left middle right)
    (happend*
     (cons
      (char left)
      (for/list ([col-w (in-list col-ws)]
                 [col-i (in-naturals)])
        (happend (hline (+ dw col-w dw))
                 (if (= last-col col-i)
                   (char right)
                   (char middle)))))))

  (define header (make-bar #\┌ #\┬ #\┐))
  (define footer (make-bar #\└ #\┴ #\┘))
  (define inbetween (make-bar #\├ #\┼ #\┤))
  (define last-row (sub1 (length rows)))
  (vappend*
   (for/list ([row (in-list rows)]
              [row-i (in-naturals)])
     (define row-h (apply max (map rart-h row)))
     (define cell-h (+ dh row-h dh))
     (define cell-wall (vline cell-h))
     (define the-row
       (happend*
        (for/list ([col (in-list row)]
                   [col-w (in-list col-ws)]
                   [col-i (in-naturals)])
          (define cell-w (+ dw col-w dw))
          (define the-cell
            (matte cell-w #:halign (col-halign col-i)
                   cell-h #:valign row-valign
                   (inset dw dh col)))
          (define cell+left
            (happend cell-wall the-cell))
          (if (= col-i last-col)
            (happend cell+left cell-wall)
            cell+left))))
     (define include-header? (zero? row-i))
     (define row-and-above
       (if include-header? (vappend header the-row) the-row))
     (define include-footer? (= row-i last-row))
     (define row-and-below
       (vappend row-and-above
                (if include-footer?
                  footer
                  inbetween)))
     row-and-below)))
(define (text-rows rows)
  (local-require racket/format)
  (for/list ([row (in-list rows)])
    (for/list ([col (in-list row)])
      (if (rart? col) col (text (~a col))))))

;; xxx render xexpr-like thing
;; xxx text... (fit text inside a width)
;; xxx paragraph (fit text inside a box)

(module+ test
  (draw 1 1
        (translate
         2 10
         (table
          #:frames? #t
          #:inset-dw 2
          #:valign 'center
          #:halign '(right left left left)
          (text-rows
           `([  "ID" "First Name" "Last Name" "Grade"]
             [70022  "John"       "Smith"     "A+"]
             [   22  "Macumber"   "Stark"     "B"]
             [ 1223  "Sarah"      ,(vappend (text "Top")
                                            (text "Mid")
                                            (text "Bot")) "C"])))))
  (newline))

(provide rart?
         draw
         style fg bg with-drawing
         blank char text
         hline vline
         vappend2 vappend
         happend2 happend
         place-at place-at*
         frame
         inset matte-at matte translate
         table text-rows)
