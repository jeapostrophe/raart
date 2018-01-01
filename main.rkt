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
;; ! : row col -> void
(struct rart (w h !))
(define (draw row col r)
  (match-define (rart w h !) r)
  (display (A:dec-soft-terminal-reset))
  (display (A:clear-screen/home))
  (set-drawing-parameters!)
  (! row col)
  (display (A:goto (+ row h) (+ col w))))

(define-syntax (with-maybe-parameterize stx)
  (syntax-parse stx
    [(_ () . body) #'(let () . body)]
    [(_ ([p:id v:id] . m) . body)
     #'(let ([t (λ () (with-maybe-parameterize m . body))])
         (if v (parameterize ([p v]) (t)) (t)))]))

(define (style s r) (with-drawing  s #f #f r))
(define (fg    f r) (with-drawing #f  f #f r))
(define (bg    b r) (with-drawing #f #f  b r))
(define (with-drawing s f b r)
  (match-define (rart w h !) r)
  (rart w h (λ (r c)
              (with-maybe-parameterize ([current-style s]
                                        [current-fg f]
                                        [current-bg b])
                (set-drawing-parameters!)
                (! r c))
              (set-drawing-parameters!))))

(define (blank [w 0] [h 1])
  (rart w h void))

;; XXX What if ch is a newline?
(define (char ch)
  (when (char-iso-control? ch)
    (error 'char "Illegal character: ~v" ch))
  (rart 1 1 (λ (r c) (display (A:goto r c)) (display ch))))

(define (text s)
  (happend* (map char (string->list s))))
(define (hline w)
  (happend* (make-list w (char #\─))))
(define (vline h)
  (vappend* (make-list h (char #\│))))

(define (vappend1 y x)
  (match-define (rart xw xh x!) x)
  (match-define (rart yw yh y!) y)
  (unless (= xw yw)
    (error 'vappend1 "Widths must be equal: ~e vs ~e" xw yw))
  (rart xw (+ xh yh)
        (λ (r c)
          (x! (+ r  0) c)
          (y! (+ r xh) c))))
(define (vappend r1 . rs)
  (foldl vappend1 r1 rs))
(define (vappend* rs) (apply vappend rs))

(define (happend1 y x)
  (match-define (rart xw xh x!) x)
  (match-define (rart yw yh y!) y)
  (unless (= xh yh)
    (error 'vappend1 "Heights must be equal: ~e vs ~e" xh yh))
  (rart (+ xw yw) xh
        (λ (r c)
          (x! r (+ c  0))
          (y! r (+ c xw)))))
(define (happend r1 . rs)
  (foldl happend1 r1 rs))
(define (happend* rs) (apply happend rs))

(define (place-at back dr dc front)
  (match-define (rart bw bh b!) back)
  (match-define (rart fw fh f!) front)
  (unless (and (<= fw bw) (<= fh bh))
    (error 'place-at "Foreground must fit inside background"))
  (rart bw bh
        (λ (r c)
          (b! r c)
          (f! (+ r dr) (+ c dc)))))
(define-syntax (place-at* stx)
  (syntax-parse stx
    [(_ b:expr) #'b]
    [(_ b:expr [dr:expr dc:expr f:expr] . more:expr)
     #'(place-at* (place-at b dr dc f) . more)]))

(define (frame #:style [s #f] #:fg [f #f] #:bg [b #f] r)
  (match-define (rart w h _) r)
  (place-at
   (with-drawing s f b
     (vappend
      (happend (char #\┌) (hline w  ) (char #\┐))
      (happend (vline  h) (blank w h) (vline  h))
      (happend (char #\└) (hline w  ) (char #\┘))))
   1 1 r))

(define (inset dw dh r)
  (match-define (rart w h !) r)
  (rart (+ dw w dw) (+ dh h dh)
        (λ (r c)
          (! (+ r dh) (+ c dw)))))

(define (scale w h
               #:ws [ws 'center]
               #:hs [hs 'middle]
               r)
  (match-define (rart rw rh r!) r)
  (unless (and (<= rw w) (<= rh h))
    (error 'scale "Original (~ax~a) must fit inside scaled (~ax~a)"
           rw rh w h))
  (rart w h
        (λ (r c)
          (r! (match hs
                ['top r]
                ['middle (+ r (floor (/ (- h rh) 2)))]
                ['bottom (+ r (- h rh))])
              (match ws
                ['left c]
                ['center (+ c (floor (/ (- w rw) 2)))]
                ['right (+ c (- w rw))])))))

;; xxx table (with optional framing)
;; xxx mask (select a piece of a largest image)
;; xxx render xexpr-like thing
;; xxx text... (fit text inside a width)
;; xxx paragraph (fit text inside a box)

(module+ test
  (draw 1 1
        (scale 80 20
               #:ws 'right
               (fg 'blue
                   (frame #:fg 'red
                          (inset
                           4 5
                           (happend (style 'underline (text "Left"))
                                    (blank 4)
                                    (style 'bold (text "Right"))))))))
  (newline))

(provide rart?
         draw
         style fg bg with-drawing
         blank char text
         hline vline
         vappend1 vappend
         happend1 happend
         place-at place-at*
         frame
         inset
         scale)
