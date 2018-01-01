#lang racket/base
(require racket/match
         racket/contract/base
         (for-syntax racket/base
                     syntax/parse)
         (prefix-in A: ansi))

(define (A:style-text-color* c)
  (if (eq? c 'default)
    A:style-default-text-color
    (A:style-text-color (hash-ref symbol->color c))))
(define (A:style-background-color* c)
  (if (eq? c 'default)
    A:style-default-background-color
    (A:style-background-color (hash-ref symbol->color c))))

(define current-style (make-parameter 'normal))
(define current-fg (make-parameter 'default))
(define current-bg (make-parameter 'default))
(define (set-drawing-parameters!)
  (display
   (A:select-graphic-rendition
    (hash-ref symbol->style (current-style))
    (A:style-text-color* (current-fg))
    (A:style-background-color* (current-bg)))))

(define symbol->style
  `#hasheq([normal . ,A:style-normal]
           [bold . ,A:style-bold]
           [inverse . ,A:style-inverse]
           [underline . ,A:style-underline]))
(define symbol->color
  `#hasheq([black . ,A:color-black]
           [red . ,A:color-red]
           [green . ,A:color-green]
           [yellow . ,A:color-yellow]
           [blue . ,A:color-blue]
           [magenta . ,A:color-magenta]
           [cyan . ,A:color-cyan]
           [white . ,A:color-white]))

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
    [(_ () e) #'e]
    [(_ ([p:id v:id] . m) e)
     #'(let ([t (λ () (with-maybe-parameterize m e))])
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
                (begin
                  (set-drawing-parameters!)
                  (! r c)))
              (set-drawing-parameters!))))

(define (blank [w 0] [h 1])
  (rart w h void))

(define (char ch)
  (rart 1 1 (λ (r c) (display (A:goto r c)) (display ch))))

;; XXX What if s contains a newline?
(define (text s)
  (rart (string-length s) 1
        (λ (r c)
          (display (A:goto r c))
          (display s))))

(define (hline w)
  (rart w 1
        (λ (r c)
          (display (A:goto r c))
          (for ([i (in-range w)])
            (display #\─)))))
(define (vline h)
  (rart 1 h
        (λ (r c)
          (for ([i (in-range h)])
            (display (A:goto (+ r i) c))
            (display #\│)))))

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

(define (place-at back dr dc front)
  (match-define (rart bw bh b!) back)
  (match-define (rart fw fh f!) front)
  (unless (and (<= fw bw) (<= fh bh))
    (error 'place-at "Foreground must fit inside background"))
  (rart bw bh
        (λ (r c)
          (b! r c)
          (f! (+ r dr) (+ c dc)))))

(define (frame r #:style [s #f] #:fg [f #f] #:bg [b #f])
  (match-define (rart w h _) r)
  (place-at
   (with-drawing s f b
     (vappend
      (happend (char #\┌) (hline w  ) (char #\┐))
      (happend (vline  h) (blank w h) (vline  h))
      (happend (char #\└) (hline w  ) (char #\┘))))
   1 1 r))
(module+ test
  (draw 10 10
        (fg 'blue
            (frame #:fg 'red
                   (happend (style 'underline (text "Left"))
                            (blank 4)
                            (style 'bold (text "Right"))))))
  (newline))

(provide rart?
         draw
         style fg bg with-drawing
         blank char text
         hline vline
         vappend1 vappend
         happend1 happend
         place-at
         frame)
