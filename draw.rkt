#lang racket/base
(require racket/match
         racket/list
         racket/contract/base
         (for-syntax racket/base
                     syntax/parse)
         "buffer.rkt")

(define (strict-or a b) (or a b))

(define current-style (make-parameter 'normal))
(define current-fg (make-parameter #f))
(define current-bg (make-parameter #f))

;; w : exact-nonnegative-integer?
;; h : exact-nonnegative-integer?
;; ! : (row col char -> void) row col -> bool
(struct raart (w h !))

(define (draw buf x)
  (match-define (raart w h !) x)
  (define draw-char! (buffer-start! buf h w))
  (! (λ (r c ch)
       (draw-char! (current-style) (current-fg) (current-bg)
                   r c ch)
       #t)
     0 0)
  (buffer-commit! buf))

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
  (match-define (raart w h !) x)
  (raart w h (λ (d r c)
               (with-maybe-parameterize ([current-style s]
                                         [current-fg f]
                                         [current-bg b])
                 (! d r c)))))

(define (blank [w 0] [h 1])
  (raart w h void))

(define (char ch)
  (when (char-iso-control? ch)
    (error 'char "Illegal character: ~v" ch))
  (raart 1 1 (λ (d r c) (d r c ch))))

(define (place-at back dr dc front)
  (match-define (raart bw bh b!) back)
  (match-define (raart fw fh f!) front)
  (unless (and (<= fw bw) (<= fh bh))
    (error 'place-at "Foreground must fit inside background"))
  (raart bw bh
         (λ (d r c)
           (strict-or
            (b! d r c)
            (f! d (+ r dr) (+ c dc))))))
(define-syntax (place-at* stx)
  (syntax-parse stx
    [(_ b:expr) #'b]
    [(_ b:expr [dr:expr dc:expr f:expr] . more:expr)
     #'(place-at* (place-at b dr dc f) . more)]))

(define (matte-at mw mh @c @r x)
  (match-define (raart xw xh x!) x)
  (unless (and (<= (+ xw @c) mw)
               (<= (+ xh @r) mh))
    (error 'matte-at "Original (~ax~a@~a,~a) must fit inside matte (~ax~a)"
           xw xh @c @r mw mh))
  (place-at (blank mw mh) @r @c x))

(define (translate dr dc x)
  (match-define (raart xw xh x!) x)
  (matte-at (+ xw dc) (+ xh dr) dc dr x))

(define (matte w h
               #:halign [ws 'center]
               #:valign [hs 'center]
               x)
  (match-define (raart xw xh x!) x)
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
  (match-define (raart w h !) x)
  (matte (+ dw w dw) (+ dh h dh)
         #:halign 'center #:valign 'center
         x))

(define (mask mc mw mr mh x)
  (match-define (raart xw xh x!) x)
  (raart xw xh
         (λ (d r c)
           (x!
            (λ (r c ch)
              (and (<= mr r) (< r (+ mr mh))
                   (<= mc c) (< c (+ mc mw))
                   (d r c ch)))
            r c))))

(define (crop cc cw cr ch x)
  (match-define (raart mw mh m!) (mask cc cw cr ch x))
  (raart cw ch
         (λ (d r c)
           (m! (λ (r c ch)
                 (d (- r cr) (- c cc) ch))
               r c))))

(define (*vappend2 y x)
  (match-define (raart xw xh x!) x)
  (match-define (raart yw yh y!) y)
  (unless (= xw yw)
    (error '*vappend2 "Widths must be equal: ~e vs ~e" xw yw))
  (raart xw (+ xh yh)
         (λ (d r c)
           (strict-or
            (x! d (+ r  0) c)
            (y! d (+ r xh) c)))))
(define (vappend2 y x #:halign [halign #f])
  (cond
    [(not halign) (*vappend2 y x)]
    [else
     (match-define (raart xw xh x!) x)
     (match-define (raart yw yh y!) y)
     (define nw (max xw yw))
     (define xp (matte nw xh #:halign halign x))
     (define yp (matte nw yh #:halign halign y))
     (*vappend2 yp xp)]))
(define (vappend #:halign [halign #f] r1 . rs)
  (foldl (λ (a d) (vappend2 #:halign halign a d)) r1 rs))
(define (vappend* #:halign [halign #f] rs)
  (apply vappend rs #:halign halign))

(define (*happend2 y x)
  (match-define (raart xw xh x!) x)
  (match-define (raart yw yh y!) y)
  (unless (= xh yh)
    (error '*happend2 "Heights must be equal: ~e vs ~e" xh yh))
  (raart (+ xw yw) xh
         (λ (d r c)
           (strict-or
            (x! d r (+ c  0))
            (y! d r (+ c xw))))))
(define (happend2 y x #:valign [valign #f])
  (cond
    [(not valign) (*happend2 y x)]
    [else
     (match-define (raart xw xh x!) x)
     (match-define (raart yw yh y!) y)
     (define nh (max xh yh))
     (define xp (matte xw nh #:valign valign x))
     (define yp (matte yw nh #:valign valign y))
     (*happend2 yp xp)]))
(define (happend #:valign [valign #f] r1 . rs)
  (foldl (λ (a d) (happend2 #:valign valign a d)) r1 rs))
(define (happend* #:valign [valign #f] rs)
  (apply happend rs #:valign valign))

(define (text s)
  (if (string=? s "")
    (blank)
    (happend* (map char (string->list s)))))
(define (hline w)
  (happend* (make-list w (char #\─))))
(define (vline h)
  (vappend* (make-list h (char #\│))))

(define (frame #:style [s #f] #:fg [f #f] #:bg [b #f] x)
  (match-define (raart w h _) x)
  (place-at
   (with-drawing s f b
     (vappend
      (happend (char #\┌) (hline w  ) (char #\┐))
      (happend (vline  h) (blank w h) (vline  h))
      (happend (char #\└) (hline w  ) (char #\┘))))
   1 1 x))

(define (table rows
               ;; XXX add more options to frames
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
      (apply max (map raart-w col))))
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

  (define    header (make-bar #\┌ #\┬ #\┐))
  (define inbetween (make-bar #\├ #\┼ #\┤))
  (define    footer (make-bar #\└ #\┴ #\┘))
  (define last-row (sub1 (length rows)))
  (vappend*
   (for/list ([row (in-list rows)]
              [row-i (in-naturals)])
     (define row-h (apply max (map raart-h row)))
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
      (if (raart? col) col (text (~a col))))))

(define (if-drawn f x)
  (match-define (raart w h !) x)
  (raart w h (λ (d r c)
               (define ? (! d r c))
               (when ? (f))
               ?)))

(define (place-cursor-after x cr cc)
  (match-define (raart w h !) x)
  (raart w h (λ (d r c)
               (strict-or (! d r c)
                          (d cr cc #f)))))

(define valign/c (or/c 'top 'center 'bottom))
(define halign/c (or/c 'left 'center 'right))
(provide
 (contract-out
  [raart? (-> any/c boolean?)]
  [draw
   (-> buffer? raart?
       void?)]
  [style (-> style/c raart? raart?)]
  [fg (-> color/c raart? raart?)]
  [bg (-> color/c raart? raart?)]
  [with-drawing
    (-> (or/c style/c #f)
        (or/c color/c #f)
        (or/c color/c #f)
        raart? raart?)]
  [blank (->* () (exact-nonnegative-integer? exact-nonnegative-integer?) raart?)]
  [char (-> (and/c char? (not/c char-iso-control?)) raart?)]
  [text (-> string? raart?)]
  [hline (-> exact-nonnegative-integer? raart?)]
  [vline (-> exact-nonnegative-integer? raart?)]
  [vappend2 (->* (raart? raart?) (#:halign (or/c halign/c #f)) raart?)]
  [vappend (->* (raart?) (#:halign (or/c halign/c #f)) #:rest (listof raart?) raart?)]
  [vappend* (->* ((non-empty-listof raart?)) (#:halign (or/c halign/c #f)) raart?)]
  [happend2 (->* (raart? raart?) (#:valign (or/c valign/c #f)) raart?)]
  [happend (->* (raart?) (#:valign (or/c valign/c #f)) #:rest (listof raart?) raart?)]
  [happend* (->* ((non-empty-listof raart?)) (#:valign (or/c valign/c #f)) raart?)]
  [place-at (-> raart? exact-nonnegative-integer? exact-nonnegative-integer? raart?
                raart?)]
  [frame (->* (raart?)
              (#:style (or/c style/c #f) #:fg (or/c color/c #f) #:bg (or/c color/c #f))
              raart?)]
  [matte-at (-> exact-nonnegative-integer? exact-nonnegative-integer?
                exact-nonnegative-integer? exact-nonnegative-integer?
                raart?
                raart?)]
  [translate (-> exact-nonnegative-integer? exact-nonnegative-integer?
                 raart? raart?)]
  [halign/c contract?]
  [valign/c contract?]
  [matte (->* (exact-nonnegative-integer? exact-nonnegative-integer? raart?)
              (#:halign halign/c #:valign valign/c)
              raart?)]
  [inset (-> exact-nonnegative-integer? exact-nonnegative-integer? raart? raart?)]
  [mask (-> exact-nonnegative-integer? exact-nonnegative-integer?
            exact-nonnegative-integer? exact-nonnegative-integer?
            raart? raart?)]
  [crop (-> exact-nonnegative-integer? exact-nonnegative-integer?
            exact-nonnegative-integer? exact-nonnegative-integer?
            raart? raart?)]
  [table (->* ((listof (listof raart?)))
              (#:frames? boolean?
               #:style (or/c style/c #f)
               #:fg (or/c color/c #f)
               #:bg (or/c color/c #f)
               #:inset-dw exact-nonnegative-integer?
               #:inset-dh exact-nonnegative-integer?
               #:valign valign/c
               #:halign (or/c halign/c (list*of halign/c (or/c halign/c '()))))
              raart?)]
  [text-rows (-> (listof (listof any/c))
                 (listof (listof raart?)))]
  [if-drawn (-> (-> any) raart? raart?)]
  [place-cursor-after
   (-> raart? exact-nonnegative-integer? exact-nonnegative-integer?
       raart?)])
 place-at*)
