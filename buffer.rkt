#lang racket/base
(require racket/generic
         racket/match
         racket/contract/base
         (prefix-in A: ansi)
         struct-define)

(define-generics buffer
  (buffer-resize! buffer rows cols)
  (buffer-start! buffer rows cols)
  (buffer-commit! buffer #:cursor? [cursor?]))

(define symbol->style
  `#hasheq([normal . ,A:style-normal]
           [bold . ,A:style-bold]
           [inverse . ,A:style-inverse]
           [underline . ,A:style-underline]))
(define style/c (apply or/c (hash-keys symbol->style)))

(define symbol->color
  `#hasheq(
           [black   .  0] [red       .  1] [green   .  2] [yellow   . 3]
           [blue    .  4] [magenta   .  5] [cyan    .  6] [white    . 7]
           [brblack .  8] [brred     .  9] [brgreen . 10] [bryellow . 11]
           [brblue  . 12] [brmagenta . 13] [brcyan  . 14] [brwhite  . 15]))
(define color/c (apply or/c byte? #f (hash-keys symbol->color)))
(define (color->code c)
  (if (byte? c) c
      (hash-ref symbol->color c)))

(define (select-style* s)
  (define (k s) (A:select-graphic-rendition (hash-ref symbol->style s)))
  (if (eq? s 'normal) (k s)
      (string-append (k 'normal) (k s))))
(define (select-text-color* c)
  (if c
    (A:select-xterm-256-text-color (color->code c))
    (A:select-graphic-rendition A:style-default-text-color)))
(define (select-background-color* c)
  (if c
    (A:select-xterm-256-background-color (color->code c))
    (A:select-graphic-rendition A:style-default-background-color)))

(define (make-terminal-buffer term-rows term-cols
                              #:clear? [clear? #t]
                              #:output [op (current-output-port)])
  (terminal-buffer clear? op term-rows term-cols))
(define-struct-define terminal-buffer-define terminal-buffer)
(struct terminal-buffer (clear? op [term-rows #:mutable] [term-cols #:mutable])
  #:methods gen:buffer
  [(define (buffer-resize! buf new-rows new-cols)
     (terminal-buffer-define buf)
     (set! term-rows new-rows)
     (set! term-cols new-cols))
   (define (buffer-start! buf draw-rows draw-cols)
     (terminal-buffer-define buf)
     (define-syntax-rule
       (maybe-update last-X X select-X)
       (unless (eq? last-X X)
         (display (select-X X) op)
         (set! last-X X)))

     (display (A:dec-soft-terminal-reset) op)
     (when (terminal-buffer-clear? buf)
       (display (A:clear-screen/home) op))
     (display (A:hide-cursor) op)
     (define last-s #f)
     (define last-f #f)
     (define last-b #f)
     (define cur-r -1)
     (define cur-c -1)
     (values
      term-rows term-cols
      (λ (s f b r c ch)
        (cond
          [(or (< r 0)
               (<= term-rows r)
               (< c 0)
               (<= term-cols c))
           #f]
          [else
           (maybe-update last-s s select-style*)
           (maybe-update last-f f select-text-color*)
           (maybe-update last-b b select-background-color*)

           (define tr (add1 r))
           (define tc (add1 c))
           (unless (and (= cur-r tr)
                        (= cur-c tc))
             (display (A:goto tr tc) op)
             (set! cur-r tr)
             (set! cur-c tc))

           (when ch
             (display ch op)
             (set! cur-c (add1 cur-c)))

           #t]))))
   (define (buffer-commit! buf #:cursor? [cursor? #t])
     (terminal-buffer-define buf)
     (when cursor? (display (A:show-cursor) op))
     (flush-output op))])

(struct output-cell (s f b ch) #:mutable #:transparent)
(define (clear-cell! c)
  (set-output-cell-s! c 'normal)
  (set-output-cell-f! c #f)
  (set-output-cell-b! c #f)
  (set-output-cell-ch! c #f))
(define (default-cell) (output-cell 'normal #f #f #f))

(struct cells (rows cols vec) #:mutable)
(define (maybe-make-cells old new-rows new-cols)
  (match-define (cells old-rows old-cols vec) old)
  ;; XXX support shrinking/growing while preserving information
  (if (and (= old-rows new-rows)
           (= old-cols new-cols))
    old
    (make-cells new-rows new-cols)))
(define (make-cells rows cols)
  (cells rows cols
         (build-vector
          rows
          (λ (r)
            (build-vector cols (λ (c) (default-cell)))))))
(define (clear-cells! cs)
  (match-define (cells _ _ vec) cs)
  (for* ([row (in-vector vec)]
         [cell (in-vector row)])
    (clear-cell! cell)))
(define (draw-cell! cs)
  (match-define (cells ok-rows ok-cols vec) cs)
  (λ (s f b r c ch)
    (cond
      [(or (< r 0)
           (<= ok-rows r)
           (< c 0)
           (<= ok-cols c))
       #f]
      [else
       (define oc (vector-ref (vector-ref vec r) c))
       (set-output-cell-s! oc s)
       (set-output-cell-f! oc f)
       (set-output-cell-b! oc b)
       (when ch
         (set-output-cell-ch! oc ch))
       #t])))

(define (make-output-buffer #:output [op (current-output-port)])
  (output-buffer op (make-cells 0 0)))
(define-struct-define output-buffer-define output-buffer)
(struct output-buffer (op [cells #:mutable])
  #:methods gen:buffer
  [(define (buffer-resize! buf new-rows new-cols)
     (output-buffer-define buf)
     (set! cells (maybe-make-cells cells new-rows new-cols)))
   (define (buffer-start! buf draw-rows draw-cols)
     (output-buffer-define buf)
     (buffer-resize! buf draw-rows draw-cols)
     (clear-cells! cells)
     (values draw-rows draw-cols (draw-cell! cells)))
   (define (buffer-commit! buf #:cursor? [cursor? #t])
     (output-buffer-define buf)
     (for/fold ([last-s #f] [last-f #f] [last-b #f])
               ([row (in-vector (cells-vec cells))])
       (begin0
           (for/fold ([last-s last-s] [last-f last-f] [last-b last-b])
                     ([oc (in-vector row)])
             (match-define (output-cell s f b ch) oc)
             (unless (eq? last-s s)
               (display (select-style* s) op))
             (unless (eq? last-f f)
               (display (select-text-color* f) op))
             (unless (eq? last-b b)
               (display (select-background-color* b) op))
             (display (or ch #\space) op)
             (values s f b))
         (newline op)))
     (flush-output op)
     (void))])

(define (make-cached-buffer term-rows term-cols
                            #:output [op (current-output-port)])
  (define (mk-term clear?)
    (make-terminal-buffer term-rows term-cols
                          #:clear? clear?
                          #:output op))
  (cached-buffer
   #t
   (mk-term #f) (mk-term #t)
   term-rows term-cols
   (make-cells term-rows term-cols)
   (make-cells term-rows term-cols)
   0 0))
(define-struct-define cached-buffer-define cached-buffer)
(struct cached-buffer
  ([clear-next? #:mutable]
   term-nclear term-yclear
   [term-rows #:mutable] [term-cols #:mutable]
   [cur-cells #:mutable] [new-cells #:mutable]
   [last-row #:mutable] [last-col #:mutable])
  #:methods gen:buffer
  [(define/generic super-buffer-resize! buffer-resize!)
   (define/generic super-buffer-start! buffer-start!)
   (define/generic super-buffer-commit! buffer-commit!)

   (define (buffer-resize! buf new-rows new-cols)
     (cached-buffer-define buf)
     (set! clear-next? #t)
     (set! cur-cells (maybe-make-cells cur-cells new-rows new-cols))
     (set! new-cells (maybe-make-cells new-cells new-rows new-cols))
     (super-buffer-resize! term-nclear new-rows new-cols)
     (super-buffer-resize! term-yclear new-rows new-cols)
     (set! term-rows new-rows)
     (set! term-cols new-cols)
     (clear-cells! cur-cells))
   (define (buffer-start! buf draw-rows draw-cols)
     (cached-buffer-define buf)
     (clear-cells! new-cells)
     (define dc (draw-cell! new-cells))
     (values term-rows term-cols
             (λ (s f b r c ch)
               (set! last-row r)
               (set! last-col c)
               (dc s f b r c ch))))
   (define (buffer-commit! buf #:cursor? [cursor? #t])
     (cached-buffer-define buf)
     (define inner-buf (if clear-next? term-yclear term-nclear))
     (set! clear-next? #f)
     (define-values (ok-rows ok-cols draw!)
       (super-buffer-start! inner-buf term-rows term-cols))
     (for ([cur-row (in-vector (cells-vec cur-cells))]
           [new-row (in-vector (cells-vec new-cells))]
           [r (in-naturals)])
       (for ([cur-cell (in-vector cur-row)]
             [new-cell (in-vector new-row)]
             [c (in-naturals)])
         (unless (equal? cur-cell new-cell)
           (match-define (output-cell _ _ _ cur-ch) cur-cell)
           (match-define (output-cell s f b new-ch) new-cell)
           (draw! s f b r c (or new-ch #\space)))))
     (draw! 'normal #f #f last-row last-col #f)
     (super-buffer-commit! inner-buf #:cursor? cursor?)
     (swap! new-cells cur-cells))])

(define-syntax-rule (swap! x y)
  (let ([tmp x])
    (set! x y)
    (set! y tmp)))

(module+ internal
  (provide
   (contract-out
    [buffer-resize!
     (-> buffer?
         exact-nonnegative-integer? exact-nonnegative-integer?
         void?)]
    [buffer-start!
     (-> buffer?
         exact-nonnegative-integer? exact-nonnegative-integer?
         (values exact-nonnegative-integer?
                 exact-nonnegative-integer?
                 (-> style/c color/c color/c
                     exact-nonnegative-integer?
                     exact-nonnegative-integer?
                     (or/c char? #f)
                     boolean?)))]
    [buffer-commit!
     (->* (buffer?) (#:cursor? boolean?) void?)])))

(provide
 (contract-out
  [color/c contract?]
  [style/c contract?]
  [buffer? (-> any/c boolean?)]
  [make-terminal-buffer
   (->* (exact-nonnegative-integer? exact-nonnegative-integer?)
        (#:clear? boolean? #:output output-port?)
        buffer?)]
  [make-output-buffer
   (->* () (#:output output-port?) buffer?)]
  [make-cached-buffer
   (->* (exact-nonnegative-integer? exact-nonnegative-integer?)
        (#:output output-port?)
        buffer?)]))


