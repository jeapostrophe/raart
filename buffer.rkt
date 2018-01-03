#lang racket/base
(require racket/generic
         racket/match
         racket/contract/base
         (prefix-in A: ansi))

(define-generics buffer
  (buffer-resize! buffer rows cols)
  (buffer-start! buffer rows cols)
  (buffer-commit! buffer))

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
(define color/c (apply or/c #f (hash-keys symbol->color)))

(define (select-style* s)
  (A:select-graphic-rendition (hash-ref symbol->style s)))
(define (select-text-color* c)
  (if c
    (A:select-xterm-256-text-color (hash-ref symbol->color c))
    (A:select-graphic-rendition A:style-default-text-color)))
(define (select-background-color* c)
  (if c
    (A:select-xterm-256-background-color (hash-ref symbol->color c))
    (A:select-graphic-rendition A:style-default-background-color)))

(define (make-terminal-buffer term-rows term-cols
                              #:clear? [clear? #t]
                              #:output [op (current-output-port)])
  (terminal-buffer clear? op term-rows term-cols))
(struct terminal-buffer (clear? op term-rows term-cols)
  #:mutable
  #:methods gen:buffer
  [(define (buffer-resize! buf new-rows new-cols)
     (set-terminal-buffer-term-rows! buf new-rows)
     (set-terminal-buffer-term-cols! buf new-cols))
   (define (buffer-start! buf draw-rows draw-cols)
     (define op (terminal-buffer-op buf))
     (display (A:dec-soft-terminal-reset) op)
     (when (terminal-buffer-clear? buf)
       (display (A:clear-screen/home) op))
     (display (A:hide-cursor) op)
     (λ (s f b r c ch)
       (display (select-style* s) op)
       (display (select-text-color* f) op)
       (display (select-background-color* b) op)
       ;; XXX maybe add1 to r & c
       (display (A:goto (add1 r) (add1 c)) op)
       (when ch (display ch op))))
   (define (buffer-commit! buf)
     (define op (terminal-buffer-op buf))
     (display (A:show-cursor) op)
     (flush-output op))])

(struct output-cell (s f b ch) #:mutable)
(define (clear-cell! c)
  (set-output-cell-s! c 'normal)
  (set-output-cell-f! c #f)
  (set-output-cell-b! c #f)
  (set-output-cell-ch! c #f))
(define (default-cell) (output-cell 'normal #f #f #f))
(define (make-cells rows cols)
  (build-vector
   rows
   (λ (r)
     (build-vector cols (λ (c) (default-cell))))))

(define (make-output-buffer #:output [op (current-output-port)])
  (output-buffer op 0 0 (make-cells 0 0)))
(struct output-buffer (op rows cols cells)
  #:mutable
  #:methods gen:buffer
  [(define (buffer-resize! buf new-rows new-cols)
     (match-define (output-buffer _ old-rows old-cols _) buf)
     (when (or (not (<= new-rows old-rows))
               (not (<= new-cols old-cols)))
       (set-output-buffer-rows! buf new-rows)
       (set-output-buffer-cols! buf new-cols)
       (set-output-buffer-cells! buf (make-cells new-rows new-cols))))
   (define (buffer-start! buf draw-rows draw-cols)
     (buffer-resize! buf draw-rows draw-cols)
     (define cells (output-buffer-cells buf))
     (for* ([row (in-vector cells)]
            [cell (in-vector row)])
       (clear-cell! cell))
     (λ (s f b r c ch)
       (define cell (vector-ref (vector-ref cells r) c))
       (set-output-cell-s! cell s)
       (set-output-cell-f! cell f)
       (set-output-cell-b! cell b)
       (set-output-cell-ch! cell ch)))
   (define (buffer-commit! buf)
     (define op (output-buffer-op buf))
     (define cells (output-buffer-cells buf))
     (for/fold ([last-s 'normal] [last-f #f] [last-b #f])
               ([row (in-vector cells)])
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

(define (make-buffered-terminal-buffer term-rows term-cols
                                       #:output [op (current-output-port)])
  ;; XXX
  (make-terminal-buffer term-rows term-cols
                        #:clear? #t
                        #:output op))

(provide
 (contract-out
  [color/c contract?]
  [style/c contract?]
  [buffer? (-> any/c boolean?)]
  [buffer-resize!
   (-> buffer?
       exact-nonnegative-integer? exact-nonnegative-integer?
       void?)]
  [buffer-start!
   (-> buffer?
       exact-nonnegative-integer? exact-nonnegative-integer?
       (-> style/c color/c color/c
           exact-nonnegative-integer? exact-nonnegative-integer? (or/c char? #f)
           void?))]
  [buffer-commit!
   (-> buffer? void?)]
  [make-terminal-buffer
   (->* (exact-nonnegative-integer? exact-nonnegative-integer?)
        (#:clear? boolean? #:output output-port?)
        buffer?)]
  [make-output-buffer
   (->* () (#:output output-port?) buffer?)]
  [make-buffered-terminal-buffer
   (->* (exact-nonnegative-integer? exact-nonnegative-integer?)
        (#:output output-port?)
        buffer?)]))
