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
(struct terminal-buffer (clear? op [term-rows #:mutable] [term-cols #:mutable])
  #:methods gen:buffer
  [(define (buffer-resize! buf new-rows new-cols)
     (set-terminal-buffer-term-rows! buf new-rows)
     (set-terminal-buffer-term-cols! buf new-cols))
   (define (buffer-start! buf draw-rows draw-cols)
     (define op (terminal-buffer-op buf))
     (define ok-rows
       (min draw-rows (terminal-buffer-term-rows buf)))
     (define ok-cols
       (min draw-cols (terminal-buffer-term-cols buf)))
     (define-syntax-rule
       (maybe-update last-X X select-X)
       (unless (eq? last-X X)
         (display (select-X X) op)
         (set! last-X X)))

     (display (A:dec-soft-terminal-reset) op)
     (when (terminal-buffer-clear? buf)
       (display (A:clear-screen/home) op))
     (display (A:hide-cursor) op)
     (define last-s 'normal)
     (define last-f #f)
     (define last-b #f)
     (define cur-r 1)
     (define cur-c 1)
     (λ (s f b r c ch)
       (cond
         [(or (< r 0)
              (<= ok-rows r)
              (< c 0)
              (<= ok-cols c))
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

          #t])))
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

(struct cells (rows cols vec) #:mutable)
(define (maybe-make-cells old new-rows new-cols)
  (match-define (cells old-rows old-cols vec) old)
  ;; XXX support shrinking/growing
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
       (set-output-cell-ch! oc ch)
       #t])))

(define (make-output-buffer #:output [op (current-output-port)])
  (output-buffer op (make-cells 0 0)))
(struct output-buffer (op [cells #:mutable])
  #:methods gen:buffer
  [(define (buffer-resize! buf new-rows new-cols)
     (set-output-buffer-cells!
      buf
      (maybe-make-cells (output-buffer-cells buf)
                        new-rows new-cols)))
   (define (buffer-start! buf draw-rows draw-cols)
     (buffer-resize! buf draw-rows draw-cols)
     (define cs (output-buffer-cells buf))
     (clear-cells! cs)
     (draw-cell! cs))
   (define (buffer-commit! buf)
     (define op (output-buffer-op buf))
     (define cells (cells-vec (output-buffer-cells buf)))
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
           boolean?))]
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
