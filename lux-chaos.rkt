#lang racket/base
(require racket/match
         racket/contract/base
         racket/list
         racket/async-channel
         racket/system
         ansi
         (submod ansi/lcd-terminal event-structs)
         unix-signals
         lux/chaos
         raart/draw
         raart/buffer
         "struct-define.rkt")
(provide (all-from-out (submod ansi/lcd-terminal event-structs)))

(struct term (f in out))

(define-syntax-rule (define-stty-term open-term close-term)
  (begin
    (define default-tty "/dev/tty")
    (define stty-minus-f-arg-string
      (case (system-type 'os)
        ((macosx) "-f")
        (else     "-F")))
    (define (open-term #:tty [tty default-tty])
      (system* "/bin/stty" stty-minus-f-arg-string tty
               "raw" "pass8" "-echo")
      (define-values (in out)
        (open-input-output-file tty #:exists 'update))
      (file-stream-buffer-mode in 'none)
      (file-stream-buffer-mode out 'none)
      (term tty in out))
    (define (close-term t)
      (match-define (term tty in out) t)
      (close-input-port in)
      (close-output-port out)
      (system* "/bin/stty" stty-minus-f-arg-string tty
               "sane"))))

(define-syntax-rule (define-stdin-term open-term close-term)
  (begin
    (require ansi/private/tty-raw-extension)
    (define (open-term #:tty [tty #f])
      (when tty
        (error 'open-term "Custom tty not supported in this version"))
      (tty-raw!)
      (term #f (current-input-port) (current-output-port)))
    (define (close-term t)
      (tty-restore!))))

#;(define-stty-term open-term close-term)
(define-stdin-term open-term close-term)

(define (display/term t v)
  (define op (term-out t))
  (unless (port-closed? op)
    (display v op)
    (flush-output op)))

;; Lux
(define x11-mouse-on
  (string-append (set-mode x11-focus-event-mode)
                 (set-mode x11-any-event-mouse-tracking-mode)
                 (set-mode x11-extended-mouse-tracking-mode)))
(define x11-mouse-off
  (string-append (reset-mode x11-extended-mouse-tracking-mode)
                 (reset-mode x11-any-event-mouse-tracking-mode)
                 (reset-mode x11-focus-event-mode)))

(define (make-raart #:mouse? [mouse? #f])
  (define alternate? #t)
  (define ch (make-async-channel))
  (*term alternate? mouse? #f #f ch #f #f #f #f))

(define-struct-define *term term-define)
(struct *term
  (alternate? mouse? t buf ch sig-th input-th rows cols)
  #:mutable
  #:methods gen:chaos
  [(define (chaos-event c)
     (term-define c)
     (handle-evt ch
                 (match-lambda
                   [(and e (screen-size-report new-rows new-cols))
                    (set! rows new-rows)
                    (set! cols new-cols)
                    (buffer-resize! buf rows cols)
                    e]
                   [e e])))
   (define (chaos-output! c o)
     (when o
       (draw (*term-buf c) o)))
   (define (chaos-label! c l)
     (display/term (*term-t c) (xterm-set-window-title l)))
   (define (chaos-start! c)
     (term-define c)
     (set! t (open-term))
     (set! rows 24)
     (set! cols 80)
     (set! buf (make-cached-buffer rows cols #:output (term-out t)))

     ;; Initialize term
     (when alternate?
       (display/term t (set-mode alternate-screen-mode)))
     (when mouse?
       (display/term t x11-mouse-on)
       (plumber-add-flush! (current-plumber)
                           (lambda (handle)
                             (display/term t x11-mouse-off))))

     ;; Listen for input
     (set! input-th
           (thread
            (λ ()
              (let loop ()
                (define v (lex-lcd-input (term-in t) #:utf-8? #t))
                (unless (eof-object? v)
                  (async-channel-put ch v)
                  (loop))))))

     ;; Register for window change events
     ;; XXX some way to force this to be first
     (display/term t (device-request-screen-size))
     (set! sig-th
           (thread
            (λ ()
              (let loop ()
                (define s (read-signal))
                (match (lookup-signal-name s)
                  ['SIGWINCH (display/term t (device-request-screen-size))
                             (loop)])))))
     (capture-signal! 'SIGWINCH)

     (void))
   (define (chaos-stop! c)
     (term-define c)

     (release-signal! 'SIGWINCH)
     (kill-thread sig-th)

     (kill-thread input-th)

     (when mouse?
       (display/term t x11-mouse-off))
     (when alternate?
       (display/term t (reset-mode alternate-screen-mode)))

     (close-term t))])

(provide
 (contract-out
  [make-raart
   (->* () () chaos?)]))
