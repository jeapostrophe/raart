#lang racket/base
(require racket/match
         racket/contract/base
         racket/list
         racket/async-channel
         racket/system
         ansi
         (submod ansi/lcd-terminal event-structs)
         ansi/private/tty-raw-extension
         unix-signals
         lux/chaos
         raart/draw
         raart/buffer)
(provide (all-from-out (submod ansi/lcd-terminal event-structs)))

(struct term (f in out))

(define default-tty "/dev/tty")

#;
(define stty-minus-f-arg-string
  (case (system-type 'os)
    ((macosx) "-f")
    (else     "-F")))
#;
(define (open-term #:tty [tty default-tty])
  (system* "/bin/stty"
           stty-minus-f-arg-string
           tty
           "raw"
           "-echo")
  (define-values (in out)
    (open-input-output-file tty #:exists 'update))
  (term tty in out))
#;
(define (close-term t)
  (match-define (term f in out) t)
  (close-input-port in)
  (close-output-port out)
  (system* "/bin/stty"
           stty-minus-f-arg-string
           f
           "cooked"
           "echo"))

(define (open-term)
  (tty-raw!)
  (term #f (current-input-port) (current-output-port)))
(define (close-term t)
  (tty-restore!))

(define (with-term f #:tty [tty default-tty])
  (define t (open-term #:tty tty))
  (define (close!) (close-term t))
  (with-handlers ([exn:fail? (λ (x) (close!) (raise x))])
    (begin0 (f t) (close!))))

(define (display/flush v op)
  (display v op)
  (flush-output op))

(define (display/term t v)
  (define op (term-out t))
  (unless (port-closed? op)
    (display/flush v op)))

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

  (define t (open-term))
  (define init-rows 24)
  (define init-cols 80)
  (define buf
    (make-buffered-terminal-buffer init-rows init-cols
                                   #:output (term-out t)))
  (define ch (make-async-channel))
  ;; Initialize term
  (when alternate?
    (display/term t (set-mode alternate-screen-mode)))
  (when mouse?
    (display/term t x11-mouse-on)
    (plumber-add-flush! (current-plumber)
                        (lambda (handle)
                          (display/term t x11-mouse-off))))
  ;; Register for window change events
  ;; XXX some way to force this to be first
  (display/term t (device-request-screen-size))
  (capture-signal! 'SIGWINCH)
  (define sig-th
    (thread
     (λ ()
       (let loop ()
         (define s (read-signal))
         (match (lookup-signal-name s)
           ['SIGWINCH (display/term t (device-request-screen-size))
                      (loop)])))))
  ;; Listen for input
  (define input-th
    (thread
     (λ ()
       (let loop ()
         (define v (lex-lcd-input (term-in t) #:utf-8? #t))
         (unless (eof-object? v)
           (async-channel-put ch v)
           (loop))))))
  ;; Return
  (*term alternate? mouse? t buf ch sig-th input-th init-rows init-cols))

(struct *term
  (alternate? mouse? t buf ch sig-th input-th [rows #:mutable] [cols #:mutable])
  #:methods gen:chaos
  [(define (chaos-event c)
     (handle-evt (*term-ch c)
                 (match-lambda
                   [(and e (screen-size-report rows cols))
                    (buffer-resize! (*term-buf c) rows cols)
                    (set-*term-rows! c rows)
                    (set-*term-cols! c cols)
                    e]
                   [e e])))
   (define (chaos-output! c o)
     (when o
       (draw (*term-buf c)
             ;; XXX put this crop inside buffer?
             (crop 0 (add1 (*term-cols c))
                   0 (add1 (*term-rows c))
                   o))))
   (define (chaos-label! c l)
     (display/term (*term-t c) (xterm-set-window-title l)))
   (define (chaos-stop! c)
     (define t (*term-t c))
     (when (*term-mouse? c)
       (display/term t x11-mouse-off))
     (when (*term-alternate? c)
       (display/term t (reset-mode alternate-screen-mode)))
     (kill-thread (*term-sig-th c))
     (kill-thread (*term-input-th c))
     (release-signal! 'SIGWINCH)
     (close-term t))])

(provide
 (contract-out
  [make-raart
   (->* () () chaos?)]))
