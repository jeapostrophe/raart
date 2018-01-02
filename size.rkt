#lang racket/base
(require racket/match
         racket/list
         racket/system
         ansi)

(define stty-minus-f-arg-string
  (case (system-type 'os)
    ((macosx) "-f")
    (else     "-F")))

(define (read-until ip char)
  (define byte (char->integer char))
  (apply bytes
         (let loop ()
           (match (read-byte ip)
             [(== byte) empty]
             [next (cons next (loop))]))))

(define (bytes->number bs)
  (string->number (bytes->string/utf-8 bs)))

(define default-tty "/dev/tty")
(struct term (f in out))
(define (open-term #:tty [tty default-tty])
  (system* "/bin/stty"
           stty-minus-f-arg-string
           tty
           "raw"
           "-echo")
  (define-values (in out)
    (open-input-output-file tty #:exists 'update))
  (term tty in out))

(define (close-term t)
  (match-define (term f in out) t)
  (close-input-port in)
  (close-output-port out)
  (system* "/bin/stty"
           stty-minus-f-arg-string
           f
           "cooked"
           "echo"))

(define (with-term f #:tty [tty default-tty])
  (define t (open-term #:tty tty))
  (define (close!) (close-term t))
  (with-handlers ([exn:fail? (λ (x) (close!) (raise x))])
    (begin0 (f t) (close!))))

(define (with-term* t f)
  (if t (f t) (with-term f)))

(define (screen-size [t #f])
  (with-term* t
    (λ (t)
      (match-define (term _ in out) t)
      (write-bytes #"\e[18t" out) (flush-output out)
      (match (read-until in #\;)
        [#"\e[8"
         (define row-s (read-until in #\;))
         (define col-s (read-until in #\t))
         (values (bytes->number row-s)
                 (bytes->number col-s))]
        [x (error 'screen-size "Something weird happened, got ~e" x)]))))

(define (cursor-position [t #f])
  (with-term* t
    (λ (t)
      (match-define (term _ in out) t)
      (display (position-report-request) out) (flush-output out)
      (match (read-bytes 2 in)
        [#"\e["
         (define row-s (read-until in #\;))
         (define col-s (read-until in #\R))
         (values (bytes->number row-s)
                 (bytes->number col-s))]
        [x (error 'cursor-position "Something weird happened, got ~e" x)]))))

;; xxx xterm-set-window-title

;; xxx Do I make a 'lux chaos' for this?
;;
;;     Or do I do the rune thing and make a separation between the
;;     commands and the keys? Also, how should the mouse events fit
;;     into that?

;; xxx render xexpr-like thing
;; xxx text... (fit text inside a width)
;; xxx paragraph (fit text inside a box)

;; xxx make a "Web" browser
;; xxx use if-drawn to figure out what links are on screen

(module+ main
  (with-term
    (λ (t)
      (screen-size t)
      (cursor-position t))))
