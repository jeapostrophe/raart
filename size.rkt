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

(define (screen-size)
  (define tty-str "/dev/tty")
  (system* "/bin/stty"
           stty-minus-f-arg-string
           tty-str
           "raw"
           "-echo")
  (define-values (in out)
    (open-input-output-file tty-str #:exists 'update))
  (write-bytes #"\e[18t" out) (flush-output out)
  (dynamic-wind
    void
    (λ ()
      (match (read-until in #\;)
        [#"\e[8"
         (define row-s (read-until in #\;))
         (define col-s (read-until in #\t))
         (values (bytes->number row-s)
                 (bytes->number col-s))]
        [_ (values #f #f)]))
    (λ ()
      (close-input-port in)
      (close-output-port out)
      (system* "/bin/stty"
               stty-minus-f-arg-string
               tty-str
               "cooked"
               "echo"))))

(define (bytes->number bs)
  (string->number (bytes->string/utf-8 bs)))

(module+ main
  (screen-size))
