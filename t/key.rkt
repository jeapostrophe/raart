#lang racket/base
(require racket/list
         racket/format
         racket/match         
         lux
         raart)

(define (key)
  (define base (word #:fps 0.0 #:label "Key Debug" #:return (void)))

  (define (show-key k)
    (word base
          #:output (text (~a k))
          #:event
          (match-lambda
            ["q" #f]
            [x (show-key x)])))

  (show-key "Please enter a key."))

(module+ main
  (require raart/lux-chaos)
  (call-with-chaos
   (make-raart)
   (λ ()
     (fiat-lux (key)))))
