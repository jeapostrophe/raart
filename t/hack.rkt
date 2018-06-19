#lang racket/base
(require racket/match
         racket/format
         racket/list
         lux
         raart
         struct-define)

(define rows 24)
(define cols 80)
(define world-rows (- rows 3))
(define world-cols cols)

(struct obj (ox oy oc))
(define-struct-define obj-define obj)

(define-struct-define hack-define hack)
(define (step w dx dy)
  (hack-define w)
  (check
   (hack (add1 steps)
         (modulo (+ dx px) world-cols)
         (modulo (+ dy py) world-rows)
         score
         objs)))
(define (check w)
  (hack-define w)
  (cond
    [(empty? objs)
     #f]
    [else
     (for/fold ([score score]
                [objs '()]
                #:result (hack steps px py score objs))
               ([o (in-list objs)])
       (obj-define o)
       (if (and (= ox px) (= oy py))
         (values (add1 score) objs)
         (values score (cons o objs))))]))

(struct hack (steps px py score objs)
  #:methods gen:word
  [(define (word-fps w) 0.0)
   (define (word-label w ft) "Hack")
   (define (word-event w e)
     (hack-define w)
     (match e
       [(screen-size-report _ _) w]
       ["<left>"  (step w -1 0)]
       ["<right>" (step w +1 0)]
       ["<up>"    (step w 0 -1)]
       ["<down>"  (step w 0 +1)]
       ["q" #f]))
   (define (word-output w)
     (hack-define w)
     (crop 0 cols 0 rows
           (vappend
            #:halign 'left
            (text (~a "Hello Jack! Enjoy the hacking! Press q to quit."))
            (place-at*
             (for/fold ([c (blank world-cols world-rows)])
                       ([o (in-list objs)])
               (obj-define o)
               (place-at c oy ox (fg 'blue (char oc))))
             [py px (fg 'red (char #\@))])
            (text (~a "Jack the Paren Hunter"))
            (happend (text (~a "Steps: " steps " Score: " score))))))
   (define (word-return w)
     (hack-define w)
     (~a "You got " score " parens in " steps " steps!"))])
(define (initial-hack-state)
  (hack 0 0 0 0
        (for/list ([i (in-range 8)])
          (obj (random world-cols) (random world-rows)
               (if (zero? (random 2)) #\( #\))))))

(module+ main
  (call-with-chaos
   (make-raart)
   (λ () (fiat-lux (initial-hack-state)))))
