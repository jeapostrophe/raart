#lang racket/base

(define p
  (let ()
    (local-require pict)
    (disk 40 #:color "Chartreuse" #:border-color "Medium Aquamarine" #:border-width 5)))

(define i
  (let ()
    (local-require 2htdp/image)
    (add-line
     (rectangle 100 100 "solid" "darkolivegreen")
     25 25 75 75
     (make-pen "goldenrod" 30 "solid" "round" "round"))))

(define pl
  (let ()
    (local-require plot racket/math racket/class racket/gui/base file/convertible)
    (plot (function sin (- pi) pi #:label "y = sin(x)"))))

"Not convertible"
1
(list "foo" "bar")
p
(list "foo" p "bar")
i
(list "foo" i "bar")
pl
(list "foo" pl "bar")
