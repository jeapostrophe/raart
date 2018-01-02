#lang racket/base
(require raart)

(module+ test
  (draw (crop 1 80 1 20
              ;;70 80 10 20
              (matte 80 20
                     #:halign 'right
                     (fg 'blue
                         (frame #:fg 'red
                                (inset
                                 4 5
                                 (happend (style 'underline (text "Left"))
                                          (blank 4)
                                          (style 'bold (text "Right")))))))))
  (newline))

(module+ test
  (draw (translate
         2 10
         (table
          #:frames? #t
          #:inset-dw 2
          #:valign 'center
          #:halign '(right left left left)
          (text-rows
           `([  "ID" "First Name" "Last Name" "Grade"]
             [70022  "John"       "Smith"     "A+"]
             [   22  "Macumber"   "Stark"     "B"]
             [ 1223  "Sarah"      ,(vappend (text "Top")
                                            (text "Mid")
                                            (text "Bot")) "C"])))))
  (newline))
