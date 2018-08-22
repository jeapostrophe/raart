#lang info
(define collection "raart")
(define deps '("lux"
               "unix-signals"
               "reprovide-lang"
               "ansi"
               "struct-define"
               "base"))
(define build-deps '("sandbox-lib"
                     "htdp-doc"
                     "racket-doc"
                     "scribble-lib"
                     ))
(define version "0.1")
(define pkg-authors '(jeapostrophe))
(define scribblings '(("raart.scrbl" () ("UI"))))
