#lang info
(define collection "raart")
(define deps '("gui-lib"
               "htdp-lib"
               "pict-lib"
               "plot-gui-lib"
               "plot-lib"
               "lux"
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
