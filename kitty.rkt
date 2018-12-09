#lang racket/base
(require racket/match
         (prefix-in pict: pict)
         file/convertible
         racket/class
         racket/gui/base)

(define (convert->png-bytes v)
  (and (convertible? v)
       (convert v 'png-bytes+bounds #f)))

(define (snip? v) (is-a? v snip%))

;; Replace "racket -t file"
;; with
;; "racket -I raart/kitty-init -i -t file -e '(exit 0)'"

(define (install-kitty-print!)
  (define t (environment-variables-ref (current-environment-variables) #"TERM"))
  (when (equal? t #"xterm-kitty")
    ;; XXX This could do better and use
    #;(pretty-print-size-hook)
    ;; and
    #;(pretty-print-print-hook)
    ;; to pretty these things inside other structures
    ;; but, then I believe I could not rely on icat, but I'd have to implement it myself

    (define old-print (current-print))
    (define (new-print v)
      (match (or (convert->png-bytes v) v)
        [(list bs w h d v)
         (define-values
           (sp stdout stdin stderr)
           (subprocess (current-output-port) #f (current-error-port)
                       (find-executable-path "kitty")
                       "+kitten" "icat"))
         (write-bytes bs stdin)
         (close-output-port stdin)
         (subprocess-wait sp)]
        [(? snip?)
         (define wb (box #f))
         (define hb (box #f))
         (send v get-extent (pict:dc-for-text-size) 0 0 wb hb)
         (define w (unbox wb))
         (define h (unbox hb))
         (new-print
          (pict:dc (λ (dc x y)
                     (send v draw dc x y 0 0 w h 0 0 'no-caret))
                   w h))]
        [v (old-print v)]))
    (current-print new-print)))

(provide install-kitty-print!)
