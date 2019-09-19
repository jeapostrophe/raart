#lang racket/base
(require racket/match
         racket/set
         racket/runtime-path
         json
         (prefix-in pict: pict)
         file/convertible
         racket/class
         racket/gui/dynamic)

(define (convert->png-bytes v)
  (and (convertible? v)
       (convert v 'png-bytes+bounds #f)))

;; Replace "racket -t file"
;; with
;; "racket -I raart/kitty-init -i -t file -e '(exit 0)'"

(define (term-is-kitty?)
  (define t (environment-variables-ref (current-environment-variables) #"TERM"))
  (equal? t #"xterm-kitty"))

(define (install-kitty-print!)  
  (when (term-is-kitty?)
    (define (snip? v)
      (and (gui-available?)
           (is-a? v (gui-dynamic-require 'snip%))))
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

(define-runtime-path kk.j "kitty-key.json")
(define kk-ht
  (for/hash ([(s e) (in-hash (with-input-from-file kk.j read-json))])
    (define o (symbol->string s))
    (values (string->bytes/utf-8 e)
            (cond
              [(= 1 (string-length o))
               (string-ref o 0)]
              [else o]))))
(define (kitty-key-lookup k)
  (hash-ref kk-ht k (λ () k)))

(define (kitty-mods-lookup mb)
  (define mn
    (- (bytes-ref mb 0) (char->integer #\A)))
  (for/fold ([s (seteq)])
            ([m (in-list '(shift meta control super))]
             [i (in-list '(1 2 4 8))])
    (if (zero? (bitwise-and mn i)) s
        (set-add s m))))

(provide install-kitty-print!
         term-is-kitty?
         kitty-key-lookup
         kitty-mods-lookup)
