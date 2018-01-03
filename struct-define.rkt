#lang racket/base
(require (for-syntax racket/base
                     racket/struct-info
                     syntax/parse))

(begin-for-syntax
  (define (make-field-name-transformer instace-id-stx field-ref-stx field-set!-stx)
    (make-set!-transformer
     (lambda (stx)
       (syntax-case stx (set!)
         [(set! id v)
          (if (syntax->datum field-set!-stx)
            (quasisyntax/loc stx
              (#,field-set!-stx #,instace-id-stx  v))
            (raise-syntax-error 'set! "field not mutable" stx #'id))]
         [id (identifier? #'id)
             (quasisyntax/loc stx
               (#,field-ref-stx #,instace-id-stx))])))))

(define-syntax (struct-define stx)
  (syntax-parse stx
    [(_ the-struct the-instance:expr)
     #:declare the-struct
     (static struct-info? "structure type transformer binding")
     #:do [(define struct+-len
             (add1 (string-length (symbol->string (syntax->datum #'the-struct)))))
           (define si (extract-struct-info (attribute the-struct.value)))]
     #:with ([field-name field-ref field-set!] ...)
     (for/list ([field-ref (in-list (list-ref si 3))]
                [field-set (in-list (list-ref si 4))])
       (define field-ref-s
         (symbol->string (syntax->datum field-ref)))
       (define field-name-s
         (substring field-ref-s struct+-len))
       (define field-name
         (datum->syntax #'the-instance (string->symbol field-name-s)))
       (list field-name field-ref field-set))
     #:with (field-val-id ...)
     (generate-temporaries #'(field-name ...))

     (syntax/loc stx
       (begin (define the-instance-id the-instance)
              (define-syntax field-name
                (make-field-name-transformer #'the-instance-id
                                             #'field-ref #'field-set!))
              ...))]))

(define-syntax-rule (define-struct-define the-struct the-struct-define)
  (define-syntax-rule (the-struct-define instance-id)
    (struct-define the-struct instance-id)))

(provide struct-define
         define-struct-define)
