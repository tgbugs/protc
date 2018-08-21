#lang racket/base
(require racket/pretty (for-syntax racket/base syntax/parse))
(provide (all-defined-out))

(define-syntax (if-defined stx)
  (syntax-case stx ()
    [(_ id iftrue iffalse)
     (let ([where (identifier-binding #'id)])
       (if where #'iftrue #'iffalse))]))

(define (name-join prefix suffix stx)
  (datum->syntax stx  ; if this is false then everything becomes unrooted and I go looking through 5 phase levels to find the problem
                 (string->symbol
                  (string-append prefix (symbol->string
                                         (syntax->datum suffix))))))

(define-syntax (ppstx stx)
  (syntax-parse stx
    [(_ thing:expr)
     #'(let ([out thing])
         (pretty-print (list 'ppstx: (syntax->datum out)))
         out)]))
