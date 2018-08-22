#lang racket/base

(require racket/pretty
         racket/syntax
         syntax/warn
         (for-syntax racket/base
                     racket/pretty
                     syntax/parse
                     racket/syntax
                     syntax/warn))

(module+ test
  (require rackunit))

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

(define (fmtid pattern stx)
  (let* ([se (syntax->datum stx)])
    (if (list? se)
        (apply format-id stx #:source stx pattern se)
        (format-id stx #:source stx pattern se))))

;;; warnings

(define-warning-kind protc-warning)

(define-warning-kind protc-missing-section)

(define-syntax (make-errors stx)
  (syntax-parse stx
    [(_ [error:expr syntax-for-locate
                    message:expr
                    (~optional (~seq #:kind kind:id)
                               #:defaults ([kind #'protc-warning]))
                    (~optional (~seq #:fix fix)
                               #:defaults ([fix #'#f]))] ...)
     ;#:do (println (map syntax-local-eval (syntax->list #'(message ...))))
     ;#:with (msgs ...) (datum->syntax #'(message ...) (map syntax-local-eval (syntax->list #'(message ...))))
     ;(println (syntax->datum #'(msgs ...)))
     #'(for/list ([ok? (list error ...)]
                #:unless ok?
                [msg (list message ...) #;(list msgs ...)]
                [sfl (list syntax-for-locate ...)]
                [kind-1 (list kind ...)]
                [fix-1 (list fix ...)])
       (syntax-warn (datum->syntax sfl msg)
                    (if fix-1
                        (syntax-warning #:message msg
                                        #:stx sfl
                                        #:kind kind-1
                                        #:fix (datum->syntax sfl fix-1))
                        (syntax-warning #:message msg
                                        #:stx sfl
                                        #:kind kind-1))))]))

(module+ test
  (require syntax/parse)
  (define (test-in-stx stx)
    (syntax-parse stx
      [_
       #:with text "some text!"
       #`(#,@(make-errors [#f #'there (format "~a that is now formatted" (syntax-e #'text))]))]))
  (test-in-stx #'"syntax in question")
  (make-errors
   [#f #'here "does this work?"])
  (make-errors
   [#f #'here (format "~a test expressions" "hello!")]
   [#f #'here "oops"])
  )
