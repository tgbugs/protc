#lang racket/base

(require protc/private/parser
         protc/private/tokenizer)

(define (read-syntax name port)
  (define parse-tree (parse-to-datum (protc-make-tokenizer port)))
  #`(module protc-module racket/base  ; protc/expander
        (quote #,parse-tree)))  ; TODO don't just return the parse tree...

(module+ reader ; syntax/module-reader protc
        (provide read-syntax))
