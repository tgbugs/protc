#lang racket/base

(require syntax/strip-context
         protc/private/parser
         protc/private/tokenizer)
(require racket/pretty)

(define printed #f)

(define (read-syntax source-name in-port)
  ; why is this called more than once???
  (define parse-tree (parse source-name (protc-make-tokenizer in-port)))
  ;(define parse-tree (parse-to-datum (protc-make-tokenizer port)))
  '(when (not printed)
    (begin (pretty-write (syntax->datum parse-tree))
           (set! printed #t)))
  (define output-syntax (strip-context  ; required to avoid issues with #%app for reasons I don't understand at the moment
  #`(module protc-module protc/private/expander
      #,parse-tree)) ; TODO don't just return the parse tree...
    )
  (pretty-write (syntax->datum output-syntax))
  output-syntax)

(module+ reader ; syntax/module-reader protc
        (provide read-syntax))
