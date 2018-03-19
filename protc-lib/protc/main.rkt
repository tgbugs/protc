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
  #`(module protc-module protc/private/expander  ; TODO this is one place that we swap could out backends
      ; for pdf/html/execution/data input OR a better strat is to
      ; have this expand to an intermediate representation
      ; from which we can then have a suite of racket functions/modules/scripts that transform to the desired output...
      ; yes, the expander here should just deal with correctness of the protocols in question
      ; rexport comes later...
      #,parse-tree))
    )
  (pretty-write (syntax->datum output-syntax))
  output-syntax)

(module+ reader ; syntax/module-reader protc
  (provide read-syntax get-info)
  (define (get-info port sourc-module source-line source-collection source-position)
    (define (handle-query key default)
      (case key
        ;[(color-lexer) (dynamic-require 'protc/colorer 'protc-color)]
        ;[(drracket:indentation) (dynamic-require 'protc/indenter 'indent-protc)]
        [(drracket:toolbar-buttons) (dynamic-require 'protc/protcheck 'button-list)]
        [else default]))
    handle-query)
  )
