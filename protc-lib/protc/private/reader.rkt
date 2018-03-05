#lang racket/base
(require syntax/strip-context
         ;protc/private/expander
         protc/private/parser
         protc/private/tokenizer)

(require racket/pretty)

;(require protc/private/expander)

(provide
 ;(except-out (all-from-out racket/base) read-syntax)  ; covered by the expander
 ;(except-out (all-from-out protc/private/expander)
             ;read read-syntax)
 (rename-out [protc-read-syntax read-syntax]
             [protc-read read]))

(define (protc-read in-port)
  (syntax->datum
   (protc-read-syntax #f in-port)))

(define (protc-read-syntax source-name in-port)
  (displayln 'we-ar-working)
  (define output-syntax
    (with-syntax ([syntaaaaaax (read-syntax source-name in-port)])
      ;(displayln (datum->syntax #f eof))
      ;(displayln (strip-context #'syntaaaaaax))
      ;(if (eq? (datum->syntax #f eof) (strip-context #'syntaaaaaax))
          ;(strip-context #'syntaaaaaax)
      (strip-context  ; required to avoid issues with #%app for reasons I don't understand at the moment
       #`(module protc-base-module protc/private/expander
           (module configure-runtime racket/base
             (require protc/private/export)  ; TODO name consistent with the controller/overseer/evaluator
             ; protc/export/html protc/export/pdf
             ; in my-protocol.html.ptc
             ; #lang protc/export/html
             ; (require "my-protocol.ptc")
             ; boom now you have an html doc that has a PID generation
             ; chain and no random lines in a .bash_history file
             ; TODO make this configurable
             ; s-exp is the default export type
             (protc-export-type 's-exp))
           (require protc/private/export)
           ;(provide protocol)  ; TODO allow tighter control of provides
           ,syntaaaaaax
           (protc-export 'protocol)  ; more like ,syntaaaaaax
           )
       )));)
  (pretty-write (syntax->datum output-syntax))
  output-syntax)
