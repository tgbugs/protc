#lang racket/base

; really specifically for #%module-begin and friends for now
(provide (except-out (all-from-out racket/base)
                     #%top
                     read read-syntax version)
         (rename-out [version racket-version]))

(module module-wrapper racket/base
  (require racket/syntax syntax/parse syntax/strip-context)
  (provide protc-module-wrapper)
  (define (protc-module-wrapper read-module)
    (syntax-parse (read-module)
      [(module mod-name mod-path
         (#%module-begin form ...))
       #:with mod-name-out (format-id #'mod-name
                                      #:source #'mod-name
                                      "protc-export-~a" (syntax-e #'mod-name))
       (datum->syntax this-syntax
                      (syntax-e (strip-context
                                 #'(module mod-name mod-path
                                     (#%module-begin
                                      ; fun stuff here?
                                      (provide (all-defined-out)
                                               (rename-out [protc-module-out mod-name-out]))
                                      protc-module-end  ; implicit from provide.rkt
                                      form ...
                                      ))))
                      this-syntax
                      this-syntax)])))
