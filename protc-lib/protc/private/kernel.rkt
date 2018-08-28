#lang racket/base

; really specifically for #%module-begin and friends for now
(provide (except-out (all-from-out racket/base)
                     #%top
                     read read-syntax version)
         (rename-out [version racket-version]))

(module module-wrapper racket/base
  (require syntax/parse syntax/strip-context)
  (provide protc-module-wrapper)
  (define (protc-module-wrapper read-module)
    (syntax-parse (read-module)
      [(module mod-name mod-path
         (#%module-begin form ...))
       (datum->syntax this-syntax
                      (syntax-e (strip-context
                                 #'(module mod-name mod-path
                                     (#%module-begin
                                      ; fun stuff here?
                                      (provide (all-defined-out))
                                      form ...))))
                      this-syntax
                      this-syntax)])))
