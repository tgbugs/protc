#lang racket/base

; really specifically for #%module-begin and friends for now
(provide (except-out (all-from-out racket/base)
                     #%top
                     read read-syntax version)
         (rename-out [version racket-version])
         black-box)

(define (black-box v) (error "sigh"))

(module module-wrapper racket/base
  (require racket/syntax syntax/parse syntax/strip-context)
  (provide protc-module-wrapper)
  (define (protc-module-wrapper read-module)
    (syntax-parse (read-module)
      #:disable-colon-notation
      #:datum-literals (protcur:protocol)
      #:local-conventions
      ([protocol-module-name id]
       [pid id]
       [hid id]
       [ac number]
       [did id])
      [(module mod-name mod-path
         (#%module-begin
          (~alt
           (protcur:protocol ; support for protcur:protocol -> module conversion
            protocol-module-name
            (~optional (~seq #:id pid))
            (~optional (~seq #:hid hid))
            (~optional (~seq #:anno-count ac))
            (~optional (~seq #:datasets (did ...)))
            body ...)
           form) ...))
       #:with mod-name-out (format-id #'mod-name
                                      #:source #'mod-name
                                      "protc-export-~a" (syntax-e #'mod-name))
       (datum->syntax
        this-syntax
        (syntax-e (strip-context
                   #'(module mod-name mod-path
                       (#%module-begin
                        ; fun stuff here?
                        (provide (except-out (all-defined-out)
                                             protc-module-out)
                                 (rename-out [protc-module-out mod-name-out]))
                        protc-module-end  ; implicit from provide.rkt
                        (module protocol-module-name protc/ur
                          (~? (define :id 'pid))
                          (~? (define :hid 'hid))
                          (~? (define :anno-count ac))
                          (~? (define :datasets '(did ...)))
                          body ...) ...
                        form ...
                        ))))
        this-syntax
        this-syntax)])))
