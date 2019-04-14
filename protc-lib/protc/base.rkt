#lang racket/base

;A pure racket implementation of protc with no major modifications to the reader.

; see https://docs.racket-lang.org/guide/language-collection.html

(require protc/private/kernel
         protc/private/direct-model #;base
         )
#;
(provide (except-out (all-from-out protc/private/kernel) provide)
         (except-out (all-from-out protc/private/direct-model #;base) my-provide)
         (rename-out [my-provide provide])
         )
(provide (all-from-out protc/private/kernel)
         (all-from-out protc/private/provide)
         (all-from-out protc/private/direct-model))

(module reader syntax/module-reader protc/base
        ; read and read-syntax are currently not enabled
        ; because they are quite broken
        ;#:read read
        ;#:read-syntax read-syntax
        ;#:whole-body-readers? #t  ; right at the moment if you don't set this to true you get an infinite loop
        #:info (dynamic-require 'protc/get-info 'protc/base-get-info
                                (λ ()
                                  (λ (key default default-filter)
                                    (display "INFO: protc-tools are not installed there will be no error reporting")
                                    (default-filter default key)
                                    )))
        #:module-wrapper protc-module-wrapper
        (require ; protc/private/reader  ; if the reader require is included
                                         ; the reader module will be used instead of module wrapper
                 (submod protc/private/kernel module-wrapper)))
