#lang racket/base

;;; the the target language for protcur

(require protc/private/kernel
         protc/private/provide
         protc/private/curation
         protc/private/curation-unprefixed
         #;base)

(provide (all-from-out protc/private/kernel
                       protc/private/curation-unprefixed
                       protc/private/provide  ; FIXME required for kenel.rkt??
                       )
         (prefix-out protc: (all-from-out protc/private/curation)))

(module reader syntax/module-reader protc/ur
        ;#:read read
        ;#:read-syntax read-syntax
        ;#:whole-body-readers? #t
        #:info (dynamic-require 'protc/get-info 'protc/base-get-info
                                (λ ()
                                  (λ (key default default-filter)
                                    (display "INFO: protc-tools are not installed there will be no error reporting")
                                    (default-filter default key)
                                    )))
        #:module-wrapper protc-module-wrapper
        (require protc/private/reader
                 (submod protc/private/kernel module-wrapper)))
