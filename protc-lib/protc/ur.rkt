#lang racket/base

;;; the the target language for protcur

(require protc/private/kernel
         protc/private/provide
         (only-in
          protc/private/curation
          aspect
          aspect-vary
          vary
          black-box
          black-box-component
          input
          output
          executor
          executor-verb
          input-instance
          symbolic-input
          symbolic-output
          qualifier
          objective*
          telos
          *measure
          parameter*
          invariant

          implied-aspect
          implied-input
          implied-output
          fuzzy-quantity
          )
         protc/private/curation-unprefixed
         )

(provide (all-from-out
          protc/private/curation-unprefixed
          protc/private/provide  ; FIXME required for kenel.rkt??
          )
         #%module-begin provide #%datum define list require module #%app except-out all-defined-out rename-out
         #%top-interaction #%top quote
         module->exports
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
        (require (submod protc/private/kernel module-wrapper)))
