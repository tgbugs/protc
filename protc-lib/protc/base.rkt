#lang racket/base

;A pure racket implementation of protc with no major modifications to the reader.

; see https://docs.racket-lang.org/guide/language-collection.html

(require protc/private/kernel
         protc/private/base
         )
(provide (all-from-out protc/private/kernel)
         ()
         )

(module reader syntax/module-reader protc/base
        ;#:read read
        ;#:read-syntax read-syntax
        ;#:whole-body-readers? #t
        #:module-wrapper protc-module-wrapper
        (require protc/private/reader
                 (submod protc/private/kernel module-wrapper)))
