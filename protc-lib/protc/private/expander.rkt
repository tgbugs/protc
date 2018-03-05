#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         protc/private/base)

(provide ;#%top
         ;#%app
         ;#%datum
         ;#%top-interaction
         (rename-out [protc-module-begin #%module-begin])
         ;(all-from-out racket/base)  ; for sexp and esexp
         (except-out (all-from-out racket/base)
                     #%module-begin)
         (all-defined-out))

(define-syntax (protc-module-begin stx)
  ;(displayln (syntax->datum stx))
  (syntax-parse stx
    [(_ ast)
     #'(#%module-begin
        ;(displayln "Y U NO WORKING")
        ;(displayln 'protc-file)
        ;(debug-repl)
        ast
        ;(define expanded (begin ast))
        ;(pretty-write expanded)
        )
    ]))

(module utility racket/base
  ; FIXME argh why do we have to do this this way...
  (require (for-syntax racket/base
                       syntax/parse))
  (provide skip-node)
           ;(for-syntax skip-node))
  (define-syntax (skip-node stx)
    (syntax-parse stx
      [(_ ast-node:id)
       #'(define-syntax (ast-node stx)
           (syntax-parse stx
             [(_ inside . rest)
              #'(begin inside . rest)]))])))

(require 'utility (for-syntax 'utility))

(define-syntax (skip-nodes stx)
  (syntax-parse stx
    [(_ to-skip ...)
     #'(begin
         (skip-node to-skip)
         ...)]))

(define-for-syntax (pass-through stx)
  ; TODO in here we need to expand identifiers before
  ; simply passing to racket/base
  (syntax-parse stx
    [(_ internal ...)
     #'(internal ...)]))

(define-syntax (esexp stx)
  (pass-through stx))

(define-syntax (sexp stx)
  (pass-through stx))

(define-syntax (racket-quote stx)
  (syntax-parse stx
    ; TODO evaluate sexpressions...
    [(_ quoted)
     #'(quote quoted)]))

(define (process-section type a-or-b [name null] [class-message null] . body)
  'section-TODO)

(define (make-section #:type type #:name name #:aspect-name aspect-name #:aspect-type aspect-type)
  `(,type ,name ,aspect-type ,aspect-name))

(define-for-syntax (do-section stx)
  ; massive TODO here
  ; need to figure out how to pass this to the section handling logic
  ; TODO FIXME for sections shouldn't we be considering making the
  ; specification of sections themselves extensible in protc?
  (syntax-case stx (section-type
                    spec
                    impl
                    section-name
                    <being>)
    [(_ (section-type spec) (<being> name) . more)
     #'(define name (string-append "(<being> #:name " (symbol->string 'name) ")"))]
    [(_ (section-type spec) (<aspect-measure aspect-name) (section-name name) . more)
     ;#'(make-section #:type (symbol->string spec)
                     ; makes no sense to do this i think...
                     ;#:aspect-type 'measure
                     ;#:aspect-name aspect-name
                     ;#:name name)]
     #'(define name (string-append "(<aspect-measure #:aspect "
                                   'aspect-name
                                   " #:name " (symbol->string 'name ) ")"))]
    [(_ (section-type spec) (aspec-param> aspect-name) (section-name name) . more)
     #'(define name (string-append "(aspect-param> #:aspect "
                                   'aspect-name
                                   " #:name " (symbol->string 'name ) ")"))]
    [(_ (section-type spec) (aspec-symbolic aspect-name) (section-name name) . more)
     #'(define name (string-append "(aspect-symbolic #:aspect "
                                   'aspect-name
                                   " #:name " (symbol->string 'name ) ")"))]
    [(_ (section-type impl) (<being> name) . more)
     ; check if behing has been speced
     #''(TODO-impl-being name more)]
    [(_ other ...) #''(TODO-not-imlemented-yet-section other ...)]))

(define-syntax (section stx)
  (do-section stx))

(define-syntax (section-lisp stx)
  (do-section stx))

(define (lookup-aspect thing-name aspect-type aspect-name)
  ; hash table vs just constructing an identifier...
  'aspect-unknown-TODO)

(define-syntax (bound-aspect stx)
  (syntax-case stx (<aspect-measure
                    aspect-param>
                    aspect-symbolic)
    [(_ thing (<aspect-measure aspect-name))
     #'(lookup-aspect thing 'meausre aspect-name)]
    [(_ thing (aspect-param> aspect-name))
     #'(lookup-aspect thing 'param aspect-name)]
    [(_ thing (aspect-symbolic aspect-name))
     #'(lookup-aspect thing 'symbolic aspect-name)]))

(define-syntax (<being> stx)
  (syntax-parse stx
    [(_ name) #'name]))

; TODO make these real things?
;(define spec "section keyword for specifying a thing")
;(define impl "section keyword for implementing a thing")

(skip-nodes
 protc-file
 ;expression
 identifier
 code-block
 ;number)
 )
